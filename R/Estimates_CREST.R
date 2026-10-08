
#' function to wrap together all the steps needed to go from iREC analysis tool output to a
#' set of database tables
#' parameters are:
#' xls_path = file path to the source data
#' xls_file = the file name of the source data. file name needs to be month_year_ResultXXXX.xlsx
#' dbDest = the oracle database we are going to use to write the data to - P(prod) or T(Test)
#' @export
Load_C_Ests <- function(xls_pathIn, xls_fileName, dbDest, session) {
  auto_notify("Setting things up")
      # checking to see if the xls file is present with its correct name or if it is from a
      # shiny upoad - shiny names the files like 0.xlsx
      # if it is a shiny upload, rename the file to its proper name
     if (utils::file_test( "-f", xls_pathIn)) {
       file.rename(xls_pathIn, paste0(dirname(xls_pathIn), '\\', xls_fileName))
       xls_file <- xls_fileName
       xls_path <- dirname(xls_pathIn)
     } else {
       xls_file <- xls_fileName
       xls_path <- xls_pathIn
     }
   # check to see if the path ends with a directory break
   if (!grepl("[/\\\\]$", xls_path)) {
     xls_path <- paste0(xls_path,"/")
   }

    dbCon <- setupOracleConn(dbDest)
  # list of sheetname to pull
  SHEET_NAMES <- c("Detailed Estimated Catch",
                   "Summary Total Variance",
                   "Survey Individual Variance")
  # break the filename down into componets where we can fid the year and month
  inds <- strsplit(str_to_upper(xls_file), "_")[[1]]
  yr <- inds[[2]]
  mnth_str <- inds[[1]]
  # build the fiscal year name
  if (mnth_str %in% c('January', 'February', 'March', 'Jan', 'Feb', 'Mar')) {
    yr_name <- paste0(as.numeric(yr) - 1, '-' , as.numeric(substr(yr, 3, 4)))
  } else {
    yr_name <- paste0(yr, '-' , as.numeric(substr(yr, 3, 4)) + 1)
  }
  auto_notify("Storing the source data")
  # call the function to store the xlsx file in the database
  src_id <- StoreSource(dbCon, xls_file, xls_path, yr_name, mnth_str)
  # call the function to load the contents of the xlsx file
  auto_notify("Loading the excel sheet")
  rslt <- loadXLSX_CREST(xls_path, xls_file, SHEET_NAMES)
  # call the function to translate the raw analysis output into database tables
  auto_notify("formatting the data")
  rslt <- formatdata(rslt, SHEET_NAMES, inds, src_id, dbCon)
  # call the function to send the formatted data to the data base
  auto_notify("Sending to CREST")
  success <- SendToCREST(rslt, dbCon)

  return(success)
}

#' Function to read selected sheets from and XLSX file into 1 data frame for each
#' parameters are:
#' xls_path = the path to the file
#' xls_file = the xlsx file name
#' SHEET_NAMES = a list of named tabs in the XLSX file to load
#' returns a list of data frames named for the xlsx tab
loadXLSX_CREST <- function (xls_path, xls_file, SHEET_NAMES) {
  fn <- paste0(xls_path, xls_file)
  sheet_list <- as.list(rep(NA, length(SHEET_NAMES)))

  if (file.exists(fn)) {
    for (sn in 1:length(SHEET_NAMES)) {
      auto_notify(SHEET_NAMES[sn])
      sheet_list[[sn]] <- openxlsx::read.xlsx(fn, sheet = SHEET_NAMES[sn])
      names(sheet_list)[sn] <- SHEET_NAMES[[sn]]
      auto_notify("Done")
    }

  }
  return(sheet_list)
}

#' Function to translate the XLSX output from the iREC analysis into
#' normalized data tables in CREST
#' parameters are:
#' dfs = list of dataframes holding the detailed estimates, detailed variances and summarized variances
#' SHEET_NAMES = list of the XLSX file tab names for the data
#' inds = a list of the comonents for the xlsx filename split on the "-" character to determin the month and year the estimates
#' represent
#' src_id = the primary key of the creel_irec_source table where the xlsx file is stored
#' dbCon = an oracle connection to the database destination
formatdata <- function(dfs, SHEET_NAMES, inds, src_id, dbCon) {
  # list of look up tables to load from CREST to link to the source data for codes
  lu_tbls <- c(
    "creel_months",
    "creel_irec_draw",
    "CREEL_IREC_RES_STATUS",
    "CREEL_IREC_LICENCE",
    "CREEL_IREC_AGE_CATEGORY",
    "creel_irec_area",
    "creel_crest_lrg_Area",
    "CREEL_IREC_METHOD",
    "creel_yn",
    "creel_irec_shellfish_t",
    "CREEL_IREC_SHELLFISH",
    "creel_irec_purchasep",
    "CREEL_IREC_ITEM"
  )
  # pass the list of look up tables to a function to pull them
  luts <- load_CE_lus(dbCon, lu_tbls)
  # the crabs vs prawns vs crabs and prawns flag has changed over time this allows old format data to be loaded with
  # the latest crab vs prawn status
  sftc <- luts$CREEL_IREC_SHELLFISH_T[luts$CREEL_IREC_SHELLFISH_T$SOURCE=="CRABS",] %>% select(-c(SHELLFISH_ID, SOURCE)) %>%
    rename("CRAB_TEXT" = "TEXT",
           "CRAB_VALUE" = "VALUE")
  sftp <- luts$CREEL_IREC_SHELLFISH_T[luts$CREEL_IREC_SHELLFISH_T$SOURCE=="PRAWNS",] %>% select(-c(SHELLFISH_ID, SOURCE)) %>%
    rename("PRAWN_TEXT" = "TEXT",
           "PRAWN_VALUE" = "VALUE")

  cnt <- 0
  # we need to standardize all three sheets and bring them up to current standards
  rslt <- as.list(rep(NA, length(SHEET_NAMES)))
  for (nme in SHEET_NAMES) {
    cnt <- cnt + 1
    xlTable <- dfs[[nme]]

    # Make column names uppercase just to simpliy things later on
    names(xlTable) <- str_to_upper(colnames(xlTable))
    # drop any rows with no "Area" value
    xlTable <- xlTable[!is.na(xlTable$AREA), ]
    # first step in standardizing is to add the year, month, a row number and a reference to the creel_irec_source table
    # where the xlsx file is stored
    xlTable <- xlTable %>%
      dplyr::mutate(MONTH = as.character(Expmonth(inds[1])), .before = 1) %>%
      dplyr::mutate(YEAR = as.integer(inds[2]), .before = 1)  %>%
      dplyr::mutate(TMP_ID = as.integer(row_number()), .before = 1)  %>%
      dplyr::mutate(SOURCE_ID_ID = as.integer(src_id), .before = 1) %>%
      # second step is to code the lodge_id_id and guide_id_id based on all possible inputs used
      # in all variations of the data
      dplyr::mutate(
        LODGE_ID_ID = case_when(
          "Yes" == LODGE ~ -1,
          "1" == LODGE ~ -1,
          "-1" == LODGE ~ -1,
          "No" == LODGE ~ 0,
          "2" == LODGE ~ 0,
          "0" == LODGE ~ 0,
          .default = -9
        )
      ) %>%
      dplyr::mutate(
        GUIDED_ID_ID= case_when(
          "Yes" == GUIDED ~ -1,
          "1" == GUIDED ~ -1,
          "-1" == GUIDED ~ -1,
          "No" == GUIDED ~ 0,
          "2" == GUIDED ~ 0,
          "0" == GUIDED ~ 0,
          .default = -9
          )
      )
    # this block checks the column names for missing columns (added over time)
    # and adds them with default valued if they are missing
      col_names <- str_to_upper(colnames(xlTable))
      if ("DRAW_TYPE" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, DRAW_TYPE = as.character("UNSP"))
      }
      if ("VLIC_TOTAL" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, VLIC_TOTAL = as.integer(0))
      }
      if ("ELIC_TOTAL" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, ELIC_TOTAL = as.integer(0))
      }
      if ("RESPONDENT_TOTAL" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, RESPONDENT_TOTAL = as.integer(0))
      }
      if ("CHECKCRABS" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, CHECKCRABS = as.character("NULL"))
      }
      if ("CHECKPRAWNS" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, CHECKPRAWNS = as.character("NULL"))
      }
      if ("CHECKCRABSPRAWNS" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, CHECKCRABSPRAWNS = as.character("NULL"))
      }
      if ("PURCHASE_PERIOD" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, PURCHASE_PERIOD = as.character("Not Specified"))
      }
      if ("LODGE" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, LODGE = as.character("Unspecified"))
      }
      if ("GUIDED" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, GUIDED = as.character("Unspecified"))
      }
      if ("RESIDENT_STATUS" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, RESIDENT_STATUS = as.character("Unspecified"))
      }
      if ("LICENCE_TYPE" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, LICENCE_TYPE = as.character("Unspecified"))
      }
      if ("AGE_CATEGORY" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, AGE_CATEGORY = as.character("Unspecified"))
      }
      if ("PURCHASE_PERIOD" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, PURCHASE_PERIOD = as.character("Unspecified"))
      }
      if ("LIC_TOTAL" %notin% col_names) {
        xlTable <- dplyr::mutate(xlTable, LIC_TOTAL = as.integer(0))
      }
    # this block names the tables in a more simplified manner
    if (cnt == 1) {
      CREEL_IREC_TMP <- xlTable
    }
    if (cnt == 2) {
      CREEL_IREC_TMP_V <- xlTable
    }
    if (cnt == 3) {
      CREEL_IREC_TMP_DV <- xlTable
    }
    rslt[[cnt]] <- xlTable
    names(rslt)[[cnt]] <- nme
  }

  rslt_out <- as.list(6)
  cnt <- 0
  # this block links the lookup tables from CREST to use their _ID codes to store, rather then free form text
  # this is done for all three sheets
  # this also splits the strata from the values to normalize the data for better use in the database
  for (DF_name in names(rslt)) {
    auto_notify(DF_name)
    cnt <- cnt + 1
    # here we do the joins and clean up so we don't end up with duplicate column names
    # we also rename sole columns to match the end tables result tables in CREST
   tbl <- rslt[[DF_name]] %>%
      left_join(luts$CREEL_MONTHS, join_by("MONTH"=="MONTH_STR")) %>%
      left_join(luts$CREEL_IREC_DRAW, join_by("DRAW_TYPE"=="TEXT")) %>%
      left_join(luts$CREEL_IREC_RES_STATUS, join_by("RESIDENT_STATUS"=="TEXT")) %>%
      left_join(luts$CREEL_IREC_LICENCE, join_by("LICENCE_TYPE"=="TEXT")) %>%
      left_join(luts$CREEL_IREC_AGE_CATEGORY, join_by("AGE_CATEGORY"=="TEXT"))%>%
      left_join(luts$CREEL_IREC_AREA, join_by("AREA"=="TEXT")) %>%
      left_join(luts$CREEL_CREST_LRG_AREA, join_by("LRG_AREA_ID_ID"=="LRG_AREA_ID")) %>%
      left_join(luts$CREEL_IREC_METHOD, join_by("METHOD"=="TEXT")) %>%
      rename("METHOD_ID_ID" = "METHOD_ID") %>%
      left_join(luts$CREEL_IREC_PURCHASEP, join_by("PURCHASE_PERIOD"=="TEXT"))  %>%
      rename("PURCHASE_ID_ID" = "PURCHASE_ID") %>%
      left_join(sftc, join_by ("CHECKCRABS"=="CRAB_TEXT"))  %>%
      left_join(sftp, join_by ("CHECKPRAWNS"=="PRAWN_TEXT"))  %>%
      left_join(luts$CREEL_IREC_SHELLFISH %>% select(-c(SHELLFISH_ID, SHELLFISHING)), join_by("CHECKCRABSPRAWNS"=="TEXT")) %>%
      rename("CRAB_PRAWN_VALUE" = "VALUE") %>%
      mutate(CHECKCRABPRAWSTOT = ifelse(CHECKCRABSPRAWNS == 'NULL', CHECKCRABS + CHECKPRAWNS, CRAB_PRAWN_VALUE)) %>%
      left_join(luts$CREEL_IREC_SHELLFISH, join_by("CHECKCRABPRAWSTOT"=="VALUE")) %>%
      rename("SHELLFISH_ID_ID" = "SHELLFISH_ID",
             "AGE_CATEGORY_ID_ID" = "AGE_CATEGORY_ID",
             "LICENCE_TYPE_ID_ID" = "LICENCE_TYPE_ID",
             "RESIDENT_STATUS_ID_ID" = "RESIDENT_STATUS_ID",
             "DRAW_ID_ID" = "DRAW_ID",
             "MONTH_STRING" = "MONTH",
             "MONTH" = "MONTH.y")

   # identify which columns form the strat of each the estimate.
   # these are all present in the detailed estimates/variances, but dummied in for the summarized variances
   # this is just to allow us to not duplicate most of the code, or look for each field before linking and selecting on
    est_clms <- c("TMP_ID", "YEAR", "MONTH", "DRAW_ID_ID", "RESIDENT_STATUS_ID_ID", "LICENCE_TYPE_ID_ID",
                  "AGE_CATEGORY_ID_ID", "VLIC_TOTAL", "ELIC_TOTAL", "LIC_TOTAL", "RESPONDENT_TOTAL",
                  "LRG_AREA_ID_ID", "METHOD_ID_ID", "LODGE_ID_ID", "GUIDED_ID_ID", "SOURCE_ID_ID", "SHELLFISH_ID_ID", "PURCHASE_ID_ID"
    )

    # this builds the strata part of the pair.  It just pulls out the columns that define the uniqueness of the data
    IREC_EST <-tbl[est_clms]
    # these are the columns found in the data that are not needed going forward, mostly from the look-up joins
    # pulling in unnecessary columns
    drp_clmns <- c("MONTH_STRING", "RESIDENT_STATUS", "LICENCE_TYPE", "AGE_CATEGORY", "PURCHASE_PERIOD",
                   "AREA", "LODGE", "GUIDED", "CHECKCRABSPRAWNS","DRAW_TYPE", "CHECKCRABS", "CHECKPRAWNS", "MONTH_STR_SHRT",
                   "AREA_ID", "LRG_AREA_NME", "PROGRAM_ID_ID", "DESCRIPTION", "LRG_AREA_TYPE", "FGUID", "FGN_ID", "LONGITUDE_C",
                   "LATITUDE_C", "UPDATED" , "UPDATEDBY",  "DEPRECATED", "DEPRECATED_CMNT", "DESCRIPTION_ALT",  "LRG_AREA_NME_ALT",
                   "DESCRIPTION_GRP", "LRG_AREA_NME_2", "MRP_CODE_ID_ID", "LASTEDIT", "REGION", "REPORT_DESCRIP", "ALTTEXT.x",
                   "GEAR", "GROUP",  "KREST_CDE", "ALTTEXT.y",  "CRAB_VALUE", "PRAWN_VALUE", "CRAB_PRAWN_VALUE", "CHECKCRABPRAWSTOT",
                   "TEXT" , "SHELLFISHING","SURVEY_YEAR", "SURVEY_MONTH" )
    # this essentially duplicates the strata column name list, but excludes the "TMP_ID" column as that is needed for the
    # item values data to link to each strata
    strataCols <- est_clms[!est_clms %in% c('TMP_ID')]
    # this pulls the columns that are not part of the strata, but also not part of the drp_clmns list
    iVals <-tbl[!names(tbl) %in% c(strataCols, drp_clmns)]
    # this builds a list of the column names excluding TMP_ID and METHOD
    itmCols <- names(iVals)[!names(iVals) %in% c("TMP_ID", "METHOD")]
    # this pivots the columns to rows.  Each column becomes a row with CAT = column name and VALUE = cell value
    # these are forced into characters.  Presumably so it doesn't crash on characters sneaking in
    iValsP <- iVals %>%
      pivot_longer(all_of(itmCols),names_to = "CAT",
                   values_to = "VALUE",
                   values_transform = list(VALUE = as.character))

    # this links the CAT (column names) to the CREEL_IREC_ITEM table which
    # fully describes the data - species/disposition/etc
    # also adds a row id
    iValsPI <- iValsP  %>%
      mutate(CATU = str_to_upper(CAT)) %>%
      left_join(luts["CREEL_IREC_ITEM"][[1]] %>%
                  mutate(COLUMN_NAMEU = str_to_upper(COLUMN_NAME)),
                join_by("CATU" == "COLUMN_NAMEU"))  %>%
      dplyr::mutate(VALUE_ID = as.integer(row_number()), .before = 1) %>%
      rename('ITEM_ID_ID' = 'ITEM_ID')

    # here we want to dump the many many many 0's except for Chinook, Coho, Halibut and lingcod - but only for angleboat method
    # so the don't end up as nulls which have in the past been interpreted as not estimated and people go looking for estimates
    # somewhere else.
    iValsPI <- iValsPI[iValsPI$VALUE > 0 | (iValsPI$TEXT %in% c('CHINOOK','COHO','HALIBUT','LINGCOD') & iValsPI$METHOD == 'angleboat'),]
    # pull just the important columns out
    iValsPIC <- iValsPI[c('VALUE_ID', 'TMP_ID', 'ITEM_ID_ID', 'VALUE', 'TEXT')]
    # Store the strata into a list
    rslt_out[[cnt]] <- IREC_EST
    # name the list item so we can find it later
    names(rslt_out)[[cnt]] <- paste0(DF_name,"_STRATA")
    cnt <- cnt + 1
    # do the same for the values
    rslt_out[[cnt]] <- iValsPIC
    names(rslt_out)[[cnt]] <- paste0(DF_name,"_VALUES")
  }

  # This last step takes all 6 data frames and finishes them off.
  # The detailed estimates get linked with the detailed variances and stored together
  # the Summary variances need (coould probably bodge this in some how) to get stored in
  # their own tables as they don't have the same fields

  # first we extract the data from the list so it is easier to work with and "fix" the column names
  # to match the database tables and style
  D_est <- rslt_out$`Detailed Estimated Catch_STRATA` %>% rename("IREC_EST_ID" = "TMP_ID")
  D_value <- rslt_out$`Detailed Estimated Catch_VALUES` %>% rename("IREC_EST_ID" = "TMP_ID")

  S_var <- rslt_out$`Summary Total Variance_STRATA` %>% rename("IREC_VAR_ID" = "TMP_ID")
  S_var_var <- rslt_out$`Summary Total Variance_VALUES`%>% rename("IREC_VAR_ID_ID" = "TMP_ID", "VAR_ID" = "VALUE_ID")

  D_est_var <- rslt_out$`Survey Individual Variance_STRATA` %>% rename("EST_VAR_ID" = "TMP_ID")
  D_est_var_value <- rslt_out$`Survey Individual Variance_VALUES` %>% rename("EST_VAR_ID_ID" = "TMP_ID")

  #  Collect up the column names to use in linking the detailed estimates to their variances
  # the various respondent numbers are not really in both datasets so we exclude them
  joinclmns <- names(D_est)[!names(D_est) %in% c("IREC_EST_ID","VLIC_TOTAL" ,"ELIC_TOTAL" ,"LIC_TOTAL" ,"RESPONDENT_TOTAL" )]
  # Join the estimate strata to the variance strata
  ev_s <- D_est %>% left_join(D_est_var[c("EST_VAR_ID", joinclmns)], by=joinclmns)
  # we only need the PK IDs of each table for the next step
  ev_s <- ev_s[c("IREC_EST_ID","EST_VAR_ID")]
  # link the matched strata IDs to the detailed variance individual values
  D_est_var_value_e <- ev_s %>% left_join(D_est_var_value, join_by("EST_VAR_ID" == "EST_VAR_ID_ID")) %>% rename("VARIANCE" = "VALUE")
  # link those records to the individual estimates based on ID and the item(id)
  D_value <- D_value %>% left_join(D_est_var_value_e, join_by("IREC_EST_ID" == "IREC_EST_ID", "ITEM_ID_ID"))
  # query CREST for the last ID used in the estimate table
  est_id <- getdata(dbCon, "select max(IREC_EST_ID) as id from otolith_v1.creel_IREC_EST")
  # populate a matching column in the data frame indexed by the row id (number)
  D_est$IREC_EST_ID <- D_est$IREC_EST_ID + est_id$ID
  # do the same for the value table so the link will persist into the database
  D_value$IREC_EST_ID_ID <- D_value$IREC_EST_ID + est_id$ID
  # query CREST for the last ID used in the estimate values table
  val_id <- getdata(dbCon, "select max(VALUE_ID) as id from OTOLITH_V1.CREEL_IREC_VALUE")
  # this is the same as above
  D_value$VALUE_ID <- D_value$VALUE_ID.x + val_id$ID
  # this pulls only the needed columns and renames one jacked in the linking
  D_value <- D_value[c("VALUE_ID", "IREC_EST_ID_ID", "ITEM_ID_ID", "VALUE", "TEXT.x", "VARIANCE")] %>%
    rename("TEXT" = "TEXT.x")
  # again get the last ID used from the summary variance table
  var_id <- getdata(dbCon, "select max(IREC_VAR_ID) as id from otolith_v1.CREEL_IREC_VAR")
  # populate the keys so we can link in the database
  S_var$IREC_VAR_ID <- S_var$IREC_VAR_ID+var_id$ID
  S_var <- S_var[c("IREC_VAR_ID", "YEAR", "MONTH", "LRG_AREA_ID_ID", "METHOD_ID_ID", "SOURCE_ID_ID")]
  S_var_var$IREC_VAR_ID_ID <- S_var_var$IREC_VAR_ID_ID + var_id$ID

  var_val_id <- getdata(dbCon, "select max(VAR_ID) as id from otolith_v1.CREEL_IREC_VAR_VALUE")
  S_var_var$VAR_ID <- S_var_var$VAR_ID + var_val_id$ID

  # pack the final steps into a list to return

  finalrslt <- list(D_est = D_est, D_value = D_value, S_var = S_var, S_var_var = S_var_var)
  auto_notify("Done formatdata")
  return(finalrslt)
}


#' Function to take the fully formated estimate data and push it to the database (CREST)
#' we use the dbWriteTable function which is crazy faster than individual inserts, but all columns in the dataframe must match
#' the fields in teh database table exactly
#' parameters are:
#' rslts = list containing the 4 tables creel_irec_Est, creel_irec_value, creel_irec_var, creel_irec_var_value
#' Crestcon = oracle connection
SendToCREST <- function(rslts, Crestcon){

  result <- tryCatch({
    # Attempt to write the data frame to the database
    ROracle::dbWriteTable(con = Crestcon, name = "CREEL_IREC_EST", value = rslts$D_est, append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
    ROracle::dbWriteTable(con = Crestcon, name = "CREEL_IREC_VALUE", value = rslts$D_value, append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
    ROracle::dbWriteTable(con = Crestcon, name = "CREEL_IREC_VAR", value = rslts$S_var, append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
    ROracle::dbWriteTable(con = Crestcon, name = "CREEL_IREC_VAR_VALUE", value = rslts$S_var_var, append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
    dbCommit(Crestcon)
    auto_notify("Success! Table written to database.")
    TRUE
  }, error = function(e) {
    # Handle the error and log the message
    auto_notify(paste("Database write failed:", e$message))
    dbRollback(Crestcon)
    FALSE
  }, warning = function(w) {
    # Handle warnings (e.g., truncation or data type coercion)
    auto_notify(paste("Warning during write:", w$message))
    invokeRestart("muffleWarning")
  })
  return(result)
}

#' Function to zip up the estiamte xlsx file and store it in the database
#' returning the source_id that was used
#' prameters are:
#' dbCon = oracle connection with write access to IREC tables
#' datafile = name of the XLSX file to store
#' datpath = path to the XLSX file to store
#' yr_name = the name of the fiscal year the estimates are part of
#' mnth_str = the name of the month the estimates are for
StoreSource <- function (dbCon, datafile, datapath, yr_name, mnth_str){

  tryCatch(
    expr = {
      # make the temporary zip file
      tmpfile <- tempfile(fileext = ".zip")
      zip::zipr(
        zipfile = tmpfile,
        files = c(paste0(datapath, datafile)),
        recurse = FALSE,
        include_directories = FALSE
      )
      if (file.exists(tmpfile)) {
        # load the binary contents into a variable
        srcbytes <- read_file_bytes(tmpfile)
        # Delete file if it exists
        # clean up the temp file
        file.remove(tmpfile)
      }

      # clear out the record if the file has been loaded before
      auto_notify("clear out the record if the file has been loaded before")
      tsql <- "Delete from otolith_V1.creel_irec_Source where filename = :filename and data_type = 'Estimate'"
      rslt <- ROracle::dbSendQuery(dbCon, tsql, list(filename = datafile))
      dbClearResult(rslt)
      dbCommit(dbCon)

      inds <- strsplit(str_to_upper(datafile), "_")[[1]]
      yrs <- inds[[2]]
      mnth <- match(toupper(inds[[1]]), toupper(month.name))
      tsql <- "Delete from otolith_V1.creel_irec_Est where year = :yr and month = :mth"
      auto_notify("Sending delete for estimates")
      rslt <- ROracle::dbSendQuery(dbCon, tsql, list(yr = yrs, mth = mnth))
      dbClearResult(rslt)
      auto_notify("Sending delete for summary variances")
      tsql <- "Delete from otolith_V1.CREEL_IREC_VAR where year = :yr and month = :mth"
      rslt <- ROracle::dbSendQuery(dbCon, tsql, list(yr = yrs, mth = mnth))
      dbClearResult(rslt)
      dbCommit(dbCon)


      # lookup the next source_id value
      # and then build the sql to send the data - using bind variables
      src_id <- dbGetQuery(dbCon,
                           "select max(source_id) + 1 as src_id from otolith_V1.creel_irec_source")[[1]]
      sql <- "
  INSERT INTO OTOLITH_V1.CREEL_IREC_SOURCE
    (source_id, filename, data_type, description, label, date_added)
    values(:p_id, :p_name, :p_datatype, :p_description, :p_label, sysdate)"

      # 3. Bind variables
      binds <- list(
        p_id     = src_id,
        p_name   = datafile,
        p_datatype = 'Estimate',
        p_description = datapath,
        p_label = paste0(yr_name, ' - ', mnth_str, ' - iREC Estimates')
      )
      data2 <- data.frame(p_data = 1)
      data2$p_data <- list(c(srcbytes))
      data2$src_id <- src_id
      auto_notify("Sending the source framework")
      rslt <- dbGetQuery(dbCon, sql, binds)
      # 5. Commit the changes
      dbCommit(dbCon)
      auto_notify("push the zipfile bytes using an update query.")
      # push the zipfile bytes using an update query.  For some reason I couldn't get it to work in 1 statement
      rslt2 <- dbGetQuery(
        dbCon,
        "update otolith_V1.creel_irec_source set data = :p_data where source_id = :src_id",
        data2
      )
      dbCommit(dbCon)
      # return the source_id
      return(src_id)
    },
    # error trapping - will return -99 as the source_id
    error = function(e) {
      warning(paste("An error occurred during dbFetch:", conditionMessage(e)))
      return(-99)
    }
  )


}

