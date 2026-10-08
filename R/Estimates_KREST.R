
#' function to read in all the sheets in and excel file
#' parameters are:
#' xlsxFile = file with path to the excel file to read
#' returns a named list of all the sheets
read_all_sheets <- function(xlsxFile, ...) {
  sheet_names <- openxlsx::getSheetNames(xlsxFile)
  sheet_list <- as.list(rep(NA, length(sheet_names)))
  names(sheet_list) <- sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] <- openxlsx::read.xlsx(xlsxFile, sheet=sn, ...)
  }
  return(sheet_list)
}

#' function to load a set of csv files into a list of dataframes
#' 3 of the files will be "refreshed" from the CREST database if there is
#' a known connection preexisting
#' parameters are:
#' none
#' return a list of dataframes
loadCSVs <- function() {
  cnt <- 0
  csvs <- c("KrestAreaLookup.csv","CRESTItemLookup.csv", "KRESTMethodLookup.csv")
  rslt <- as.list(rep(NA, length(csvs)+1))
  for (c in csvs) {
    cnt = cnt + 1
    if (exists('Lcon')){
      data_path <- system.file("extdata", paste0(tools::file_path_sans_ext(c), ".sql"), package = "iRECUIpkg")
      qry <- read_file(data_path)
      tbl <- getdata(Lcon, qry)
      data_path <- system.file("extdata", c, package = "iRECUIpkg")
      write_csv(tbl, file =data_path)
    }
    data_path <- system.file("extdata", c, package = "iRECUIpkg")
    df <- read.csv(data_path,sep=",")
    rslt[[cnt]] <- df
  }
  data_path <- system.file("extdata", "months.csv", package = "iRECUIpkg")
  rslt[[4]] <- read.csv2(data_path,sep=",")
return(rslt)
}

#' function to load iREC estimates into dataframes
#' also to perform some standardization as the iREC analysis output has changed over time
#' this functions to make the source data equal to the current analysis output
#' this can use a file on the fil system or pull the file from the CREST database table
#' OTOLITH_V1.CREEL_IREC_SOURCE - this extra function is useful for transferring the
#' already published estimates in CREST to KREST without finding the source xlsx files
#' paramaters are:
#' xls_file = the filename with no path
#' xls_paths = a list of paths to search for the file named above with trailing slash. - Not sure why i did it this way :-P
#' inds = the components of the file name in the form Month_Year_... like "May_2026_Result_20260723_092514.xlsx"
#'       this is to readily identify the time period the data represents.  The iREC analysis output does not identify the year or the month
#'       it is only in the file name
#' SHEET_NAMES = a list of the sheet(tabs) names to pull for the analysis
#' source_id = the primary key of the record in the table OTOLITH_V1.CREEL_IREC_SOURCE to fetch the source excel file from
loadXLSX_KREST <- function (xls_file,  xls_paths, inds,  SHEET_NAMES,  source_id) {
  rslt <- as.list(rep(NA, length(SHEET_NAMES)))
  # if the source_id = 0 pulling from filesystem file
  if (source_id == 0) {
    # make a dummy value for the xls_path incase we can't find the file
    xls_path <- 'XXX'
    # loop throught the list of paths
    for (pth in xls_paths) {
      # if the path provided actually includes the file name - go with that
      if (endsWith(tolower(pth), ".xlsx")) {
        fn <- pth
      } else {
        # otherwise add the path and the filename
        fn <- paste0(pth, xls_file)
      }
      # check to see if the file exists in the given path
      if (file.exists(fn)) {
        # if found break out of the loop
        xls_path <- pth
        break
      }
    }
    # continue as long as we found the file in one of the paths
    if (xls_path != 'XXX') {
      #check to see if the path has a file name already attached otherwise add the path to the filename and
      # call real_all_sheets passing the filename with path
      if (basename(xls_path) != '') {
        dfs <- read_all_sheets(fn)
      } else {
        dfs <- read_all_sheets(paste0(xls_path, xls_file))
      }
    }
  } else {
    # get the data from the CREST database
    dfs <- LoadxlsxFromCREST(source_id)
  }


  # check to see if the list of dataframes exists otherwise return with an empty list
  if (exists("dfs")) {
    cnt <- 0
    rslt <- as.list(rep(NA, length(SHEET_NAMES)))
    # we need to ensure each sheet is up to standards, so loop though the list SHEET_NAMES
    for (nme in SHEET_NAMES) {
      cnt <- cnt + 1
      xlTable <- dfs[[nme]]
      xlTable <- xlTable[!is.na(xlTable$area), ]
      xlTable <- xlTable %>%
        dplyr::mutate(MONTH = as.character(Expmonth(inds[1])), .before = 1) %>%  # add the month to the dataframe
        dplyr::mutate(YEAR = as.integer(inds[2]), .before = 1)  %>%              # add the year
        dplyr::mutate(tmp_id = as.integer(row_number()), .before = 1)  %>%       # add a rownumber so we can link once we break things up
        dplyr::mutate(source_id = as.integer(1), .before = 1) %>%                # add a placeholder for the source_id
        dplyr::mutate(
          lodge = case_when(
            # standardiaze the lodge column to Yes/No
            "Yes" == lodge ~ 'Yes',
            "1" == lodge ~ 'Yes',
            "-1" == lodge ~ 'Yes',
            "No" == lodge ~ 'No',
            "2" == lodge ~ 'No',
            "0" == lodge ~ 'No',
            .default = 'Unspecified'
          )
        ) %>%
        dplyr::mutate(
          # standardiaze the guided column to Yes/No
          guided = case_when(
            "Yes" == guided ~ 'Yes',
            "1" == guided ~ 'Yes',
            "-1" == guided ~ 'Yes',
            "No" == guided ~ 'No',
            "2" == guided ~ 'No',
            "0" == guided ~ 'No',
            .default = 'Unspecified'
          )
        )
      # check all the expected fields exist in the detailed estimate sheets add them if necessary and set the default values
      if (nme %in% c('Detailed Estimated Catch', 'Survey Individual Variance')) {
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
      }
      # I think this is redundant?
      if (cnt == 1) {
        CREEL_IREC_TMP <- xlTable
      } else if (cnt == 2) {
        CREEL_IREC_TMP_V <- xlTable
      } else if (cnt == 3) {
        CREEL_IREC_TMP_DV <- xlTable
      }
      # add the separate tables to a list to be returned.  Probably would be more robust to name them, rather than rely on the
      # order
      rslt[[cnt]] <- xlTable
    }
  }
  return (rslt)
}

#' function to load and fully format the iREC estimates - Calls loadXLSX to actually load the data and do some of the standardizing
#' but this does the heavy lifting in terms of formatting
#' parameters are:
#' inds = the components of the file name in the form Month_Year_... like "May_2026_Result_20260723_092514.xlsx"
#' xls_file = the name of the excel sheet to use
#' xls_path = a list of paths to search for the file in
#' SHEET_NAMES = the named tabs that holds the data wee need
#' source_id = the primary key of the record in the table OTOLITH_V1.CREEL_IREC_SOURCE to fetch the source excel file from
#' returns a list of data products:
#' rslt position one <- TRUE - presumably a flag that everything worked
#' rslt position two <- t_est_g - detailed estimates with variance formatted for KREST
#' rslt position three <- xls_file - the name of the file the estimates came from
#' rslt position four <- csvs - the look-up tables used
#' rslt position five <- St_est_g - The summary estimates with variance formatted for KREST
getandformatKData <- function(inds, xls_file, xls_path, SHEET_NAMES, source_id) {
  # Load some look up tables we need to normalize the data against and make some adjustments for maybe tuna?
  # the order is important - probably should have been named, but also the order shouldn't need to change.
  csvs <- loadCSVs()
  areas <- csvs[[1]]
  citems <- csvs[[2]]
  methods<- csvs[[3]]
  months <- csvs[[4]]
  citems$SPECIES[citems$SPECIES == '30'] <- '369' # This may be Tunas   - Need to confirm
  citems$COLUMN_NAME <- tolower(citems$COLUMN_NAME)

  # call loadXLSX to pull the necessary sheets
  tbls <- loadXLSX_KREST(xls_file, xls_path, inds, SHEET_NAMES, source_id)
  # break out the dataframes from the list returned - Order specific
  dec <- tbls[[1]] # This is the detail estimates
  stv <- tbls[[2]] # This is the summary variances
  siv <- tbls[[3]] # This is the detail variances

  # Build and empty return set
  rslt <- as.list(rep(NA, 4))
  # link the areas tables to the 3 dataframes
  dec <- left_join(dec, areas, by = c('area' = 'TEXT'))
  stv <- left_join(stv, areas, by = c('area' = 'TEXT'))
  siv <- left_join(siv, areas, by = c('area' = 'TEXT'))

  # Check areas link and OK skipping any mismatches.  Should probably just fail if they don't match
  areachk <- dec[!is.na(dec$area) & is.na(dec$LRG_AREA_ID), ] %>%
    dplyr::group_by(area) %>%
    dplyr::summarize(cnt = n(), .groups = 'drop')
  if (nrow(areachk) > 0) {
    auto_notify("Whoops")
    auto_notify("Can't find these areas in the DB")
    auto_notify(paste0(areachk$area, " "))
    answr <- readline(prompt = "Skip them and continue? (Y/N)")
    if (str_to_upper(answr) %notin% c('Y')){
      return(FALSE)
    } else {
      auto_notify("Continuing anyway")
    }
  }
  # Done area Check
  # Check items are good - Checks the column names against the item look up excluding any columns not an estimated item
  exc_clmns <- c('AREA_ID', 'ESTIMATIONAREA', 'LRG_AREA_ID', 'LRG_AREA_NME', 'SML_AREA_NME', 'STD_REF_TYPE_CDE',
  'SOURCE_ID', 'TMP_ID', 'YEAR', 'MONTH', 'MONTH_NUM', 'RESIDENT_STATUS', 'LICENCE_TYPE', 'AGE_CATEGORY', 'AREA',
  'METHOD', 'LODGE', 'GUIDED', 'CHECKCRABS', 'CHECKPRAWNS', 'DRAW_TYPE', 'VLIC_TOTAL', 'ELIC_TOTAL', 'LIC_TOTAL',
  'RESPONDENT_TOTAL', 'CHECKCRABSPRAWNS', 'PURCHASE_PERIOD', 'METHOD_ID', 'KREST_CDE', 'PROGRAM',"PROJECT_GRP_LNG",
  "ADMINISTRATIVE_AREA","survey_month", "survey_year")
  # this builds a dataframe where each row is a column name not in the list above
  itemchk <- dec[,!tolower(names(dec)) %in% tolower(exc_clmns)] %>% names() %>% as_tibble()
  # join this to the items we know
  itemchk <- left_join(itemchk, citems, by=c("value"="COLUMN_NAME"))
  # Find the records where there is no match to the known items
  itemchk <- itemchk[is.na(itemchk$DB_ITEM), ] %>%
    group_by(value) %>%
    dplyr::summarize(cnt = n(), .groups = 'drop')
  # Done item checks
  # if the result has any records, we know there are estimates we don't know what they are for.
  # again we ask if we should skip - Should probably be a fail
  if (nrow(itemchk) > 0) {
    auto_notify("Whoops")
    auto_notify("Can't find these items in the DB")
    auto_notify(paste0(itemchk$value, " "))
    answr <- readline(prompt = "Skip them and continue? (Y/N)")
    if (str_to_upper(answr) %notin% c('Y')){
      return(FALSE)
    } else {
      auto_notify("Continuing anyway")
    }
  }
  # join the months and methods lookups to codify them later
  dec <- left_join(dec, months, by = "MONTH")
  dec <- left_join(dec, methods, by = c('method'="TEXT"))
  # put the method code in as the DATATYPE field
  dec$DATATYPE <- dec$KREST_CDE
  # add estimateion start and end dates
  dec$EstimationPeriodStart <- make_datetime(year = dec$YEAR, month = dec$month_num, day = 1, hour=0, min=0)
  dec$EstimationPeriodEnd <- ceiling_date(make_date(year = dec$YEAR, month = dec$month_num, day = 1),"month") -days(1)
  # add the number of days in the month
  daysinperiod <- difftime(dec$EstimationPeriodEnd[1],dec$EstimationPeriodStart[1],units=c("days"))
  # KREST expects estimates by day type (weekday/weekend) iREC doesn't do that, so we dummy in Weekday as it cannot be blank
  dec$DAYTYPE <-"Weekday"


#### Do the sam efo rhte other 2 dataframe - cleaner if we did this in a loop before naming them maybe?
  stv <- left_join(stv, months, by = "MONTH")
  stv <- left_join(stv, methods, by = c('method'="TEXT"))
  stv$DATATYPE <- stv$KREST_CDE

  stv$EstimationPeriodStart <- make_datetime(year = stv$YEAR, month = stv$month_num, day = 1, hour=0, min=0)
  stv$EstimationPeriodEnd <- ceiling_date(make_date(year = stv$YEAR, month = stv$month_num, day = 1),"month") -days(1)
  stv$DAYTYPE <-"Weekday"
  siv <- left_join(siv, months, by = "MONTH")
  siv <- left_join(siv, methods, by = c('method'="TEXT"))
  siv$DATATYPE <- siv$KREST_CDE

  siv$EstimationPeriodStart <- make_datetime(year = siv$YEAR, month = siv$month_num, day = 1, hour=0, min=0)
  siv$EstimationPeriodEnd <- ceiling_date(make_date(year = siv$YEAR, month = siv$month_num, day = 1),"month") -days(1)
  siv$DAYTYPE <-"Weekday"
##############

# These columns represent the detail estimate strata
  est_names <- c("tmp_id","estimationperiodstart","estimationperiodend","std_ref_type_cde","resident_status",
                 "licence_type", "age_category", "purchase_period", "geartype", "lodge", "guided", "checkcrabsprawns",
                  "area", "group", "datatype", "daytype", "PROGRAM","ADMINISTRATIVE_AREA")
# These columns represent the summary variance strata
  var_names <- c("tmp_id", "estimationperiodstart", "estimationperiodend", "std_ref_type_cde", "geartype", "area", "datatype","daytype" )

  # make a dataframe of the detailed estimates only the strata
  iest <- dec[,toupper(names(dec)) %in% toupper(est_names)]
  # make the column names lower case
  colnames(iest) <- tolower(colnames(iest))

 # make a dataframe of the summary variance only the strata
  var <- stv[,toupper(names(stv)) %in% toupper(est_names)]
  # again lower case everything
  colnames(var) <- tolower(colnames(var))

# make a dataframe of the detailed variaces only the strata
  ivar <- siv[,toupper(names(siv)) %in% toupper(est_names)]
  colnames(ivar) <- tolower(colnames(ivar))

  # KREST doesn't allow for all the additional strata details, so we make an XML of them and use the "GROUP" field we don't
  # explicitly need in the estimate structure KREST provides
  iest$group <- paste0("<ROW><RESIDENCE>",iest$resident_status, "</RESIDENCE>" ,
                     "<LICENSE_TYPE>" ,  iest$licence_type ,  "</LICENSE_TYPE>" ,
                     "<AGE_CATEGORY>" ,  iest$age_category ,  "</AGE_CATEGORY>" ,
                     "<PURCHASE_TYPE>" ,  iest$purchase_period ,  "</PURCHASE_TYPE>" ,
                     "<METHOD>" ,  iest$datatype ,   "</METHOD>" ,
                     "<LODGE>" ,  iest$lodge ,   "</LODGE>" ,
                     "<GUIDED>" ,  iest$guided ,   "</GUIDED>" ,
                     "<SHELLFISHING>" ,  iest$checkcrabsprawns ,  "</SHELLFISHING>",
                     "<BIAS_COR>N/A</BIAS_COR>" ,
                    "<SIMPLE_LM>N/A</SIMPLE_LM>" ,
                    "<SD>N/A</SD>" ,
                    "<R2>N/A</R2>" ,
                    "<SAMPLE_SIZE>N/A</SAMPLE_SIZE>",
                    "<EFF_R2>N/A</EFF_R2>",
                    "<EFF_SUM_CREEL_IREC>N/A</EFF_SUM_CREEL_IREC></ROW>"
                     )
 # we do the same for the detailed variances
  ivar$group <- paste0("<ROW><RESIDENCE>",ivar$resident_status, "</RESIDENCE>" ,
                      "<LICENSE_TYPE>" ,  ivar$licence_type ,  "</LICENSE_TYPE>" ,
                      "<AGE_CATEGORY>" ,  ivar$age_category ,  "</AGE_CATEGORY>" ,
                      "<PURCHASE_TYPE>" ,  ivar$purchase_period ,  "</PURCHASE_TYPE>" ,
                      "<METHOD>" ,  ivar$datatype ,   "</METHOD>" ,
                      "<LODGE>" ,  ivar$lodge ,   "</LODGE>" ,
                      "<GUIDED>" ,  ivar$guided ,   "</GUIDED>" ,
                      "<SHELLFISHING>" ,  ivar$checkcrabsprawns ,  "</SHELLFISHING>",
                      "<BIAS_COR>N/A</BIAS_COR>" ,
                      "<SIMPLE_LM>N/A</SIMPLE_LM>" ,
                      "<SD>N/A</SD>" ,
                      "<R2>N/A</R2>" ,
                      "<SAMPLE_SIZE>N/A</SAMPLE_SIZE>",
                      "<EFF_R2>N/A</EFF_R2>",
                      "<EFF_SUM_CREEL_IREC>N/A</EFF_SUM_CREEL_IREC></ROW>"
                      )

  # not sure why I'm doing this...  It should already be done?
  iest <- iest[, tolower(names(iest)) %in% tolower(est_names)]
  var <- var[, tolower(names(var)) %in% tolower(est_names)]
  ivar <- ivar[,tolower(names(ivar)) %in% tolower(est_names)]

  # Build some dataframes of the items the estimates are for and link them to the items table for the breakdown
  # kept/released/marked etc
  items <- dec[, !tolower(names(dec)) %in% tolower(c(est_names, exc_clmns))] %>% names() %>% as_tibble()
  items <- left_join(items, citems, by = c("value"="COLUMN_NAME"))

  itemsV <- stv[, !tolower(names(stv)) %in% tolower(c(var_names, exc_clmns))] %>% names() %>% as_tibble()
  itemsV <- left_join(itemsV, citems, by = c("value"="COLUMN_NAME"))

  itemsIV <- siv[, !tolower(names(siv)) %in% tolower(c(est_names, exc_clmns))] %>% names() %>% as_tibble()
  itemsIV <- left_join(itemsIV, citems, by = c("value"="COLUMN_NAME"))

  # call norm_Data to pull the columns into rows based on the list provided
  # we do this for all 3 datasets
  # we rename the value found to represent what the value was representing (detailed or summary variance)
 valuesI <- NULL
 valuesI <- norm_Data(items, dec)

 Vvalues <- NULL
 Vvalues <- norm_Data(itemsV,stv)
 Vvalues <- Vvalues %>% dplyr::rename(sumvariance = value)

 VIvalues <- NULL
 VIvalues <- norm_Data(itemsIV,siv)
 VIvalues <- VIvalues %>% dplyr::rename(variance = value)

 # join the detailed variances to the detailed estimates
 valuesV <- left_join(valuesI,VIvalues,by = join_by(tmp_id, item, TEXT, DB_ITEM, DISPOSITION, MODIFIER, GROUPING, COMMON_NAME, SPECIES, LENGTHCLASS))
 # join the detailed strata to the detailed estimates
 tot_est <- left_join(iest, valuesV, by = join_by(tmp_id))
 # join the summary variance strata to the summary variances
 sum_var <- left_join(var, Vvalues, by = join_by(tmp_id))

 # effectively rename a few fields
 sum_var$SPECIES[sum_var$DB_ITEM=='EFFORT_DAYS'] <- 'ANGLER_DAY'
 sum_var$SPECIES[sum_var$DB_ITEM=='JUV_EFFORT_DAYS'] <- 'ANGLER_D_J'

# Can't figure out what this does other than collapse multiples of the same estimate strata and that shouldn't exist.
 sum_var <- sum_var %>% group_by_at(names(sum_var)[!names(sum_var) %in% c("sumvariance", "tmp_id")]) %>% dplyr::summarise (
    sumvariance = sum(sumvariance),
    .groups = 'drop')
 sum_var$row_num <- 1

 # effectively rename a couple fields
 tot_est$SPECIES[tot_est$DB_ITEM=='EFFORT_DAYS'] <- 'ANGLER_DAY'
 tot_est$SPECIES[tot_est$DB_ITEM=='JUV_EFFORT_DAYS'] <- 'ANGLER_D_J'
  # Numbers rows in a matching sub-group. I think this was for the original method to send the data to KREST
 # such that all the details were there.  THe detailed estimates would be seperate and the 1st instance of this sub-group
 # would get the summary variance applied.  Not doing this any more.  We are sending 2 estimates, but only publishing the 2nd one.
 tot_est <- tot_est %>%
   group_by(area, datatype, item, lodge, guided, checkcrabsprawns, LENGTHCLASS) %>%
   dplyr::mutate(row_num = row_number(tmp_id )) %>%
   ungroup()


 sumnames <- c("area", "lodge", "guided", "checkcrabsprawns", "program", "std_ref_type_cde", "administrative_area",
               "geartype", "datatype", "estimationperiodstart", "estimationperiodend", "daytype","item", "TEXT", "DB_ITEM",
               "DISPOSITION", "MODIFIER", "GROUPING", "COMMON_NAME","SPECIES", "LENGTHCLASS"
               )
 # figure out the summary estimate by summing the detailed estimates together
 sum_est <- tot_est %>%
   group_by(pick(all_of(sumnames)))%>%
   dplyr::summarise (value = sum(value),
     .groups = 'drop') %>%
   ungroup()
 # encode the columns KREST doesn't support
 sum_est$group <- paste0("<ROW><RESIDENCE>N/A</RESIDENCE>" ,
                       "<LICENSE_TYPE>N/A</LICENSE_TYPE>" ,
                       "<AGE_CATEGORY>N/A</AGE_CATEGORY>" ,
                       "<PURCHASE_TYPE>N/A</PURCHASE_TYPE>" ,
                       "<METHOD>" ,  sum_est$datatype ,   "</METHOD>" ,
                       "<LODGE>" ,  sum_est$lodge ,   "</LODGE>" ,
                       "<GUIDED>" ,  sum_est$guided ,   "</GUIDED>" ,
                       "<SHELLFISHING>" ,  sum_est$checkcrabsprawns ,  "</SHELLFISHING>",
                       "<BIAS_COR>N/A</BIAS_COR>" ,
                       "<SIMPLE_LM>N/A</SIMPLE_LM>" ,
                       "<SD>N/A</SD>" ,
                       "<R2>N/A</R2>" ,
                       "<SAMPLE_SIZE>N/A</SAMPLE_SIZE>",
                       "<EFF_R2>N/A</EFF_R2>",
                       "<EFF_SUM_CREEL_IREC>N/A</EFF_SUM_CREEL_IREC></ROW>"
 )
  # Join the summary estimates with the summary variance ensuring na's show as 0's
  f_s_est <- left_join(sum_est, sum_var, by = c(
      "std_ref_type_cde" = "std_ref_type_cde" ,
      "estimationperiodstart" = "estimationperiodstart",
      "estimationperiodend" = "estimationperiodend",
      "area" = "area",
      "datatype" = "datatype",
      "daytype" = "daytype",
      "geartype" = "geartype",
      "item" = "item",
      "lodge" = "lodge",
      "guided" = "guided",
      "checkcrabsprawns" = "checkcrabsprawns",
      "TEXT" = "TEXT",
      "DB_ITEM" = "DB_ITEM" ,
      "DISPOSITION" = "DISPOSITION",
      "MODIFIER" = "MODIFIER",
      "GROUPING" = "GROUPING",
      "COMMON_NAME" = "COMMON_NAME",
      "SPECIES" = "SPECIES",
      "LENGTHCLASS" = "LENGTHCLASS",
      "program" = "program"
    )
  ) %>%
    dplyr::mutate(sumvariance = ifelse(is.na(sumvariance), 0, sumvariance)) %>%
    dplyr::mutate(value = ifelse(is.na(value), 0, value))
  # This part is a lot of hoop jumping as KREST stores effort as a component of each estimated species strata.
  # So we need to extract the effort and then add it onto each estimate row.
  # This almost makes sense as a traditional creel estimate is effort * CPUE, but that is not iREC math.
  f_est <- tot_est
  f_est$sumvariance <- 0

  # detailed effort estimate strata
  eff_names <- c("resident_status", "licence_type", "age_category", "purchase_period", "area",
                 "lodge", "guided", "checkcrabsprawns", "std_ref_type_cde", "geartype", "datatype", "estimationperiodstart",
                 "estimationperiodend", "daytype", "program", "group")

    # summary effort estimate strata
  Seff_names <- c("area", "lodge", "guided", "checkcrabsprawns", "std_ref_type_cde", "geartype", "datatype",
                  "estimationperiodstart", "estimationperiodend", "daytype", "program", "group")


  neff_names <- c(eff_names, "item", "TEXT", "DB_ITEM", "DISPOSITION", "MODIFIER",
                  "GROUPING", "COMMON_NAME", "SPECIES","LENGTHCLASS")
  Sneff_names <- c(Seff_names, "item", "TEXT", "DB_ITEM", "DISPOSITION", "MODIFIER",
                  "GROUPING", "COMMON_NAME", "SPECIES","LENGTHCLASS")

  # Stashing the detailed juvenile effort estimate into some columns we don't use
  effj <- f_est[f_est$GROUPING == "EFFORT" & f_est$SPECIES == "ANGLER_D_J", ] %>% group_by_at(eff_names) %>% dplyr::summarise (
    ICEInTimeblock1 = sum(value), ICEInTimeblock2 = sum(variance), ICEInTimeblock3 = sum(sumvariance),
    .groups = 'drop')

  # Stashing the summary juvenile effort estimate into some columns we don't use
  Seffj <- f_s_est[f_s_est$GROUPING == "EFFORT" & f_s_est$SPECIES == "ANGLER_D_J", ] %>% group_by_at(Seff_names) %>% dplyr::summarise (
    ICEInTimeblock1 = sum(value), ICEInTimeblock3 = sum(sumvariance),
    .groups = 'drop')

  # getting the detailed effort estimates and variances
  effa <- f_est[f_est$GROUPING == "EFFORT" & f_est$SPECIES == "ANGLER_DAY", ] %>% group_by_at(eff_names) %>% dplyr::summarise (
    TotalEffort = sum(value), VarianceTotalEffort = sum(variance), VarianceDailyEffort = sum(sumvariance),
    .groups = 'drop')

  # getting the summary effort estimates and variances
  Seffa <- f_s_est[f_s_est$GROUPING == "EFFORT" & f_s_est$SPECIES == "ANGLER_DAY", ] %>% group_by_at(Seff_names) %>% dplyr::summarise (
    TotalEffort = sum(value), VarianceTotalEffort = sum(sumvariance),
    .groups = 'drop')
# building 1 set of detailed efforts for adult and juvenile
  eff <- left_join(effa, effj, by=join_by(estimationperiodstart, estimationperiodend, resident_status, licence_type, age_category,
                                           purchase_period, area, lodge, guided, checkcrabsprawns, std_ref_type_cde, geartype, datatype, daytype, program, group))
  # building 1 set of summary efforts for adult and juvenile
  Seff <- left_join(Seffa, Seffj, by=join_by(estimationperiodstart, estimationperiodend, area, lodge, guided, checkcrabsprawns,
                                             std_ref_type_cde, geartype, datatype, daytype, program, group))

  # building the detailed catch for everything other than effort
  neff <- f_est[f_est$GROUPING != "EFFORT", ] %>% group_by_at(neff_names) %>% dplyr::summarise (
    TotalCatch = sum(value), VarianceTotalCatch = sum(variance), VarianceDailyCatch = sum(sumvariance),
    .groups = 'drop')

  # building the summary catch for everything other than effort
  Sneff <- f_s_est[f_s_est$GROUPING != "EFFORT", ] %>% group_by_at(Sneff_names) %>% dplyr::summarise (
    TotalCatch = sum(value), VarianceTotalCatch = sum(sumvariance),
    .groups = 'drop')

  # join the effort estimates to the catch estimates for the detailed side
  t_est <- left_join(eff, neff, by=join_by(estimationperiodstart, estimationperiodend, resident_status, licence_type, age_category,
                                           purchase_period, area, lodge, guided, checkcrabsprawns, std_ref_type_cde, geartype, datatype, daytype, program, group))

  # join the effort estimates to the catch estimates for the summary side
  St_est <- left_join(Seff, Sneff, by=join_by(estimationperiodstart, estimationperiodend, area, lodge, guided,
                                              checkcrabsprawns, std_ref_type_cde, geartype, datatype, daytype, program, group))
##############
  # this section fills out the data structure that KREST is expecting and will not accept nulls for
  # This is the detailed estimate that will not be published in KREST
   # set the maturity field to legel/sublegal
  t_est$Maturity<-ifelse(grepl("Legal", t_est$LENGTHCLASS), "LEGAL", "SUB-LEGAL")
  t_est$LENGTHCLASS
  t_est$Party<-"N/A"
  # stash the juv effort into the Licence field.
  t_est$Licence <- paste0("<ROW><Effort_JUV>",t_est$ICEInTimeblock1, "</Effort_JUV>",
                        "<VAR_Effort_JUV>",t_est$ICEInTimeblock2,"</VAR_Effort_JUV>",
                        "<VAR_SUM_Effort_JUV>",t_est$ICEInTimeblock3,"</VAR_SUM_Effort_JUV></ROW>"
  )
  t_est$Program<-"N/A"
  t_est$CatchUnit<-"PC"
  t_est$Incomplete<-"N/A"
  t_est$MeanCPUE <- 0 #t_est$TotalCatch / (t_est$TotalEffortA + t_est$TotalEffortJ)
  t_est$VarianceCPUE <- 0
  t_est$SeCPUE <- 0
  t_est$MeanDailyEffort <- 0
  t_est$SeDailyEffort<-sqrt(t_est$VarianceDailyEffort)
  t_est$MeanDailyCatch <- 0
  t_est$SeDailyCatch <- sqrt(t_est$VarianceDailyCatch)
  t_est$NumberOfDaysInEstPeriod <- as.integer(daysinperiod)
  t_est$CensusEffort<-0
  t_est$SurveyEffort<-0
  t_est$SeTotalEffort<-sqrt(t_est$VarianceTotalEffort)
  t_est$CensusCatch<-0
  t_est$SurveyCatch<-0
  t_est$SeTotalCatch<-sqrt(t_est$VarianceTotalCatch)
  t_est$NumberOfInterviews<-0
  t_est$NumberOfIC<-0
  t_est$NumberOfInterviewDays<-0
  t_est$MeanStartTimeOfIC<-0
  t_est$MeanEndTimeOfIC<-0
  t_est$MeanMidPointTimeOfIC<-0
  t_est$MeanIC<-0
  t_est$MeanICE<-0
  #t_est$ICEInTimeblock1<-0
  #t_est$ICEInTimeblock2<-0
  #t_est$ICEInTimeblock3<-0
  t_est$ICEInTimeblock4<-0
  t_est$ICEInTimeblock5<-0
  t_est$ICEInTimeblock6<-0
  t_est$ICEInTimeblock7<-0
  t_est$ICEInTimeblock8<-0
  t_est$ICEInTimeblock9<-0
  t_est$ICEInTimeblock10<-0
  t_est$ICEInTimeblock11<-0
  t_est$ICEInTimeblock12<-0
  t_est$ICEInTimeblock13<-0
  t_est$ICEInTimeblock14<-0
  t_est$ICEInTimeblock15<-0
  t_est$ICEInTimeblock16<-0
  t_est$ICEInTimeblock17<-0
  t_est$ICEInTimeblock18<-0
  t_est$ICEInTimeblock19<-0
  t_est$ICEInTimeblock20<-0
  t_est$ICEInTimeblock21<-0
  t_est$ICEInTimeblock22<-0
  t_est$ICEInTimeblock23<-0
  t_est$ICEInTimeblock24<-0
  #t_est$Purpose<-"N/A"
  ##############

  # rename some fields to match KREST's expectations
  t_est <- t_est %>% dplyr::rename(
    EstimationArea = std_ref_type_cde,
    GearType = geartype,
    DataType = datatype,
    EstimationPeriodStart = estimationperiodstart,
    EstimationPeriodEnd = estimationperiodend,
    DayType = daytype,
    Group = group,
    Disposition = DISPOSITION,
    Modifiers = MODIFIER,
    Species = SPECIES,
    LengthClass = LENGTHCLASS
  )

  ##############
  # this section fills out the data structure that KREST is expecting and will not accept nulls for
  # This is the summary estimate that will be published in KREST
  # Essentiall the same as above

  St_est$Maturity<-ifelse(grepl("Legal", St_est$LENGTHCLASS), "LEGAL", "SUB-LEGAL")
  St_est$LENGTHCLASS
  St_est$Party<-"N/A"

  St_est$Licence <- paste0("<ROW><Effort_JUV>",St_est$ICEInTimeblock1, "</Effort_JUV>",
                          "<VAR_Effort_JUV>",St_est$ICEInTimeblock3,"</VAR_Effort_JUV>",
                          "<VAR_SUM_Effort_JUV>N/A</VAR_SUM_Effort_JUV></ROW>"
  )
  St_est$Program<-"N/A"
  St_est$CatchUnit<-"PC"
  St_est$Incomplete<-"N/A"
  St_est$MeanCPUE <- 0 #St_est$TotalCatch / (St_est$TotalEffortA + St_est$TotalEffortJ)
  St_est$VarianceCPUE <- 0
  St_est$SeCPUE <- 0
  St_est$MeanDailyEffort <- 0
  St_est$SeDailyEffort<- 0 #sqrt(St_est$VarianceDailyEffort)
  St_est$VarianceDailyEffort <- 0
  St_est$MeanDailyCatch <- 0
  St_est$SeDailyCatch <- 0 #sqrt(St_est$VarianceDailyCatch)
  St_est$VarianceDailyCatch <- 0
  St_est$NumberOfDaysInEstPeriod <- as.integer(daysinperiod)
  St_est$CensusEffort<-0
  St_est$SurveyEffort<-0
  St_est$SeTotalEffort<-sqrt(St_est$VarianceTotalEffort)
  St_est$CensusCatch<-0
  St_est$SurveyCatch<-0
  St_est$SeTotalCatch<-sqrt(St_est$VarianceTotalCatch)
  St_est$NumberOfInterviews<-0
  St_est$NumberOfIC<-0
  St_est$NumberOfInterviewDays<-0
  St_est$MeanStartTimeOfIC<-0
  St_est$MeanEndTimeOfIC<-0
  St_est$MeanMidPointTimeOfIC<-0
  St_est$MeanIC<-0
  St_est$MeanICE<-0
  #St_est$ICEInTimeblock1<-0
  St_est$ICEInTimeblock2<-0
  #St_est$ICEInTimeblock3<-0
  St_est$ICEInTimeblock4<-0
  St_est$ICEInTimeblock5<-0
  St_est$ICEInTimeblock6<-0
  St_est$ICEInTimeblock7<-0
  St_est$ICEInTimeblock8<-0
  St_est$ICEInTimeblock9<-0
  St_est$ICEInTimeblock10<-0
  St_est$ICEInTimeblock11<-0
  St_est$ICEInTimeblock12<-0
  St_est$ICEInTimeblock13<-0
  St_est$ICEInTimeblock14<-0
  St_est$ICEInTimeblock15<-0
  St_est$ICEInTimeblock16<-0
  St_est$ICEInTimeblock17<-0
  St_est$ICEInTimeblock18<-0
  St_est$ICEInTimeblock19<-0
  St_est$ICEInTimeblock20<-0
  St_est$ICEInTimeblock21<-0
  St_est$ICEInTimeblock22<-0
  St_est$ICEInTimeblock23<-0
  St_est$ICEInTimeblock24<-0
  #St_est$Purpose<-"N/A"

  St_est <- St_est %>% dplyr::rename(
    EstimationArea = std_ref_type_cde,
    GearType = geartype,
    DataType = datatype,
    EstimationPeriodStart = estimationperiodstart,
    EstimationPeriodEnd = estimationperiodend,
    DayType = daytype,
    Group = group,
    Disposition = DISPOSITION,
    Modifiers = MODIFIER,
    Species = SPECIES,
    LengthClass = LENGTHCLASS
  )
  ##############

  xlsx_clmns1 <- c("EstimationPeriodStart",	"EstimationPeriodEnd",	"EstimationArea",	"GearType",	"Species",
                  "Disposition",	"Modifiers",	"Maturity",	"LengthClass",	"Group",	"Party",	"Licence",
                  "Program",	"CatchUnit",	"DataType",	"DayType",	"Incomplete",	"MeanCpue",	"VarianceCpue",
                  "SeCpue",	"MeanDailyEffort",	"VarianceDailyEffort",	"SeDailyEffort",	"MeanDailyCatch",
                  "VarianceDailyCatch",	"SeDailyCatch",	"NumOfDaysInEstPeriod",	"TotalEffort",	"CensusEffort",
                  "SurveyEffort",	"VarianceTotalEffort",	"SeTotalEffort",	"TotalCatch",	"CensusCatch",
                  "SurveyCatch",	"VarianceTotalCatch",	"SeTotalCatch",	"NumberOfInterviews",	"NumberOfIc",
                  "NumberOfInterviewDays",	"MeanStartTimeOfIc",	"MeanEndTimeOfIc",	"MeanMidPointTimeOfIc",
                  "MeanIc",	"MeanIce",	"ICEInTimeblock1",	"ICEInTimeblock2",	"ICEInTimeblock3",
                  "ICEInTimeblock4",	"ICEInTimeblock5",	"ICEInTimeblock6",	"ICEInTimeblock7",
                  "ICEInTimeblock8",	"ICEInTimeblock9",	"ICEInTimeblock10",	"ICEInTimeblock11",
                  "ICEInTimeblock12",	"ICEInTimeblock13",	"ICEInTimeblock14",	"ICEInTimeblock15",
                  "ICEInTimeblock16",	"ICEInTimeblock17",	"ICEInTimeblock18",	"ICEInTimeblock19",
                  "ICEInTimeblock20",	"ICEInTimeblock21",	"ICEInTimeblock22",	"ICEInTimeblock23",
                  "ICEInTimeblock24", "program")#,	"Purpose"
  xlsx_clmns2 <- c("EstimationPeriodStart", "EstimationPeriodEnd", "EstimationArea", "GearType", "Species",
                   "Disposition", "Modifiers", "Maturity", "LengthClass", "Group", "Party", "Licence", "Program",
                   "CatchUnit",  "DataType", "DayType", "MeanCPUE", "VarianceCPUE", "SeCPUE", "MeanDailyEffort",
                   "VarianceDailyEffort", "SeDailyEffort", "MeanDailyCatch", "VarianceDailyCatch", "SeDailyCatch",
                   "NumberOfDaysInEstPeriod", "TotalEffort", "CensusEffort", "SurveyEffort", "VarianceTotalEffort",
                   "SeTotalEffort", "TotalCatch", "CensusCatch", "SurveyCatch", "VarianceTotalCatch", "SeTotalCatch",
                   "NumberOfInterviews", "NumberOfIC", "NumberOfInterviewDays", "MeanStartTimeOfIC", "MeanEndTimeOfIC",
                   "MeanMidPointTimeOfIC", "MeanIC", "MeanICE", "ICEInTimeblock1", "ICEInTimeblock2", "ICEInTimeblock3",
                   "ICEInTimeblock4",	"ICEInTimeblock5",	"ICEInTimeblock6",	"ICEInTimeblock7",
                   "ICEInTimeblock8",	"ICEInTimeblock9",	"ICEInTimeblock10",	"ICEInTimeblock11",
                   "ICEInTimeblock12",	"ICEInTimeblock13",	"ICEInTimeblock14",	"ICEInTimeblock15",
                   "ICEInTimeblock16",	"ICEInTimeblock17",	"ICEInTimeblock18",	"ICEInTimeblock19",
                   "ICEInTimeblock20",	"ICEInTimeblock21",	"ICEInTimeblock22",	"ICEInTimeblock23",
                   "ICEInTimeblock24", "program")

  xlsx_K_clmns <- c("EstimationPeriodStart", "EstimationPeriodEnd", "EstimationArea", "GearType", "Species",
                   "Disposition", "Modifiers", "Maturity", "LengthClass", "Group", "Party", "Licence", "Program",
                   "CatchUnit",  "DataType", "DayType",	"Incomplete", "MeanCPUE", "VarianceCPUE", "SeCPUE", "MeanDailyEffort",
                   "VarianceDailyEffort", "SeDailyEffort", "MeanDailyCatch", "VarianceDailyCatch", "SeDailyCatch",
                   "NumberOfDaysInEstPeriod", "TotalEffort", "CensusEffort", "SurveyEffort", "VarianceTotalEffort",
                   "SeTotalEffort", "TotalCatch", "CensusCatch", "SurveyCatch", "VarianceTotalCatch", "SeTotalCatch",
                   "NumberOfInterviews", "NumberOfIC", "NumberOfInterviewDays", "MeanStartTimeOfIC", "MeanEndTimeOfIC",
                   "MeanMidPointTimeOfIC", "MeanIC", "MeanICE", "ICEInTimeblock1", "ICEInTimeblock2", "ICEInTimeblock3",
                   "ICEInTimeblock4",	"ICEInTimeblock5",	"ICEInTimeblock6",	"ICEInTimeblock7",
                   "ICEInTimeblock8",	"ICEInTimeblock9",	"ICEInTimeblock10",	"ICEInTimeblock11",
                   "ICEInTimeblock12",	"ICEInTimeblock13",	"ICEInTimeblock14",	"ICEInTimeblock15",
                   "ICEInTimeblock16",	"ICEInTimeblock17",	"ICEInTimeblock18",	"ICEInTimeblock19",
                   "ICEInTimeblock20",	"ICEInTimeblock21",	"ICEInTimeblock22",	"ICEInTimeblock23",
                   "ICEInTimeblock24", "program")

    # Pull the columns in the xlsx_K_clmns list dropping the rest
    t_est <- t_est[, xlsx_K_clmns]
    # Check for na's and deal with them so KREST is happy
    t_est <- t_est %>%
      dplyr::mutate(Species = ifelse(is.na(Species), "124", Species)) %>%
      dplyr::mutate(Disposition = ifelse(is.na(Disposition), "Released", Disposition)) %>%
      dplyr::mutate(Modifiers = ifelse(is.na(Modifiers), "N/A", Modifiers)) %>%
      dplyr::mutate(LengthClass = ifelse(is.na(LengthClass), "Unspecified", LengthClass)) %>%
      dplyr::mutate(MeanCPUE = ifelse(is.na(MeanCPUE), 0, MeanCPUE)) %>%
      dplyr::mutate(VarianceTotalCatch = ifelse(is.na(VarianceTotalCatch), 0, VarianceTotalCatch)) %>%
      dplyr::mutate(TotalCatch = ifelse(is.na(TotalCatch), 0, TotalCatch)) %>%
      dplyr::mutate(SeTotalCatch = ifelse(is.na(SeTotalCatch), 0, SeTotalCatch))

    # doing the same as above, but for the summary estimates
    St_est <- St_est[, xlsx_K_clmns]
    St_est <- St_est %>%
      dplyr::mutate(Species = ifelse(is.na(Species), "124", Species)) %>%
      dplyr::mutate(Disposition = ifelse(is.na(Disposition), "Released", Disposition)) %>%
      dplyr::mutate(Modifiers = ifelse(is.na(Modifiers), "N/A", Modifiers)) %>%
      dplyr::mutate(LengthClass = ifelse(is.na(LengthClass), "Unspecified", LengthClass)) %>%
      dplyr::mutate(MeanCPUE = ifelse(is.na(MeanCPUE), 0, MeanCPUE)) %>%
      dplyr::mutate(VarianceTotalCatch = ifelse(is.na(VarianceTotalCatch), 0, VarianceTotalCatch)) %>%
      dplyr::mutate(TotalCatch = ifelse(is.na(TotalCatch), 0, TotalCatch)) %>%
      dplyr::mutate(SeTotalCatch = ifelse(is.na(SeTotalCatch), 0, SeTotalCatch))

     # The method to send data to KREST is size limited, so we break up the data by program designation GST, JDF, KYU etc
    # and count the number of rows in each group
    t_est_g <- t_est %>%
      group_by(program) %>%
      dplyr::mutate(rn = row_number())

  # this is the maxiumum number of rows we feel confident sending
  bin <- 1000

  ##############
  # This code "numbers" the groups of rows for each program
  # Example: if the program GST has 2000 rows of estimates, the first 1000 would get the rn = 1
  # and the 2nd 1000 would get the rn = 2
  # once done, we drop the rn = 1 value as the estimate packet sent to KREST is named with the rn value and
  # if there is only 1 packet the rn is redundent
  # We do this for both the detailed and summary even though the summary never hits over 1000, just in-case
  it <- 1
  ite <- ceiling(max(t_est_g$rn) / bin) * bin
  sbin <- 0
  for (i in seq(bin, ite, by = bin)) {
    t_est_g$rn[t_est_g$rn >= sbin & t_est_g$rn <= i] <- it
    it <- it + 1
    sbin <- sbin + bin
  }
  t_est_g$program <-ifelse(t_est_g$rn > 1, paste0(t_est_g$program,'-',t_est_g$rn), t_est_g$program)
  t_est_g$rn <- NULL

  St_est_g <- St_est %>%
    group_by(program) %>%
    dplyr::mutate(rn = row_number())
  it <- 1
  ite <- ceiling(max(St_est_g$rn) / bin) * bin
  sbin <- 0
  for (i in seq(bin, ite, by = bin)) {
    St_est_g$rn[St_est_g$rn >= sbin & St_est_g$rn <= i] <- it
    it <- it + 1
    sbin <- sbin + bin
  }
  St_est_g$program <-ifelse(St_est_g$rn > 1, paste0(St_est_g$program,'-',St_est_g$rn), St_est_g$program)
  St_est_g$rn <- NULL
  ##############

  # pack up the data products into a list(vector?)
  rslt[[1]] <- TRUE
  rslt[[2]] <- t_est_g
  rslt[[3]] <- xls_file
  rslt[[4]] <- csvs
  rslt[[5]] <- St_est_g

 # and return it
  return(rslt)
}

#' function to normalize a data table based on a list of column names
#' this isn't universal - it expects a column tmp_id and method at the least
#' it also does some double duty and forces there to be at least an estimate of 0 for species
#' matched in the vector "pattern" - there is a tendency to assume that no estimate means there wasn't a
#' survey and to go looking eslewere for an estimate.
#' parameters are:
#' items = list of column names to normalize.  This column codes in the disposition, mark status, size, etc
#'         we also break all those potentials out into their own fields
#' dat_tbl = the data frame to work on
#' returns dataframe with minimal id's and item/value pair
norm_Data <- function(items, data_tbl){
  pattern <- paste(c('chinook','coho','halibut'), collapse="|") # a vector of species to force an estimate over
  # create a variable to hold the result
  values <- NULL
  for (i in 1:nrow(items)) {
    itm <- items[[1]][[i]]
    # Check to see if we are working with detailed estimates or summary.  Detailed has more fields to stratify over.
    # builds a temp dataframe of the strata and the "item"
    chk <- 'resident_status' %in% names(data_tbl)
    if (chk) {
    i1 <- data_tbl[c("tmp_id","method", "resident_status", "licence_type", "age_category", "area", itm)]
    } else {
      i1 <- data_tbl[c("tmp_id","method", itm)]
    }
    # add a field for the item name
    i1$item <- itm
    # link to the breakdown of the "item"
    i1<- left_join(i1, items, by=c("item"="value"))
    # move the value that was under the item to its own field
    i1$value <- i1[[itm]]
    # drop the original "item" field
    i1 <- i1[, !names(i1) %in% c(itm)]
    # check to see if the "item" is in the "pattern" vector we want to keep for sure
    if (grepl(pattern,itm) == FALSE) {
      # if not, drop all the 0 estimates
     i1 <- i1[i1$value >0, ]
     } else {
       if (chk){
      # keep all the non zero estimates
       i2 <- i1[i1$value > 0,]
       # make 1 estimate for retained annual resident angling from boat 0 estimate for each area
       i3 <- i1 %>% group_by(area) %>%
         filter(DISPOSITION == 'Kept', method == 'angleboat', licence_type == 'Annual', resident_status == "Resident", value == 0) %>%
         slice_head(n=1) %>%
         ungroup()
       # join the non zeros up with the forced zeros
       i1 <- rbind(i2,i3)
       }
     }
    i1 <- i1[, !names(i1) %in% c('method')] # drop the "method" field
    # if the result is null then make it from the temp data frame
    if(is.null(values)) {
      values <- i1
    } else {
      # otherwise append it to the result data frame
      values <- rbind(values, i1)
    }
  }
  return(values)
}

#' The data sent to KREST is sent as a JSON string.
#' This funcion transforms the dataframe to a JSON string
#' parameters are:
#' rslt = the dataframe containing the formatted for KREST estimates
#' inds = the components of the file name in the form Month_Year_... like "May_2026_Result_20260723_092514.xlsx"
#' out_path_in = Optional-- the name of the path where we would write an xlsx form of the data that will form the individual
#' data packets sent to KREST.
buildJSONS <- function (rslt, inds, out_path_in = "") {
  # make sure there is some data to work with
  if (length(rslt) > 1) {
    # break out the individual components we need to work with
    tot_est <- rslt[[2]]
    xls_file <- rslt[[3]]
    csvs <- rslt[[4]]
    sum_est <- rslt[[5]]
    areas <- csvs[[1]]
    citems <- csvs[[2]]
    months <- csvs[[4]]
    methods <- csvs[[3]]

    #build the dataframe we need with 3 columns and the number of rows of "program"s - data packets are broken up by program
    # rename the the generic column names.
    # each row will hold the the JSON string as well as its name
    jsons <- data.frame(matrix(0, ncol = 3, nrow = length(unique(tot_est$program)))) %>%
      dplyr::rename('g' = X1, 'bodyj' = X2, 'name' = X3)
    cnt <- 0
    # sort the programs alphabetically - This is for testing and aesthetics
    prgs <- sort(unique(tot_est$program))
    # loop through each program
    for (prg in sort(unique(tot_est$program))) {
      # pull the detailed estimate data matching the program
      outp <- tot_est[tot_est$program == prg, ]
      auto_notify(paste0(prg, ' - ', nrow(outp),' '))
      # drop the program field as it is not used
      outp$program <- NULL
      # do the same for the summary estimates
      soutp <- sum_est[sum_est$program == prg,]
      soutp$program <- NULL
      # write out the detailed estimates if needed or wanted
      xls_file_out <- paste0(fs::path_ext_remove(xls_file), "_", prg, "_expR.xlsx")
      auto_notify(xls_file_out)
      if (out_path_in != ""){
      openxlsx::write.xlsx(outp, paste0(out_path_in, xls_file_out))
      }
      # build the estimate name
      mnth_num <- str_sub(paste0("0", months[months$MONTH == Expmonth(inds[1]), ][[2]]), -2, -1)
      estname <-  paste0("i ", inds[2], " " , mnth_num, " ", prg)#, " XC")
      admin_area <- areas[areas$PROGRAM == prg, ][["ADMINISTRATIVE_AREA"]][1]

      KrestEstimateVersions <- data.frame()
      # each estimate version needs a GUID.  here we make one from the estimate name so it is repeatable.
      # this isn't really GUID, but KREST has no constraints on what an estimate covers and we only want 1 estimate
      # for each month/year/area/item combo
      g <- uuid::UUIDfromName("00000000-0000-0000-0000-000000000000", estname, type =
                                "sha1")
      # I'm not sure why I did this, because gV gets assigned later on.  Maybe a left over piece
      gV <- g

      # "Estimates" are a nested structure:
      #   EstimateRecord - The envelope / list to hold all potential estimate records
      #   └──  EstimateRecords = The name and "type" of estimate
      #       └── EstimateVersion
      #          └── EstimateResultsRecords
      # We have to build them backwards to stack up
      # We want to store both the detailed and summary estimates w/variance so we are making 2 estimate versions
      # 1 is the detailed and 2 is the summary.  We "promote" the summary, but the detailed is still available through some
      # SQL work


    for ( vi in 1:2){
      # add the estimate to the result record
      if (vi == 1) {
        auto_notify(nrow(outp))
        KrestEstimateResultRecords <- outp # detailed estimate
      } else {
        KrestEstimateResultRecords <- soutp # summary estimate
      }
      # add a row to hold the estimate "version"
      KrestEstimateVersions[nrow(KrestEstimateVersions) + 1, ] <- NA
      # insert the estimate into the version
      KrestEstimateVersions$KrestEstimateResultRecords[[vi]] <- KrestEstimateResultRecords
      # build a GUID based on the estimate name so it is repeatable
      gV <- uuid::UUIDfromName("00000000-0000-0000-0000-000000000000", paste0(estname,vi), type =
                                "sha1")
      # add the GUID we just made into the estimate versions layer
      KrestEstimateVersions$ReferenceGuid[vi] <- gV

      # build the structure for the top layer "Estimate Records"
      # this forces the datatypes. Maybe
      EstimateRecords  <- data.frame(
        EstimateName = character(),
        EstimateType = integer(),
        EstimateTypeSpecified = integer(),
        SeasonType = integer(),
        AdministrativeArea = integer(),
        Fishery = integer(),
        GearLevel = integer(),
        PublicationType = integer(),
        PublicationStatus = integer(),
        EstimateStatus = integer(),
        Comment = character(),
        KrestEstimateVersions = data.frame(),
        ReferenceGuid = character(),
        stringsAsFactors = FALSE
      )
      # Populate the estimaterecord level as needed.  Some of this is in code from KREST
      # ideally they would be "looked" up from KREST.  They shouldn't change but still not perfect.
      EstimateRecords[nrow(EstimateRecords) + 1, ] <- NA
      EstimateRecords$EstimateName[1]  <- estname
      EstimateRecords$EstimateType[1] <- 1
      EstimateRecords$EstimateTypeSpecified[1] <- "True"
      EstimateRecords$SeasonType[1] <- 1 #
      EstimateRecords$AdministrativeArea[1] <- admin_area #
      EstimateRecords$Fishery[1] <- 0 #
      EstimateRecords$GearLevel[1] <- 0
      EstimateRecords$PublicationType[1] <- 1 #
      EstimateRecords$PublicationStatus[1] <- 0 #
      EstimateRecords$EstimateStatus[1] <- 1 #
      EstimateRecords$Comment[1] <- xls_file #"May_2024_Result_20240808_113953.xlsx" #
      EstimateRecords$KrestEstimateVersions[[1]] <- KrestEstimateVersions
      EstimateRecords$ReferenceGuid[1] <- g #
      # in theory there can be multiple estimate records sent in one packet
      # we are keeping everything really simple, so only one estimate records
      EstimateRecord <- data.frame(EstimateRecords = list())
      EstimateRecord[nrow(EstimateRecord) + 1, ] <- NA
      # add the estimate records in
      EstimateRecord$EstimateRecords <- EstimateRecords
    }
      # we need to build the JSON string from the dataframe passing it as a list.
      # then populate some GUID
      bodyj <- jsonlite::toJSON(as.list(EstimateRecord))
      cnt = cnt + 1
      jsons$g[[cnt]] <- g
      jsons$gV[[cnt]] <- gV
      jsons$bodyj[[cnt]] <- bodyj
      jsons$name[[cnt]] <- prg
      jsons$estname[[cnt]] <- estname
      jsons$rowcnt[[cnt]] <- nrow(outp)
    }
    # return the data package
    return(jsons)
  }
}

#' function to take the estimate data packages with the JSON string and send them to KREST
#' via a HTTPS web call
#' parameters are:
#' Jsons = the data frame of the estimates to send including a field containing the JSON string
#' krestendpoint = the url of the destination server
#' logincreds = the account credentials used to send the data to KREST
SendJSON <- function(Jsons, krestendpoint, logincreds, krestroot) {
  # Check to see if we are running inside Shiny or not to determine where the progress goes
  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {
    }
    withProgress <- function(...)
      list(...)[["expr"]]
  }
  withProgress(message = 'Processing ', value = 0, {
    # count the number of estimates to send.  For progress tracking
    tstps = nrow(Jsons)
    # keep track of which estimate number we are working with.  For progress reporting
    stp = 1

    # loop through the estimates, get the names and purge the destination database of any occurances of them
    for (i in 1:nrow(Jsons)) {
      estname <- strsplit(Jsons$estname[[i]], '-')[[1]][1]
      incProgress(1 / tstps,
                  detail = paste("Deleting ", estname, ' ', stp, ' of ', tstps))
      stp = stp + 1
      purgeEstWoptions(Jsons$estname[[i]], krestroot)
    }
  })
  withProgress(message = 'Processing ', value = 0, {
    stp = 1
    # loop through the estimates
    for (i in 1:nrow(Jsons)) {
      # get the authorization token needed to send the data
      LoginToken <- getAuthToken(krestendpoint, logincreds)
      # check to see if we are good.  Should report back that this failed, but we don't expect that to be the case
      if (LoginToken[[1]] != "Not Authorized") {
        # report on the progress
        incProgress(1 / tstps,
                    detail = paste("Loading ", Jsons$estname[[i]], ' ', stp, ' of ', tstps))
        stp = stp + 1
        auto_notify(Jsons$estname[[i]])
        # double check the estimate name doesn't already exist
        SentD <- SendiRECD(krestendpoint, LoginToken, Jsons$g[[i]])
        # send the data
        SentP <- SendiRECP(krestendpoint, LoginToken, Jsons$bodyj[[i]])

        # check to see if any error messages were returned from the send transaction
        # and report on them.
        if (!is.null(SentP$results[[1]]$errorMessages)) {
          auto_notify(paste0('Import ', SentP$results[[1]]$errorMessages))
        }
        # "promote" the data just sent
        SentPro <- SendiRECPro(krestendpoint, LoginToken, Jsons$gV[[i]])
      }

    }
  })
}

#' function to set a view filter in the KREST UI
#' using the function FetchGrid we can get a dataframe of the available estimates that match the name
#' of interest.  We are pretending to be using the UI, but are just using the UIs web calls
#' used for determining if an estimate exists effectively with a wildcard
#' useful for when an estimate has been split i.e. GST-1, GST-2
#' there is no guarentee that a subsequent estimate will have the same number of splits
#' and not checking first could leave orphaned splits in the database
#' parameters are:
#' est_name = the base name of the estimate to search for
#' krestroot = the database we are interested in.
setFilter <- function (est_name, krestroot) {
  # set a cookie identifying the session id.  Should probably make this dynamic, but it hasn't caused any troubles as of yet.
  cookies = c(ASP.NET_SessionId = "gy5gsj2w13dvgqxajfjniia1")
  # build the web call header.  Most of all this was scraped from monitoring web calls using the UI
  headers = c(
    Accept = "*/*",
    `Accept-Language` = "en-US,en;q=0.9",
    Connection = "keep-alive",
    `Content-Length` = "0",
    DXCss = "0_4414,1_66,0_4418,1_72,1_208,0_4288,1_207,0_4293,0_4302,0_4306,1_82,1_71,0_4315,4_120,4_111,4_112,0_4319,4_121,5_4,0_4354,0_4326,7_11,0_4330,7_13,7_12,0_4253,24_378,24_379,24_414,24_442,24_443,24_478,26_37,26_36,26_35,0_4437,26_34,0_4441,26_31,1_74,0_4348,8_82,0_4352,8_79,0_4365,6_12,0_4369",
    DXScript = "1_9,1_62,1_10,1_253,1_11,1_12,1_13,1_14,1_18,1_64,1_46,1_15,1_7,17_0,17_8,1_25,1_37,1_29,17_36,1_21,1_53,17_35,1_39,1_52,1_51,17_34,1_181,1_182,1_22,1_31,1_44,1_211,1_225,1_209,1_241,1_45,1_50,17_6,1_49,17_15,1_19,1_20,1_38,1_32,1_17,1_224,1_226,1_210,1_218,1_214,1_219,1_220,1_215,1_221,1_216,1_217,1_212,1_222,1_223,1_228,1_237,1_239,1_240,1_227,1_232,1_233,1_234,1_213,1_229,1_230,1_231,1_235,1_236,1_238,17_49,17_50,17_2,1_57,1_55,17_39,1_54,17_40,1_56,17_41,17_42,1_58,17_3,1_47,17_9,17_10,1_33,17_11,1_61,1_60,17_12,1_48,1_36,17_44,1_41,17_13,17_14,1_65,4_0,1_16,5_1,5_2,4_115,4_98,4_100,4_99,4_101,4_102,4_105,4_108,4_109,4_107,4_106,4_104,4_103,4_110,4_1,4_34,4_113,4_3,4_31,4_2,4_24,4_22,4_23,4_27,4_28,4_32,4_29,4_35,4_47,4_48,4_42,4_80,4_36,4_37,4_38,4_39,4_40,4_43,4_44,4_45,4_46,4_49,4_50,4_51,4_52,4_41,4_53,4_54,4_55,4_56,4_57,4_58,4_59,4_60,4_61,4_62,4_67,4_68,4_69,4_70,4_71,4_72,4_73,4_74,4_75,4_76,4_77,4_63,4_64,4_65,4_66,4_78,4_79,4_86,4_87,4_89,4_93,4_88,4_90,4_91,4_81,4_84,4_83,4_85,4_82,4_15,4_16,4_17,4_18,4_19,4_20,4_92,4_25,4_26,4_30,4_14,4_6,4_8,4_7,4_9,4_10,4_11,4_21,4_12,4_4,4_5,4_13,17_4,4_94,4_95,1_188,1_184,1_191,4_96,4_97,1_197,1_198,4_33,1_180,1_186,17_24,1_251,17_1,7_7,7_5,7_6,7_9,7_4,17_30,1_183,1_203,17_25,1_192,17_18,1_201,17_20,1_196,1_202,17_17,17_22,1_190,17_19,1_59,1_193,1_187,17_16,1_195,1_189,17_43,1_200,1_194,17_21,10_0,10_1,10_2,10_3,10_4,17_23,8_19,8_15,17_33,8_0,8_1,8_2,8_3,8_4,8_5,8_22,1_23,8_23,8_29,8_28,8_30,8_27,8_6,8_7,8_16,8_17,8_18,8_20,8_24,8_32,8_33,8_34,8_35,8_36,8_37,8_61,8_21,8_53,8_60,8_45,8_57,8_54,8_55,8_46,8_39,8_44,8_52,8_58,8_47,8_48,8_50,8_59,8_51,8_56,8_49,8_40,8_62,8_64,8_63,8_66,8_72,8_67,8_69,8_70,8_68,8_25,8_38,8_11,17_31,8_9,8_14,8_10,8_31,8_8,8_26,8_77,8_12,8_13,8_78,17_32,6_0,6_1,6_2,6_3,6_4,6_5,6_7,6_8,6_6,17_37",
    Origin = paste0("http://", krestroot),
    Referer = paste0("http://", krestroot,"/krest"),
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36 Edg/140.0.0.0",
    `X-Requested-With` = "XMLHttpRequest"
  )
  # set the parameters for the web call
  params = list(gridName = "Estimates", parameter = est_name)
  # post the web call.  Authentication is based on your network login, so you need a KREST UI
  # account that can see the data you are interested in.
  res <- httr::POST(
    url = paste0("http://", krestroot, "/KREST/DataManagement/SetFilter"),
    httr::add_headers(.headers = headers),
    query = params,
    httr::set_cookies(.cookies = cookies),
    config = httr::config(ssl_verifypeer = FALSE),
    authenticate(":", ":", "ntlm"),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )
# This is some checking for errors in the call
  outfile <- "junk.html"
  rslt = rawToChar(res$content)
  if (rslt != "") {
    if (file.exists(outfile)) {
      file.remove(outfile)
    }
    readr::write_lines(rslt, outfile)
    browseURL(outfile)
  }

}

#' function to fetch the data grid view of the available estimates from the KREST UI server
#' the filter set is based on what was previously sent by the setFilter function.
#' parameters are:
#' krestroot = the database url we are interested in.
FetchGrid <- function (krestroot) {
  # set a cookie identifying the session id.  Should probably make this dynamic, but it hasn't caused any troubles as of yet.
  cookies = c(ASP.NET_SessionId = "gy5gsj2w13dvgqxajfjniia1")
  # set the web call headers
  headers = c(
    Accept = "application/json, text/javascript, */*; q=0.01",
    `Accept-Language` = "en-US,en;q=0.9",
    Connection = "keep-alive",
    DXCss = "0_4414,1_66,0_4418,1_72,1_208,0_4288,1_207,0_4293,0_4302,0_4306,1_82,1_71,0_4315,4_120,4_111,4_112,0_4319,4_121,5_4,0_4354,0_4326,7_11,0_4330,7_13,7_12,0_4253,24_378,24_379,24_414,24_442,24_443,24_478,26_37,26_36,26_35,0_4437,26_34,0_4441,26_31,1_74,0_4348,8_82,0_4352,8_79,0_4365,6_12,0_4369",
    DXScript = "1_9,1_62,1_10,1_253,1_11,1_12,1_13,1_14,1_18,1_64,1_46,1_15,1_7,17_0,17_8,1_25,1_37,1_29,17_36,1_21,1_53,17_35,1_39,1_52,1_51,17_34,1_181,1_182,1_22,1_31,1_44,1_211,1_225,1_209,1_241,1_45,1_50,17_6,1_49,17_15,1_19,1_20,1_38,1_32,1_17,1_224,1_226,1_210,1_218,1_214,1_219,1_220,1_215,1_221,1_216,1_217,1_212,1_222,1_223,1_228,1_237,1_239,1_240,1_227,1_232,1_233,1_234,1_213,1_229,1_230,1_231,1_235,1_236,1_238,17_49,17_50,17_2,1_57,1_55,17_39,1_54,17_40,1_56,17_41,17_42,1_58,17_3,1_47,17_9,17_10,1_33,17_11,1_61,1_60,17_12,1_48,1_36,17_44,1_41,17_13,17_14,1_65,4_0,1_16,5_1,5_2,4_115,4_98,4_100,4_99,4_101,4_102,4_105,4_108,4_109,4_107,4_106,4_104,4_103,4_110,4_1,4_34,4_113,4_3,4_31,4_2,4_24,4_22,4_23,4_27,4_28,4_32,4_29,4_35,4_47,4_48,4_42,4_80,4_36,4_37,4_38,4_39,4_40,4_43,4_44,4_45,4_46,4_49,4_50,4_51,4_52,4_41,4_53,4_54,4_55,4_56,4_57,4_58,4_59,4_60,4_61,4_62,4_67,4_68,4_69,4_70,4_71,4_72,4_73,4_74,4_75,4_76,4_77,4_63,4_64,4_65,4_66,4_78,4_79,4_86,4_87,4_89,4_93,4_88,4_90,4_91,4_81,4_84,4_83,4_85,4_82,4_15,4_16,4_17,4_18,4_19,4_20,4_92,4_25,4_26,4_30,4_14,4_6,4_8,4_7,4_9,4_10,4_11,4_21,4_12,4_4,4_5,4_13,17_4,4_94,4_95,1_188,1_184,1_191,4_96,4_97,1_197,1_198,4_33,1_180,1_186,17_24,1_251,17_1,7_7,7_5,7_6,7_9,7_4,17_30,1_183,1_203,17_25,1_192,17_18,1_201,17_20,1_196,1_202,17_17,17_22,1_190,17_19,1_59,1_193,1_187,17_16,1_195,1_189,17_43,1_200,1_194,17_21,10_0,10_1,10_2,10_3,10_4,17_23,8_19,8_15,17_33,8_0,8_1,8_2,8_3,8_4,8_5,8_22,1_23,8_23,8_29,8_28,8_30,8_27,8_6,8_7,8_16,8_17,8_18,8_20,8_24,8_32,8_33,8_34,8_35,8_36,8_37,8_61,8_21,8_53,8_60,8_45,8_57,8_54,8_55,8_46,8_39,8_44,8_52,8_58,8_47,8_48,8_50,8_59,8_51,8_56,8_49,8_40,8_62,8_64,8_63,8_66,8_72,8_67,8_69,8_70,8_68,8_25,8_38,8_11,17_31,8_9,8_14,8_10,8_31,8_8,8_26,8_77,8_12,8_13,8_78,17_32,6_0,6_1,6_2,6_3,6_4,6_5,6_7,6_8,6_6,17_37",
    Referer = paste0("http://", krestroot, "/krest"),
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36 Edg/140.0.0.0",
    `X-Requested-With` = "XMLHttpRequest"
  )
 # set the web call parameters
  params = list(
    page = "1",
    start = "0",
    limit = "17",
    `_` = "1758321713179"
  )
  # make the web call using network login credentials
  res <- httr::GET(
    url = paste0("http://", krestroot, "/KREST/EstimateManagement/AllEstimatesData"),
    httr::add_headers(.headers = headers),
    query = params,
    httr::set_cookies(.cookies = cookies),
    config = httr::config(ssl_verifypeer = FALSE),
    authenticate(":", ":", "ntlm"),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )

  outfile2 <- "junk2.html"
  # process the output - get the list of estimates provided in the filter
  rslt = rawToChar(res$content)
  # we are using some error checking for a change :-P
  response <- tryCatch({
    g_df <- fromJSON(rslt)
    # return the dataframe of estimates
    return(g_df$Data)
  }, error = function(e) {
    if (file.exists(outfile2)) {
      file.remove(outfile2)
    }
    # report out any errors
    readr::write_lines(rslt, outfile2)
    browseURL(outfile2)
    stop(paste("Error executing REST call:", e$message))
  })

}

#' function to delete an estimate based on its internal KREST ID
#' parameters are:
#' est_id = the internal KREST estimate ID
#' krestroot = the database url we are interested in.
deleteEst <- function(est_id, krestroot) {

  # set a cookie identifying the session id.  Should probably make this dynamic, but it hasn't caused any troubles as of yet.
  cookies = c(
    ASP.NET_SessionId = "gy5gsj2w13dvgqxajfjniia1"
  )
  # build the web call headers
  headers = c(
    Accept = "*/*",
    `Accept-Language` = "en-US,en;q=0.9",
    Connection = "keep-alive",
    `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
    DXCss = "0_4414,1_66,0_4418,1_72,1_208,0_4288,1_207,0_4293,0_4302,0_4306,1_82,1_71,0_4315,4_120,4_111,4_112,0_4319,4_121,5_4,0_4354,0_4326,7_11,0_4330,7_13,7_12,0_4253,24_378,24_379,24_414,24_442,24_443,24_478,26_37,26_36,26_35,0_4437,26_34,0_4441,26_31,1_74,0_4348,8_82,0_4352,8_79,0_4365,6_12,0_4369",
    DXScript = "1_9,1_62,1_10,1_253,1_11,1_12,1_13,1_14,1_18,1_64,1_46,1_15,1_7,17_0,17_8,1_25,1_37,1_29,17_36,1_21,1_53,17_35,1_39,1_52,1_51,17_34,1_181,1_182,1_22,1_31,1_44,1_211,1_225,1_209,1_241,1_45,1_50,17_6,1_49,17_15,1_19,1_20,1_38,1_32,1_17,1_224,1_226,1_210,1_218,1_214,1_219,1_220,1_215,1_221,1_216,1_217,1_212,1_222,1_223,1_228,1_237,1_239,1_240,1_227,1_232,1_233,1_234,1_213,1_229,1_230,1_231,1_235,1_236,1_238,17_49,17_50,17_2,1_57,1_55,17_39,1_54,17_40,1_56,17_41,17_42,1_58,17_3,1_47,17_9,17_10,1_33,17_11,1_61,1_60,17_12,1_48,1_36,17_44,1_41,17_13,17_14,1_65,4_0,1_16,5_1,5_2,4_115,4_98,4_100,4_99,4_101,4_102,4_105,4_108,4_109,4_107,4_106,4_104,4_103,4_110,4_1,4_34,4_113,4_3,4_31,4_2,4_24,4_22,4_23,4_27,4_28,4_32,4_29,4_35,4_47,4_48,4_42,4_80,4_36,4_37,4_38,4_39,4_40,4_43,4_44,4_45,4_46,4_49,4_50,4_51,4_52,4_41,4_53,4_54,4_55,4_56,4_57,4_58,4_59,4_60,4_61,4_62,4_67,4_68,4_69,4_70,4_71,4_72,4_73,4_74,4_75,4_76,4_77,4_63,4_64,4_65,4_66,4_78,4_79,4_86,4_87,4_89,4_93,4_88,4_90,4_91,4_81,4_84,4_83,4_85,4_82,4_15,4_16,4_17,4_18,4_19,4_20,4_92,4_25,4_26,4_30,4_14,4_6,4_8,4_7,4_9,4_10,4_11,4_21,4_12,4_4,4_5,4_13,17_4,4_94,4_95,1_188,1_184,1_191,4_96,4_97,1_197,1_198,4_33,1_180,1_186,17_24,1_251,17_1,7_7,7_5,7_6,7_9,7_4,17_30,1_183,1_203,17_25,1_192,17_18,1_201,17_20,1_196,1_202,17_17,17_22,1_190,17_19,1_59,1_193,1_187,17_16,1_195,1_189,17_43,1_200,1_194,17_21,10_0,10_1,10_2,10_3,10_4,17_23,8_19,8_15,17_33,8_0,8_1,8_2,8_3,8_4,8_5,8_22,1_23,8_23,8_29,8_28,8_30,8_27,8_6,8_7,8_16,8_17,8_18,8_20,8_24,8_32,8_33,8_34,8_35,8_36,8_37,8_61,8_21,8_53,8_60,8_45,8_57,8_54,8_55,8_46,8_39,8_44,8_52,8_58,8_47,8_48,8_50,8_59,8_51,8_56,8_49,8_40,8_62,8_64,8_63,8_66,8_72,8_67,8_69,8_70,8_68,8_25,8_38,8_11,17_31,8_9,8_14,8_10,8_31,8_8,8_26,8_77,8_12,8_13,8_78,17_32,6_0,6_1,6_2,6_3,6_4,6_5,6_7,6_8,6_6,17_37",
    Origin = paste0("http://",krestroot),
    Referer = paste0("http://", krestroot, "/krest"),
    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/140.0.0.0 Safari/537.36 Edg/140.0.0.0",
    `X-Requested-With` = "XMLHttpRequest"
  )
 # set the web call data
  data = list(
    id = est_id
  )
  # make the web call
  res <- httr::POST(
    url = paste0("http://", krestroot, "/krest/EstimateManagement/Delete/"),
    httr::add_headers(.headers = headers),
    httr::set_cookies(.cookies = cookies),
    body = data,
    encode = "form",
    config = httr::config(ssl_verifypeer = FALSE),
    authenticate(":", ":", "ntlm"),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )
  # return the data sent from the server after the call was made.
  rslt = rawToChar(res$content)
  return(rslt)

}

#' function to test if a named estimate exists
#' parameters are:
#' estname = the named estimate we are looking for
#' krestroot = the database url we are interested in.
#' returns either false if no estimate name matches
#' else returns the internal KREST id for the first estimate matching that name.
checkEstExists <- function(estname, krestroot) {
  # set the filter
  setFilter(estname, krestroot)
  # fetch the KREST UI datagrid
  rslts <- FetchGrid(krestroot)
  # make sure we are getting a dataframe and not an error message
  if (class(rslts) == "data.frame") {
    # I do not know what I'm having it do here.
    # if first sets found to be TRUE
    # then checks to see if only 1 row is returned
    # if only 1 then set found to the ID of the estimate,
    # but if more than 1 is returned, still return the 1st ID.
    found = TRUE
    if (nrow(rslts) == 1) {
      found = rslts$Sequence[[1]]
    } else {
      found = rslts$Sequence[[1]]
    }
  } else {
    found = FALSE
  }

  return(found)
}

#' function to delete an estimate by its GUID
#' parameters are:
#' krestendpoint = the KREST url
#' authToken = and authorization token from KREST
#' GUID the GUID of the estimate to delete
SendiRECD  <- function(krestendpoint, authToken, guid) {
 # set up the web call headers
  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )
  request.Resource = paste0("/southcoast/deleteEstimate?estimateReferenceGuid=",guid)
  urls <- paste0(krestendpoint,request.Resource)
  # sent the delete command to KREST
  res <- httr::DELETE(url = urls,
                      encode = "json",
                      add_headers(.headers = headers),
                      config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
                      )
  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  # take a look at the returned value to see if an error occured
  if (!grepl('Sequence contains no matching element', rlst)) {
    if (!is.null(json$results[[1]]$errorMessages)){
      auto_notify(paste0('Import ',json$results[[1]]$errorMessages))
    }
  }
  return(json)
}

#' function to send an estimate packet by its GUID
#' parameters are:
#' krestendpoint = the KREST url
#' authToken = and authorization token from KREST
#' bodyj = the JSON data packet to send
SendiRECP  <- function(krestendpoint, authToken, bodyj) {

  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )


  bodyj2 <- toJSON(fromJSON(bodyj), auto_unbox = TRUE)

  request.Resource = "/southcoast/importEstimate"

  urls <- paste0(krestendpoint,request.Resource)

  res <- httr::POST(
    url = urls,
    body = bodyj2,
    encode = "json",
    add_headers(.headers = headers),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )
  rlst <- rawToChar(res$content)

  json <- content(res, type="application/json")
  if (!is.null(json$results[[1]]$errorMessages)){
    auto_notify(paste0('Import ',json$results[[1]]$errorMessages))
  }

  return(json)
}

#' function to promote an estimate by its GUID
#' parameters are:
#' krestendpoint = the KREST url
#' authToken = and authorization token from KREST
#' GUID the GUID of the estimate to delete
SendiRECPro  <- function(krestendpoint, authToken, guid) {

  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate',
    `Accept` = 'Accept: application/json, text/json, text/x-json, text/javascript, application/xml, text/xml'
  )

  bodyj <- paste0('{"EstimateVersionReferenceGuid":"',guid,'"}')
  bodyj2 <- toJSON(fromJSON(bodyj), pretty = TRUE, auto_unbox = TRUE)

  request.Resource = "/southcoast/promoteEstimateAreas"

  urls <- paste0(krestendpoint,request.Resource)
  res <- httr::PUT(
    url = urls,
    body = bodyj2,
    encode = "json",
    add_headers(.headers = headers),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )

  rlst <- rawToChar(res$content)

  json <- content(res, type="application/json")
  if (!is.null(json$results[[1]]$errorMessages)){
    auto_notify(paste0('Promotion ',json$results[[1]]$errorMessages))
  }

  return(json)
}

#' function to purge an estimate by its base name
#' parameters are:
#' estname = the base of the estimate name
#' krestroot = the KREST url
purgeEstWoptions <- function(estname, krestroot) {
  nme <- strsplit(estname, '-')[[1]][1]
  if(nchar(nme) >= 12){
    repeat{
      tst <- checkEstExists(nme, krestroot)
      if (tst) {
        deleteEst(tst, krestroot)
      } else {
        break
      }
    }
  }

}

#' wrapper function to send and promote an estimate packet to KREST
#' @export
Load_K_Ests <- function(xls_path, xls_file, source_id, session) {

  SHEET_NAMES <- c("Detailed Estimated Catch",
                   "Summary Total Variance",
                   "Survey Individual Variance")

  inds <- strsplit(str_to_upper(xls_file), "_")[[1]]
  rslt <- getandformatKData(inds, xls_file, xls_path, SHEET_NAMES, source_id)
  message(xls_file)
  Jsons <- buildJSONS(rslt, inds)
  rws <- sum(as.integer(Jsons$rowcnt))
  SendJSON(Jsons, krestendpoint, logincreds, krestroot)
  return(rws)
}

#' function to load an excel estimate as provided by the analysis package
#' that was previously stored in CREST
#' parameters are:
#' source_id = the source_id field of the table OTOLITH_V1.CREEL_IREC_SOURCE
#' that we want to extract the xlsx file that was stored there and process and load it into KREST
#' @export
LoadxlsxFromCREST <- function (source_id) {
  # fetch the record with the data we want
  d <- getdata(Kcon,
               paste0("select * from otolith_v1.creel_irec_source where source_id = ", source_id))
  # write the out a zip file with the xlsx stored in it
  tmpfile <- tempfile(fileext = ".zip")
  writeBin(d$DATA[[1]], tmpfile)
  # unzip the file
  extracted_path <- unzip(tmpfile, files = d$FILENAME[[1]], exdir = tempdir())
  # read all(?) of the sheets in the xlsx file into a list of data frames
  sheet_names <- openxlsx::getSheetNames(extracted_path)
  sheet_list <- as.list(rep(NA, length(sheet_names)))
  names(sheet_list) <- sheet_names
  for (sn in sheet_names) {
    sheet_list[[sn]] <- openxlsx::read.xlsx(extracted_path, sheet = sn)
  }
  # clean up the temp files
  file.remove(tmpfile)
  file.remove(extracted_path)
  return(sheet_list)
}

