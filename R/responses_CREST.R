#' function to load excluded data into the CREST database
#' parameters are:
#' datapath = the location of the source exclude file including file name
#' datafile = the source name of the file - shiny renames the files 0.csv when transferring them, so we need to
#'            know the real name
#' yr_name = the licence year the file is to be applied to
#' df_read = the dataframe as loaded from the source file
#' @export
write_C_ExcludesIn <- function(datapath, datafile, yr_name, df_read){
  # make the column named upper case for consistency
  names(df_read) <- toupper(names(df_read))
  # set the data type when writing to the database
  datatype <- "Exclude"
  # make a temporary file to store the zipped contents
  tmpfile <- tempfile(fileext=".zip")
  # If there is no datefished in the source data make one up - the databse table requires a few columns so we improvise
  #  if they do not exist
  if("DATEFISHED" %notin% names(df_read)){
    df_read$YEAR <- substr(yr_name, 1, 4)
    df_read$MONTH <- 8
    df_read$DATEFISHED <- as.Date(paste0(df_read$MONTH, '/', '15/', df_read$YEAR), "%m/%d/%Y")
    df_read$COMMENT <- NULL
    df_read$COMPLETESURVEY_ID_ID <- 0
    df_read$FISHED_ID_ID <- 0
    # this was some code to link to the response data to glean the licence year and date fished and such.
    # it was decided that the simplest method was to pick the licence year from a list instead.

          # keys <- paste0("'", df_read$SURVEY_ACCESS_KEY, "'", collapse = ", ")
          # sql <- paste0("Select licence_id, round(year,0) as year, round(min(month),0) as month, min(datefished) as DateFished, 0 as completesurvey_id_id, 0 as fished_id_id
          #               from otolith_v1.creel_irec_resp
          #               where licence_id in (", keys, ")
          #               group by licence_id, round(year,0)
          #               order  by licence_id"
          #               )
          # key2 <- dbGetQuery(Ccon, sql)
          # df_read <- df_read %>%
          #   left_join(key2, join_by('SURVEY_ACCESS_KEY' == 'LICENCE_ID'))
          # df_read$TCOMMENT <- ""
  } else {
    # if datefished is present then fill in the other required fields
  df_read$COMMENT <- NULL
  df_read$COMPLETESURVEY_ID_ID <- 0
  df_read$FISHED_ID_ID <- 0
    # if the DFO.COMMENT column is present, move it to the comment field for the database
  if("DFO.COMMENT" %in% names(df_read)) { df_read <- df_read %>% dplyr::rename("TCOMMENT" = "DFO.COMMENT") }
  # only select the required fields in case there are others hanging around
  df_read <- df_read[,c("SURVEY_ACCESS_KEY", "YEAR", "MONTH", "TCOMMENT", "DATEFISHED","COMPLETESURVEY_ID_ID", "FISHED_ID_ID")]
  # drop the DidNotFish column if present
  df_read$DidNotFish <- NULL
  }

  # check the datefished and get a default date
  dfs <- df_read[!is.na(df_read$DATEFISHED),]
  dfs$chkdte <- parse_date_time(dfs$DATEFISHED, orders = c("ymd", "dmy", "mdy"), quiet = FALSE)
  yr <- get_mode(year(as.Date(dfs$chkdte)))
  mn <- get_mode(month(as.Date(dfs$chkdte)))
  # fill in any blank rows with default values
  df_read$YEAR[is.na(df_read$YEAR)] <- yr
  df_read$MONTH[is.na(df_read$MONTH)] <- mn
  df_read$COMPLETESURVEY_ID_ID[is.na(df_read$SURVEY_ACCESS_KEY)] <- 0
  df_read$FISHED_ID_ID[is.na(df_read$FISHED_ID_ID)] <- 0
  df_read$DATEFISHED <- as.character(df_read$DATEFISHED)
  df_read <- df_read %>% dplyr::rename("LICENCE_ID" = "SURVEY_ACCESS_KEY") %>%
        dplyr::mutate(IREC_RESP_ID = as.integer(row_number()), .before = 1)


  # check to see if this filename has already been loaded
  # filename is the licence year + ' -iREC excludes'
  # if not set the src_id to -999 as a flag to get a new one later on.
  rws <- 0
  tsql <- paste0("select source_id from otolith_V1.creel_irec_Source where filename = :filename and data_type = '",  datatype,  "'")
  SRC_ID <- dbGetQuery(Ccon, tsql, list( filename = paste0(yr_name, ' - iREC ', datatype, 's ', datafile)))[[1]]
  if (identical(SRC_ID, numeric(0))){SRC_ID <- -999}

  # check to see if the file to load is from a shiny upload (named 0.csv) or the full name
  t1 <- gsub("\\", "/", datapath, fixed = TRUE)
  t2 <- gsub("\\", "/", paste0(dirname(datapath),"/",datafile), fixed = TRUE)
  # if it is a 0.csv type rename it to its proper name
  if (t1 != t2){
    fs::file_copy(datapath, paste0(dirname(datapath),"/",datafile))
  }

  # zip the source file read the file's bytes and delete the zip
  zip::zipr(zipfile = tmpfile, files = c(paste0(dirname(datapath),"/",datafile)), recurse = FALSE, include_directories = FALSE)
  if (file.exists(tmpfile)) {
    srcbytes <- read_file_bytes(tmpfile)
    file.remove(tmpfile)
  }
  # if we renamed the 0.csv then delete it.  If it was fully named then don't
  if (file.exists(paste0(dirname(datapath),"/",datafile)) & t1 != t2) {
    auto_notify(paste0("Removing ", paste0(dirname(datapath),"/",datafile)))
    file.remove(paste0(dirname(datapath),"/",datafile))
  }
# if the file has been loaded before, delete the data from the response table in chunks to show progress
 if (SRC_ID != -999) {
  auto_notify(paste0("Clearing existing ", datatype))
  chunks <- 1000
    WhereSQL <- paste0('where source_id_id = ', SRC_ID)
    tsql <- paste0("delete from otolith_v1.creel_irec_RESP ", WhereSQL, " and rownum <= ", chunks)
  # check for the total number of rows
  trwsSQL <- paste0("Select count(*) as rws from otolith_v1.creel_irec_resp ", WhereSQL)
  trws <- dbGetQuery(Ccon, trwsSQL)[[1]]
  rwls <- 0
  rwsdelete <- 0
  # delete a "chunk" worth of records repeat until no more records are deleted
  repeat {
    if (rwls < 1) {
      break
    }
    rslt <- ROracle::dbSendQuery(Ccon, tsql)
    rwls <- dbGetInfo(rslt)$rowsAffected
    rwsdelete <- rwsdelete + rwls
    auto_notify(paste0("Deleted ", rwsdelete, " of ", trws, " ", round((
      rwsdelete / trws
    ) * 100, 2), "%"))
    dbClearResult(rslt)
    dbCommit(Ccon)
    if (rwls < 1) {
      break
    }
  }
}

  # delete the data from the SOURCE table assuming the data has changed
  tsql <- paste0("Delete from otolith_V1.creel_irec_Source where filename = :filename and data_type = '", datatype, "'")
  rslt <- ROracle::dbSendQuery(Ccon, tsql, list(filename = paste0(yr_name, ' - iREC ', datatype, 's ', datafile)))
  dbClearResult(rslt)
  dbCommit(Ccon)

auto_notify(paste0("Processing ", datatype, " file for loading"))
# look up a new src_id
  if (SRC_ID == -999) {
    SRC_ID <- dbGetQuery(Ccon,
                         "select max(source_id) + 1 as SRC_ID from otolith_V1.creel_irec_source")[[1]]
  }

# insert into the SOURCE table the file identifiers
sql <- "
  INSERT INTO OTOLITH_V1.CREEL_IREC_SOURCE
    (source_id, filename, data_type, description, label, date_added)
    values(:p_id, :p_name, :p_datatype, :p_description, :p_label, sysdate)"

# 3. Bind variables
binds <- list(
  p_id     = SRC_ID,
  p_name   = paste0(yr_name, ' - iREC ', datatype, 's ', datafile),
  p_datatype = datatype,
  p_description = datapath,
  p_label = paste0(yr_name, ' - iREC ', datatype, 's ', datafile)
)
data2 <- data.frame(P_DATA = 1)
data2$P_DATA <- list(c(srcbytes))
data2$SRC_ID <- SRC_ID

rslt <- dbGetQuery(Ccon, sql, binds)
# 5. Commit the changes
dbCommit(Ccon)
# update the source table record to insert the zipped csv file
rslt2 <- dbGetQuery(Ccon, "update otolith_V1.creel_irec_source set data = :p_data where source_id = :SRC_ID", data2)
dbCommit(Ccon)

# get the next available response table ID and apply it to the dataset
RESP_ID <- dbGetQuery(Ccon, "select max(IREC_RESP_ID) as RESP_ID from otolith_V1.creel_irec_resp")[[1]]
df_read$IREC_RESP_ID <- df_read$IREC_RESP_ID + RESP_ID
df_read$SOURCE_ID_ID <- SRC_ID

# write the data to the database in chunks to show progress
    chunk_size <- 100
    row_count <- nrow(df_read)
    chunks <- split(df_read, (seq_len(row_count) - 1) %/% chunk_size)
    ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP", value = chunks[[1]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")

    # Append the remaining chunks
    if (length(chunks) > 1) {
      for (i in 2:length(chunks)) {
        ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP", value = chunks[[i]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
      }
    }

}

#' function wrapper to load responses and adjusts into CREST
#' datapath = the location of the source exclude file including file name
#' datafile = the source name of the file - shiny renames the files 0.csv when transferring them, so we need to
#'            know the real name
#' yr_name = the licence year the file is to be applied to
#' yr = the year the data is from
#' mnth_str = the string name for the month the data is from
#' df_read = the dataframe as loaded from the source file
#' datatype = either "Response" or "Adjust" for the type of data to be inserted
#' @export
Write_C_ResponsesIn <- function(datapath, datafile, yr_name, yr,  mnth_str, df_read, datatype) {
  # columns to include - probably a better way to do this.
  cols_to_keep_C <- c("SURVEYKEY", "TYPE", "STARTDAY", "ENDDAY","FIRST_NAME", "LAST_NAME", "EMAIL", "DIDNOTFISH", "COMPLETESURVEY", "DATEFISHED",
                      "YEAR", "MONTH", "DAY", "METHOD", "AREA", "TOTALJUVENILES", "FISHEDFROMLODGE", "FISHEDWITHGUIDE",
                      "CHECKCRABSPRAWNS", "CHECKSALMON", "COMMENT", "SALMON_CHINOOK_HATCH_KEPT", "SALMON_CHINOOK_HATCH_RELE",
                      "SALMON_CHINOOK_WILD_KEPT","SALMON_CHINOOK_WILD_RELE", "SALMON_CHINOOK_UNK_KEPT","SALMON_CHINOOK_UNK_RELE",
                      "SALMON_CHINOOK_SUBL_RELE", "SALMON_CHINOOK_US_HATCHERY_KEPT","SALMON_CHINOOK_US_WILD_KEPT",
                      "SALMON_CHINOOK_US_UNKOWN_KEPT", "SALMON_CHINOOK_US_HATCHERY_RELE","SALMON_CHINOOK_US_WILD_RELE",
                      "SALMON_CHINOOK_US_UNKOWN_RELE", "SALMON_COHO_HATCH_KEPT","SALMON_COHO_HATCH_RELE","SALMON_COHO_WILD_KEPT",
                      "SALMON_COHO_WILD_RELE","SALMON_COHO_UNK_KEPT","SALMON_COHO_UNK_RELE", "SALMON_SOCKEYE_KEPT","SALMON_SOCKEYE_RELE","SALMON_PINK_KEPT",
                      "SALMON_PINK_RELE","SALMON_CHUM_KEPT","SALMON_CHUM_RELE", "SALMON_UNKNOWN_KEPT","SALMON_UNKNOWN_RELE","HALIBUT_HALIBUT_KEPT",
                      "HALIBUT_HALIBUT_RELE","HALIBUT_LINGCOD_KEPT","HALIBUT_LINGCOD_RELE", "HALIBUT_ROCKFISH_BLACK_KEPT","HALIBUT_ROCKFISH_BLACK_RELE","HALIBUT_ROCKFISH_BOCACCIO_KEPT",
                      "HALIBUT_ROCKFISH_BOCACCIO_RELE","HALIBUT_ROCKFISH_CANARY_KEPT","HALIBUT_ROCKFISH_CANARY_RELE", "HALIBUT_ROCKFISH_CHINA_KEPT","HALIBUT_ROCKFISH_CHINA_RELE","HALIBUT_ROCKFISH_COPPER_KEPT",
                      "HALIBUT_ROCKFISH_COPPER_RELE","HALIBUT_ROCKFISH_QUILLBACK_KEPT","HALIBUT_ROCKFISH_QUILLBACK_RELE", "HALIBUT_ROCKFISH_TIGER_KEPT","HALIBUT_ROCKFISH_TIGER_RELE","HALIBUT_ROCKFISH_VERMILLION_KEPT",
                      "HALIBUT_ROCKFISH_VERMILLION_RELE","HALIBUT_ROCKFISH_YELLOWEYE_KEPT","HALIBUT_ROCKFISH_YELLOWEYE_RELE",
                      "HALIBUT_ROCKFISH_YELLOWTAIL_KEPT","HALIBUT_ROCKFISH_YELLOWTAIL_RELE","HALIBUT_ROCKFISH_OTHER_KEPT",
                      "HALIBUT_ROCKFISH_OTHER_RELE","FINFISH_ALTSAL_KEPT","FINFISH_ALTSAL_RELE", "FINFISH_COD_KEPT","FINFISH_COD_RELE","FINFISH_CTT_KEPT",
                      "FINFISH_CTT_RELE","FINFISH_DOL_KEPT","FINFISH_DOL_RELE", "FINFISH_EULACHON_KEPT","FINFISH_EULACHON_RELE","FINFISH_GRNLING_KEPT",
                      "FINFISH_GRNLING_RELE","FINFISH_HERRING_KEPT","FINFISH_HERRING_RELE","FINFISH_MAC_KEPT","FINFISH_MAC_RELE","FINFISH_NANCHOVY_KEPT",
                      "FINFISH_NANCHOVY_RELE","FINFISH_PACSANDLANCE_KEPT","FINFISH_PACSANDLANCE_RELE", "FINFISH_PACSARDINE_KEPT","FINFISH_PACSARDINE_RELE","FINFISH_PERCH_KEPT",
                      "FINFISH_PERCH_RELE","FINFISH_SABLEFISH_KEPT","FINFISH_SABLEFISH_RELE", "FINFISH_SCULPIN_KEPT","FINFISH_SCULPIN_RELE","FINFISH_SPINYDOG_KEPT",
                      "FINFISH_SPINYDOG_RELE","FINFISH_SALMON_SHARK_KEPT","FINFISH_SALMON_SHARK_RELE", "FINFISH_OTHER_SHARKS_RELE","FINFISH_SKATE_KEPT","FINFISH_SKATE_RELE",
                      "FINFISH_SMELT_KEPT","FINFISH_SMELT_RELE","FINFISH_SOLE_KEPT",  "FINFISH_SOLE_RELE","FINFISH_STEEL_KEPT","FINFISH_STEEL_RELE",
                      "FINFISH_STURGEON_RELE","FINFISH_ALBACORE_TUNA_KEPT","FINFISH_ALBACORE_TUNA_RELE", "FINFISH_BLUEFIN_TUNA_KEPT","FINFISH_BLUEFIN_TUNA_RELE","FINFISH_TUNA_OTHER_KEPT",
                      "FINFISH_TUNA_OTHER_RELE","FINFISH_WOLF_RELE","FINFISH_SQUID_KEPT", "FINFISH_OTH_KEPT","FINFISH_OTH_RELE","PRAWNCRAB_DUNCRABLEGAL_KEPT",
                      "PRAWNCRAB_REDRCRAB_KEPT","PRAWNCRAB_AKINGCRAB_KEPT","PRAWNCRAB_BOXCRAB_KEPT",  "PRAWNCRAB_SHORECRAB_KEPT","PRAWNCRAB_OTHUNCRAB_KEPT","PRAWNCRAB_COONSHRIMP_KEPT",
                      "PRAWNCRAB_HUMPSHRIMP_KEPT","PRAWNCRAB_OTHSHRIMP_KEPT","PRAWNCRAB_PRAWN_KEPT", "PRAWNCRAB_GSHRIMP_KEPT","BIVALVES_RAZORCLAM_KEPT","BIVALVES_BUTTERCLAM_KEPT",
                      "BIVALVES_HORSECLAM_KEPT","BIVALVES_LNECK_KEPT","BIVALVES_MANILA_KEPT", "BIVALVES_SOFTSHELL_KEPT","BIVALVES_VARNISH_KEPT","BIVALVES_GEODUCK_KEPT",
                      "BIVALVES_UNKCLAM_KEPT","BIVALVES_BLUEMUSSEL_KEPT","BIVALVES_CALIMUSSEL_KEPT","BIVALVES_PACIFICOYSTER_KEPT","BIVALVES_PINKSCALLOP_KEPT","BIVALVES_ROCKSCALLOP_KEPT",
                      "BIVALVES_WEATHERVANE_KEPT","BIVALVES_UNKSCALLOP_KEPT","BIVALVES_COCKLE_KEPT", "OTHSHELLFISH_GIANTBARN_KEPT","OTHSHELLFISH_GOOSEBARN_KEPT","OTHSHELLFISH_MOONSNAIL_KEPT",
                      "OTHSHELLFISH_LIMPET_KEPT","OTHSHELLFISH_SEAURCHIN_KEPT","OTHSHELLFISH_SEACUCUMBER_KEPT", "OTHSHELLFISH_SANDDOLLAR_KEPT","OTHSHELLFISH_SEASTAR_KEPT","OTHSHELLFISH_OCTOPUS_KEPT",
                      "OTHSHELLFISH_OTHERUNK_KEPT", "CRAB_CENTREOFFISHING_LATITUDE", "CRAB_CENTREOFFISHING_LONGITUDE", "PRAWN_CENTREOFFISHING_LATITUDE", "PRAWN_CENTREOFFISHING_LONGITUDE",
                      "HALIBUTLENGTH.1", "HALIBUTLENGTH.2", "HALIBUTLENGTH.3", "HALIBUTLENGTH.4", "HALIBUTLENGTH.5", "HALIBUTLENGTH.6", "HALIBUTLENGTH.7", "HALIBUTLENGTH.8",
                      "NUMBERACTIVITY", "VISITS", "SAVECOUNT", "SESSIONS", "DATECREATED", "DATECREATED_DAY", "DATECREATED_MONTH", "DATECREATED_YEAR", "DATECOMPLETED",
                      "DATECOMPLETED_DAY", "DATECOMPLETED_MONTH", "DATECOMPLETED_YEAR")

  auto_notify(paste0("mnth_str = ", mnth_str))

# Not sure the history of this.  Seems if you don't pass a mnth_str we try to
# figure it out from the file name

  if (is.null(mnth_str)){
    mnth_str <- ""
    for (m in month_nms) {
      if (grepl(tolower(m), tolower(basename(datafile)))) {
        mnth_str <- m
        break
      }
    }
  }
 # the same as above but short month names
  if (mnth_str == "") {
    for (m in month_nms_shrt) {
      if (grepl(tolower(m), tolower(basename(datafile)))) {
        mnth_str <- m
        break
      }
    }
  }

  auto_notify(paste0("mnth_str = ", mnth_str))
  auto_notify(yr_name)

  rslt <- FALSE
  # set up a temporary zip file to store the source data in the database
  tmpfile <- tempfile(fileext=".zip")
  # check the dates if the data is adjusts
  if (datatype == "Adjust") {
    df_read <- fixAdjDates(df_read, '')
  }
  # make the column named upper case for consistency
  names(df_read) <- toupper(names(df_read))

  # rework adjusts to make them the same as responses
  if (datatype == "Adjust") {
    df_read <- df_read %>% rename("FISHEDFROMLODGE" = "LODGE",
                                  "FISHEDWITHGUIDE" = "GUIDED",
                                  "DIDNOTFISH" = "DID_NOT_FISH",
                                  "TOTALJUVENILES" = "JUV_EFFORT_DAYS")
    if ("COMMENT" %notin% names(df_read)) {
      df_read$COMMENT <- "No Comment provided"
    }

    for (c in names(df_read)) {
      if (grepl('COMMENT', c, ignore.case = TRUE)) {
        df_read <- fixComments(df_read, c)
      }
    }

    df_read <- linkTables(df_read, FALSE)
    df_read <- fixValues(df_read)

    df_read$YEAR <- yr
    df_read$MONTH <- match(mnth_str, month.name)
    df_read$DAY <- day(df_read$DATEFISHED)
    df_read$COMPLETESURVEY <- 'Y'
  }
 # check to see if we using a shiny transfer or the source data path
  t1 <- gsub("\\", "/", datapath, fixed = TRUE)
  t2 <- gsub("\\", "/", paste0(dirname(datapath),"/",datafile), fixed = TRUE)
 # if shiny upload then rename it to its original name
  if (t1 != t2){
    fs::file_copy(datapath, paste0(dirname(datapath),"/",datafile))
  }
 # build a zipfile of the source data
  zip::zipr(zipfile = tmpfile, files = c(paste0(dirname(datapath),"/",datafile)), recurse = FALSE, include_directories = FALSE)
  if (file.exists(tmpfile)) {
    # read the zipfile into memory (bytes)
    srcbytes <- read_file_bytes(tmpfile)
    #Delete file if it exists
    file.remove(tmpfile)
  }
 #if we were renaming the source file, delete it as we are done with it
  if (file.exists(paste0(dirname(datapath),"/",datafile)) & t1 != t2) {
    auto_notify(paste0("Removing ", paste0(dirname(datapath),"/",datafile)))
    file.remove(paste0(dirname(datapath),"/",datafile))
  }

baseName <- tools::file_path_sans_ext(datafile)

dfr <- df_read
# essentially check if the data is from PRA or EKOS.
if ("REPYEAR" %in% names(dfr)) {
  SurveyStartDate <- as.Date("2012-04-01")
} else {
  if ("Licence_ID" %in% names(dfr)){
    dfr <- dfr %>%
      dplyr::rename(surveykey = "Licence_ID")
  }
  SurveyStartDate <- as.Date(format(Sys.Date(), "%Y-%m-%d") )
}
excludes <- c()
adjusts <- c()
dfrr <- dfr

# if we need to, convert the data from EKOS format to PRA
if (SurveyStartDate < PraStartDate & datatype == "Response") {
  survey_results <-
    df <- convertEKOSData (dfr)
  df <- df %>%
    mutate(did_not_fish = if_else(did_not_fish == TRUE, Pradidnotfish, PraDidFish)) %>%
    dplyr::rename(didnotfish = did_not_fish)
} else {
  df <- dfr
}
# fix common differences in what is delivered vs what is expected
 df1 <- fixValues(df)
 # load look up tables for codes
 lus <- load_CR_lus()
 # link the look ups get the needed ID codes
 df1 <- link_Lus(df1, lus)
 # fix comments for length, emails and phone numbers
 df1 <- fixComments(df1)
 # add the data type to the data if not present (older data?)
 if ("TYPE" %notin% names(df1)) {
   df1$TYPE <- paste0("iREC ", mnth_str, " ", yr)
 }
 # pull only columns we need now
 df_filtered <- filterColumns(df1,c("LRG_AREA_ID_ID","METHOD_ID", "SHELLFISH_ID", "LODGE_ID", "GUIDED_ID", "FISHED_ID", "COMPLETESURVEY_ID", cols_to_keep_C))
 # add row numbers for linking later
 df_filtered <- df_filtered %>%
   dplyr::mutate(TMP_ID = as.integer(row_number()), .before = 1)
 strataCols <- c('YEAR', 'MONTH', 'DAY', 'SURVEYKEY', 'LRG_AREA_ID_ID', 'METHOD_ID', 'LODGE_ID','GUIDED_ID',
                 'FISHED_ID', 'COMPLETESURVEY_ID', 'DATEFISHED', 'TOTALJUVENILES')
 if (datatype == "Response") {
 strataCols <- c(strataCols, "COMMENT", "NUMBERACTIVITY", "SAVECOUNT", "VISITS", "SESSIONS", "DATECREATED", "DATECREATED_DAY",
                 "DATECREATED_MONTH", "DATECREATED_YEAR", "DATECOMPLETED","DATECOMPLETED_DAY", "DATECOMPLETED_MONTH", "DATECOMPLETED_YEAR", "SHELLFISH_ID", "FIRST_NAME", "LAST_NAME", "EMAIL")
 } else {
   strataCols <- c(strataCols, "COMMENT", "SHELLFISH_ID", "FIRST_NAME", "LAST_NAME", "EMAIL")
}

  oth_cols <- c("DIDNOTFISH", "COMPLETESURVEY", "FISHEDFROMLODGE", "FISHEDWITHGUIDE", "CHECKCRABSPRAWNS","TYPE", "STARTDAY", "ENDDAY")
  # pull the strat columns out
  df_filtered <- df_filtered[, !(names(df_filtered) %in% oth_cols)]
  # pull the item type columns
   itmCols <- names(df_filtered)[!names(df_filtered) %in% c('TMP_ID',strataCols)]

 resp <- df_filtered[c('TMP_ID', strataCols)]
 df_itms <- df_filtered[,!names(df_filtered) %in% strataCols]

 # pivot the item values so many rows with 1 item - value pair
 respV <- df_itms %>%
   pivot_longer(all_of(itmCols),names_to = "CAT",
                values_to = "VALUE",
                values_transform = list(VALUE = as.character))
 # link the items we know of to get the ID code for each
 respVI <- respV %>% left_join(lus["CREEL_IREC_ITEM"][[1]], join_by("CAT"=="COLUMN_NAME"))
 # look for items we don't know of
 respVB <- unique(respVI$CAT[is.na(respVI$ITEM_ID)])
# if we find any, add them to the item table
 if (length(respVB > 0)) {
   itm_id <- dbGetQuery(Lcon,
                        "select max(item_id) as itm_id from otolith_v1.creel_irec_item")
   for (itm in 1:length(respVB)) {
     isql <- paste0(
       "insert into otolith_v1.creel_irec_item (item_id, text, COLUMN_NAME) values(", itm_id[[1]] + itm, ",'", respVB[itm],"','", respVB[itm],"')"
     )
     rslt <- ROracle::dbSendQuery(Ccon, isql)
     auto_notify(dbGetRowsAffected(rslt))
     dbClearResult(rslt)
   }
   dbCommit(Ccon)
   lus <- load_CR_lus()
   # try linking the item code table again.
   respVI <- respV %>% left_join(lus["CREEL_IREC_ITEM"][[1]], join_by("CAT"=="COLUMN_NAME"))

 }
 # get a list of year/month combos to work through.  Really should only be 1
 mnths <- unique(resp[c('YEAR', 'MONTH')])
 mnths <- haven::zap_labels(mnths, user_na = TRUE)
 respSQL <- 'INSERT INTO otolith_v1.creel_irec_RESP (IREC_RESP_ID, YEAR, MONTH, DAY, LICENCE_ID, LRG_AREA_ID_ID, METHOD_ID_ID, LODGE_ID_ID, GUIDED_ID_ID, FISHED_ID_ID, COMPLETESURVEY_ID_ID,
 DATEFISHED, TOTALJUVENILES, CHECKCRAB_ID, CHECKPRAWN_ID, SHELLFISH_ID_ID, TCOMMENT, SOURCE_ID_ID,
 NUMBERACTIVITY, SAVECOUNT, VISITS, SESSIONS, DATECREATED, DATECREATED_DAY, DATECREATED_MONTH, DATECREATED_YEAR, DATECOMPLETED,
 DATECOMPLETED_DAY, DATECOMPLETED_MONTH, DATECOMPLETED_YEAR ) Values'

 for (m in 1:nrow(mnths)) {
   yr <- mnths[m, "YEAR"]
   mnth <- mnths[m, "MONTH"]
   # delete any existing item value data for that year/month combo.
   auto_notify(paste0("Clearing existing ", datatype, " values"))
   chunks <- 15000
   # responses are unique to a set of year month values
   if (datatype == 'Response') {
     wsql <- paste0(
       "WHERE irec_resp_id_id IN
                      (SELECT irec_resp_id FROM otolith_v1.creel_irec_resp r left join otolith_V1.creel_irec_source s on r.source_id_id = s.source_id
                        WHERE r.year = ",  yr, " AND r.month = ", mnth, " and s.data_type = 'Response')"
     )
     tsql <- paste0("Delete from otolith_V1.creel_irec_resp_V ",  wsql, " and rownum <= ", chunks)
   } else if (datatype == 'Adjust') {
     # there can be many adjusts for the same year/month, but they must be named uniquely, so clear them out based on the filename
     tsql <- paste0("select source_id from otolith_V1.creel_irec_Source where filename = :filename and data_type = '",  datatype,  "'")
     #browser()
     SRC_ID <- dbGetQuery(Ccon, tsql, list( filename = paste0(yr_name, ' - ', mnth_str, ' - iREC ', datatype, 's ', datafile)))[[1]]
     if (identical(SRC_ID, numeric(0))){SRC_ID <- -999}
     wsql <- paste0(
       "where irec_resp_id_id in (select irec_resp_id from otolith_V1.creel_irec_resp where year = ",  yr, " and month = ",  mnth,
       " and source_id_id = ",  SRC_ID, ")"
     )
     tsql <- paste0("Delete from otolith_V1.creel_irec_resp_V " , wsql, " and rownum <= ", chunks)
   }
     # we delete them in chunks so we can see progress, otherwise it grinds on with no idea what is going on.
     trwsSQL <- paste0("Select count(*) as rws from otolith_v1.creel_irec_resp_v ", wsql)
     trws <- dbGetQuery(Ccon, trwsSQL)[[1]]
     rwls <- 1
     rwsdelete <- 0
     repeat {
       rslt <- ROracle::dbSendQuery(Ccon, tsql)
       rwls <- dbGetInfo(rslt)$rowsAffected
       rwsdelete <- rwsdelete + rwls
       auto_notify(paste0("Deleted ", rwsdelete, " of ", trws, " ", round((
         rwsdelete / trws
       ) * 100, 2), "%"))
       dbClearResult(rslt)
       dbCommit(Ccon)
       if (rwls < 1) {
         break
       }
     }


     auto_notify(paste0("Clearing existing ", datatype))
     chunks <- 1000
     # do the same but for the response strata. In chunks to monitor progress
     if (datatype == 'Response') {
       WhereSQL <- paste0("WHERE irec_resp_id IN
                      (SELECT irec_resp_id FROM otolith_v1.creel_irec_resp r left join otolith_V1.creel_irec_source s on r.source_id_id = s.source_id
                        WHERE r.year = ", yr, " AND r.month = ", mnth, " and s.data_type = 'Response')"  )
       tsql <- paste0("delete from otolith_v1.creel_irec_RESP ",  WhereSQL,  " and rownum <= ", chunks)
     } else {
       WhereSQL <- paste0("where year = ", yr, ' and month = ', mnth, ' and source_id_id = ', SRC_ID)
       tsql <- paste0("delete from otolith_v1.creel_irec_RESP ", WhereSQL, " and rownum <= ", chunks)
     }

     trwsSQL <- paste0("Select count(*) as rws from otolith_v1.creel_irec_resp ", WhereSQL)
     trws <- dbGetQuery(Ccon, trwsSQL)[[1]]
     rwls <- 1
     rwsdelete <- 0
     repeat {
       rslt <- ROracle::dbSendQuery(Ccon, tsql)
       rwls <- dbGetInfo(rslt)$rowsAffected
       rwsdelete <- rwsdelete + rwls
       auto_notify(paste0("Deleted ", rwsdelete, " of ", trws, " ", round((
         rwsdelete / trws
       ) * 100, 2), "%"))
       dbClearResult(rslt)
       dbCommit(Ccon)
       if (rwls < 1) {
         break
       }
     }
 }

 # delete the source record
 tsql <- paste0("Delete from otolith_V1.creel_irec_Source where filename = :filename and data_type = '", datatype, "'")
 rslt <- ROracle::dbSendQuery(Ccon, tsql, list(filename = paste0(yr_name,' - ', mnth_str, ' - iREC ', datatype, 's ', datafile)))
 dbClearResult(rslt)
 dbCommit(Ccon)
 auto_notify(paste0("Processing ", datatype, " file for loading"))
 # reuse the source_id if we had one, otherwise look one up
 if (SRC_ID <- -999) {
 SRC_ID <- dbGetQuery(Ccon, "select max(source_id) + 1 as SRC_ID from otolith_V1.creel_irec_source")[[1]]
 }
 RESP_ID <- dbGetQuery(Ccon, "select max(IREC_RESP_ID) as RESP_ID from otolith_V1.creel_irec_resp")[[1]]
 # insert the source iD record
 sql <- "
  INSERT INTO OTOLITH_V1.CREEL_IREC_SOURCE
    (source_id, filename, data_type, description, label, date_added)
    values(:p_id, :p_name, :p_datatype, :p_description, :p_label, sysdate)"

 # 3. Bind variables
 binds <- list(
   p_id     = SRC_ID,
   p_name   = paste0(yr_name,' - ', mnth_str, ' - iREC ', datatype, 's ', datafile),
   p_datatype = datatype,
   p_description = datapath,
   p_label = paste0(yr_name,' - ', mnth_str, ' - iREC ', datatype, 's')
   )
 data2 <- data.frame(P_DATA = 1)
 data2$P_DATA <- list(c(srcbytes))
 data2$SRC_ID <- SRC_ID

 rslt <- dbGetQuery(Ccon, sql, binds)
 # 5. Commit the changes
 dbCommit(Ccon)
 # update it with the zipfile blob
 rslt2 <- dbGetQuery(Ccon, "update otolith_V1.creel_irec_source set data = :p_data where source_id = :SRC_ID", data2)
 dbCommit(Ccon)
 # start getting the dataframe to match the columns in the database
 respTbl <- resp %>% dplyr::rename(
   IREC_RESP_ID = "TMP_ID",
   LICENCE_ID = "SURVEYKEY",
   TCOMMENT = "COMMENT",
   METHOD_ID_ID = "METHOD_ID",
   LODGE_ID_ID = "LODGE_ID",
   GUIDED_ID_ID = "GUIDED_ID",
   FISHED_ID_ID = "FISHED_ID",
   COMPLETESURVEY_ID_ID = "COMPLETESURVEY_ID",
   SHELLFISH_ID_ID = "SHELLFISH_ID"
 )
 names(respTbl) <- toupper(names(respTbl))


 respTbl$IREC_RESP_ID <- respTbl$IREC_RESP_ID + RESP_ID
 respTbl$SOURCE_ID_ID <- SRC_ID

 if (datatype == 'Response') {
   respTbl$DATECREATED <- as.character(respTbl$DATECREATED)
   respTbl$DATECOMPLETED <- as.character(respTbl$DATECOMPLETED)
   respTbl$NUMBERACTIVITY	 <- as.character(respTbl$NUMBERACTIVITY)
   respTbl$SAVECOUNT	 <- as.character(respTbl$SAVECOUNT)
   respTbl$VISITS	 <- as.character(respTbl$VISITS)
   respTbl$SESSIONS	 <- as.character(respTbl$SESSIONS)
   respTbl$DATECREATED_DAY	 <- as.character(respTbl$DATECREATED_DAY)
   respTbl$DATECREATED_MONTH	 <- as.character(respTbl$DATECREATED_MONTH)
   respTbl$DATECREATED_YEAR	 <- as.character(respTbl$DATECREATED_YEAR)
   respTbl$DATECOMPLETED_DAY	 <- as.character(respTbl$DATECOMPLETED_DAY)
   respTbl$DATECOMPLETED_MONTH	 <- as.character(respTbl$DATECOMPLETED_MONTH)
   respTbl$DATECOMPLETED_YEAR	 <- as.character(respTbl$DATECOMPLETED_YEAR)
 }

 respTbl$DATEFISHED <- as.character(respTbl$DATEFISHED)
 respTbl$YEAR <- as.character(respTbl$YEAR)
 respTbl$MONTH <- as.character(respTbl$MONTH)
 respTbl$DAY <- as.character(respTbl$DAY)
 respTbl$LODGE_ID_ID <- as.numeric(respTbl$LODGE_ID_ID)
 respTbl$GUIDED_ID_ID <- as.numeric(respTbl$GUIDED_ID_ID)
 respTbl$FISHED_ID_ID <- as.numeric(respTbl$FISHED_ID_ID)
 respTbl$COMPLETESURVEY_ID_ID <- as.numeric(respTbl$COMPLETESURVEY_ID_ID)
 respTbl$TOTALJUVENILES	 <- as.character(respTbl$TOTALJUVENILES)
 respTbl$CHECKCRAB_ID <- 0
 respTbl$CHECKPRAWN_ID <- 0
 # we encrypt the email and names so someone can't stumble upon them
  data_path <- system.file("extdata", "sodium", package = "iRECUIpkg")
  load(file=data_path)
  K_nonce <- sodium::random(24)
  respTbl$FIRST_NAME <- lapply(respTbl$FIRST_NAME, function(x){
  paste0(base64enc::base64encode(sodium::data_encrypt(serialize(x, NULL), K_key, K_nonce)), "#", base64enc::base64encode(K_nonce))
  })

  respTbl$LAST_NAME <- lapply(respTbl$LAST_NAME, function(x){
    paste0(base64enc::base64encode(sodium::data_encrypt(serialize(x, NULL), K_key, K_nonce)), "#", base64enc::base64encode(K_nonce))
  })

  respTbl$EMAIL <- lapply(respTbl$EMAIL, function(x){
    paste0(base64enc::base64encode(sodium::data_encrypt(serialize(x, NULL), K_key, K_nonce)), "#", base64enc::base64encode(K_nonce))
  })
 # finish formatting the dataframe to match the database table
 if (datatype == 'Response') {
 respTbl <- respTbl[,c("IREC_RESP_ID",
                       "YEAR",
                       "MONTH",
                       "DAY",
                       "LICENCE_ID",
                       "FIRST_NAME",
                       "LAST_NAME",
                       "EMAIL",
                       "LRG_AREA_ID_ID",
                       "METHOD_ID_ID",
                       "LODGE_ID_ID",
                       "GUIDED_ID_ID",
                       "FISHED_ID_ID",
                       "COMPLETESURVEY_ID_ID",
                       "DATEFISHED",
                       "TOTALJUVENILES",
                       "TCOMMENT",
                       "NUMBERACTIVITY",
                       "SAVECOUNT",
                       "VISITS",
                       "SESSIONS",
                       "DATECREATED",
                       "DATECREATED_DAY",
                       "DATECREATED_MONTH",
                       "DATECREATED_YEAR",
                       "DATECOMPLETED",
                       "DATECOMPLETED_DAY",
                       "DATECOMPLETED_MONTH",
                       "DATECOMPLETED_YEAR",
                       "SOURCE_ID_ID",
                       "CHECKCRAB_ID",
                       "CHECKPRAWN_ID",
                       "SHELLFISH_ID_ID"
 )]
 } else {
   respTbl <- respTbl[,c("IREC_RESP_ID",
                         "YEAR",
                         "MONTH",
                         "DAY",
                         "LICENCE_ID",
                         "FIRST_NAME",
                         "LAST_NAME",
                         "EMAIL",
                         "LRG_AREA_ID_ID",
                         "METHOD_ID_ID",
                         "LODGE_ID_ID",
                         "GUIDED_ID_ID",
                         "FISHED_ID_ID",
                         "COMPLETESURVEY_ID_ID",
                         "DATEFISHED",
                         "TOTALJUVENILES",
                         "TCOMMENT",
                         "SOURCE_ID_ID",
                         "CHECKCRAB_ID",
                         "CHECKPRAWN_ID",
                         "SHELLFISH_ID_ID"
   )]
 }
 auto_notify(paste0("Writing ", datatype))

 if (length(shiny::getDefaultReactiveDomain()) > 0) {
   incProgress <- shiny::incProgress
   withProgress <- shiny::withProgress
 } else {
   incProgress <- function(...) {}
   withProgress <- function(...) list(...)[["expr"]]
 }

 withProgress(message = 'Processing ', value = 0, {
   # write the data to the database in chunks so we can monitor the progress
   # Calculate chunks
   chunk_size <- 5000
   row_count <- nrow(respTbl)
   chunks <- split(respTbl, (seq_len(row_count) - 1) %/% chunk_size)
   # Set up a progress bar
   pb <- progress_bar$new(total = length(chunks))
   # Write the first chunk to create/overwrite the table
   incProgress(1/ length(chunks), detail = paste("Loading", datatype, "Values  ", 1))
   ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP", value = chunks[[1]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
   pb$tick()
   # Append the remaining chunks
   if (length(chunks) > 1) {
     for (i in 2:length(chunks)) {
       incProgress(i / length(chunks), detail = paste("Loading", datatype, "Values  ", i))
       ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP", value = chunks[[i]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
       pb$tick()
     }
   }

 })

 auto_notify(paste0("Done Writing ", datatype))

 # do the same for the item values
  respV1 <- respVI[!is.na(respVI$VALUE) & respVI$VALUE != 0, ] %>%
    dplyr::mutate(TMPV_ID = as.integer(row_number()), .before = 1)

 respVITbl <- respV1[, c("TMPV_ID", "TMP_ID", "ITEM_ID", "VALUE", "TEXT")]
 respVITbl$TMP_ID <- respVITbl$TMP_ID + RESP_ID
 RESPV_ID <- dbGetQuery(Ccon,
                        "select max(VALUE_ID) as respV_id from otolith_V1.creel_irec_resp_V")[[1]]
 respVITbl$TMPV_ID <- respVITbl$TMPV_ID + RESPV_ID

 respVITbl <- respVITbl %>% dplyr::rename(VALUE_ID = "TMPV_ID",
                                     IREC_RESP_ID_ID = "TMP_ID",
                                     ITEM_ID_ID = "ITEM_ID")
 auto_notify(paste0("Writing ", datatype, " values"))

 if (length(shiny::getDefaultReactiveDomain()) > 0) {
   incProgress <- shiny::incProgress
   withProgress <- shiny::withProgress
 } else {
   incProgress <- function(...) {}
   withProgress <- function(...) list(...)[["expr"]]
 }

 withProgress(message = 'Processing ', value = 0, {
 # Calculate chunks
 chunk_size <- 50000
 row_count <- nrow(respVITbl)
 chunks <- split(respVITbl, (seq_len(row_count) - 1) %/% chunk_size)
 # Set up a progress bar
 pb <- progress_bar$new(total = length(chunks))
 # Write the first chunk to create/overwrite the table
 incProgress(1 / length(chunks), detail = paste("Loading", datatype, "Values  ", 1))
 ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP_V", value = chunks[[1]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
 pb$tick()
 # Append the remaining chunks
 if (length(chunks) > 1) {
   for (i in 2:length(chunks)) {
     incProgress(i / length(chunks), detail = paste("Loading", datatype, "Values  ", i))
     ROracle::dbWriteTable(con = Ccon, name = "CREEL_IREC_RESP_V", value = chunks[[i]], append = TRUE, row.names = FALSE, schema = "OTOLITH_V1")
     pb$tick()
   }
 }

})

 dbCommit(Ccon)
 return(TRUE)
}

#' function to load look-ups from CREST to aid in formatting response data for loading into CREST
load_CR_lus <- function() {
  rtn_list <- list()
  df_tbl_nmes <- c('CREEL_IREC_ITEM', 'CREEL_IREC_AREA', 'CREEL_IREC_METHOD', 'CREEL_IREC_SHELLFISH')
  for (q in df_tbl_nmes) {

    clmsql <- paste0("select column_name, data_type from all_tab_columns where 1=1 and owner = 'OTOLITH_V1' and table_name = '", q, "'")
    clms <- getdata(Ccon, clmsql, FALSE)
    qry <- "Select "
    for (c in 1:nrow(clms)) {
      if (!endsWith(qry, " ")){qry <- paste0(qry, ", ") }
      if (clms[[2]][[c]] == 'VARCHAR2') {
        qry <- paste0(qry, 'upper("', clms[[1]][[c]], '") as "', clms[[1]][[c]],'"')
      } else {
        qry <- paste0(qry, '"',clms[[1]][[c]], '" as "', clms[[1]][[c]], '"')
      }
    }
    qry <- paste0(qry, " from OTOLITH_V1.", trimws(q))
    auto_notify(paste0(q, ' Starting '))
    #print(qry)
    tmp <- getdata(Ccon, qry, FALSE)

    tmp_lst <- list(tmp)
    tmp_lst <- setNames(tmp_lst, q)

    if (length(rtn_list) == 0) {
      rtn_list <- tmp_lst
    } else {
      rtn_list <- c(rtn_list, tmp_lst)
    }
    auto_notify(paste0(q, ' Done ', nrow(q)))
  }

  return(rtn_list)


}

#' function to link look-ups to the SAV file in dataframe form
link_Lus <- function(df_sav, lus) {

  CREEL_IREC_AREA <- lus["CREEL_IREC_AREA"][[1]]
  CREEL_IREC_METHOD <- lus["CREEL_IREC_METHOD"][[1]]
  CREEL_IREC_SHELLFISH <- lus["CREEL_IREC_SHELLFISH"][[1]]

  if ("AREASCODE" %notin% toupper(names(df_sav))){
    df1 <- df_sav %>% left_join(CREEL_IREC_AREA, join_by("AREA" == "TEXT"))
    names(df1)[names(df1) == 'AREA'] <- 'Areascode'
    names(df1)[names(df1) == 'TYPE_NAME'] <- 'AREA'
  } else {
    df1 <- df_sav
    df1$AREA <- NULL
  }

  if ("METHODCODE" %notin% toupper(names(df_sav))) {
    CREEL_IREC_METHOD$TEXT <- toupper(CREEL_IREC_METHOD$TEXT)
    df1$METHOD <- toupper(df1$METHOD)
    df1 <- df1 %>% left_join(CREEL_IREC_METHOD, join_by('METHOD' == 'TEXT'))
    names(df1)[names(df1) == 'METHOD'] <- 'METHODCODE'
  } else {
    df1$METHOD <- NULL
  }

  if ("CHECKCRABSPRAWNS_TXT" %notin% toupper(names(df_sav))) {
    CREEL_IREC_SHELLFISH$VALUE <- as.character(CREEL_IREC_SHELLFISH$VALUE)
    if (haven::is.labelled(df1$CHECKCRABSPRAWNS)) {
      df1 <- haven::zap_labels(df1, user_na = TRUE)
    }
    if (class(df1$CHECKCRABSPRAWNS) != "character") {
      df1$CHECKCRABSPRAWNS <- as.character(df1$CHECKCRABSPRAWNS)
    }
    df1 <- df1 %>% left_join(CREEL_IREC_SHELLFISH,
                             join_by("CHECKCRABSPRAWNS" == "VALUE"))
    df1$CheckCrabsPrawnsTXT <- df1$CHECKCRABSPRAWNS
    names(df1)[names(df1) == 'TEXT'] <- 'CHECKCRABSPRAWNS_TXT'
  }

    df1$FISHEDFROMLODGE <- as.character(df1$FISHEDFROMLODGE)
    df1$FISHEDWITHGUIDE <- as.character(df1$FISHEDWITHGUIDE)
    df1$DIDNOTFISH <- as.character(df1$DIDNOTFISH)
    df1$COMPLETESURVEY <- as.character(df1$COMPLETESURVEY)

    df1 <- df1 %>%
      mutate(
        LODGE_ID = case_when (
          FISHEDFROMLODGE == '1' ~ '2',
          FISHEDFROMLODGE == 'Yes' ~ '2',
          FISHEDFROMLODGE == '2' ~ '1',
          FISHEDFROMLODGE == 'No' ~ '1',
          is.na(FISHEDFROMLODGE) ~ '0',
          TRUE ~ '0'
        )
      ) %>%
      mutate(
        GUIDED_ID = case_when (
          FISHEDWITHGUIDE == '1' ~ '2',
          FISHEDWITHGUIDE == 'Yes' ~ '2',
          FISHEDWITHGUIDE == '2' ~ '1',
          FISHEDWITHGUIDE == 'No' ~ '1',
          is.na(FISHEDWITHGUIDE) ~ '0',
          TRUE ~ '0'
        )
      ) %>%
      mutate(
        FISHED_ID = case_when (
          DIDNOTFISH == '1' ~ '2',
          DIDNOTFISH == 'Yes' ~ '2',
          DIDNOTFISH == '2' ~ '1',
          DIDNOTFISH == 'No' ~ '1',
          is.na(DIDNOTFISH) ~ '0',
          TRUE ~ '0'
        )
      ) %>%
      mutate(
        COMPLETESURVEY_ID = case_when (
          COMPLETESURVEY == 'COMPLETE' ~ '2',
          COMPLETESURVEY == 'INCOMPLETE' ~ '1',
          is.na(COMPLETESURVEY) ~ '0',
          TRUE ~ '0'
        )
      )

  return(df1)
}

testing <- function() {
  if (!shiny::isRunning()){
    month_nms <<- c("January", "February","March","April","May", "June", "July",
                    "August","September","October", "November", "December")
    month_nms_shrt <<- c("Jan", "Feb","Mar","Apr","May", "Jun", "Jul",
                         "Aug","Sept","Oct", "Nov", "Dec")


    source("R\\common_Code.R")
    source("R\\KREST_common.R")
    source("R\\Common_UniformSav.R")
    #SetupKrest("P")
    tryCatch({
      lapply(DBI::dbListConnections(ROracle::Oracle()), DBI::dbDisconnect)
    },

    error = function(e) {
      # Error Handler
      message(paste("DBI failed:", e$message))
      rws <- 0
    },

    warning = function(w) {
      # Warning Handler
      message(paste("DBI write:", w$message))
      invokeRestart("muffleWarning")
    },

    finally = {
      # Cleanup / Log action
      auto_notify("DBI finished.")
    })
    #check to see if oracle credentials exist
    if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsL.txt"))) {
      oracredsL <<- TRUE
      Lcon <<- setupOracleConn("L")
      if (is.null(Lcon)) {
        oracredsL <<- FALSE
      }
    } else {
      oracredsL <<- FALSE
    }

    if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsP.txt"))) {
      oracredsP <<- TRUE
      Ccon <<- setupOracleConn("P")
      if (is.null(Ccon)) {
        oracredsP <<- FALSE
      }
    } else {
      oracredsP <<- FALSE
    }
    if (file.exists(paste0(path.expand('~'),"/OracleCreds/credsT.txt"))) {
      oracredsT <<- TRUE
      Ccon <<- setupOracleConn("T")
      if (is.null(Ccon)) {
        oracredsT <<- FALSE
      }
    } else {
      oracredsT <<- FALSE
    }

 #---------------------------------------------------------------

  resp <- FALSE
  adj <- FALSE
  exc <- TRUE

  adjpath <- "E:/data/SAS/2026/iREC/sample data"
  exc_file <- "202526 exclude notes.xlsx"
  exc_file <- "exclude2526.csv"
  excpath <- paste0("E:/data/SAS/2026/iREC/sample data/", exc_file)

  adj_file <- "survey_adj_V2.csv"
  resp_file <- "DFO_Monthly Catch Survey_June2025(p).sav" #
  #"DFO_Monthly Catch Survey_July2025(p).sav"
  #  "DFO_Monthly Catch Survey_June2026(p).sav" #  "DFO_Monthly Catch Survey_May2026(p).sav" #  "DFO_Monthly Catch Survey_July2026(p)_v2.sav" #"DFO_Monthly Catch Survey_April2026(p).sav" #"DFO_Monthly Catch Survey_August2025(p).sav" #"DFO_Monthly Catch Survey_September2025(p).sav" #
  adjpath <- paste0("E:/data/SAS/2026/iREC/sample data/", adj_file)
  resppath <- paste0("E:/data/SAS/2026/iREC/sample data/",resp_file)

  if (resp == TRUE) {
    resp_df <- haven::read_sav(resppath)
    if ("TYPE" %in% names(df)) {
      yr <- gsub("[^0-9.-]", "", df$TYPE[[1]])
      mnth_key <- tolower(df$TYPE[[1]])
    } else {
      for (y in 2012:year(now())) {
        if (grepl(y, resp_file)) {
          yr <- y
          break
        }
      }
      mnth_key <- tolower(resp_file)
    }

    for (m in month_nms) {
      if (grepl(tolower(m), mnth_key)) {
        mnth_str <- m
        break
      }
    }

    if (mnth_str %in% c('January', 'February', 'March')) {
      yr_name <- paste0(as.numeric(yr) - 1, '-' , as.numeric(substr(yr, 3, 4)))
    } else {
      yr_name <- paste0(yr, '-' , as.numeric(substr(yr, 3, 4)) + 1)
    }
    rws <- Write_C_ResponsesIn(
      resppath,
      resp_file,
      yr_name,
      yr,
      mnth_str,
      resp_df,
      "Response"
    )

  }

  if (exc == TRUE) {
    message("reading exclude")
    if (endsWith(tolower(exc_file), ".xlsx")) {
      exc_df <- openxlsx::read.xlsx(excpath, sheet = 1, detectDates = TRUE)
    } else {
      exc_df <- read_csv(excpath, show_col_types = FALSE)
    }
    message("done reading exclude")
    yr_name <- '2025-26'
    rws <- write_C_ExcludesIn(excpath, exc_file, yr_name, exc_df)
  }

  if (adj == TRUE) {
    adj_df <- readr::read_csv(adjpath, show_col_types = FALSE)
    rws <- Write_C_ResponsesIn(adjpath, adj_file, yr_name, yr,  mnth_str, adj_df, "Adjust")
  }


  message(rws)
  }
}

#testing()
