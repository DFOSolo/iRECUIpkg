
#' wrapper function to load response data into KREST
#' @export
Write_K_ResponsesIn <- function(sourcepath, destpath, datapath, datafile, yr_name, df_read) {
  rslt <- FALSE
  auto_notify(progSeq)
  names(df_read) <- toupper(names(df_read))
  dte <- mean(df_read[!is.na(df_read$DATEFISHED),]$DATEFISHED)
  yr <- year(dte)
  mnth_str <- months.Date(dte)
  yr_str <- as.numeric(yr)

  auto_notify(paste0("mnth_str = ", mnth_str))

  mnth_str <- ""

  for (m in month_nms) {
    if (grepl(tolower(m), tolower(basename(datafile)))) {
      mnth_str <- m
      break
    }
  }

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

baseName <- tools::file_path_sans_ext(datafile)

dfr <- df_read

if ("REPYEAR" %in% names(dfr)) {
  SURVEYSTARTDATE <- as.Date("2012-04-01")
} else {
  if ("LICENCE_ID" %in% names(dfr)){
    dfr <- dfr %>%
      dplyr::rename(surveykey = "LICENCE_ID")
  }
  SURVEYSTARTDATE <- as.Date(format(Sys.Date(), "%Y-%m-%d") )
}
excludes <- c()
adjusts <- c()
dfrr <- dfr

if (SURVEYSTARTDATE < PraStartDate) {
  auto_notify("Converting Ekos data")
  survey_results <-
    df <- convertEKOSData (dfr)
  df <- df %>%
    mutate(DID_NOT_FISH = if_else(DID_NOT_FISH == TRUE, PraDidNotFish, PraDidFish)) %>%
    dplyr::rename(DIDNOTFISH = DID_NOT_FISH)
} else {
  df <- dfr
  #names(df) <- tolower(names(df))
}
auto_notify("Fixing Values")
 df1 <- fixValues(df)
 auto_notify("Linking tables")
 df1 <- linkTables(df1, TRUE)
 auto_notify("Fixing Comments")
 df1 <- fixComments(df1)

 if ("TYPE" %notin% names(df1)) {
   df1$TYPE <- paste0("iREC ", mnth_str, " ", yr)
 }

 df_filtered <- filterColumns(df1, cols_to_keep_K)

 for (n in 1:8) {
   fld <- paste0("HALIBUTLENGTH.",n)
   if ( fld %notin% names(df_filtered)) {
     df_filtered[[fld]] <- 0
   }
 }

 df_filtered <- df_filtered %>%
   dplyr::rename(
     SurveyKey	 = SURVEYKEY,
     StartDay	 = STARTDAY,
     EndDay	 = ENDDAY,
     DidNotFish	 = DIDNOTFISH,
     CompleteSurvey	 = COMPLETESURVEY,
     DateFished	 = DATEFISHED,
     Year	 = YEAR,
     Month	 = MONTH,
     Day	 = DAY,
     TotalJuveniles	 = TOTALJUVENILES,
     FishedFromLodge	 = FISHEDFROMLODGE,
     FishedWithGuide	 = FISHEDWITHGUIDE,
     CheckCrabsPrawns	 = CHECKCRABSPRAWNS,
     Crab_CentreOfFishing_Latitude	 = CRAB_CENTREOFFISHING_LATITUDE,
     Crab_CentreOfFishing_Longitude	 = CRAB_CENTREOFFISHING_LONGITUDE,
     Prawn_CentreOfFishing_Latitude	 = PRAWN_CENTREOFFISHING_LATITUDE,
     Prawn_CentreOfFishing_Longitude	 = PRAWN_CENTREOFFISHING_LONGITUDE,
     HalibutLength.1	 = HALIBUTLENGTH.1,
     HalibutLength.2	 = HALIBUTLENGTH.2,
     HalibutLength.3	 = HALIBUTLENGTH.3,
     HalibutLength.4	 = HALIBUTLENGTH.4,
     HalibutLength.5	 = HALIBUTLENGTH.5,
     HalibutLength.6	 = HALIBUTLENGTH.6,
     HalibutLength.7	 = HALIBUTLENGTH.7,
     HalibutLength.8	 = HALIBUTLENGTH.8,
     Comment	 = COMMENT,
     numberactivity	 = NUMBERACTIVITY,
     savecount	 = SAVECOUNT,
     Visits	 = VISITS,
     Sessions	 = SESSIONS,
     datecreated	 = DATECREATED,
     datecreated_Day	 = DATECREATED_DAY,
     datecreated_Month	 = DATECREATED_MONTH,
     datecreated_Year	 = DATECREATED_YEAR,
     datecompleted	 = DATECOMPLETED,
     Area	 = AREA,
     Method	 = METHOD
   )
    csvFile <- paste0(baseName,"_OUT.csv")
    outfile <- paste0(destpath,"\\",yr_name, "-", mnth_str, "_CSV-KREST_RSLT.html")
    upload_Label <- paste0(yr_name, " - ", mnth_str, " iREC Responses ",datapath,"\\",datafile)
    reference_Label <- paste0(yr_name, " - ", mnth_str, " iREC")

csv_outfile <- paste0(paste0(destpath,"/",csvFile))
write.csv(df_filtered, csv_outfile, na = "", row.names = FALSE)

cookies = c(ASP.NET_SessionId = "u4buuzrm2zebf13fmel5upui")
headers = c(
  Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
  'Accept-Language' = "en-US,en;q=0.9",
  'Cache-Control' = "max-age=0",
  Connection = "keep-alive",
  DNT = "1",
  Origin = paste0("http://", krestroot),
  Referer = paste0("http://", krestroot, "/KREST/iRec/ImportiRecData/"),
  'Upgrade-Insecure-Requests' = "1",
  'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 Edg/138.0.0.0",
  'Accept-Encoding' = "gzip, deflate",
  Host = krestroot
)

auto_notify(paste0("Year - ", yr_str))
 #browser()
files = list(
  ProgramSequence = progSeq,
  Year = yr_str,
  Month = mnth_str,
  Type = "iRec Response Data",
  Reference = reference_Label,
  files = upload_file(csv_outfile),
  Label = upload_Label
)
auto_notify("Posting Response data to KREST")
res <- httr::POST(
  url = paste0("http://", krestroot, "/KREST/iRec/ImportiRecData/"),
  httr::add_headers(.headers = headers),
  httr::set_cookies(.cookies = cookies),
  body = files,
  encode = "multipart",
  config = httr::config(ssl_verifypeer = FALSE),
  authenticate(":", ":", "ntlm")
)
auto_notify("Reading the results of the Post")
    rslt = rawToChar(res$content)
    if (file.exists(outfile)) {
      file.remove(outfile)
    }
    #browser()
    auto_notify(paste0('Result Length ',nchar(rslt)))
   if (!grepl('parent.success = true', rslt)) {
    readr::write_lines(rslt,outfile)
    browseURL(outfile)
    rslt <- FALSE
   } else {rslt <- TRUE}
 return(rslt)
}

#' @export
write_K_AdjustsIn <- function(sourcepath, destpath, adjpath, adj_file, yr_name, yr,  mnth_str, adj_df) {

  if (nrow(adj_df) > 0) {
    baseName <- tools::file_path_sans_ext(adj_file)
    csvFile <- paste0(baseName, "_OUT.html")
    outpath <- paste0(destpath, "\\", yr, "_", mnth_str, "_" , csvFile)

    adj_df <- fixAdjDates(adj_df,outpath)


    names(adj_df) <- tolower(names(adj_df))
    adj_df <- adj_df %>%
      mutate(did_not_fish = if_else(did_not_fish == 'yes', PraDidNotFish, PraDidFish)) %>%
      dplyr::rename(
        didnotfish = did_not_fish,
        fishedfromlodge = lodge,
        fishedwithguide = guided,
        totaljuveniles = juv_effort_days
      )

    adj_df$fishedfromlodge <- case_when (
      tolower(adj_df$fishedfromlodge) == 'yes' ~ 1,
      tolower(adj_df$fishedfromlodge) == 'no' ~ 2,
      TRUE ~ 0
    )

    adj_df$fishedwithguide <- case_when (
      tolower(adj_df$fishedwithguide) == 'yes' ~ 1,
      tolower(adj_df$fishedwithguide) == 'no' ~ 2,
      TRUE ~ 0
    )

    if ("completesurvey" %notin% names(adj_df)) {
      adj_df$completesurvey <- 'Complete'
    }

    for (c in names(adj_df)) {
      if (grepl('comment', c, ignore.case = TRUE)) {
        adf_df <- fixComments(adj_df, c)
      }
    }

    adj_df <- adj_df %>% rename('AREA' = 'area',
                                'METHOD' = 'method',
                                'CHECKCRABSPRAWNS' = 'checkcrabsprawns',
                                'DATEFISHED' = 'datefished')

    adj_df <- linkTables(adj_df, FALSE)
    adj_df <- fixValues(adj_df)

    remove_list <- c(
      "HALIBUTLENGTH.1", "HALIBUTLENGTH.2", "HALIBUTLENGTH.3", "HALIBUTLENGTH.4",
      "HALIBUTLENGTH.5", "HALIBUTLENGTH.6", "HALIBUTLENGTH.7", "HALIBUTLENGTH.8")

    cols <- cols_to_keep_K[!cols_to_keep_K %in% remove_list]
    adj_df <- filterColumns(adj_df,cols)

    adj_df <- adj_df %>% rename("SurveyKey" = "SURVEYKEY",
    "DidNotFish" = "DIDNOTFISH",
    "DateFished" = "DATEFISHED",
    "FishedFromLodge" = "FISHEDFROMLODGE",
    "FishedWithGuide" = "FISHEDWITHGUIDE",
    "TotalJuveniles" = "TOTALJUVENILES",
    "CompleteSurvey" = "COMPLETESURVEY",
    "Area" = "AREA",
    "Method" = "METHOD",
    "CheckCrabsPrawns" = "CHECKCRABSPRAWNS",
    "StartDay" = "STARTDAY",
    "EndDay" = "ENDDAY")


    if ("datecompleted" %notin% names(adj_df)) {
      adj_df$datecompleted <- as.Date(format(Sys.Date(), format = "%Y-%m-%d"))
    } else  {
      adj_df$datecompleted[is.na(adj_df$datecompleted)]<- as.Date(format(Sys.Date(), format = "%Y-%m-%d"))
    }


    adj_df$StartDay <- mday(adj_df$DateFished)
    adj_df$EndDay <- mday(adj_df$DateFished)
    adj_df$StartDay[is.na(adj_df$DateFished)] <- 0
    adj_df$EndDay[is.na(adj_df$DateFished)] <- 0
    adj_df$DateFished[adj_df$DateFished == 0] <- NA

    baseName <- tools::file_path_sans_ext(adj_file)
    csvFile <- paste0(baseName, "_OUT.csv")
    outpath <- paste0(destpath, "\\", yr, "_", mnth_str, "_" , csvFile)

    write.csv(adj_df, outpath, na = "", row.names = FALSE) #, na = "0"


    cookies = c(ASP.NET_SessionId = "u4buuzrm2zebf13fmel5upui")
    headers = c(
      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
      'Accept-Language' = "en-US,en;q=0.9",
      'Cache-Control' = "max-age=0",
      Connection = "keep-alive",
      DNT = "1",
      Origin = paste0("http://",krestroot),
      Referer = paste0("http://",krestroot,"/KREST/iRec/ImportiRecExclusionsOrAdjustments/?IsExclusion=False"),
      'Upgrade-Insecure-Requests' = "1",
      'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 Edg/138.0.0.0",
      'Accept-Encoding' = "gzip, deflate",
      Host = krestroot
    )

    matches_paren <- regmatches(adj_file, gregexpr("\\((.*?)\\)", adj_file, perl = TRUE))[[1]]
    modifier <- gsub("[\\(\\)]", "", matches_paren)
    if (length(modifier) == 0) {
      lbl <- paste0(yr_name, " - ", mnth_str, " iREC Adjustments")
    } else {
      lbl <-  paste0(yr_name, " - ", mnth_str, ' - ', modifier, " - iREC Adjustments")
    }

    files = list(
      files = upload_file(outpath),
      Label = lbl,
      Type = "Adjustment",
      Notes = paste0(adjpath, "\\", adj_file)

    )
    auto_notify("Posting Adjusts data to KREST")
    res <- httr::POST(
      url = paste0("http://", krestroot,"/KREST/iRec/ImportiRecExclusionsOrAdjustments/?IsExclusion=False"),
      httr::add_headers(.headers = headers),
      httr::set_cookies(.cookies = cookies),
      body = files,
      encode = "multipart",
      config = httr::config(ssl_verifypeer = FALSE),
      authenticate(":", ":", "ntlm")
    )
    auto_notify("Reading the results of the Post")
    rslt = rawToChar(res$content)
    outfile <- paste0(destpath,
                      "\\",
                      yr_name,
                      " - ",
                      mnth_str,
                      "_CSV-KREST_ADJ.html")
    auto_notify(outfile)
    auto_notify(paste0('Result Length ', nchar(rslt)))
    if (!grepl('Successfully imported records', rslt)) {
      file.remove(outfile)
      readr::write_lines(rslt, outfile)
      browseURL(outfile)
      rslt <- FALSE
    }else {rslt <- TRUE}
    return(rslt)
  }
}

#' @export
write_K_ExcludesIn <- function (destpath, exc_path, exc_file, exc_df, yr_name){

  names(exc_df) <- tolower(names(exc_df))

  if ("licence_id" %in% names(exc_df)){
    exc_df <- exc_df %>%
      dplyr::rename(surveykey = "licence_id")
  }

  if ("Comment" %in% names(exc_df)) {
    exc_df <- fixComments(exc_df,"Comment")
  } else if ("comment" %in% names(exc_df)) {
    exc_df <- fixComments(exc_df,"comment")
  } else if ("comments" %in% names(exc_df)) {
    exc_df <- fixComments(exc_df,"comments")
  } else if ("Comments" %in% names(exc_df)) {
    exc_df <- fixComments(exc_df,"Comments")
  }

  names(exc_df)[names(exc_df)=='survey_access_key'] <- 'surveykey'
  exc_df$'dfo comment' <- "Historical exclude"
  cols_to_keep <- c("SurveyKey", "TYPE", "StartDay", "EndDay", "DidNotFish", "CompleteSurvey", "First_name"
                    , "Last_name", "EMAIL", "Comment", "dfo comment", "AnalystRational")

  for (item in cols_to_keep) {
    names(exc_df)[names(exc_df)==tolower(item)] <- item
  }

  exc_df <- exc_df[, names(exc_df) %in% cols_to_keep]
  #browser()
  baseName <- tools::file_path_sans_ext(exc_file)
  csvFile <- paste0(baseName,"_OUT.csv")
  yr <- gsub("[^0-9.-]", "", basename(exc_path))
  if (yr=='') {
    outpath <- paste0(destpath, "\\", csvFile)
  } else {
    outpath <- paste0(destpath, "\\", yr, "_" , csvFile)
  }
  #browser()
  write.csv(exc_df,outpath, na = "0", row.names = FALSE)

  cookies = c(ASP.NET_SessionId = "u4buuzrm2zebf13fmel5upui")
  headers = c(
    Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    'Accept-Language' = "en-US,en;q=0.9",
    'Cache-Control' = "max-age=0",
    Connection = "keep-alive",
    DNT = "1",
    Origin = paste0("http://", krestroot),
    Referer = paste0("http://", krestroot,"/KREST/iRec/ImportiRecExclusionsOrAdjustments/?IsExclusion=False"),
    'Upgrade-Insecure-Requests' = "1",
    'User-Agent' = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/138.0.0.0 Safari/537.36 Edg/138.0.0.0",
    'Accept-Encoding' = "gzip, deflate",
    Host = krestroot
  )

  matches_paren <- regmatches(exc_file, gregexpr("\\((.*?)\\)", exc_file, perl = TRUE))[[1]]
  modifier <- gsub("[\\(\\)]", "", matches_paren)
  if (length(modifier) == 0) {
    lbl <- paste0(yr_name, " - iREC Excludes")
  } else {
    lbl <-  paste0(yr_name, " - ", modifier, " - iREC Excludes")
  }


  files = list(
    files = upload_file(outpath),
    Label = lbl,
    Type = "Exclusion",
    Notes = paste0(exc_path, "\\", exc_file)
  )

  auto_notify("Posting Excludes data to KREST")
  res <- httr::POST(
    url = paste0("http://",krestroot,"/KREST/iRec/ImportiRecExclusionsOrAdjustments/?IsExclusion=True"),
    httr::add_headers(.headers = headers),
    httr::set_cookies(.cookies = cookies),
    body = files,
    encode = "multipart",
    config = httr::config(ssl_verifypeer = FALSE),
    authenticate(":", ":", "ntlm")
  )
  auto_notify("Reading the results of the Post")

  rslt = rawToChar(res$content)
  outfile <- paste0(destpath,"\\", yr_name, "_CSV-KREST_Excludes.html")
  message(outfile)
  message(paste0('Result Length ',nchar(rslt)))
  if (!grepl('Successfully imported records', rslt)) {
    file.remove(outfile)
    readr::write_lines(rslt,outfile)
    browseURL(outfile)
  }

}

testing <- function() {
  sourcepath <- ""
  destpath <- "E:\\data\\SAS\\2026\\iREC\\sample data"
  adjpath <- ""
  adj_file <- "survey_adj.csv"
  yr_name <- "2026-27"
  yr <- 2026
  mnth_str <- "April"
  datapath <- "E:\\data\\SAS\\2026\\iREC\\sample data\\survey_adj.csv"

  adj_df <- read_csv(datapath, show_col_types = FALSE)
  source("R\\common_Code.R")
  source("R\\KREST_common.R")
  source("R\\Common_UniformSav.R")
  SetupKrest("P")

  rws <- write_K_AdjustsIn(sourcepath, destpath, adjpath, adj_file, yr_name, yr,  mnth_str, adj_df)
}

#testing()

