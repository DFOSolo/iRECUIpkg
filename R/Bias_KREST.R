
#' function to format the csv file data to a format KREST can injest
#' parameters are:
#' cor_vals = the csv file from the bias code loaded into a dataframe
buildData <- function(cor_vals) {
  # list of column names expected by KREST
  xlsx_K_clmns <- c("EstimationPeriodStart", "EstimationPeriodEnd", "EstimationArea", "GearType", "Species",
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
# KREST doesn't handle effort as an "ITEM" but as a component of other estimates, so we pull it off to deal with it
eff_bc <- cor_vals$BIAS_COR[cor_vals$SPECIES=='Effort'][1]
eff_sd <- cor_vals$SD[cor_vals$SPECIES=='Effort'][1]
eff_r2 <- cor_vals$R2[cor_vals$SPECIES=='Effort'][1]
eff_SUM_CREEL_IREC <- paste0(trunc(cor_vals$SUM_CREEL[cor_vals$SPECIES=='Effort'][1]),'.',trunc(cor_vals$SUM_IREC[cor_vals$SPECIES=='Effort'][1]))

# rename some of the fields to match what KREST needs
cor_vals <- cor_vals %>%
   dplyr::rename(dispo = Disposition,
                 Disposition = DISPOSITION )

# add some fields that KREST requires
cor_vals$EstimationPeriodStart <- format(as.Date(paste0(cor_vals$LIC_YEAR,'-04-01'), '%Y-%m-%d'))
cor_vals$EstimationPeriodEnd <- format(as.Date(paste0(cor_vals$LIC_YEAR + 1,'-03-31'), '%Y-%m-%d'))
cor_vals$EstimationArea <- '1'
cor_vals$GearType <- 'JHO'
cor_vals$Modifiers <- 'N/A'
cor_vals$Maturity <- 'LEGAL'
cor_vals$LengthClass <- 'Unspecified'
# sneak some extra values into the GROUP field
cor_vals$Group <- paste0("<ROW><RESIDENCE>N/A</RESIDENCE>" ,
                         "<LICENSE_TYPE>N/A</LICENSE_TYPE>" ,
                         "<AGE_CATEGORY>N/A</AGE_CATEGORY>" ,
                         "<PURCHASE_TYPE>N/A</PURCHASE_TYPE>" ,
                         "<METHOD>Angling from boat</METHOD>" ,
                         "<LODGE>N/A</LODGE>" ,
                         "<GUIDED>N/A</GUIDED>" ,
                         "<SHELLFISHING>N/A</SHELLFISHING>",
                         "<BIAS_COR>",cor_vals$BIAS_COR, "</BIAS_COR>" ,
                    "<SIMPLE_LM>" ,  cor_vals$SIMPLE_LM ,  "</SIMPLE_LM>" ,
                    "<SD>" ,  cor_vals$SD ,  "</SD>" ,
                    "<R2>" ,  cor_vals$R2 ,  "</R2>" ,
                    "<SAMPLE_SIZE>" ,  cor_vals$SAMPLE_SIZE ,   "</SAMPLE_SIZE>",
                    "<EFF_R2>" ,  eff_r2, "</EFF_R2>",
                    "<EFF_SUM_CREEL_IREC>" , eff_SUM_CREEL_IREC, "</EFF_SUM_CREEL_IREC></ROW>"
                    )
cor_vals$Party<-"N/A"
cor_vals$Licence<-"N/A"
cor_vals$Program<-"N/A"
cor_vals$CatchUnit<-"PC"
cor_vals$DataType<- 'Boat'
cor_vals$DayType<- 'Weekday'
cor_vals$MeanCPUE<- 0
cor_vals$VarianceCPUE<-0
cor_vals$SeCPUE<-0
cor_vals$MeanDailyEffort<-0
cor_vals$VarianceDailyEffort<-0
cor_vals$SeDailyEffort<-0
cor_vals$MeanDailyCatch<-0
cor_vals$VarianceDailyCatch<-0
cor_vals$SeDailyCatch<-0
cor_vals$NumberOfDaysInEstPeriod <- 0
cor_vals$TotalEffort<-eff_bc
cor_vals$CensusEffort<-cor_vals$SUM_CREEL
cor_vals$SurveyEffort<-cor_vals$SUM_IREC
cor_vals$VarianceTotalEffort<-sqrt(eff_sd)
cor_vals$SeTotalEffort<-0#eff_sd
cor_vals$TotalCatch<- 0# cor_vals$BIAS_COR
cor_vals$CensusCatch<-0
cor_vals$SurveyCatch<-0
cor_vals$VarianceTotalCatch<- 0 #sqrt(cor_vals$SD)
cor_vals$SeTotalCatch<-0 #cor_vals$SD
cor_vals$NumberOfInterviews<- 0#cor_vals$SAMPLE_SIZE
cor_vals$NumberOfIC<-0
cor_vals$NumberOfInterviewDays<-0
cor_vals$MeanStartTimeOfIC<-0
cor_vals$MeanEndTimeOfIC<-0
cor_vals$MeanMidPointTimeOfIC<-0
cor_vals$MeanIC<-0
cor_vals$MeanICE<-0
cor_vals$ICEInTimeblock1<-0
cor_vals$ICEInTimeblock2<-0
cor_vals$ICEInTimeblock3<-0
cor_vals$ICEInTimeblock4<-0
cor_vals$ICEInTimeblock5<-0
cor_vals$ICEInTimeblock6<-0
cor_vals$ICEInTimeblock7<-0
cor_vals$ICEInTimeblock8<-0
cor_vals$ICEInTimeblock9<-0
cor_vals$ICEInTimeblock10<-0
cor_vals$ICEInTimeblock11<-0
cor_vals$ICEInTimeblock12<-0
cor_vals$ICEInTimeblock13<-0
cor_vals$ICEInTimeblock14<-0
cor_vals$ICEInTimeblock15<-0
cor_vals$ICEInTimeblock16<-0
cor_vals$ICEInTimeblock17<-0
cor_vals$ICEInTimeblock18<-0
cor_vals$ICEInTimeblock19<-0
cor_vals$ICEInTimeblock20<-0
cor_vals$ICEInTimeblock21<-0
cor_vals$ICEInTimeblock22<-0
cor_vals$ICEInTimeblock23<-0
cor_vals$ICEInTimeblock24<-0
cor_vals$program<-'Pacific Region'
# drop any columns not in the "list"
cor_vals <- cor_vals[, names(cor_vals) %in% xlsx_K_clmns]
 # return the data with the item effort removed - effort is part of the items.  KREST stuff we have to deal with
 return (cor_vals[cor_vals$Species!='B_TRIPS',])
}

#' function to create a JSON dat packet that KREST can injest
#' parameters are:
#' rslt = the dataframe that buildData constructs
#' lic_str = the name of the bias correction data
#' csv_file = the name of the csv file that held the source bias correction factors
buildBiasJSONS <- function (rslt, lic_str, csv_file) {
  if (nrow(rslt) > 0) {
    tot_est <- rslt
    # build a data frame to hold the data in and set the column names
    jsons <- data.frame(matrix(0, ncol = 3, nrow = length(unique(tot_est$program)))) %>%
      dplyr::rename('g' = X1, 'bodyj' = X2, 'name' = X3)

    cnt <- 0
    # loop through each "program" area
    for (prg in unique(tot_est$program)) {
      outp <- tot_est[tot_est$program == prg, ]
      auto_notify(paste0(lic_str, ' ' ,prg, ' - ', nrow(outp)))
      # drop the program column as it is now irrelevant
      outp$program <- NULL
      # create the name for the "estimate"
      estname <-  paste0("i Bias ", lic_str, " X")

      admin_area <- 4 #areas[areas$PROGRAM == prg,][["ADMINISTRATIVE_AREA"]][1]
      # build a GUID from the estimate name so it is repeatable.  KREST estimates are only unique by GUID
      g <- uuid::UUIDfromName("00000000-0000-0000-0000-000000000000", estname[1], type="sha1")
      # effectively rename the dataframe of the program subset so it is easier for my brain to follow the process
      KrestEstimateResultRecords <- outp

      # create the estimate versions holder
      KrestEstimateVersions <- data.frame()
      KrestEstimateVersions[nrow(KrestEstimateVersions)+1,] <- NA
      # insert the estimates into the estimates version
      KrestEstimateVersions$KrestEstimateResultRecords[[1]] <- KrestEstimateResultRecords
      # assign the GUID
      KrestEstimateVersions$ReferenceGuid[1] <- g

      # build the estimate records holder
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
      # populate the estimate record
      # it would probably be safer to find the default values for season and administrative area and such, but they don't change
      EstimateRecords[nrow(EstimateRecords)+1,] <- NA
      EstimateRecords$EstimateName[1]  <- estname[1]
      EstimateRecords$EstimateType[1] <- 1 #
      EstimateRecords$EstimateTypeSpecified[1] <- "True"
      EstimateRecords$SeasonType[1] <- 1 #
      EstimateRecords$AdministrativeArea[1] <- admin_area #
      EstimateRecords$Fishery[1] <- 0 #
      EstimateRecords$GearLevel[1] <- 0
      EstimateRecords$PublicationType[1] <- 1 #
      EstimateRecords$PublicationStatus[1] <- 0 #
      EstimateRecords$EstimateStatus[1] <- 1 #
      EstimateRecords$Comment[1] <- csv_file #"May_2024_Result_20240808_113953.xlsx" #
      EstimateRecords$KrestEstimateVersions[[1]] <- KrestEstimateVersions
      EstimateRecords$ReferenceGuid[1] <- g #

      EstimateRecord <- data.frame(EstimateRecords = list())
      EstimateRecord[nrow(EstimateRecord)+1,] <- NA
      EstimateRecord$EstimateRecords <- EstimateRecords

      # convert the estimate records data frame to JSON
      bodyj <- jsonlite::toJSON(as.list(EstimateRecord))
      cnt = cnt + 1
      # package up the JSON along with it's identifiers
      jsons$g[[cnt]] <- g
      jsons$bodyj[[cnt]] <- bodyj
      jsons$name[[cnt]] <- prg
      jsons$estname[[cnt]] <- estname
      jsons$rowcnt[[cnt]] <- nrow(outp)

    }

    return(jsons)
  }
}

#' function wrapper to send and promote bias correction data in/to KREST
#' parameters are:
#' Jsons = the dataframe with JSONs for the estimates from buidlBiasJSONS
#' krestendpoint = the KREST server endpoint we need to talk to
#' login = the login credentials to use to talk to KREST
SendBiasJSON <- function(Jsons, krestendpoint, login) {
  # loop through the possible estimates
  for (i in 1:nrow(Jsons)) {
    # ask for a authorization token from KREST
    LoginToken <- getAuthToken(krestendpoint, login)
    # only process if we get a token
    if (LoginToken[[1]] != "Not Authorized") {
      # send a delete command using the GUID of the estimate
      SentD <- SendBiasiRECD(krestendpoint, LoginToken, Jsons$g[[i]])
      # send/post the estimate to KREST
      SentP <- SendBiasiRECP(krestendpoint, LoginToken, Jsons$bodyj[[i]], FALSE)
      # check the results of the post for errors
      if (length(SentP$results[[1]]$errorMessages)) {
        if (grepl('leady exists in KREST', SentP$results[[1]]$errorMessages[1])) {
          # if the estimate already exists, resend with overwrite turned on
          SentP <- SendBiasiRECP(krestendpoint, LoginToken, Jsons$bodyj[[i]], TRUE)
        } else {
          # let us know if anything wen wrong
          auto_notify(SentP$results[[1]]$errorMessages[1])
        }
      }
      # assuming the post was good, promote the estimate as published
      SentPro <- SendBiasiRECPro(krestendpoint, LoginToken, Jsons$g[[i]])
    }

  }
}

#' function to delete a bias correction data packet in KREST based on GUID
#' parameters are:
#' krestendpoint = the krest server endpoint we want to talk to
#' authToken = the authorization token used to authenticate
#' guid = the guid of the estimate we want to delete
SendBiasiRECD  <- function(krestendpoint, authToken, guid) {
# set up the delete command for KREST sending the token to the correct endpoint with the GUID we want to delete
  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )
  request.Resource = paste0("/southcoast/deleteEstimate?estimateReferenceGuid=",guid)
  urls <- paste0(krestendpoint,request.Resource)
    res <- httr::DELETE(url = urls, encode = "json",
                                   add_headers(.headers = headers))
  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  return(json)
}

#' function to "post" a bias correction data packet to KREST
#' parameters are:
#' krestendpoint = the krest server endpoint we want to talk to
#' authToken = the authorization token used to authenticate
#' bodyj = the JSON data to send
#' updateEstimate = boolena for new or update
SendBiasiRECP  <- function(krestendpoint, authToken, bodyj, updateEstimate) {
  # set up the post command for KREST sending the JSON data and token to the correct endpoint with the GUID to ID it
  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )


  bodyj2 <- toJSON(fromJSON(bodyj), auto_unbox = TRUE)

  # buidl th ecommand as with a PUT to replace, or a POST for a new estimate
  if (updateEstimate) {
    request.Resource <- "/southcoast/updateEstimate"
    urls <- paste0(krestendpoint,request.Resource)
    query_params <- list()
    query_params$estimateReferenceGuid <- fromJSON(bodyj)$EstimateRecords$ReferenceGuid
    res <- httr::PUT(url = urls, query = query_params, body = bodyj2, encode = "json",
                        add_headers(.headers = headers))
  } else {
    request.Resource <- "/southcoast/importEstimate"
    urls <- paste0(krestendpoint,request.Resource)
    res <- httr::POST(url = urls, body = bodyj2, encode = "json",
                        add_headers(.headers = headers))
  }
  # process the return value a bit and return it to the calling routine
  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  return(json)

}

#' function to "promote" a bias correction data packet in KREST based on GUID
#' parameters are:
#' krestendpoint = the krest server endpoint we want to talk to
#' authToken = the authorization token used to authenticate
#' guid = the guid of the estimate we want to promote
SendBiasiRECPro  <- function(krestendpoint, authToken, guid) {
# build the PUT command to send the Token and guid to the correct server to promote to published
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
  res <- httr::PUT(url = urls, body = bodyj2, encode = "json",
                                add_headers(.headers = headers))
  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  return(json)
  #transactionGUID <-  fromJSON(rlst)$transactionGuid
}

#' wrapper function to process a set of bias correction factors in CSV format as provided by the
#' analysis tool
#' bias corrections are stored as "estimates" but named so they can be easily identified
#' parameters are:
#' datapath = the path to the CSV file
#' krestendpoint is a global variable of the destination KREST server
#' logincreds is a global variable of the KREST login credentials to use
#' @export
Load_K_Bias <- function (datapath) {
  # read in the csv file
  Rcor_vals <- read.csv(paste0(datapath))
  # read in some stock look up tables
  data_path <- system.file("extdata", "species.csv", package = "iRECUIpkg")
  species_df <<- read.csv(data_path)
  text <- c('Kept', 'Rel', 'Released', 'N/A')
  disposition <<- c('Kept', 'Released', 'Released', 'N/A')
  disp_Df <<- data.frame(text, disposition)

  # loop through each year of data present
  yrs <- unique(Rcor_vals$LIC_YEAR)
  for (lyr in yrs) {
    # subset the year in question
    cor_vals <- Rcor_vals[Rcor_vals$LIC_YEAR == lyr, ]
    # build the name of the dataset based on the year
    lic_str <<- paste0(median(cor_vals$LIC_YEAR), '-', as.integer(substr(median(cor_vals$LIC_YEAR), 3, 4)) + 1)
    # step 1 of reformatting the data for KREST
    rslts <- buildData(cor_vals)
    # step 2 of formatting the data for KREST
    Jsons <- buildBiasJSONS(rslts, lic_str, basename(datapath))
    rws <- sum(as.integer(Jsons$rowcnt))
    # send the formatted data to KREST
    SendBiasJSON(Jsons, krestendpoint, logincreds)
    auto_notify(rws)
  }
  return(rws)
}
