
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

#' function to format the csv file data to a format KREST can injest

buildData <- function(cor_vals) {
  #browser()
cor_vals$EstimationPeriodStart <- format(as.Date(paste0(cor_vals$LIC_YEAR,'-04-01'), '%Y-%m-%d'))
cor_vals$EstimationPeriodEnd <- format(as.Date(paste0(cor_vals$LIC_YEAR + 1,'-03-31'), '%Y-%m-%d'))

#lic_str <<- paste0(cor_vals$LIC_YEAR[1], '-', substr(cor_vals$LIC_YEAR[1]+1, start=3,stop=4))
eff_bc <- cor_vals$BIAS_COR[cor_vals$SPECIES=='Effort'][1]
eff_sd <- cor_vals$SD[cor_vals$SPECIES=='Effort'][1]
eff_r2 <- cor_vals$R2[cor_vals$SPECIES=='Effort'][1]
eff_SUM_CREEL_IREC <- paste0(trunc(cor_vals$SUM_CREEL[cor_vals$SPECIES=='Effort'][1]),'.',trunc(cor_vals$SUM_IREC[cor_vals$SPECIES=='Effort'][1]))
cor_vals$EstimationArea <- '1'
cor_vals$GearType <- 'JHO'

# cor_vals <- left_join(cor_vals, species_df, by = c('SPECIES' = 'SPECIES_TXT')) %>%
#   dplyr::rename(Species = HART_CD)
#
# cor_vals <- cor_vals[!is.na(cor_vals$Species) , ]
# cor_vals <- left_join(cor_vals, disp_Df, by = c('DISPOSITION' = 'text')) %>%
#
 cor_vals <- cor_vals %>%
   dplyr::rename(dispo = Disposition,
                 Disposition = DISPOSITION )


cor_vals$Modifiers <- 'N/A'
cor_vals$Maturity <- 'LEGAL'
cor_vals$LengthClass <- 'Unspecified'
# cor_vals$Group <- ''
# cor_vals$Group <- paste0("<ROW><R2>",cor_vals$R2, "</R2></ROW>")

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

cor_vals <- cor_vals[, names(cor_vals) %in% xlsx_K_clmns]
 #browser()
 return (cor_vals[cor_vals$Species!='B_TRIPS',])
}

#' function to create a JSON dat packet that KREST can injest

buildBiasJSONS <- function (rslt, lic_str, csv_file) {
   #browser()
  if (nrow(rslt) > 0) {
    #browser()
    tot_est <- rslt
    #jsons <- as.list(rep(NA, length(unique(tot_est$program))))
    jsons <- data.frame(matrix(0, ncol = 3, nrow = length(unique(tot_est$program)))) %>%
      dplyr::rename('g' = X1, 'bodyj' = X2, 'name' = X3)

    cnt <- 0
    for (prg in unique(tot_est$program)) {
      outp <- tot_est[tot_est$program == prg, ]
      auto_notify(paste0(lic_str, ' ' ,prg, ' - ', nrow(outp)))
      outp$program <- NULL
      estname <-  paste0("i Bias ", lic_str, " X")

      admin_area <- 4 #areas[areas$PROGRAM == prg,][["ADMINISTRATIVE_AREA"]][1]

      #    g <- as.character(uuid::UUIDgenerate())
      g <- uuid::UUIDfromName("00000000-0000-0000-0000-000000000000", estname[1], type="sha1")
      #g1 <- uuid::UUIDfromName("00000000-0000-0000-0000-000000000000", paste0(estname[1],date()) , type="sha1")

      KrestEstimateResultRecords <- outp#[1,]

      KrestEstimateVersions <- data.frame()

      KrestEstimateVersions[nrow(KrestEstimateVersions)+1,] <- NA

      KrestEstimateVersions$KrestEstimateResultRecords[[1]] <- KrestEstimateResultRecords

      KrestEstimateVersions$ReferenceGuid[1] <- g#1


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
      bodyj <- jsonlite::toJSON(as.list(EstimateRecord))
      #clipr::write_clip(bodyj)
      cnt = cnt + 1
      #browser()
      # jsons[[cnt]]$g <- g
      # jsons[[cnt]]$bodyj <- bodyj
      jsons$g[[cnt]] <- g
      jsons$bodyj[[cnt]] <- bodyj
      jsons$name[[cnt]] <- prg
      jsons$estname[[cnt]] <- estname
      jsons$rowcnt[[cnt]] <- nrow(outp)

    }
    #browser()
    return(jsons)
  }
}

#' function wrapper to send and promote bias correction data in/to KREST

SendBiasJSON <- function(Jsons, krestendpoint, login) {
  #browser()
  for (i in 1:nrow(Jsons)) {
    LoginToken <- getAuthToken(krestendpoint, login)
    if (LoginToken[[1]] != "Not Authorized") {
      SentD <- SendBiasiRECD(krestendpoint, LoginToken, Jsons$g[[i]])
      SentP <- SendBiasiRECP(krestendpoint, LoginToken, Jsons$bodyj[[i]], FALSE)
      #browser()
      if (length(SentP$results[[1]]$errorMessages)) {
        if (grepl('leady exists in KREST', SentP$results[[1]]$errorMessages[1])) {
          SentP <- SendBiasiRECP(krestendpoint, LoginToken, Jsons$bodyj[[i]], TRUE)
        } else {
          auto_notify(SentP$results[[1]]$errorMessages[1])
        }
      }
      SentPro <- SendBiasiRECPro(krestendpoint, LoginToken, Jsons$g[[i]])
    }

  }
}

#' function to delete a bias correction data packet in KREST based on GUID

SendBiasiRECD  <- function(krestendpoint, authToken, guid) {

  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )
  #guid <- FlipEndian(guid)
  request.Resource = paste0("/southcoast/deleteEstimate?estimateReferenceGuid=",guid)
  #request.Resource = paste0("/southcoast/deleteEstimate?estimateReferenceGuid=","a7ca1883-32b7-4ecb-ae07-622923a6dcd5")
  urls <- paste0(krestendpoint,request.Resource)
    res <- httr::DELETE(url = urls, encode = "json",
                                   add_headers(.headers = headers))
  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  return(json)
}

#' function to "post" a bias correction data packet to KREST

SendBiasiRECP  <- function(krestendpoint, authToken, bodyj, updateEstimate) {

  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate'
  )


  bodyj2 <- toJSON(fromJSON(bodyj), auto_unbox = TRUE)
   #browser()

  if (updateEstimate) {

    request.Resource <- "/southcoast/updateEstimate"
    urls <- paste0(krestendpoint,request.Resource)
    query_params <- list()
      query_params$estimateReferenceGuid <- fromJSON(bodyj)$EstimateRecords$ReferenceGuid
      #browser()

      res <- httr::PUT(url = urls, query = query_params, body = bodyj2, encode = "json",
                        add_headers(.headers = headers))

  } else {
    request.Resource <- "/southcoast/importEstimate"
    urls <- paste0(krestendpoint,request.Resource)
      res <- httr::POST(url = urls, body = bodyj2, encode = "json",
                        add_headers(.headers = headers))
  }


  rlst <- rawToChar(res$content)
  json <- content(res, type="application/json")
  return(json)
  #transactionGUID <-  fromJSON(rlst)$transactionGuid
}

#' function to "promote" a bias correction data packet in KREST based on GUID

SendBiasiRECPro  <- function(krestendpoint, authToken, guid) {

  headers = c(
    `Content-Type` = 'application/json; charset=utf-8',
    `Authorization` = authToken,
    `Accept-Encoding` = 'gzip, deflate',
    `Accept` = 'Accept: application/json, text/json, text/x-json, text/javascript, application/xml, text/xml'
  )
  #guid <- FlipEndian(guid)

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

Load_K_Bias <- function (datapath,dbDest) {
  Rcor_vals <- read.csv(paste0(datapath))
  data_path <- system.file("extdata", "species.csv", package = "iRECUIpkg")
  species_df <<- read.csv(data_path)
  text <- c('Kept', 'Rel', 'Released', 'N/A')
  disposition <<- c('Kept', 'Released', 'Released', 'N/A')
  disp_Df <<- data.frame(text, disposition)


  yrs <- unique(Rcor_vals$LIC_YEAR)
for (lyr in yrs){
  cor_vals <- Rcor_vals[Rcor_vals$LIC_YEAR == lyr, ]
  lic_str <<- paste0(median(cor_vals$LIC_YEAR), '-', as.integer(substr(median(cor_vals$LIC_YEAR),3,4)) + 1)

  rslts <- buildData(cor_vals) #[1:1,]
  #browser()
  Jsons <- buildBiasJSONS(rslts, lic_str, basename(datapath))
  rws <- sum(as.integer(Jsons$rowcnt))
  SendBiasJSON(Jsons, krestendpoint, logincreds)
  auto_notify(rws)
}
  return(rws)
}
