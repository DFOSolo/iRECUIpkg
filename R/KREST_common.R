
#' function to build and send the web call to the KREST server identified in the variable "krestendpoint"
#' to get the authorization token - Token lives for about 10 mins
#' parameters are:
#' krestendpoint = KREST server test vs prod
#' Logincreds = structure passed to KREST contains username and password of KREST account to use
#' logincreds <<- list(grant_type = "password",
#'                     password = "password",
#'                     username = "username")
#' returns the bearer token for use in transactions
getAuthToken <- function(krestendpoint, logincreds) {
  authcnt <- 1
  request.Resource <- "/auth"
  urls <- paste0(krestendpoint,request.Resource)
  # OAuth2BearerToken <- "KREST is still initalizing. Please retry your request in a few minutes. system_notready"
  repeat {
    res <- httr::POST(
      url = urls,
      body = logincreds,
      encode = "form",
      config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
    )
    hdrs <- rawToChar(res$content)

    OAuth2BearerToken <<- fromJSON(hdrs)
    if (length(OAuth2BearerToken) == 3){
      break
    }
    else {
      if (authcnt >= 100) break
      Sys.sleep(20)
      auto_notify(paste0(OAuth2BearerToken[[1]], " waiting 20 seconds try # ", authcnt))
      authcnt <- authcnt + 1
    }
  }
  if (length(OAuth2BearerToken) == 3) {
    return (paste0(OAuth2BearerToken[[2]]," ", OAuth2BearerToken[[1]]))
  } else {
    auto_notify(krestendpoint)
    auto_notify(logincreds$username)
    return ("Not Authorized")
  }
}

#' wrapper function to authenticate with the KREST API
Authenticate <- function(username, password) {
  # Logger::info(paste("VersaPayProvider: Show Transaction for Username:", username, "| Token:", token))
  browser()
  # this is the one!
  OAuth2BearerToken <- "Not Yet!"
  auto_notify("Trying to authenticate")
  tryCatch({
    result <- GetOAuth2Token(username, password)
    # check if OAuth token request returns an error
    if (!is.null(result$Error) && result$Error != "") {
      auto_notify(result$Error)
      auto_notify(result$Error_description)

      # if the username and password is incorrect
      if (result$Error == "invalid_grant") {
        return(NULL)
      }

      # if KREST is still initializing
      if (result$Error == "system_notready") {
        stop(KrestNotInitializedException(result$Error_description))
      }

      stop(KrestRestException(result$Error_description))
    }

    OAuth2BearerToken <- result$access_token

    # return the response
    return(OAuth2BearerToken)
  }, error = function(e) {
    #browser()
    if (inherits(e, "KrestNotInitializedException")) {
      return(OAuth2BearerToken)
    } else {
      return(OAuth2BearerToken)
    }
  })
  # Logger::info(paste("VersaPayProvider: Show Transaction completed, Token:", response$Data$Token))
}

#' function to get authorization token from KREST api

GetOAuth2Token <- function(username, password) {
  if (is.null(krestendpoint) || krestendpoint == "") {
    stop("KREST REST Endpoint URL setting is empty or null")
  }

  auto_notify(username)
  # build the request
  #browser()
  response <- POST(
    url = paste0(krestendpoint, "/auth"),
    body = list(
      grant_type = "password",
      username = username,
      password = password
    ),
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded"),
    config(ssl_verifyhost = useSSH, ssl_verifypeer = useSSH)
  )
  #browser()
  # check if response is null
  if (is.null(response)) {
    auto_notify("Null response")
    auto_notify("Not Yet!")
  }

  # check if the http response code is valid
  if (status_code(response) != 200) {
    auto_notify(paste("Error retrieving KREST OAuth token:", content(response, "text")))
    return("Not Yet!")
  }

  return(content(response, "parsed"))
}

#' function to fetch look-up tables from KREST
GetLookupCodes <- function(BearerToken, resource, dateTime = NULL, showAssignmentHistory = FALSE) {
  if (is.null(krestendpoint) || krestendpoint == "") {
    stop("KREST REST Endpoint URL setting is empty or null")
  }

  # in null, use the earliest date
  if (is.null(dateTime)) dateTime <- as.POSIXct("1970-01-01")

  # build the request
  url <- paste0(krestendpoint, resource)
  headers <- add_headers("Content-Type" = "application/x-www-form-urlencoded",
                         Authorization = BearerToken) #paste("Bearer", BearerToken)

  params <- list(showAssignmentHistory = showAssignmentHistory,
                 date = format(dateTime, "%Y-%m-%d"))

  # create the auth response
  response <- tryCatch({
    GET(url, headers, query = params)
  }, error = function(e) {
    stop(paste("Error executing REST call:", e$message))
  })

  # check if the http response code is valid
  if (http_status(response)$category != "Success" ||
      is.null(response$content) ||
      !is.null(content(response, "parsed")$ErrorMessage) ) {
    stop(paste("Error retrieving KREST Lookups:",
               content(response, "parsed")$ErrorMessage %||%
                 http_status(response)$message))
  }

  return(response$content)
}

#' function to initialize comms with the KREST api
#' @export
SetupKrest <- function (DestServer) {
  cols_to_keep_K <<- c("SURVEYKEY", "TYPE", "STARTDAY", "ENDDAY", "DIDNOTFISH", "COMPLETESURVEY", "DATEFISHED",
                       "YEAR", "MONTH", "DAY", "METHOD", "AREA", "TOTALJUVENILES", "FISHEDFROMLODGE",
                       "FISHEDWITHGUIDE", "CHECKCRABSPRAWNS", "CHECKSALMON", "COMMENT",
                       "SALMON_CHINOOK_HATCH_KEPT",
                       "SALMON_CHINOOK_HATCH_RELE","SALMON_CHINOOK_WILD_KEPT","SALMON_CHINOOK_WILD_RELE",
                       "SALMON_CHINOOK_UNK_KEPT","SALMON_CHINOOK_UNK_RELE","SALMON_CHINOOK_SUBL_RELE",
                       "SALMON_CHINOOK_US_HATCHERY_KEPT","SALMON_CHINOOK_US_WILD_KEPT","SALMON_CHINOOK_US_UNKOWN_KEPT",
                       "SALMON_CHINOOK_US_HATCHERY_RELE","SALMON_CHINOOK_US_WILD_RELE","SALMON_CHINOOK_US_UNKOWN_RELE",
                       "SALMON_COHO_HATCH_KEPT","SALMON_COHO_HATCH_RELE","SALMON_COHO_WILD_KEPT",
                       "SALMON_COHO_WILD_RELE","SALMON_COHO_UNK_KEPT","SALMON_COHO_UNK_RELE",
                       "SALMON_SOCKEYE_KEPT","SALMON_SOCKEYE_RELE","SALMON_PINK_KEPT",
                       "SALMON_PINK_RELE","SALMON_CHUM_KEPT","SALMON_CHUM_RELE",
                       "SALMON_UNKNOWN_KEPT","SALMON_UNKNOWN_RELE","HALIBUT_HALIBUT_KEPT",
                       "HALIBUT_HALIBUT_RELE","HALIBUT_LINGCOD_KEPT","HALIBUT_LINGCOD_RELE",
                       "HALIBUT_ROCKFISH_BLACK_KEPT","HALIBUT_ROCKFISH_BLACK_RELE","HALIBUT_ROCKFISH_BOCACCIO_KEPT",
                       "HALIBUT_ROCKFISH_BOCACCIO_RELE","HALIBUT_ROCKFISH_CANARY_KEPT","HALIBUT_ROCKFISH_CANARY_RELE",
                       "HALIBUT_ROCKFISH_CHINA_KEPT","HALIBUT_ROCKFISH_CHINA_RELE","HALIBUT_ROCKFISH_COPPER_KEPT",
                       "HALIBUT_ROCKFISH_COPPER_RELE","HALIBUT_ROCKFISH_QUILLBACK_KEPT","HALIBUT_ROCKFISH_QUILLBACK_RELE",
                       "HALIBUT_ROCKFISH_TIGER_KEPT","HALIBUT_ROCKFISH_TIGER_RELE","HALIBUT_ROCKFISH_VERMILLION_KEPT",
                       "HALIBUT_ROCKFISH_VERMILLION_RELE","HALIBUT_ROCKFISH_YELLOWEYE_KEPT","HALIBUT_ROCKFISH_YELLOWEYE_RELE",
                       "HALIBUT_ROCKFISH_YELLOWTAIL_KEPT","HALIBUT_ROCKFISH_YELLOWTAIL_RELE","HALIBUT_ROCKFISH_OTHER_KEPT",
                       "HALIBUT_ROCKFISH_OTHER_RELE","FINFISH_ALTSAL_KEPT","FINFISH_ALTSAL_RELE",
                       "FINFISH_COD_KEPT","FINFISH_COD_RELE","FINFISH_CTT_KEPT",
                       "FINFISH_CTT_RELE","FINFISH_DOL_KEPT","FINFISH_DOL_RELE",
                       "FINFISH_EULACHON_KEPT","FINFISH_EULACHON_RELE","FINFISH_GRNLING_KEPT",
                       "FINFISH_GRNLING_RELE","FINFISH_HERRING_KEPT","FINFISH_HERRING_RELE",
                       "FINFISH_MAC_KEPT","FINFISH_MAC_RELE","FINFISH_NANCHOVY_KEPT",
                       "FINFISH_NANCHOVY_RELE","FINFISH_PACSANDLANCE_KEPT","FINFISH_PACSANDLANCE_RELE",
                       "FINFISH_PACSARDINE_KEPT","FINFISH_PACSARDINE_RELE","FINFISH_PERCH_KEPT",
                       "FINFISH_PERCH_RELE","FINFISH_SABLEFISH_KEPT","FINFISH_SABLEFISH_RELE",
                       "FINFISH_SCULPIN_KEPT","FINFISH_SCULPIN_RELE","FINFISH_SPINYDOG_KEPT",
                       "FINFISH_SPINYDOG_RELE","FINFISH_SALMON_SHARK_KEPT","FINFISH_SALMON_SHARK_RELE",
                       "FINFISH_OTHER_SHARKS_RELE","FINFISH_SKATE_KEPT","FINFISH_SKATE_RELE",
                       "FINFISH_SMELT_KEPT","FINFISH_SMELT_RELE","FINFISH_SOLE_KEPT",
                       "FINFISH_SOLE_RELE","FINFISH_STEEL_KEPT","FINFISH_STEEL_RELE",
                       "FINFISH_STURGEON_RELE","FINFISH_ALBACORE_TUNA_KEPT","FINFISH_ALBACORE_TUNA_RELE",
                       "FINFISH_BLUEFIN_TUNA_KEPT","FINFISH_BLUEFIN_TUNA_RELE","FINFISH_TUNA_OTHER_KEPT",
                       "FINFISH_TUNA_OTHER_RELE","FINFISH_WOLF_RELE","FINFISH_SQUID_KEPT",
                       "FINFISH_OTH_KEPT","FINFISH_OTH_RELE","PRAWNCRAB_DUNCRABLEGAL_KEPT",
                       "PRAWNCRAB_REDRCRAB_KEPT","PRAWNCRAB_AKINGCRAB_KEPT","PRAWNCRAB_BOXCRAB_KEPT",
                       "PRAWNCRAB_SHORECRAB_KEPT","PRAWNCRAB_OTHUNCRAB_KEPT","PRAWNCRAB_COONSHRIMP_KEPT",
                       "PRAWNCRAB_HUMPSHRIMP_KEPT","PRAWNCRAB_OTHSHRIMP_KEPT","PRAWNCRAB_PRAWN_KEPT",
                       "PRAWNCRAB_GSHRIMP_KEPT","BIVALVES_RAZORCLAM_KEPT","BIVALVES_BUTTERCLAM_KEPT",
                       "BIVALVES_HORSECLAM_KEPT","BIVALVES_LNECK_KEPT","BIVALVES_MANILA_KEPT",
                       "BIVALVES_SOFTSHELL_KEPT","BIVALVES_VARNISH_KEPT","BIVALVES_GEODUCK_KEPT",
                       "BIVALVES_UNKCLAM_KEPT","BIVALVES_BLUEMUSSEL_KEPT","BIVALVES_CALIMUSSEL_KEPT",
                       "BIVALVES_PACIFICOYSTER_KEPT","BIVALVES_PINKSCALLOP_KEPT","BIVALVES_ROCKSCALLOP_KEPT",
                       "BIVALVES_WEATHERVANE_KEPT","BIVALVES_UNKSCALLOP_KEPT","BIVALVES_COCKLE_KEPT",
                       "OTHSHELLFISH_GIANTBARN_KEPT","OTHSHELLFISH_GOOSEBARN_KEPT","OTHSHELLFISH_MOONSNAIL_KEPT",
                       "OTHSHELLFISH_LIMPET_KEPT","OTHSHELLFISH_SEAURCHIN_KEPT","OTHSHELLFISH_SEACUCUMBER_KEPT",
                       "OTHSHELLFISH_SANDDOLLAR_KEPT","OTHSHELLFISH_SEASTAR_KEPT","OTHSHELLFISH_OCTOPUS_KEPT",
                       "OTHSHELLFISH_OTHERUNK_KEPT",
                       "CRAB_CENTREOFFISHING_LATITUDE", "CRAB_CENTREOFFISHING_LONGITUDE",
                       "PRAWN_CENTREOFFISHING_LATITUDE", "PRAWN_CENTREOFFISHING_LONGITUDE",
                       "HALIBUTLENGTH.1", "HALIBUTLENGTH.2", "HALIBUTLENGTH.3", "HALIBUTLENGTH.4",
                       "HALIBUTLENGTH.5", "HALIBUTLENGTH.6", "HALIBUTLENGTH.7", "HALIBUTLENGTH.8",
                       "NUMBERACTIVITY", "VISITS", "SAVECOUNT", "SESSIONS", "DATECREATED",
                       "DATECREATED_DAY", "DATECREATED_MONTH", "DATECREATED_YEAR", "DATECOMPLETED",
                       "DATECOMPLETEDDAY", "DATECOMPLETEDMONTH", "DATECOMPLETEDYEAR")
  .OAuth2BearerToken <- "Not Yet!"

  krestendpointTest <<- "https://krest-test.dfo-mpo.gc.ca/krestws/api"
  krestrootTest <<- "kresttstin.ent.dfo-mpo.ca"

  krestendpointProd <<- "https://krest.dfo-mpo.gc.ca/krestws/api"
  krestrootProd <<- "krestprdin.ent.dfo-mpo.ca"

  krestendpointDev <<- "https://krestdevin.ent.dfo-mpo.ca/krestws/api"
  krestrootDev <<- "krestdevin.ent.dfo-mpo.ca"

  useSSH <<- TRUE

  if (DestServer == 'P') {
    krestendpoint <<- krestendpointProd
    krestroot <<- krestrootProd
  } else if (DestServer == 'T') {
    krestendpoint <<- krestendpointTest
    krestroot <<- krestrootTest
  } else {
    krestendpoint <<- krestendpointDev
    krestroot <<- krestrootDev
    useSSH <<- FALSE
  }

  #
  # oraclePServer <<- 'L' # P' # 'T' #
  # oracleLServer <<- 'L' #

  logincreds <<- list(grant_type = "password",
                      password = "iRECData99",
                      username = "IREC_MIGRATOR")

  repeat {
    auto_notify("Trying to authenticate...")
    auto_notify(.OAuth2BearerToken)
    .OAuth2BearerToken <- getAuthToken(krestendpoint, logincreds)
    auto_notify(.OAuth2BearerToken)
    if (.OAuth2BearerToken != "Not Yet!") {
      break
    } else {
      auto_notify ("No Auth")
      auto_notify()
      for (i in 1:10) {
        cat(".")
        Sys.sleep(1)
      }
      auto_notify("")
    }
  }
  Prg <- GetLookupCodes(.OAuth2BearerToken, "/lookups/Program")
  Prg <- fromJSON(rawToChar(Prg))$result

  progSeq <<- as.character(Prg[Prg$value %like% "iREC",]$id[1])

}




