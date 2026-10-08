
#' this code is almost entirely Nick's  Just copied here to allow for converting EKOS to PRA data fromat


#' Convert SPSS labels to text
#'
UnspecifiedText <- "Unspecified"

"%notin%" <- Negate("%in%")
PraStartDate <- as.Date("2015-07-01")

#' Clear Log Messages
#'
#' Clears the globally stored log message queue
#'
clearLogMessages <- function() {
  log_env <- .GlobalEnv
  log_env$LogMessageQueue <- NULL
}

#' Add Log Message
#'
#' Add a vector of log messages to the global log message queue
#'
#' @param ... character string of the message, use curly brackets (braces) to input objects (glue notation)
#'
#'
addLogMessages <- function(...) {
  logMsg <- glue(..., .sep = " ", .envir = parent.frame())

  log_env <- .GlobalEnv
  log_env$LogMessageQueue <- c(log_env$LogMessageQueue, logMsg)
}

#' Get Log Messages
#'
#' Retrieve the global vector of vector log messages
#'
#' @return A tibble with the log messages as a column
#'
#' @importFrom dplyr tibble
#'
getLogMessages <- function() {
  log_env <- .GlobalEnv
  return(tibble(log_messages = log_env$LogMessageQueue))
}


#' Columns read from an SPSS file with the haven R package provides labelled columns.
#' This function converts the labels (that are kind of like factors) into a text column.
#'
#' @param values A vector of labelled values
#'
#' @return Text vector
#'
#' @importFrom haven as_factor
#'
labelText <- function(values) {
  text <-
    values %>%
    haven::as_factor() %>%
    as.character()

  return(text)
}
#' Validate Value domains
#'
#' A helper function for validating data within a pre-defined set of possible values.
#'
#' @param values The data that is to be validated
#' @param domain The set of valid values that "values" can take on.
#' @param error_message A template of a printf message for invalid values.
#'
#' @return The values provided
#'
#' @note If the values vector contains values that are not in the domain vector, then the method calls stop
#'   with an error message using the template provide in the "error_message" parameter.
validateValueDomain <- function(values,
                                domain,
                                error_message = "The following values are invalid:\n\n%s\n\n") {
  invalid_values <- values %notin% domain
  if (any(invalid_values)) {
    invalid_values <- values[invalid_values]
    invalid_values <- unique(invalid_values)
    stop(sprintf(error_message, paste(invalid_values, collapse = ", ")))
  }
  return(values)
}

#' Convert Column Types
#'
#' Convert column types of a data frame to match another tibble with the same column names.
#' This function helps to setup tibbles to use dplyr::bind_rows with
#'
#' NOTE: the column names must be exactly the same between the two tibbles
#'
#' @param df The tibble whose columns are to be converted
#' @param df_to_match The tibble with the column type definitions
#'
#' @return The df tibble with the column types converted to match the df_to_match tibble
#'
#' @importFrom tidyr gather nest unnest
#' @importFrom dplyr summarize_all vars
#'
convertColTypes <- function(df, df_to_match) {
  col_types <-
    df_to_match %>%
    summarize_all(class) %>%
    gather(col_name, col_type) %>%
    nest(data = -col_type)

  for (colIdx in 1:nrow(col_types)) {
    type_name <- pull(col_types[colIdx, 1])
    col_names <-
      col_types[colIdx, ] %>%
      unnest(cols = data) %>%
      pull(col_name)
    if ("character" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.character)
    } else if ("numeric" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.double)
    } else if ("integer" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.integer)
    } else if ("logical" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), as.logical)
    } else if ("Date" == type_name) {
      df <-
        df %>%
        mutate_at(vars(col_names), parseDate)
    } else {
      stop(glue(
        "Unknown column type '{type_name}': {paste0(col_names, collapse=',')}"
      ))
    }
  }

  return(df)
}

#' Parse dates.  This function tries to parse using the ymd then myd and finally dmy format.
#'
#' @param date_text Dates to parse
#'
#' @importFrom lubridate parse_date_time as_date
#'
parseDate <- function(date_text) {
  date_values <-
    parse_date_time(date_text,
                    orders = c("Ymd", "mdY", "dmY", "dbY", "dby")) %>%
    as_date()
  return(date_values)
}

PraDidNotFish <- 1 # "Did not fish in the month"
PraDidFish <- 2 # "Fished in the month"
PraDidFishValid <- c(PraDidNotFish, PraDidFish)

PraCompleteSurvey <- "Complete"
PraIncompleteSurvey <- "Incomplete"
PraCompleteSurveyValid <- c(PraCompleteSurvey, PraIncompleteSurvey)


CatchColPrefix <- c("effort_days", "juv_effort_days", "salmon_", "halibut_", "finfish_", "prawncrab_", "bivalves_", "othshellfish_")
#The next regular expression identifies the catch column names
CatchColRegEx <- stringr::str_c("^(", stringr::str_c(CatchColPrefix, collapse="|"), ")")

#' Get Catch Column Names
#'
#' Retrieve a vector of catch column names from the survey data frame provided
#'
#' @param survey_df A survey related data frame
#'
#' @return A vector of column names related to catch, including effort_days and juv_effort_days
#'
getCatchColNames <- function(survey_df) {
  survey_col_names <- colnames(survey_df)
  catch_col_name_idx <- grep(CatchColRegEx,
                             survey_col_names)
  return(survey_col_names[catch_col_name_idx])
}
#' Load Exclude File
#'
#' Read in the exclude csv file of licence IDs or Survey Keys that are to be
#' excluded from iRec Survey
#'
#' @param exclude_file_name File name of excluded licences or survey access code
#'
#' @return A data frame of licence_id or survey_access_key to exclude
#'
loadExcludeFile <- function(excludes = NA) {
  key_col_names <- c("licence_id", "survey_access_key")

  if (length(excludes) == 0 || is.null(excludes)) {
    return(NULL)
  }
  # if (is.na(excludes)) {
  #   return(NULL)
  # }

  # if (fs::file_exists(exclude_file_name) == FALSE) {
  #   addLogMessages("WARNING - exclude file '{exclude_file_name}' does not exist")
  #   return(NULL)
  # }

  exclude_df <- excludes #read_csv(exclude_file_name)
  if ("licence.id" %in%  names(exclude_df)) {
    exclude_df %<>%
      dplyr::rename(licence_id = licence.id)
  }

  found_key_col <-
    names(exclude_df) %>%
    dplyr::intersect(key_col_names)

  if (length(found_key_col) == 0) {
    key_col_txt <-
      stop(glue("No ",
                str_c("\"", key_col_names, "\"", collapse=" or "),
                " columns identified in the exclude file"))
  }

  return(exclude_df)
}

loadEkosSurveyResults <- function(ekos_data, exclude_lic_id) {
  #The number below identifies the maximum total proportion that does not have a Licence ID in the results
  # For example, 0.01 means that if more then 1% of ekos records is missing licence IDs then throw an error.
  max_prop_miss_id <- 0.01

  #Ekos file name is in SPSS format.


  ekos_data <- ekos_data %>%
    as_tibble()
  #%>%
   # select_all(tolower)

  total_ekos_rows <- nrow(ekos_data)
  if (total_ekos_rows == 0) {
    stop("The Ekos file has no responses, please check the file and contact Ekos for an upated file.")
  }

  #remove results that do not have a licence ID, these seem like dumby responses
  ekos_data <-
    ekos_data %>%
    filter(!is.na(Licence_ID))

  if ( total_ekos_rows > nrow(ekos_data)) {
    addLogMessages("{total_ekos_rows - nrow(ekos_data)} Ekos result records are missing ",
                   "licence IDs")

    if ((total_ekos_rows - nrow(ekos_data)) > (total_ekos_rows * max_prop_miss_id)) {
      stop(glue("More then {format(max_prop_miss_id * 100.0, 3)}% ",
                "of {total_ekos_rows} Ekos result records are missing licence IDs"))
    }
  }

  ekos_data <-
    ekos_data %>%
    mutate(Licence_ID = trimws(Licence_ID),
           DNF_1 = !is.na(DNF_1),
           First_name = trimws(First_name),
           Last_name = trimws(Last_name),
           AMAIL = trimws(AMAIL),
           REPDAY = as.integer(REPDAY - 1000),
           REPYEAR = as.integer(REPYEAR)) %>%
    dplyr::rename(did_not_fish = DNF_1,
           licence_id = Licence_ID,
           first_name = First_name,
           last_name = Last_name,
           email = AMAIL,
           area = REPZONE,
           method = REPMETHOD,
           year = REPYEAR,
           month = REPMONTH,
           day = REPDAY,
           effort_days = COMPLETE,
           comment = AFINISHED)

    #ekos_data[,"datefished"]<-ISOdate(ekos_data$year,as.integer(ekos_data$month - 1000),ekos_data$day)

  ekos_data[,"datefished"] <- as.Date(paste(ekos_data$year,as.integer(ekos_data$month - 1000),ekos_data$day, sep = "-"), )

    ekos_data <- ekos_data %>%  mutate_at(vars(area, method, month), labelText)

  if (!is.null(exclude_lic_id) && length(exclude_lic_id) > 0) {
    ekos_data <-
      ekos_data %>%
      filter(licence_id %notin% exclude_lic_id)
    if (total_ekos_rows > nrow(ekos_data)) {
      addLogMessages("Based on excluded licence IDs, {total_ekos_rows - nrow(ekos_data)}",
                     " Ekos result records were exclude from analysis.")
    }
  } else {
    addLogMessages("WARNING - no licences were excluded from the EKOS results")
  }

  # complete_surveys <-
  #   ekos_data %>%
  #   filter(effort_days == 1 | did_not_fish == TRUE)
  #
  # addLogMessages("Total number of incomplete survey responses: ",
  #                "{nrow(ekos_data) - nrow(complete_surveys)}")

  ekos_data <-
    ekos_data %>%
    mutate(effort_days = if_else(did_not_fish == 1, 0, effort_days))

  if ("QLODGE" %in% names(ekos_data)) {
    ekos_data <-
      ekos_data %>%
      dplyr::rename(lodge = QLODGE,
             guided = QGUIDE)

    ekos_last_catch_col <- which(names(ekos_data) == "REVIEW") - 1
    ekos_data_catch <- ekos_data[,c(13,16:ekos_last_catch_col)]

    later_col <- which(names(ekos_data) %in% c("effort_days", "did_not_fish", "comment", "datefished"))
    ekos_data_strata <- ekos_data[,c(3:11,14:15,later_col)]

    ekos_data_strata <-
      ekos_data_strata %>%
      mutate(lodge = coalesce(as.character(lodge), UnspecifiedText),
             guided = coalesce(as.character(guided), UnspecifiedText))

  } else {
    #Past ekos result files do not have the QLODGE or QGUIDE columns
    later_col <- which(names(ekos_data) %in% c("comment","datefished"))
    ekos_data_catch <- ekos_data[,13:338]
    ekos_data_strata <-
      ekos_data[,c(3:11,341:342,later_col)] %>%
      mutate(lodge = UnspecifiedText,
             guided = UnspecifiedText)
  }

  #ensure the order of the strata columns, important for column indexing later.
  ekos_data_strata <-
    ekos_data_strata %>%
    select(licence_id,
           first_name,
           last_name,
           email,
           area,
           method,
           year,
           month,
           day,
           datefished,
           guided,
           lodge,
           did_not_fish,
           effort_days,
           comment)

  #extracting all of the columns in the df starting with A
  ekos_data_catch <- ekos_data_catch[,str_sub(colnames(ekos_data_catch), 1, 1) == "A"]
  #Remove the leading "A" from from the catch data fields
  colnames(ekos_data_catch) <-
    str_sub(colnames(ekos_data_catch), 2) %>%
    str_to_lower()

  ekos_data_catch <-
    ekos_data_catch %>%
    mutate_all(coalesce, 0)

  ekos_data <-
    bind_cols(ekos_data_strata, ekos_data_catch) %>%
    dplyr::rename(juv_effort_days = juvepres,
                  surveykey = licence_id)

  return(ekos_data)
}


loadPraSurveyResultsC <- function(survey_result,
                                 exclude_id,
                                 strata_col_names) {

  #After some trial and error, the below "read_spss" function from
  # survey_result <-
  #   read_spss(survey_result_filename) %>%
  #   as_tibble() %>%
  #   select_all(tolower)

  survey_result <- survey_result %>%
    as_tibble() %>%
    select_all(tolower)

  survey_result_spp <-
    survey_result %>%
    select()

  strata_col_names <-
    c("licence_id",
      "surveykey",
      "first_name",
      "last_name",
      "email",
      "didnotfish",
      "completesurvey",
      "datefished",
      "area",
      "method",
      "fishedfromlodge",
      "fishedwithguide",
      "totaljuveniles",
      "checkprawns",
      "checkcrabs",
      "checkcrabsprawns",
      "comment") %>%
    intersect(colnames(survey_result))

  survey_result <-
    survey_result %>%
    select(all_of(c(
      strata_col_names, getCatchColNames(survey_result)
    )))

  #The number below identifies the maximum total proportion that does not have a Licence ID in the results
  # For example, 0.01 means that if more then 1% of PRA records is missing licence IDs then throw an error.
  max.prop.miss.id <- 0.01

  total_rows <- nrow(survey_result)
  if (total_rows == 0) {
    stop("The PRA file has no responses, please check the file and contact PRA for an upated file.")
  }

  key_id <- ""
  if ("licence_id" %in% names(survey_result)) {
    key_id <- "licence_id"
    survey_result <-
      survey_result %>%
      mutate(licence_id = str_trim(str_replace_all(licence_id, "[.]", "")))
  } else if ("surveykey" %in% names(survey_result)) {
    key_id <- "surveykey"
  } else {
    stop("No key column found in survey results!")
  }
  key_id <- sym(key_id)

  #remove results that do not have a licence ID or survey keys, these seem like dumby responses
  survey_result <-
    survey_result %>%
    filter(!is.na(!!key_id)) %>%
    mutate_at(vars(!!key_id), str_trim)

  survey_result <-
    survey_result %>%
    mutate(area = str_trim(area),
           method = str_trim(method)) %>%
    mutate(area = if_else(str_length(area) == 0,
                          NA_character_,
                          area),
           method = if_else(str_length(method) == 0,
                            NA_character_,
                            method),
           didnotfish = validateValueDomain(didnotfish, PraDidFishValid))

  if (total_rows > nrow(survey_result)) {
    addLogMessages("{total_rows - nrow(survey_result)} PRA result records are missing licence IDs or survey keys")
    if ((total_rows - nrow(survey_result)) > (total_rows * max.prop.miss.id)) {
      stop(sprintf("More then %.3f%% of %d PRA result records are missing licence IDs or survey keys\n", max.prop.miss.id * 100.0, total_rows))
    }
  }

  #The effort days for each row is 1 if they actual fished (!DidNotFish)
  survey_result <-
    survey_result %>%
    mutate(didnotfish = if_else(didnotfish == PraDidNotFish, TRUE, FALSE)) %>%
    mutate(effort_days = if_else(didnotfish, 0L, 1L)) %>%
    dplyr::rename(did_not_fish = didnotfish)

  if (!is.null(exclude_id) && any(!is.na(exclude_id))) {
    pre.exclude.total.rows <- nrow(survey_result)

    survey_result <-
      survey_result %>%
      filter(!!key_id %notin% exclude_id)

    if (pre.exclude.total.rows > nrow(survey_result)) {
      addLogMessages("Based on excluded licence IDs/access keys, ",
                     "{pre.exclude.total.rows - nrow(survey_result)} PRA result records were exclude from analysis.\n")
    }
  } else {
    addLogMessages("WARNING - no licences were identified for exclusion from the PRA results")
  }

  #make sure the licence ID is character as the licence file is loaded with character licence ID
  if ("licence_id" %in% colnames(survey_result)) {
    if (is.character(survey_result$licence_id) == FALSE) {
      survey_result <-
        survey_result %>%
        mutate(licence_id = as.character(licence_id))
    }
  }

  survey_result <-
    survey_result %>%
    mutate(completesurvey = validateValueDomain(str_trim(completesurvey),
                                                PraCompleteSurveyValid)) %>%
    mutate(completesurvey = completesurvey == PraCompleteSurvey)

  incomplete_total <-
    survey_result %>%
    filter(completesurvey == FALSE) %>%
    count() %>%
    pull()


  addLogMessages("Total number of incomplete survey responses: {incomplete_total}\n")

  #Reuse the Complete Survey column to use as effort days value
  survey_result <-
    survey_result %>%
    filter(completesurvey == TRUE) %>%
    dplyr::rename(lodge = fishedfromlodge,
           guided = fishedwithguide,
           juv_effort_days = totaljuveniles) %>%
    select(-completesurvey)

  strata_col_names <-
    c("licence_id",
      "surveykey",
      "first_name",
      "last_name",
      "email",
      "did_not_fish",
      "datefished",
      "area",
      "method",
      "lodge",
      "guided",
      "checkcrabs",
      "checkprawns",
      "checkcrabsprawns",
      "comment") %>%
    intersect(colnames(survey_result))

  #Catch columns are all columns that are not stratafication fields
  #The "select(effort_days, everything())" moves the effort_days column to the first column
  survey_result_catch <-
    survey_result %>%
    select(all_of(getCatchColNames(survey_result))) %>%
    select(effort_days, everything()) %>%
    mutate_if(is.integer,coalesce, 0L) %>% #autofill all the empty catch cells with zero
    mutate_if(is.double,coalesce, 0)

  factor_to_char_col <-
    c("lodge", "guided", "checkcrabs", "checkprawns", "checkcrabsprawns") %>%
    base::intersect(colnames(survey_result))

  survey_result_strata <-
    survey_result %>%
    select(all_of(strata_col_names)) %>%
    mutate_at(factor_to_char_col, ~ coalesce(labelText(.), UnspecifiedText))

  if("checkcrabsprawns" %in% factor_to_char_col) {
    survey_result_strata <-
      survey_result_strata %>%
      mutate(checkcrabsprawns = str_replace_all(checkcrabsprawns, "rrawn", "prawn"))

  }

  #ensure the order of the strata columns, important for column indexing later.
  survey_result <-
    survey_result_strata %>%
    bind_cols(survey_result_catch) %>%
    mutate_if(is.labelled, zap_labels)

  return(survey_result)
}

#' Load Survey Adjustments
#'
#' Loads and validates the survey adjustment file
#'
#' @param survey_adj_filename CSV file name of survey adjustments
#'
#' @return A tibble of updated survey data to replace original survey results
#'
#' @importFrom dplyr distinct count starts_with
#' @importFrom readr read_csv cols
#'
#'
#'

loadSurveyAdjustments <- function(adjusts) {
  # adj_data <-
  #   read_csv(survey_adj_filename, col_types = cols(.default = "c")) %>%
  #   mutate(did_not_fish = str_to_lower(did_not_fish)) %>%
  #   mutate(did_not_fish = case_when("yes" == did_not_fish ~ TRUE,
  #                                   "no" == did_not_fish ~ FALSE,
  #                                   TRUE ~ NA)) %>%
  #   select_all(str_to_lower) %>%
  #   select(-starts_with("...")) #Remove empty columns in adj file
  adj_data <-
    adjusts %>%
    mutate(did_not_fish = str_to_lower(did_not_fish)) %>%
    mutate(did_not_fish = case_when("yes" == did_not_fish ~ TRUE,
                                    "no" == did_not_fish ~ FALSE,
                                    TRUE ~ NA)) %>%
    select_all(str_to_lower) %>%
    select(-starts_with("...")) #Remove empty columns in adj file

  id_col_name <-
    if_else("surveykey" %in% colnames(adj_data),
            "surveykey",
            "licence_id") %>%
    sym()
  #Identify if and licence IDs have mixed fishing/did not fish records
  mix_dnf <-
    adj_data %>%
    distinct(!!id_col_name, did_not_fish) %>%
    count(!!id_col_name) %>%
    filter(n > 1)

  if (nrow(mix_dnf) > 0) {
    mix_dnf_lic_text <-
      pull(mix_dnf, !!id_col_name) %>%
      paste0(collapse = ",")

    error_msg <- glue("Can not load survey adjustment file because the following licence IDs have a mix of fishing and did not fish:\n",
                      mix_dnf_lic_text,
                      "\n")
    stop(error_msg)
  }

  catch_col_names <- getCatchColNames(adj_data)

  mix_na <-
    adj_data %>%
    group_by(!!id_col_name) %>%
    summarize_at(catch_col_names, function(.) {all(is.na(.)) | all(!is.na(.))}) %>%
    ungroup() %>%
    gather(col_name, status, -!!id_col_name) %>%
    filter(status == FALSE) %>%
    select(-status)

  if (nrow(mix_na) > 0) {
    mix_na_lic_text <- str_c(pull(mix_na, !!id_col_name),
                             pull(mix_na, col_name),
                             sep=",",
                             collapse="\n")
    error_msg <- glue("Can not load survey adjustment file because the following licence IDs have a mix of NA and values for particular catch columns:\n{mix_na_lic_text}\n")
    stop(error_msg)
  }

  return(adj_data)
}



loadSurveyResults <- function(survey_result,
                              survey_start_date,
                              excludes,
                              adjusts) {

  exclude_id <- c()

  exclude_lic <- loadExcludeFile(excludes)
  if (!is.null(exclude_lic) && nrow(exclude_lic) > 0) {
    if ("licence_id" %in% colnames(exclude_lic)) {
      exclude_id <- pull(exclude_lic, licence_id)
    } else {
      exclude_id <- pull(exclude_lic, survey_access_key)
    }
  }

  if (survey_start_date < PraStartDate) {
    survey_results <-
      loadEkosSurveyResults(survey_result, exclude_id)
  } else {
    survey_results <-
      loadPraSurveyResultsC(survey_result, exclude_id)
  }
  id_col_name <-
    if_else("surveykey" %in% colnames(survey_results),
            "surveykey",
            "licence_id") %>%
    sym()
   if (!is.null(adjusts)) {
  #   if (!is.na(survey_adj_filename) &
  #       str_length(survey_adj_filename) > 0) {
  #
  #     if (!file_exists(survey_adj_filename)) {
  #       addLogMessages("WARNING-The specified adjustment file does not exist: {survey_adj_filename}")
  #     } else {
        #Adjustments provided, so apply...

     if ("comment" %notin% names(adjusts)) {
       adjusts$comment <- ""
     }

     #KH July 14, 2026 -- Added so recent adjusts match old data format

     NclmnstoAdj <- names(survey_results) #c("month", "year", "day")
     for (ncn in NclmnstoAdj) {
       cat(".")
       if(ncn %notin% names(adjusts)){
         adjusts[[ncn]] <- ""
         print("X")
       }
     }
     #KH July 14, 2026

     adjust <-
          loadSurveyAdjustments(adjusts) %>%
          convertColTypes(survey_results)

        adjust_lic_ids <-
          adjust %>%
          pull(!!id_col_name) %>%
          unique()

        survey_results <-
          survey_results %>%
          filter(!!id_col_name %notin% adjust_lic_ids) %>%
          bind_rows(adjust)
      # }
    # }
  }

  return(survey_results)
}

