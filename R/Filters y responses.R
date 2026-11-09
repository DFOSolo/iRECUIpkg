# IMPORT (READ EVERYTHING AS TEXT)
#' function to process the iREC sav file looking for duplicate records
#' parameters are:
#' @export
FiltersyResponses <- function(Dataset) {
  # IMPORT (READ EVERYTHING AS TEXT)

  # Dataset <- read_excel(
  #   "C:/Users/heink/Documents/DFO_Monthly Catch Survey_September2026(p).xlsx",
  #   col_types = "text"
  # )

# this bit looks for the end of the data and makes sure it ends with 12 HalibutLengths.X
  otherFields <- c("OTHSHELLFISH_OTHERUNK_KEPT", "Crab_CentreOfFishing_Latitude", "Crab_CentreOfFishing_Longitude",
                   "Prawn_CentreOfFishing_Latitude", "Prawn_CentreOfFishing_Longitude")
# finds the position of the last item above
  for (f in rev(otherFields)){
    Ttarget_pos <- which(names(Dataset) == f)
    if(length(Ttarget_pos) > 0) {
      break
    }
  }
# if none of those are present then it uses the last element as the last postion
  if (length(Ttarget_pos) == 0) {
    Ttarget_pos = ncol(Dataset)
  }
# loop thorough the HalibutLength options inserting them as 0s if necessary in their best order and position
  for(hl in 1:12) {
    message(hl)
    if (paste0('HalibutLength.', hl) %notin% names(Dataset))
    {
      message(paste0('Not found ', hl))
      target_pos <- which(names(Dataset) == paste0('HalibutLength.', hl - 1))
      if (length(target_pos) == 0) {
        target_pos = Ttarget_pos
      }
      Dataset[[paste0('HalibutLength.', hl)]] <- c(rep(' ', nrow(Dataset)))
      total_cols <- ncol(Dataset)
      Dataset <- Dataset[, c(1:target_pos, total_cols, (target_pos + 1):(total_cols - 1))]
    }
  }

  # HALIBUT LENGTH (KEEP "UN")


  Dataset <- Dataset %>%
    mutate(across(
      starts_with("HalibutLength"),
      ~ {
        x <- . %>%
          stringr::str_replace_all("[\r\n]", "") %>%  # remove hidden chars
          stringr::str_trim() %>%
          toupper()

        ifelse(
          x == "UN" | x == "",
          " ",
          as.character(suppressWarnings(as.numeric(x)))
        )
      }
    ))


  # CRITICAL: CONVERT KEPT / RELE TO NUMERIC

  Dataset <- Dataset %>%
    mutate(across(
      matches("KEPT|RELE"),
      ~ suppressWarnings(as.numeric(.))
    ))

  num_cols_extra <- c("StartDay", "EndDay", "DidNotFish", "Year",
                      "Month", "Day", "TotalJuveniles",
                      "FishedFromLodge", "FishedWithGuide")

  Dataset <- Dataset %>%
    mutate(across(
      all_of(num_cols_extra),
      ~ suppressWarnings(as.numeric(.))
    ))

  # FILTERS
  #suppress warnings dates
  #  Don't need to do this as we are getting the data from the SAV file not excel
  # date_cols <- c(
  #   "Survey_start",
  #   "Survey_end",
  #   "DateFished",
  #   "datecreated",
  #   "datecompleted"
  # )
  #
  # Dataset <- Dataset %>%
  #   mutate(
  #     across(
  #       any_of(date_cols),
  #       ~ suppressWarnings(
  #         as.Date(
  #           as.numeric(.),
  #           origin = "1899-12-30"
  #         )
  #       )
  #     )
  #   )

  # Complete surveys
  Complete <- Dataset %>%
    filter(CompleteSurvey == "Complete")

  # Tuna
  Tuna <- Complete %>%
    filter(
      FINFISH_ALBACORE_TUNA_KEPT >= 1 |
        FINFISH_ALBACORE_TUNA_RELE >= 1 |
        FINFISH_BLUEFIN_TUNA_KEPT >= 1 |
        FINFISH_BLUEFIN_TUNA_RELE >= 1 |
        FINFISH_TUNA_OTHER_KEPT >= 1 |
        FINFISH_TUNA_OTHER_RELE >= 1
    )

  # Halibut
  Halibut <- Complete %>%
    filter(
      HALIBUT_HALIBUT_KEPT >= 1 |
        HALIBUT_HALIBUT_RELE >= 1
    )

  # Sturgeon
  Sturgeon <- Complete %>%
    filter(FINFISH_STURGEON_RELE >= 1)

  # Comments
  Comments_edit <- Complete %>%
    filter(!is.na(Comment) & Comment != "") %>%
    distinct(SurveyKey, .keep_all = TRUE)

  # Unique responses
  Responses <- Dataset %>%
    distinct(SurveyKey, .keep_all = TRUE)

  # Duplicate reports
  # Same information in all fields except SurveyKey

  dup_cols <- c(
    "StartDay",
    "EndDay",
    "Survey_start",
    "Survey_end",
    names(Complete)[
      match("DateFished", names(Complete)):
        match("HalibutLength.12", names(Complete))
    ]
  )

  catch_cols <- names(Complete)[
    match("SALMON_CHINOOK_HATCH_KEPT", names(Complete)):
      match("OTHSHELLFISH_OTHERUNK_KEPT", names(Complete))
  ]

  Duplicate_reports <- Complete %>%
    filter(DidNotFish == 2) %>%
    filter(
      rowSums(
        across(
          all_of(catch_cols),
          ~ replace_na(., 0)
        )
      ) > 0
    ) %>%
    group_by(across(all_of(dup_cols))) %>%
    filter(n() > 1) %>%
    mutate(DuplicateGroup = cur_group_id()) %>%
    ungroup()

  Duplicate_counts <- Duplicate_reports %>%
    group_by(DuplicateGroup) %>%
    summarise(
      MatchingCatchColumns = sum(
        sapply(catch_cols, function(col) {

          vals <- get(col)

          length(unique(vals)) == 1 &&
            !is.na(vals[1]) &&
            vals[1] > 0

        })
      ),
      .groups = "drop"
    )

  # Agregar el conteo a cada fila
  Duplicate_reports <- Duplicate_reports %>%
    left_join(Duplicate_counts, by = "DuplicateGroup") %>%
    arrange(desc(MatchingCatchColumns), DuplicateGroup)

  # EXPORT


  #export

  # dir.create("salida", showWarnings = FALSE)
  #
  # write_xlsx(
  #   list(
  #     Tuna = Tuna,
  #     Halibut = Halibut,
  #     Sturgeon = Sturgeon,
  #     Responses = Responses,
  #     Comments_edit = Comments_edit,
  #     Duplicate_reports = Duplicate_reports,
  #     Complete = Complete
  #   ),
  #   path = file.path("salida", "iREC_filters_sep2026.xlsx")
  # )

  return( list(
    Tuna = Tuna,
    Halibut = Halibut,
    Sturgeon = Sturgeon,
    Responses = Responses,
    Comments_edit = Comments_edit,
    Duplicate_reports = Duplicate_reports,
    Complete = Complete
  ))

}

