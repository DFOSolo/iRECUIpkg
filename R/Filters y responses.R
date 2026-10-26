# IMPORT (READ EVERYTHING AS TEXT)
#' function to process the iREC sav file looking for duplicate records
#' parameters are:
#' @export
FiltersyResponses <- function(Dataset) {

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
        "UN",
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
date_cols <- c(
  "Survey_start",
  "Survey_end",
  "DateFished",
  "datecreated",
  "datecompleted"
)

#  Don't need to do this as we are getting the data from the SAV file not excel

# Dataset <- Dataset %>%
#   mutate(
#     across(
#       any_of(date_cols),
#       ~ suppressWarnings(
#         as.Date(
#           as.numeric(.) ,
#            origin = "1899-12-30"
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

dup_cols <- setdiff(names(Dataset), "SurveyKey")

Duplicate_reports <- Complete %>%
  group_by(across(all_of(dup_cols))) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(across(all_of(dup_cols)))


#dir.create("salida", showWarnings = FALSE)

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
#   path = file.path("salida", "iREC_filters_May2026.xlsx")
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
