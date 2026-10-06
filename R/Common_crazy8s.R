library(dplyr)
library(tidyr)
library(haven)

#' wrapper to occurances of potential flag crazy 8's
#' @export
flagged_8s <- function(df) {

  # 1) Clean and standardise column names
  df <- haven::zap_label(df)
  names(df) <- tolower(names(df))

  # 2) Normalise key fields
  if (!"surveykey" %in% names(df)) {
    if ("licence_id" %in% names(df)) {
      df <- dplyr::rename(df, surveykey = licence_id)
    } else if ("licenceid" %in% names(df)) {
      df <- dplyr::rename(df, surveykey = licenceid)
    } else if ("licence_id" %in% names(df) == FALSE && "licence.id" %in% names(df)) {
      df <- dplyr::rename(df, surveykey = `licence.id`)
    }
  }

  if (!"completesurvey" %in% names(df) && "completesurvey" %in% names(df) == FALSE && "complete" %in% names(df)) {
  }

  # keep only complete
  if ("completesurvey" %in% names(df)) {
    df <- df %>% filter(completesurvey == "Complete" | completesurvey == TRUE)
  } else if ("completesurvey" %in% names(df) == FALSE && "completesurvey" %in% names(df) == FALSE && "completesurvey" %in% names(df) == FALSE) {
    # nothing
  } else if ("completesurvey" %in% names(df) == FALSE && "completesurvey" %in% names(df) == FALSE) {
    # nothing
  }

  # total juveniles
  if (!"totaljuveniles" %in% names(df)) {
    if ("juv_effort_days" %in% names(df)) {
      df$totaljuveniles <- suppressWarnings(as.integer(df$juv_effort_days))
    } else if ("juvepres" %in% names(df)) {
      df$totaljuveniles <- suppressWarnings(as.integer(df$juvepres))
    } else {
      df$totaljuveniles <- 0L
    }
  }

  # 3) People count
  df <- df %>%
    mutate(
      totaljuveniles = coalesce(suppressWarnings(as.integer(totaljuveniles)), 0L),
      numpeople = 1L + totaljuveniles
    )

  # 4) Rules
  list_rule <- tolower(c(
    "HALIBUT_ROCKFISH_BOCACCIO_KEPT",
    "HALIBUT_ROCKFISH_YELLOWTAIL_KEPT",
    "FINFISH_ALTSAL_KEPT",
    "FINFISH_ALTSAL_RELE",
    "FINFISH_SALMON_SHARK_KEPT",
    "FINFISH_SALMON_SHARK_RELE",
    "FINFISH_OTHER_SHARKS_RELE",
    "FINFISH_SKATE_KEPT",
    "FINFISH_SKATE_RELE",
    "FINFISH_WOLF_RELE",
    "BIVALVES_GEODUCK_KEPT",
    "OTHSHELLFISH_SANDDOLLAR_KEPT",
    "OTHSHELLFISH_SEASTAR_KEPT",
    "OTHSHELLFISH_SEACUCUMBER_KEPT",
    "OTHSHELLFISH_OCTOPUS_KEPT",
    "FINFISH_SABLEFISH_KEPT",
    "FINFISH_SABLEFISH_RELE",
    "OTHSHELLFISH_MOONSNAIL_KEPT",
    "OTHSHELLFISH_LIMPET_KEPT",
    "OTHSHELLFISH_GIANTBARN_KEPT",
    "OTHSHELLFISH_GOOSEBARN_KEPT",
    "FINFISH_BLUEFIN_TUNA_RELE",
    "FINFISH_TUNA_OTHER_KEPT",
    "FINFISH_BLUEFIN_TUNA_KEPT",
    "FINFISH_ALBACORE_TUNA_RELE",
    "FINFISH_ALBACORE_TUNA_KEPT"
  ))
  list_rule <- intersect(list_rule, names(df))

  all_catch_cols <- grep("(_kept|_rele)$", names(df), value = TRUE)

  selected_columns <- grep("(_kept)$", names(df), value = TRUE)
  selected_columns <- selected_columns[!grepl(
    "chinook|coho|halibut_halibut_kept|finfish_sole_|finfish_cod_|finfish_perch_|finfish_sculpin_|bivalves_pacificoyster_",
    selected_columns,
    ignore.case = TRUE
  )]

  cols_stats <- setdiff(selected_columns, list_rule)

  # convert catch columns to numeric, treat 0 as missing for checks
  df <- df %>%
    mutate(across(all_of(union(selected_columns, all_catch_cols)),
                  ~ suppressWarnings(as.numeric(.x)))) %>%
    mutate(across(all_of(union(selected_columns, all_catch_cols)),
                  ~ na_if(.x, 0)))

  # 5) Stat thresholds (per person)
  summary_stats_high_tb <- tibble::tibble(column = character(), mean = numeric(), sd = numeric(), lower = numeric(), upper = numeric())

  if (length(cols_stats) > 0) {
    df_pp <- df %>%
      mutate(across(all_of(cols_stats), ~ .x / numpeople))

    summary_stats_high_tb <- df_pp %>%
      summarise(
        across(
          all_of(cols_stats),
          list(
            mean = ~ mean(.x, na.rm = TRUE),
            sd   = ~ sd(.x,   na.rm = TRUE)
          ),
          .names = "{.col}__{.fn}"
        )
      ) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      separate(variable, into = c("column", "statistic"), sep = "__") %>%
      pivot_wider(names_from = statistic, values_from = value) %>%
      mutate(
        lower = mean - 2 * sd,
        upper = mean + 2 * sd
      )
  }

  # 6) List rule hits
  list_hits <- tibble::tibble(surveykey = character(), reason = character())

  if (length(list_rule) > 0 && "surveykey" %in% names(df)) {
    list_hits <- df %>%
      select(surveykey, all_of(list_rule)) %>%
      pivot_longer(-surveykey, names_to = "column", values_to = "val") %>%
      filter(!is.na(val) & val == 8) %>%
      distinct(surveykey) %>%
      mutate(reason = "list_rule")
  }

  # 7) Stat rule hits (only among cols_stats)
  stat_hits <- tibble::tibble(surveykey = character(), reason = character())

  if (length(cols_stats) > 0 && nrow(summary_stats_high_tb) > 0 && "surveykey" %in% names(df)) {
    stat_hits <- df %>%
      select(surveykey, numpeople, all_of(cols_stats)) %>%
      pivot_longer(cols = -c(surveykey, numpeople), names_to = "column", values_to = "val") %>%
      left_join(select(summary_stats_high_tb, column, lower, upper, sd), by = "column") %>%
      mutate(
        adj_upper = upper * numpeople,
        adj_lower = lower
      ) %>%
      filter(
        !is.na(sd),
        !is.na(val),
        val == 8,
        (val < adj_lower | val > adj_upper)
      ) %>%
      distinct(surveykey) %>%
      mutate(reason = "stat_rule")
  }

  # 8) Combine reasons safely
  hits_tbl <- bind_rows(list_hits, stat_hits)

  if (nrow(hits_tbl) == 0) {
    return(tibble::tibble())
  }

  reason_tbl <- hits_tbl %>%
    group_by(surveykey) %>%
    summarise(reason_of_hit = paste(unique(reason), collapse = "+"), .groups = "drop") %>%
    mutate(reason_of_hit = case_when(
      reason_of_hit == "list_rule+stat_rule" ~ "both",
      TRUE ~ reason_of_hit
    ))

  selected_responses <- df %>%
    semi_join(reason_tbl, by = "surveykey") %>%
    left_join(reason_tbl, by = "surveykey")

  # 9) Which kept columns have an 8
  selected_cols_in_result <- intersect(selected_columns, names(selected_responses))
  if (length(selected_cols_in_result) == 0) {
    selected_responses <- selected_responses %>%
      mutate(catch_with_c8 = NA_character_)
  } else {
    mat <- as.data.frame(select(selected_responses, all_of(selected_cols_in_result))) == 8
    mat[is.na(mat)] <- FALSE

    selected_responses$catch_with_c8 <- apply(
      mat, 1,
      function(idx) {
        hits <- selected_cols_in_result[idx]
        if (length(hits) == 0) NA_character_ else paste(hits, collapse = ", ")
      }
    )
  }

  # 10) List all non-missing catch values for context
  catch_cols_in_result <- intersect(all_catch_cols, names(selected_responses))
  if (length(catch_cols_in_result) == 0) {
    selected_responses <- selected_responses %>%
      mutate(species_caught = NA_character_)
  } else {
    catch_df <- select(selected_responses, all_of(catch_cols_in_result))

    selected_responses$species_caught <- apply(
      catch_df,
      1,
      function(row) {
        keep <- !is.na(row) & row != 0
        if (!any(keep)) return(NA_character_)
        paste0(names(row)[keep], "=", row[keep], collapse = ", ")
      }
    )
  }

  # 11) Return a stable output (uses any_of so it never errors)
  selected_responses %>%
    select(any_of(c(
      "surveykey",
      "datefished",
      "area",
      "method",
      "totaljuveniles",
      "lodge",
      "guided",
      "checkcrabsprawns",
      "comment",
      "species_caught",
      "reason_of_hit",
      "catch_with_c8"
    )))
}
