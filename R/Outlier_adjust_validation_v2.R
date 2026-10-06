
#' function to setup the code and variables for validating the adjustment data
#'
#' @export
setupOAV <- function () {
  # ---- Load species/method matrix ----

data_path <- system.file("extdata", "species_method_matrix.csv", package = "iRECUIpkg")

species_method_matrix <<- read_csv(data_path,
  col_types = cols(.default = "c")
)

allowed_methods <<- species_method_matrix %>%
  pivot_longer(
    cols = starts_with("method"),
    names_to = "method_col",
    values_to = "method"
  ) %>%
  filter(!is.na(method) & method != "") %>%
  group_by(Column) %>%
  summarise(Allowed = list(unique(tolower(method))), .groups = "drop")

allowed_method_list <<- setNames(allowed_methods$Allowed, allowed_methods$Column)
data_path <- system.file("extdata", "area_adj.csv", package = "iRECUIpkg")
valid_areas <<- read_csv(data_path, col_types = cols(.default = "c")) %>%
  pull(area) %>%
  unique()
}

#' ---- Helper: standardize old crab/prawn columns ----
#' Function to standardize the crab/prawn fields over time
standardize_crabprawn <- function(df) {

  # If already in new format, keep as is
  if ("checkcrabsprawns" %in% names(df)) {
    return(df)
  }

  # If old format exists, convert to new format
  if (all(c("checkcrabs", "checkprawns") %in% names(df))) {

    df <- df %>%
      mutate(
        checkcrabs  = trimws(as.character(checkcrabs)),
        checkprawns = trimws(as.character(checkprawns)),
        checkcrabs_lc  = tolower(checkcrabs),
        checkprawns_lc = tolower(checkprawns),
        checkcrabsprawns = case_when(
          checkcrabs_lc == "yes" & checkprawns_lc == "yes" ~ "Crab and prawn/shrimp",
          checkcrabs_lc == "yes" & (is.na(checkprawns_lc) | checkprawns_lc == "" | checkprawns_lc == "no") ~ "Crab",
          checkprawns_lc == "yes" & (is.na(checkcrabs_lc) | checkcrabs_lc == "" | checkcrabs_lc == "no") ~ "Prawn/shrimp",
          TRUE ~ "Unspecified"
        )
      ) %>%
      select(-checkcrabs_lc, -checkprawns_lc)
  }

  return(df)
}

#' ---- adjustment Validation function ----
#'@export
validate_adjustments <- function(adjusts, excludes = NULL, excel_file = NULL) {

  # Standardize old/new crab-prawn structure first
  adjusts <- standardize_crabprawn(adjusts)

  required_cols <- c(
    "lodge", "guided", "method", "checkcrabsprawns",
    "did_not_fish", "effort_days", "juv_effort_days",
    "datefished", "area", "surveykey"
  )

  missing <- setdiff(required_cols, names(adjusts))
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }

  problem_rows <- list()

  # ------------------- RULE 0 -------------------
  if (!is.null(excludes)) {

    norm_key <- function(x) {
      x <- as.character(x)
      x <- tolower(x)
      x <- gsub("\u00A0", "", x, fixed = TRUE)
      x <- gsub("[[:cntrl:]]+", "", x)
      x <- gsub("[[:space:]]+", "", x)
      x
    }

    ex_keys <- excludes %>%
      transmute(key = norm_key(survey_access_key)) %>%
      filter(!is.na(key) & key != "") %>%
      distinct()

    bad_rows0 <- adjusts %>%
      mutate(
        rowid = row_number(),
        key = norm_key(surveykey)
      ) %>%
      semi_join(ex_keys, by = "key") %>%
      mutate(Reason = paste0("surveykey appears in excludes (row ", rowid, ")")) %>%
      select(-key)

    if (nrow(bad_rows0) > 0) {
      problem_rows[["Surveykey_InExcludes"]] <- bad_rows0
    }
  }

  # ------------------------------------------------------
  all_dnf_yes <- adjusts %>%
    mutate(dnf_lc = tolower(trimws(did_not_fish))) %>%
    summarise(all_yes = n() > 0 && all(dnf_lc == "yes")) %>%
    pull(all_yes)

  # ------------------- RULES 1-4 and 6-8 -------------------
  if (!all_dnf_yes) {

    # ------------------- RULE 1 -------------------
    bad_rows1_yes <- adjusts %>%
      mutate(
        rowid = row_number(),
        method_lc = tolower(trimws(method))
      ) %>%
      filter((lodge == "Yes" | guided == "Yes") & method_lc != "angleboat")

    if (nrow(bad_rows1_yes) > 0) {
      bad_rows1_yes$Reason <- "Lodge/Guided = Yes but Method is not angleboat"
      problem_rows[["Lodge_Guided_Yes"]] <- bad_rows1_yes
    }

    bad_rows1_no <- adjusts %>%
      mutate(
        rowid = row_number(),
        method_lc = tolower(trimws(method))
      ) %>%
      filter((lodge == "No" | guided == "No") & method_lc != "angleboat")

    if (nrow(bad_rows1_no) > 0) {
      bad_rows1_no$Reason <- "Lodge/Guided = No but Method is not angleboat"
      problem_rows[["Lodge_Guided_No"]] <- bad_rows1_no
    }

    # ------------------- RULE 2 -------------------
    bad_rows2_nontrap <- adjusts %>%
      mutate(rowid = row_number()) %>%
      filter(
        tolower(method) != "trapboat" &
          !is.na(checkcrabsprawns) &
          checkcrabsprawns != "Unspecified"
      )

    if (nrow(bad_rows2_nontrap) > 0) {
      bad_rows2_nontrap$Reason <- "checkcrabsprawns must be 'Unspecified' for non trapboat methods"
      problem_rows[["CrabPrawn_NonTrap"]] <- bad_rows2_nontrap
    }

    cutoff_date <- as.Date("2018-04-01")

    valid_trapboat_crabprawn <- c(
      "Crab",
      "Prawn/shrimp",
      "Crab and prawn/shrimp"
    )

    bad_rows2_trap <- adjusts %>%
      mutate(
        rowid = row_number(),
        date_parsed = as.Date(datefished, format = "%m/%d/%Y")
      ) %>%
      filter(
        tolower(method) == "trapboat" &
          !is.na(checkcrabsprawns) &
          checkcrabsprawns != "" &
          !is.na(date_parsed) &
          (
            (date_parsed < cutoff_date &
               !(checkcrabsprawns %in% c(valid_trapboat_crabprawn, "Unspecified"))) |
              (date_parsed >= cutoff_date &
                 !(checkcrabsprawns %in% valid_trapboat_crabprawn))
          )
      )

    if (nrow(bad_rows2_trap) > 0) {
      bad_rows2_trap$Reason <- paste0(
        "For method = trapboat, checkcrabsprawns must be one of: ",
        paste(valid_trapboat_crabprawn, collapse = ", "),
        " (before 01 April 2018, 'Unspecified' is also allowed)"
      )
      problem_rows[["CrabPrawn_Trapboat"]] <- bad_rows2_trap
    }

    # ------------------- RULE 3 -------------------
    for (col in names(adjusts)) {
      if (col %in% names(allowed_method_list)) {
        non_empty_rows <- which(
          adjusts[[col]] != "" &
            !is.na(adjusts[[col]]) &
            suppressWarnings(as.numeric(adjusts[[col]]) > 0)
        )

        if (length(non_empty_rows) > 0) {
          bad_rows <- non_empty_rows[
            !(adjusts$method[non_empty_rows] %in% allowed_method_list[[col]])
          ]

          if (length(bad_rows) > 0) {
            df_bad <- adjusts[bad_rows, ]
            df_bad$rowid <- bad_rows
            df_bad$SpeciesColumn <- col
            df_bad$Reason <- paste("Method not allowed for", col)
            problem_rows[[paste0("Species_", col)]] <- df_bad
          }
        }
      }
    }

    # ------------------- RULE 4 -------------------
    area_exceptions <- tibble::tribble(
      ~base_area, ~cutoff,
      "A020",     as.Date("2020-04-01"),
      "A019",     as.Date("2014-04-01"),
      "A023",     as.Date("2014-04-01"),
      "A029",     as.Date("2014-04-01"),
      "A002",     as.Date("2014-04-01")
    )

    bad_rows4 <- adjusts %>%
      mutate(
        rowid = row_number(),
        date_parsed = as.Date(datefished, format = "%m/%d/%Y")
      ) %>%
      left_join(area_exceptions, by = c("area" = "base_area")) %>%
      filter(
        !is.na(area) & area != "" &
          !(area %in% valid_areas) &
          !(!is.na(cutoff) & !is.na(date_parsed) & date_parsed < cutoff)
      )

    if (nrow(bad_rows4) > 0) {
      bad_rows4$Reason <- paste(
        "Area value not in valid area list.",
        "Exception: A020 allowed before 01 April 2020; A019, A023, A029, A002 allowed before 01 April 2014."
      )
      problem_rows[["Area_Check"]] <- bad_rows4
    }

    # ------------------- RULE 6 -------------------
    valid_methods_exact <- c("angleboat", "angleshore", "beach", "trapboat", "trapshore", "dive")

    bad_rows6 <- adjusts %>%
      mutate(rowid = row_number()) %>%
      filter(!is.na(method) & method != "" & !(method %in% valid_methods_exact))

    if (nrow(bad_rows6) > 0) {
      bad_rows6$Reason <- paste0(
        "Invalid method value: '", bad_rows6$method,
        "'. Must exactly match one of: ",
        paste(valid_methods_exact, collapse = ", ")
      )
      problem_rows[["Method_CaseMismatch"]] <- bad_rows6
    }

    # ------------------- RULE 7 -------------------
    adjusts_dates <- adjusts %>%
      mutate(
        rowid      = row_number(),
        date_clean = trimws(datefished),
        date_only  = sub(" .*", "", date_clean),
        df_date    = suppressWarnings(as.Date(date_only, format = "%m/%d/%Y"))
      )

    dates_with_parts <- adjusts_dates %>%
      filter(!is.na(df_date)) %>%
      mutate(
        df_year  = format(df_date, "%Y"),
        df_month = format(df_date, "%m")
      )

    if (nrow(dates_with_parts) > 0) {

      year_counts <- dates_with_parts %>%
        count(df_year, name = "n") %>%
        arrange(desc(n))

      majority_year <- year_counts$df_year[1]

      bad_year <- dates_with_parts %>%
        filter(df_year != majority_year)

      if (nrow(bad_year) > 0) {
        bad_year$Reason <- paste0(
          "datefished must be in the majority year (", majority_year, ")"
        )
        problem_rows[["Date_YearMismatch"]] <- bad_year
      }

      majority_year_subset <- dates_with_parts %>%
        filter(df_year == majority_year)

      month_counts <- majority_year_subset %>%
        count(df_month, name = "n") %>%
        arrange(desc(n))

      majority_month <- month_counts$df_month[1]

      bad_month <- majority_year_subset %>%
        filter(df_month != majority_month)

      if (nrow(bad_month) > 0) {
        month_label <- format(as.Date(paste0("2025-", majority_month, "-01")), "%B")
        bad_month$Reason <- paste0(
          "datefished must be in the majority month (", month_label,
          " ", majority_year, ")"
        )
        problem_rows[["Date_MonthMismatch"]] <- bad_month
      }
    }

    # ------------------- RULE 8 -------------------
    bad_rows8 <- adjusts_dates %>%
      mutate(
        date_clean = trimws(datefished),
        date_only  = sub(" .*", "", date_clean),
        day_raw = sapply(
          strsplit(date_only, "/", fixed = TRUE),
          function(x) if (length(x) >= 2) x[[2]] else NA_character_
        ),
        day_val = suppressWarnings(as.integer(day_raw))
      ) %>%
      filter(
        !is.na(date_clean) & date_clean != "" &
          (is.na(day_val) | day_val < 1 | day_val > 31)
      )

    if (nrow(bad_rows8) > 0) {
      bad_rows8$Reason <- "datefished day must be between 1 and 31 or date format is wrong"
      problem_rows[["Date_DayOutOfRange"]] <- bad_rows8
    }
  }

  # ------------------- RULE 5 -------------------
  adjusts_dnf <- adjusts %>%
    mutate(
      rowid = row_number(),
      dnf_raw = trimws(did_not_fish),
      dnf_lc  = tolower(dnf_raw)
    )

  bad_rows5_inconsistent <- adjusts_dnf %>%
    group_by(surveykey) %>%
    filter(n_distinct(dnf_lc[dnf_lc %in% c("yes", "no")]) > 1) %>%
    ungroup()

  if (nrow(bad_rows5_inconsistent) > 0) {
    bad_rows5_inconsistent$Reason <- "did_not_fish must be consistent (all yes or all no) within the same surveykey"
    problem_rows[["DidNotFish_Inconsistent"]] <- bad_rows5_inconsistent
  }

  bad_rows5_invalid <- adjusts_dnf %>%
    filter(
      !is.na(dnf_raw) &
        dnf_raw != "" &
        !(dnf_raw %in% c("yes", "no"))
    )

  if (nrow(bad_rows5_invalid) > 0) {
    bad_rows5_invalid$Reason <- "did_not_fish must be lower case yes or no"
    problem_rows[["DidNotFish_InvalidCase"]] <- bad_rows5_invalid
  }

  bad_rows5C_no <- adjusts_dnf %>%
    filter(
      dnf_lc == "no" &
        (
          is.na(effort_days) | effort_days != 1 |
            is.na(datefished) |
            is.na(area) |
            is.na(method) | method == "" |
            is.na(lodge) | lodge == "" |
            is.na(guided) | guided == "" |
            is.na(checkcrabsprawns) | checkcrabsprawns == ""
        )
    )

  if (nrow(bad_rows5C_no) > 0) {
    bad_rows5C_no$Reason <- paste(
      "When did_not_fish is 'no', effort_days must be 1 and",
      "datefished, area, method, lodge, guide, and checkcrabsprawns must be non blank"
    )
    problem_rows[["DidNotFish_No_Inconsistent"]] <- bad_rows5C_no
  }

  bad_rows5C_yes <- adjusts_dnf %>%
    filter(
      dnf_lc == "yes" &
        (
          is.na(effort_days) | effort_days != 0 |
            is.na(juv_effort_days) | juv_effort_days != 0 |
            !is.na(datefished) |
            !is.na(area) |
            (!is.na(method) & method != "") |
            (!is.na(lodge) & lodge != "Unspecified") |
            (!is.na(guided) & guided != "Unspecified") |
            (!is.na(checkcrabsprawns) & checkcrabsprawns != "Unspecified")
        )
    )

  if (nrow(bad_rows5C_yes) > 0) {
    bad_rows5C_yes$Reason <- paste(
      "When did_not_fish is 'yes': effort_days and juv_effort must be 0,",
      "datefished and area must be NA, method must be blank,",
      "and lodge, guide and checkcrabsprawns must be 'Unspecified'"
    )
    problem_rows[["DidNotFish_Yes_Inconsistent"]] <- bad_rows5C_yes
  }

  juv_pos <- which(names(adjusts_dnf) == "juv_effort_days")

  if (length(juv_pos) == 1 && juv_pos < ncol(adjusts_dnf)) {

    skip_cols <- c("Comment", "DFO comment", "rowid", "dnf_raw", "dnf_lc", "checkcrabsprawns")

    catch_cols <- setdiff(
      names(adjusts_dnf)[(juv_pos + 1):ncol(adjusts_dnf)],
      intersect(names(adjusts_dnf), skip_cols)
    )

    bad_rows5D <- adjusts_dnf %>%
      filter(dnf_lc == "yes") %>%
      mutate(rowid = row_number()) %>%
      rowwise() %>%
      mutate(
        NonZero_Catch_Columns = paste(
          catch_cols[sapply(catch_cols, function(x) {
            val <- get(x)
            !is.na(val) && val != 0
          })],
          collapse = ", "
        )
      ) %>%
      ungroup() %>%
      filter(NonZero_Catch_Columns != "")

    if (nrow(bad_rows5D) > 0) {
      bad_rows5D$Reason <- "When did_not_fish is 'yes', all catch fields after juv_effort_days must be zero"
      problem_rows[["Catch_NotZero_DNFYes"]] <- bad_rows5D
    }
  }

  problems_df <- if (length(problem_rows) > 0) {
    bind_rows(problem_rows, .id = "Rule")
  } else {
    NULL
  }

  adjusts <- adjusts %>%
    select(-any_of(c(
      "dnf_raw", "dnf_lc",
      "date_clean", "date_only", "df_date",
      "day_raw", "day_val"
    )))

  if (!is.null(problems_df)) {
    problems_df <- problems_df %>%
      select(-any_of(c(
        "dnf_raw", "dnf_lc",
        "date_clean", "date_only", "df_date",
        "df_month", "df_year",
        "day_raw", "day_val"
      )))
  }

  result <- list(
    validated = adjusts,
    problems  = problems_df
  )

  return(result)
}
