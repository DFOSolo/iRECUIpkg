# load some extra code
#source("R/CommonCode/UniformSav.R")
#source("R/outlierCode/adjust_validation_v2.R")

#' function to process the iREC sav file as delivered by th econtractor
#' parameters are:
#' SavDat = Name (and path?) to the .sav file
#' xlsFile = Name of the output XLSX file - THis might be a bit of legacy?
#' HABAG = the Daily Halibut Bag limit if not using FRIS directly
#' CNBAG = the Daily Chinook Bag limit if not using FRIS directly
#' COBAG = the Daily COHO Bag limit if not using FRIS directly
#' ENCBAG = the Daily Encounter Bag limit used for all three species
#' ANN_CN = the Annual limit for retained Chinook
#' ANN_LC = the Annual limit for retained Lingcod
#' ANN_HA = the Annual limit for retained Halibut
#' excludes = the dataframe holding the exclude data
#' adjusts = the dataframe holding the adjusts data
#' areaout_returns = Boolean flag to return all repeating area patters, or just the summer excludes
#' FRIS_USE = Boolean flag to pull bag limits data from FRIS or use defaults
#' FRIS_GRACE = integer to add to the FRIS daily limits before flagging the record
#' @export
processSav <- function(SavDat, xlsFile, HABAG, CNBAG, COBAG, ENCBAG, ANN_CN, ANN_LC, ANN_HA, excludes,
                       adjusts, areaout_returns, FRIS_USE, FRIS_GRACE){
  data_path <- system.file("extdata", "areas.csv", package = "iRECUIpkg")
  areas_df <- read.csv(data_path) %>%
    select_all(tolower)
  auto_notify("Areas")
  data_path <- system.file("extdata", "neighbours.csv", package = "iRECUIpkg")
  neighbours_df = read.csv(data_path) %>%
    select_all(tolower)
  auto_notify("Neighbours")

  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {}
    withProgress <- function(...) list(...)[["expr"]]
  }

  withProgress(message = 'Processing ', value = 0, {
    tstps = 8
    stp = 1
    auto_notify(paste0(xlsFile," ", HABAG," ",CNBAG))
  # process the SAV file to the same format as the current system and apply the adjusts and excludes
  SavDat <- LoadSav(SavDat, excludes, adjusts)

  incProgress(1/ tstps, detail = paste("Loading FRIS Data ", stp))
  stp = stp + 1
  # check to ensure we can make a connection to FRIS or not.  Force use fris to false if no connection
  # this should actually try to build the connection as well.
  if (!oracredsL) {FRIS_USE = FALSE}

  # If using Fris pull the applicable daily limits strata
  if (FRIS_USE) {
  fris_d <- GetYearMonth(SavDat)
  fris_yr <- year(fris_d)
  fris_mn <- month(fris_d)
  data_path <- system.file("extdata", "FRIS_Limits.txt", package = "iRECUIpkg")
  fris_qry <- read_file(data_path)
  fris_qry <- gsub(':Year',fris_yr, fris_qry)
  fris_qry <- gsub(':Month',fris_mn, fris_qry)
  fris_qry <- gsub(':Day','15', fris_qry)
  fris_qry <- paste0(fris_qry, "  select * from sum3
order by cast(pfma as number), species")
  CCon <- setupOracleConn("L")
  fris_data <- getdata(CCon, fris_qry)
  fris_data <- fris_data %>%
    left_join(areas_df, join_by("AREA_NAME"=="area_name"))
  } else {
    # if not using FRIS, pull some bogus data that won't link so the process is easier to flow.  Kind of a hack.
    data_path <- system.file("extdata", "fris_data.csv", package = "iRECUIpkg")
    fris_data <- read.csv(data_path)
  }
  # incProgress(1/ tstps, detail = paste("Loading Lookup Tables ", stp))
  # stp = stp + 1

  incProgress(1 / tstps, detail = paste("Checking comment profanity ", stp))
  cmnts <- CheckProfanity(SavDat)
  stp = stp + 1

  incProgress(1 / tstps, detail = paste("Adjusting Dates ", stp))
  SavDat <- fixDates(SavDat)

  responses <- pivot_longer(SavDat,cols =! c("tid","surveykey"),names_to="variable", values_to="value")
  responses <- responses %>%
    dplyr::rename(
      "val" = value,
      "item" = variable
    )
  stp = stp + 1

  incProgress(1/ tstps, detail = paste("Building Catch Table", stp))
  stp = stp + 1

  # pull the records where 'area' is the item
  areas <- responses[responses$item == 'area' & responses$val != "" & !is.na(responses$val),]
  # join those records with the ref data names/ids
  areas <- areas %>% left_join (areas_df, by=c("val" = "area_name"))
  # drop unnecessary fields
  areas <- areas[, !(names(areas) %in% c("item","val"))]

  r_B <- SavDat[c('tid','datefished','juv_effort_days','comment')]
  # purge any "did not fish" type records with no date
  r_B <- r_B %>% drop_na(datefished)
  # add number of anglers field (1 + juveniles)
  r_B$anglers <- as.integer(r_B$juv_effort_days) + 1
  # join back with areas fished(?)
  resp <- r_B %>% inner_join(areas,by=c("tid"))
  # drop some vestigial fields
  resp <- resp[, !(names(resp) %in% c("item","val"))]

  catch <- BuildCatch(responses, resp)
  incProgress(1 / tstps, detail = paste("Checking Catch Outliers ", stp))

  df_ctchOut <- catchOutlier(catch, areas_df, HABAG, CNBAG, COBAG, ENCBAG, fris_data, FRIS_GRACE)
  stp = stp + 1
  incProgress(1 / tstps, detail = paste("Checking Annual Outliers ", stp))
  df_annOut <- annualOutlier(catch, ANN_CN, ANN_LC, ANN_HA)
  stp = stp + 1
  nn <- LoadNeighbours(neighbours_df,areas_df)
  incProgress(1 / tstps, detail = paste("Checking area Ourliers ", stp))
  df_areaOut_D <- areaOutlier(SavDat, areas_df, nn, areaout_returns)
  stp = stp + 1
  auto_notify(paste0(xlsFile,' \n'))
  # package up all the pieces to return
  rtn_list <- list("catch" = df_ctchOut, "annual" = df_annOut, "areaD" = df_areaOut_D, "responses" = SavDat, "comments" = cmnts, "bookname" = xlsFile, "fris" = fris_data)
  return(rtn_list)
  })
}

#' function to build an XLSX file from the output of the function processSav and crazy8s
#' parameters are:
#' n_dfs = list of dataframes returned from processSav
#' flagged_df = dataframe returned from crazy8's code
#' @export
buildOutlierXLS <- function(n_dfs,flagged_df){
  auto_notify("create Work Book")
  wrkbk = openxlsx::createWorkbook() #type = 'xlsx'
  auto_notify("Catch Outliers")
  auto_notify("Create Sheet Catch Outliers")
  openxlsx::addWorksheet(wrkbk,"Catch Outliers")
  openxlsx::writeData(wrkbk, "Catch Outliers", n_dfs$catch, startCol = 1, rowNames = FALSE)
  auto_notify("add data frame Catch outliers")

  auto_notify("Annual Outliers")
  auto_notify("Create Sheet Annual Outliers")

  openxlsx::addWorksheet(wrkbk,"Annual Outliers")
  openxlsx::writeData(wrkbk, "Annual Outliers", n_dfs$annual, startCol = 1, rowNames = FALSE)
  auto_notify("add data frame Annual outliers")


  auto_notify("Area Outliers")

  openxlsx::addWorksheet(wrkbk,"Area Outliers")
  auto_notify("Create Sheet Area Outliers")
  openxlsx::writeData(wrkbk, "Area Outliers", n_dfs$area, startCol = 1, rowNames = FALSE)
  auto_notify("add data frame Area outliers")

  auto_notify("comments")
  openxlsx::addWorksheet(wrkbk,"comments")
  auto_notify("Create Sheet comments")
  openxlsx::writeData(wrkbk, "comments", n_dfs$comments, startCol = 1, rowNames = FALSE)
  auto_notify("add data frame comments")

  openxlsx::addWorksheet(wrkbk,"Crazy 8s")
  openxlsx::writeData(wrkbk, "Crazy 8s", flagged_df, startCol = 1, rowNames = FALSE)

  openxlsx::addWorksheet(wrkbk,"FRIS Limits")
  openxlsx::writeData(wrkbk, "FRIS Limits", n_dfs$fris, startCol = 1, rowNames = FALSE)

  auto_notify(n_dfs$xlsFile)
  return(wrkbk)
}

#' function to check for multiple areas fished and their likelihood based on a predefined "neighbours" table
#' parameters are:
#' SavDat = Dataframe of response data, processed for format and adjusts/excludes
#' areas_df = dataframe for to map iREC reponse areas to REF data areas using CREST mappings
#' nn = dataframe of acceptable area combinations for same day fishing.
#' areaout_returns = boolena flag to return all repeating areas fished patters or not
areaOutlier <- function(SavDat, areas_df, nn, areaout_returns){

  dte <- GetYearMonth(SavDat)
  mnth <- tolower(month.name[month(dte)])

  # pull needed columns just to make things easier to read while debugging
  a_data <- SavDat[c('surveykey','method', 'area','datefished','comment', 'lodge', 'guided')]

  # summarize the data to to count the number of areas fished by surveykey
  a_mults <- a_data %>%
    group_by(surveykey, datefished) %>%
    summarize(
      areas = n(),
      .groups = 'drop'
    )
  # pull only the records with more than 1 area fished in a day
  a_mults <- subset(a_mults, areas > 1)

  # pull the rows from the simplified data set that were identifed as having more than 1 area fished in a day and then count them.
  a_t1 <- subset(a_data, surveykey %in% a_mults$surveykey) %>%
    group_by(surveykey, datefished) %>%
    summarize(
      first_area = min(area),
      areas = n(),
      .groups = 'drop'
    )
  # pull only the rows where ore than 1 area fish in a day
  a_t1 <- subset(a_t1, areas > 1)
  # join the summarized data with the simplifed dataset. Check if the multiple areas are different.
  a_t2 <- a_t1 %>% left_join(subset(a_data[c("surveykey","datefished", "area","comment", 'lodge', 'guided')], !is.na(area)), join_by("surveykey"=="surveykey","datefished"=="datefished","first_area" < "area"))
  # drop any records with a null area and create a yes/no column for enhanced Yes is if either guided or lodge is a Yes otherwise it is No
  a_t2 <- subset(a_t2, !is.na(area)) %>%
    mutate(enhanced = if_else(lodge == 'Yes' | guided == 'Yes', "Yes", "No"))
  # join the areas table to the first_area column
  nb <- a_t2 %>% left_join(areas_df, join_by("first_area"=="area_name"))
  # join the areas table to the other area column
  # this gets a "pretty" area name
  nb <- nb %>% left_join(areas_df, join_by("area"=="area_name"))
  # join the neighbour table
  nb <- nb %>% left_join(nn, join_by("lrg_area_id_id.x"=="lrg_area_id", "lrg_area_id_id.y"=="lrg_area_id_id"))

  # if the "other" area is a blank then it is not in the neighbours table an is not a neighbour.
  # also checks if it is in the neighbour table, but not for all months then mark it as not a neighbour.
  nb <- nb %>%
    mutate(regular_neighbour = if_else(is.na(lrg_area_neighbour),"IS NOT" , "Is")) %>%
    mutate(regular_neighbour = if_else(rowSums(nb[tolower(c(month.name))], na.rm = TRUE) < 12, "IS NOT", "Is"))
  # recheck for areas that are month specific and if the current month is = 1 then the area is a neighbour
  # any blank summer_neighbours are automatically marke not neighbours
  nb <- nb %>%
    mutate(summer_neighbour = if_else(rowSums(nb[tolower(c(month.name))], na.rm = TRUE) < 12 & !!sym(mnth) == 1,"Is" , "IS NOT")) %>%
    mutate(summer_neighbour = if_else(is.na(summer_neighbour),"IS NOT", summer_neighbour))

  nb <- nb %>%
    mutate(regular_neighbour = if_else(summer_neighbour == "Is","Summer" , regular_neighbour))

  # if the day was marked as enhanced fishing and the neighbour was month specific flag it
  # make sure if the neighbour was regular, or not month specific then mark it as a neighbour.
  nb <- nb %>%
    mutate(lodge_recheck = if_else(!!sym(mnth) == 1 & enhanced == 'Yes', "Summer but lodge/guided",NA) )%>%
    mutate(lodge_recheck = if_else(enhanced == 'No' & !is.na(lodge_recheck), NA, lodge_recheck) )%>%
    mutate(lodge_recheck = if_else(regular_neighbour == 'Is' | is.na(regular_neighbour), NA, lodge_recheck) )
  # rename the other area column
  nb <- nb %>% dplyr::rename("Other_area"="area")

  # Exemptions only for summer
  # Show the area combination if the same fisher reports those two areas on multiple days.
  # Show the area combination if, on the same day, both areas have a “Yes” response to Guide and/or Lodge.

  # check for multiple areas repeating on different days.
  nb2 <- nb %>% group_by(surveykey, first_area, Other_area) %>% summarize(dups = n(), .groups = 'drop')
  #join that summary back to the neighbour check table and flag repeating areas
  nb3 <- nb %>% left_join(nb2, join_by("surveykey", "first_area", "Other_area"))%>%
    mutate(repeat_areas = if_else(dups > 1, "Repeat" , NA))
  # pull the useful columns
  nb3 <- nb3[c("surveykey","datefished","areas","first_area","Other_area","regular_neighbour","summer_neighbour", "lodge_recheck", "repeat_areas", "comment", 'enhanced', 'lodge', 'guided')]

  nb3 <- as.data.frame(nb3)

  if(areaout_returns){
    df_areaOut_D <- subset(nb3, (regular_neighbour != "Is" & regular_neighbour != "Summer" & summer_neighbour == "IS NOT") | !is.na(lodge_recheck) | !is.na(repeat_areas))
  } else {
    df_areaOut_D <- subset(nb3, (regular_neighbour != "Is" & regular_neighbour != "Summer" & summer_neighbour == "IS NOT") | !is.na(lodge_recheck)| ( !is.na(repeat_areas) & summer_neighbour == "Is"))
  }

  return(df_areaOut_D)
}

#' function to check if annual limits are exceeded by angler over the month.
#' parameters are:
#' catch = responses where Chinook, Coho, Lingcod and Halibut were encountered
#' ANN_CN = Annual limit for Chinook based on the UI values provided
#' ANN_LC = Annual limit for Lingcod based on the UI values provided
#' ANN_HA = Annual limit for Halibut based on the UI values provided
annualOutlier <- function(catch, ANN_CN, ANN_LC, ANN_HA) {
  #summarize the kept catches based on surveykey (Licence holder) and compare to the limits passed as parameters
  df_annOut <- subset(catch, disposition == "Kept" & item %in% c("Chinook", "Lingcod", "Halibut")) %>%
    group_by(surveykey, item, comment, name_email, anglers) %>%
    summarize(
      catch = sum(as.integer(val_angler)),
      .groups = "drop"
    ) %>%
    mutate(limit = dplyr::case_when(
      item == "Chinook" ~ ANN_CN,
      item == "Lingcod" ~ ANN_LC,
      item == "Halibut" ~ ANN_HA,
      TRUE ~ NA_real_
    )) %>%
    dplyr::filter(!is.na(limit) & catch > limit)

  df_annOut <- as.data.frame(df_annOut)
  return(df_annOut)
}

#' function to check for reported kept catch in excess of daily bag limits as well as reported large total encounters (kept + released)
#' parameters are:
#' catch = responses where Chinook, Coho, Lingcod and Halibut were encountered
#' areas_df = dataframe for to map iREC responses areas to REF data areas using CREST mappings
#' HABAG = Halibut daily bag limit based on the UI values provided
#' CNBAG = Chinook daily bag limit based on the UI values provided
#' COBAG = Coho daily bag limit based on the UI values provided
#' ENCBAG = Kept + Released daily limit based on UI values provided used for all 4 species
#' fris_data = dataframe of bag limits by month, area and species - used in place of UI limits
#' FRIS_GRACE = integer number of fish in excess of FRIS limits to allow.
catchOutlier <- function(catch, areas_df, HABAG, CNBAG, COBAG, ENCBAG, fris_data, FRIS_GRACE) {

  catch <- catch %>% left_join(fris_data[!is.na(fris_data$area_id),], by=c("area_id"="area_id","item" = "SPECIES", "lrg_area_id_id" = "lrg_area_id_id"))

  # determine multiple area names where catch occured
  CatchAreas <- catch[!is.na(catch$area_id) & catch$val > 0,c("surveykey", "datefished", "area_id")] %>%
    group_by(surveykey, datefished, area_id) %>%
    summarize(.groups = "drop_last") %>%
    left_join(areas_df[c("area_id","area_name")], by=c("area_id"))  %>%
    group_by(surveykey, datefished) %>%
    summarise(area_name = paste(area_name, collapse = ", "),  .groups = "drop_last")

  if (nrow(fris_data[!is.na(fris_data$area_id),]) == 0) {
    FRIS_USE <- FALSE
  } else {
    FRIS_USE <- TRUE
  }

  # Summarize catch by licence, date, item and number of anglers. Sums up over method
  # if multiple areas were fished, it picks the highest (max) of the possible daily limits
  sum1a <- catch %>%
    group_by(surveykey, datefished, anglers, item, Total_Kept, Total_Released, comment, name_email) %>%
    summarize(
      kept = sum(case_when(disposition == 'Kept' ~ as.integer(val), TRUE ~ 0)),
      released = sum(case_when(disposition == 'Released' ~ as.integer(val), TRUE ~ 0)),
      enc = sum(as.integer(val)),
      DAILY_LIMIT_PC = max(DAILY_LIMIT_PC),
      .groups = 'drop'
    )
  # joins the summary back to the catch table to get the number of records.
  # first counts the records with catch
  catchCnt <- catch[catch$val > 0, c('surveykey', 'datefished', 'anglers', 'item', 'comment', 'name_email')] %>%
    group_by(surveykey, datefished, anglers, item, comment, name_email) %>%
    summarize(
      catch_records=n(),
      .groups = 'drop')
  # then joins that to the catch summary table
  sum1 <- sum1a %>% left_join(catchCnt,
                              by = c("surveykey", "datefished", "anglers", "item", "comment", "name_email"))



  if (!FRIS_USE) {
    # if not using FRIS data, check for excess catch based on UI values
    # drop the daily limit field as it is both empty and irrelevant
    sum1b <- sum1[, !(names(sum1) %in% c("DAILY_LIMIT_PC"))]
    # set the flags based on kept per angler over the UI set bag limits
    sum2 <- sum1b %>%
      mutate(
        # Kept catch
        fk = case_when (
          item == 'Chinook' & kept / anglers >= CNBAG ~ 'K',
          item == 'Halibut' &
            kept / anglers >= HABAG ~ 'K',
          item == 'Coho' &
            kept / anglers >= COBAG ~ 'K',
          TRUE ~ ''
        ),
        # releases checked against encounter limits There are no specific release limits
        fr = case_when (
          item == 'Chinook' & released / anglers >= ENCBAG ~ 'R',
          item == 'Halibut' &
            released / anglers >= ENCBAG ~ 'R',
          item == 'Coho' &
            released / anglers >= ENCBAG ~ 'R',
          TRUE ~ ''
        ),
        # encounters checked against encounter limits
        fc = case_when (
          item == 'Chinook' & enc / anglers >= ENCBAG ~ 'C',
          item == 'Halibut' & enc / anglers >= ENCBAG ~ 'C',
          item == 'Coho' & enc / anglers >= ENCBAG ~ 'C',
          TRUE ~ ''
        )
      )

  } else {
    # using FRIS data for daily bag limits
    sum2 <- sum1 %>%
      # kept catch vs bag limit + grace
      mutate(
        fk = case_when (
          item == 'Chinook' &
            kept / anglers > DAILY_LIMIT_PC + FRIS_GRACE ~ 'K',
          item == 'Halibut' &
            kept / anglers > DAILY_LIMIT_PC + FRIS_GRACE ~ 'K',
          item == 'Coho' &
            kept / anglers > DAILY_LIMIT_PC + FRIS_GRACE ~ 'K',
          TRUE ~ ''
        ),
        # released catch vs UI encounter limit
        fr = case_when (
          item == 'Chinook' & released / anglers >= ENCBAG ~ 'R',
          item == 'Halibut' &
            released / anglers >= ENCBAG ~ 'R',
          item == 'Coho' &
            released / anglers >= ENCBAG ~ 'R',
          TRUE ~ ''
        ),
        # kept + released catch vs UI encounter limit
        fc = case_when (
          item == 'Chinook' & enc / anglers >= ENCBAG ~ 'C',
          item == 'Halibut' & enc / anglers >= ENCBAG ~ 'C',
          item == 'Coho' & enc / anglers >= ENCBAG ~ 'C',
          TRUE ~ ''
        )
      )
    names(sum2) <- tolower(names(sum2))
    # pull records with excess catch
    sum <- subset(sum2, paste0(sum2$fk, sum2$fr, sum2$fc) != '')


  }

  # pull records where catch was exceeded and join ref data area names
  sum <- subset(sum2, paste0(sum2$fk, sum2$fr, sum2$fc) != '') %>% left_join(CatchAreas, by=c("surveykey", "datefished"))

  df_ctchOut <- as.data.frame(sum)
  return(df_ctchOut)
}

#' function to see if the date is a date, or if it is an integer representing the number of days since
#' January 1, 1970 - I think this is from old format data
#' parameters are:
#' SavDat = response file with item = "datefished" as the date field to check/correct
fixDates <- function(SavDat){
  d <- try(as.Date(subset(SavDat, !is.na(datefished))$datefished[[1]], format="%Y-%m-%d"))
  if("try-error" %in% class(d) || is.na(d)) {
    auto_notify("Date wasn't correct!")
    # if date is number of days, make it into a real date.
    SavDat$datefished <- format(as.Date('1970-01-01', '%Y-%m-%d') + as.integer(SavDat$datefished), '%Y-%m-%d')
  } else {
    auto_notify("Dates Already Good!")
  }
  return(SavDat)

}

#' Function to load the table of areas fished in a sigle day that are not flagged.
#' figuratively which areas "neighbours", but in practice, there may be gaps between
#' parameters are:
#' neighbours_df = dataframe loaded from csv of each area and a csv list of acceptable "neighbours"
#' areas_df = dataframe of iREC areas and their ref data names
LoadNeighbours <- function(neighbours_df,areas_df){
  neighbours_df <- as.data.frame(neighbours_df)
  areas_df <- as.data.frame(areas_df)
  # split csv neighbours field and un-pivot data
  nn <- neighbours_df %>%
    mutate(neighbours = strsplit(as.character(neighbours), ",")) %>%
    unnest(neighbours)
  # make the neighbour id an integer to link on
  nn$neighbours <- as.integer(nn$neighbours)
  # join with ref data area names
  nn <- nn %>% left_join(neighbours_df[,c("lrg_area_id_id","lrg_area_nme")]%>% distinct(lrg_area_id_id, .keep_all = TRUE), by=c("neighbours"="lrg_area_id_id"))
  # clean up field names
  nn <- nn %>%
    dplyr::rename(
      lrg_area_id = "lrg_area_id_id",
      lrg_area_id_id = "neighbours",
      lrg_area_neighbour = "lrg_area_nme.y",
      lrg_area_nme = "lrg_area_nme.x"
    )
  # return the re-organized neighbour data
  return(nn)

}

#' Function to take the raw SAV file eoncvert it to current standards if necessary and apply the excludes and adjusts
#' parameters are:
#' SavDat = the SAV file loaded using haven::read_sav
#' excludes - the csv file containing the licences to exclude
#' adjusts = the csv file containing the adjusted records
LoadSav <- function (SavDat, excludes, adjusts) {
   # check to see if the data is in EKOS format or not then assumed to be PRA format
   # this forces the surveystartdate to April 2012 if it is seems like EKOS
    if ("REPYEAR" %in% names(SavDat)) {
      SurveyStartDate <- as.Date("2012-04-01")
    }
    else {
      # check to see if field Licence_ID exists and rename to surveyKey if it does
      if ("Licence_ID" %in% names(SavDat)){
        SavDat <- SavDat %>%
          dplyr::rename(surveykey = "Licence_ID")
      }
      # set the SurveyStartDate to today
      SurveyStartDate <- as.Date(format(Sys.Date(), "%Y-%m-%d") )
    }
    # call the loadSurveyResults function passing all the responses, adjusts, exlcudes and the bodged surveystartdate
    # LoadSurveyResults is copied from analysis package code,
    SavDat <- loadSurveyResults (SavDat,  SurveyStartDate, excludes, adjusts)
    SavDat <- as.data.frame(SavDat)
    # add row id as the field "tid"
    SavDat <- tibble::rowid_to_column(SavDat, "tid")
    SavDat <- as.data.frame(SavDat)
    # force everything to a character field. Not sure why.
    SavDat <- SavDat %>%
      mutate(across(everything(), as.character))
    # return the converted and adjusted response data
    return(SavDat)
}


#' Function to pull Chinook, Coho, Halibut and lingcod catches out of the normalized response data
#' Parameters are:
#' responses = normalized response data tid, surveykey, item, val
#' resp = the number of anglers (juveniles), areas and dates reported fished
BuildCatch <- function(responses, resp) {

    if (exists('Lcon')){
    items <- getdata(Lcon, "select i.column_name, d.text as disposition, g.text as spp_group
from otolith_V1.creel_irec_item i
left join otolith_V1.creel_irec_disposition d on i.disposition_id_id = d.disposition_id
left join otolith_V1.creel_irec_grouping g on i.grouping_id_id = g.grouping_id")
    data_path <- system.file("extdata", "CRESTIRECITEMS.CSV", package = "iRECUIpkg")
    write_csv(items, file = data_path)
  }
  items <- read.csv(data_path)
  items$item <- tolower(items$COLUMN_NAME)
  respT <- responses %>%
    left_join(items[items$SPP_GROUP %notin% c('BIVALVES', 'EFFORT', 'PRAWNCRAB', 'OTHSHELLFISH') , c('item', 'DISPOSITION')], join_by('item' == 'item')) %>%
    mutate(DISPOSITION = case_when(item == "datefished" &
                                     !is.na(val) ~ "datefished",
                                   TRUE ~ DISPOSITION))

  responsesI <- respT[grepl("^[+-]?[0-9]*\\.?[0-9]+$", respT$val),] %>%
    group_by(tid,surveykey, DISPOSITION) %>%
    summarize(ctch = sum(as.numeric(val), na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = DISPOSITION, values_from = ctch) %>%
    rename("Total_Kept" = 'Kept',
           "Total_Released" = 'Released')

  responsesD <- respT[respT$DISPOSITION == 'datefished',] %>%
    group_by(tid,surveykey, DISPOSITION) %>%
    summarize(datefished = max(val), na.rm = TRUE, .groups = 'drop')

  resp_id <- respT[respT$item %in% c('first_name', 'last_name', 'email'),] %>%
    arrange(tid,surveykey,item) %>%
    group_by(tid, surveykey) %>%
    summarise(name_email = str_flatten(val, collapse = " - "))

  respC <- responsesI[c('tid', 'surveykey', 'Total_Kept', 'Total_Released')] %>%
    left_join(responsesD, by = c('tid', 'surveykey')) %>%
    group_by(surveykey, datefished) %>% summarize(
      Total_Kept = sum(Total_Kept),
      Total_Released = sum(Total_Released),
      .groups = 'drop'
    ) %>%
    left_join(resp, by = c ('surveykey', 'datefished')) %>%
    left_join(resp_id, by = c('tid','surveykey'))

  # extract the species of interest
  catch <- subset(responses, grepl("chinook|halibut_halibut|lingcod|coho",responses$item) )
  # join the normalized catch data to the species encountered
  catch <- catch %>% left_join(respC, by = c("tid","surveykey"))
  # rename some fields so they make better sense
  catch <- catch %>% dplyr::rename(itemS = item)
  # add item field to more simply represent the species rather than the long form iREC response items
  catch <- catch %>%
    mutate(item = case_when(grepl('^.*chinook.*',itemS)  ~ 'Chinook',
                            grepl('^.*coho.*',itemS)  ~ 'Coho',
                            grepl('^.*lingcod.*',itemS)  ~ 'Lingcod',
                            grepl('^.*halibut_halibut.*',itemS)  ~ 'Halibut'
                            , TRUE ~ 'XXYYZZ'
    ))
  # make any null catch values 0
  catch$val[is.na(catch$val)] <- 0
  # calculate catch per angler only for areas where there are daily Lingcod limits
  # PFMA 12, PFMA 13, PFMA 14, PFMA 15, PFMA 16, PFMA 17, PFMA 18, PFMA 19

  catch <- catch %>%
    mutate(
      val_angler = case_when(
        item %in% c('Chinook', 'Halibut', 'Coho')  ~ as.integer(val) / anglers,
        item == 'Lingcod' &
          lrg_area_id_id %in% c(1, 2, 3, 4, 5, 6, 7, 209, 337)  ~ as.integer(val) / anglers,
        item == 'Lingcod' &
          lrg_area_id_id %notin% c(1, 2, 3, 4, 5, 6, 7, 209, 337)  ~ 0
      )
    )


  # tag each record if it is a 'Lingcod area' where daily limits are reduced
  catch <- catch %>%
    mutate(area_type = case_when(grepl('^.*lingcod*',itemS) & lrg_area_id_id %in% c(1, 2, 3, 4, 5, 6, 7, 209, 337)  ~ 'Lingcod areas',
                                 .default = 'All areas'
    ))
  # Determine the catch disposition as either Kept or Released
  catch <- catch %>%
    mutate(disposition = case_when (grepl('^.*_kept', itemS) ~ 'Kept',
                                    grepl('^.*_rele', itemS) ~ 'Released',
                                    .default = itemS
    ))
  return(catch)
}

#' Function to assess the comments for sentement - How angry or happy the comments seem to be
#' Uses the package syuzhet
#' parameters are:
#' SavDat = raw response data
GetSentiment <- function(SavDat) {
  auto_notify("Getting sentiment scores")
  # drop empty comments
  sdat <- SavDat[SavDat$comment != "",]
  # calculate the "sentiment" score.  Larger the number the more pleasent.
  sentiment_scores <- syuzhet::get_sentiment(sdat$comment, method = "syuzhet")
  # calculate the emotions and valence from NRC Dictionary
  # anger,   anticipation, disgust, fear, joy, sadness, surprise, trust, negative, positive
  nrc_data <- syuzhet::get_nrc_sentiment(sdat$comment)
  # package the 2 score sets together to return
  rtn_list <- list("nrc_data" = nrc_data, "sentiment_scores" = sentiment_scores)
  return(rtn_list)
}

#' Function to build a compound word profanity dictionary
#' seems not so nice to leave a list of nasty words for someone to stumble upon
#' No parameters needed
GetCompoundProfanity <- function() {
  auto_notify("Getting extra profanity to check")
  # https://github.com/colinmorris/pejorative-compounds
  # source of words:
  #  "a dataset mapping ~4,800 compound pejoratives to the number of Reddit comments containing that compound"
  # this is the original source of the word table.  Can't hit it too many times before you get blocked, so
  # I downloaded the table and zipped it with a password.
  #  raw_url <- "https://raw.githubusercontent.com/colinmorris/pejorative-compounds/refs/heads/master/counts.csv"
  #  cnts <- read.csv(raw_url)

  # password used in the zip
  password <- "badbadwords"
  # the name of the zip file and the csv file inside
  filebase <- "pejorative-compounds"
  # unzip the file
  data_path <- system.file("extdata", paste0(filebase, ".zip"), package = "iRECUIpkg")
  cmnd <- paste0("unzip -p -P ", password, " ", data_path, " ", filebase, ".csv")
  # read the file into a dataframe
  cnts <- read.table(
    text = system(cmnd , intern = TRUE), stringsAsFactors = FALSE, header = TRUE, sep = ","
  )
  # delete the csv file so it can't offend or shock
  data_path <- system.file("extdata", paste0(filebase, ".csv"), package = "iRECUIpkg")
  csvfile <- trimws(data_path)
  if (file.exists(csvfile)) {
    file.remove(csvfile)
  }
  # only pull compound words that where from source was identified more than 200 times.
  cntsF <- cnts[cnts$count > 200,]
  # merge the prefix and suffix together
  compoundBW <- c(as.vector(paste0(cntsF$pre, cntsF$suff)),as.vector(paste0(cntsF$pre, cntsF$suff, "s")))
  # return the nasty list
  return(compoundBW)
}

#' Function to build list of all the nasty works we can find.
#' No parameters needed
BuildProfanityLists <- function() {
  auto_notify("building profanity lists")
  # make a list of all the sources I could find
  # the lexicon package holds many
  # also pull compound words to add to the list
  # comparing the comments to the list doesn't do much "like" or "contains"
  # type search, so adding extra words to a nasty word would probably get missed if not for the compound list
  badwrds <-  c(
    lexicon::profanity_alvarez,
    GetCompoundProfanity(),
    lexicon::profanity_zac_anger,
    lexicon::profanity_arr_bad,
    lexicon::profanity_banned,
    lexicon::profanity_racist
  )
  # make everything lowercase
  badwrds <- unique(tolower(badwrds))
  # drop some punctuation and such
  badwrds <- gsub("[*]", "", badwrds)
  badwrds <- gsub("[+]", "", badwrds)
  badwrds <- gsub("[(]", "", badwrds)
  badwrds <- gsub("[)]", "", badwrds)
  badwrds <- gsub("[!]", "", badwrds)
  badwrds <- gsub("\\\\", "", badwrds)
  badwrds <- gsub("/", "", badwrds)
  # some not so nasty words got picked up, so they are dropped before use
  dropwrds <- c("period","crabs","bait","hard","chick","play","barely","breast","chick", "faith","hard","hardcore",
                "killer", "nook", "play", "hook", "bigger","straight","huge","giant","hole","loin","loins","stringer",
                "nasty","illegal","ugly","conservative","clamps","hump","liberal","starfish","alaskan","harder","throat",
                "position","flasher","funeral","laid","brown", "rigger")

  badwrds <- badwrds[!badwrds %in% dropwrds]
  # drop really short words
  badwrds <- badwrds[nchar(badwrds) >= 4]

  return (badwrds)
}

#' Function to build the parameters and then check the comments for sentiment and profanity
#' Parameters are:
#' SavDat = reponse data
CheckProfanity <- function(SavDat){
  # build nasty word list
  badwrds <- BuildProfanityLists()
  # calculate the sentiment of each comment
  sentiments <- GetSentiment(SavDat)
  # calculate the profanity score of each comment
  scores <- GetProfanityScores(SavDat,
                               badwrds,
                               sentiments$nrc_data,
                               sentiments$sentiment_score)
  # filter out empty comments to speed things up
  scores <- scores %>%
    mutate(comment = case_when(comment == "" ~ NA, TRUE ~ comment)) %>%
    filter(comment != "NA")
  # add the field "bad_words_found" - helps to identiy why a score was derived
  scores$bad_words_found <- ""
  srows <- nrow(scores)

  # for each comment find the bad words
  for (i in 1:srows) {
    if (scores[i,c("profanity")] > 0) {
      cmntwrds <- str_extract_all(tolower(scores$comment[i]), "\\w+")[[1]]

      fnd <- 0

      # make a list of the bad words found
      for (w in cmntwrds) {
            if (length(grep(paste0("^",w,"$"),badwrds)) != 0) {
            fnd <- fnd + 1
            scores[i, 'bad_words_found'] <- trimws(paste0(scores[i, 'bad_words_found'], " ", w))
#            auto_notify(paste0(w, ' '))
          }
      }
      # if (fnd > 0) {
      #   auto_notify(paste0(i , " of ", srows))
      # }

    }
  }
  return(scores)
}

#' Function to get the profanity scores and merge in the sentiment scores
#' parameters are:
#' SavDat = response data
#' lexicons = nasty word list
#' nrc_data = previously calculated emotions and valence from NRC Dictionary
#' sentiment_scores = previously calculated sentiment score for each comment
GetProfanityScores <- function(SavDat, lexicons, nrc_data, sentiment_scores) {
  auto_notify("Getting profanity counts and scores")

  suppressWarnings(
    # call sentimentr profanity using the word list provided
    # breaks up the sentences and then sums them up for each comment
    probs <- profanity(sentimentr::get_sentences(SavDat[SavDat$comment != "" , c('tid', 'comment')]), profanity_list = lexicons) %>%
      group_by(element_id, tid) %>%
      summarise(
        .groups = "drop_last",
        profanity = sum(profanity),
        profanity_count = sum(profanity_count)
      )
  )

  auto_notify("Building results")
  # combine the profanity and sentiment scores and then join that to the response data
  results <- data.frame(
    score = sentiment_scores,
    positive = nrc_data$positive,
    negative = nrc_data$negative,
    profanity = round(probs$profanity,3),
    bad_words = probs$profanity_count,
    tid = probs$tid
  ) %>%
    left_join(SavDat[c('surveykey', 'tid', 'did_not_fish', 'comment', 'email')], join_by("tid" == "tid")) %>%
    group_by(score,
             positive,
             negative,
             profanity,
             bad_words,
             surveykey,
             did_not_fish,
             comment,
             email) %>%
    summarize(.groups = 'drop_last') %>%
    rename("Fished? Yes/No" = did_not_fish)
   # round the profanity score to 3 decimal places to make it more clear to quickly read
   results$score <- round(results$score,3)
   # change tru false column to Yes/No
   results$"Fished? Yes/No" <- ifelse(results$"Fished? Yes/No", "Yes", "No")
   # only return the necessary values.  We only use "positive" and "negative"
  return(results[c(
    "surveykey",
    "score",
    "negative",
    "positive",
    "profanity",
    "bad_words",
    "Fished? Yes/No",
    "comment",
    "email"
  )])
}
