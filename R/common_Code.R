"%notin%" <- Negate("%in%")

#' function to push messages to web browser
#' @export
auto_notify <- function(text) {
  # Automatically grabs the active Shiny session
  ctx_session <- shiny::getDefaultReactiveDomain()
 # if in shiny then show it as a notification
  if (!is.null(ctx_session)) {
    shiny::showNotification(text, session = ctx_session)
  }
  # always send the message to the console
    message(text)
}


#' Function to determine the year an month of a response file including adjusted responses
#' adjust file can introduce date records with different formats
#' parameters are:
#' dfIn = response file with fields did_not_fish, datefished
#' @export
GetYearMonth <- function(dfIn) {
  # filter out non date filled records include records with a "-" or a "/"
  dfs <- dfIn[dfIn$did_not_fish == FALSE | dfIn$did_not_fish == 'no', ]
  dfs <- dfs[dfs$datefished != "1900-01-01", ]
  dfs <- dfs[!is.na(dfs$datefished), ]
  dfs <- dfs[dfs$datefished != '', ]
  dfs <- dfs[grepl("/", dfs$datefished) | grepl("-", dfs$datefished), ]
  # parse the records with some sort of date looking field into a format date
  dfs$chkdte <-  parse_date_time(dfs$datefished, orders = c("ymd", "dmy", "mdy"), quiet = TRUE)
  yr <- 0
  mn <- 0
  # assuming anything is left, get the mode year and month from the remaining records
  if (nrow(dfs)>0){
    yr <- get_mode(year(as.Date(dfs$chkdte)))
    mn <- get_mode(month(as.Date(dfs$chkdte)))
  } else {
    # or if no records are left, default yr to 1900 and mn to 1
    yr=1900
    mn <- 1
  }
  # build a date as the 1st of the month and return it.
  return(as.Date(paste0(yr,"-",mn,"-01")))
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


#' fix common differences in what is delivered vs what is expected
fixValues <- function(survey_data) {

  df1 <- survey_data

  if ("CHECKCRABSPRAWNS" %in% names(df1)) {
    df1$CHECKCRABSPRAWNS[df1$CHECKCRABSPRAWNS == 999] <- 9
  }

  if ("STARTDAY" %notin% names(df1)) {
    df1$STARTDAY <- 15
    df1$ENDDAY <- 15
  }

  if ("COMPLETESURVEY" %in% names(df1)) {
    df1$COMPLETESURVEY[df1$COMPLETESURVEY == 1] <- 'Complete'
    df1$COMPLETESURVEY[df1$COMPLETESURVEY == 0] <- 'Incomplete'
    df1$COMPLETESURVEY[is.na(df1$COMPLETESURVEY)] <- 'NA'
  }

  for (hl in 1:12) {
    hlf <- paste0("HALIBUTLENGTH.", hl)
    if (hlf %in% names(df1)) {
      if (haven::is.labelled(df1[[hlf]])) {
        df1 <- haven::zap_labels(df1, user_na = TRUE)
      }
      if (class(df1[[hlf]]) == "character") {
        df1[[hlf]] <- suppressWarnings(as.double(df1[[hlf]]))
      }
    }
  }

  return (df1)
}

#' fix the comments parsing for email adress and phone numbers and emoji's
#' parameters are:
#' survey_data = dataframe with the comment column to check
#' cmnt = the column name to search for - optional default is COMMENT
fixComments <- function(survey_data, cmnt = 'COMMENT' ) {
  df1 <- survey_data
  if (cmnt %in% names(df1)) {
    # check for emojis
  df1$TST <- emoji::emoji_detect(df1[[cmnt]])
    # replace any emoji's found with string emoji::emoji_replace_name("🤔") = ":thinking_face:"
  df1[[cmnt]][df1$TST == TRUE & !is.na(df1$TST)] <- emoji::emoji_replace_name(df1[[cmnt]][df1$TST == TRUE & !is.na(df1$TST)])
   # handle any weird character sets
  df1[[cmnt]] <- stringi::stri_trans_general(df1[[cmnt]], "latin-ascii")
   # remove any non-printable characters
  df1[[cmnt]] <- textTools::str_rm_non_printable(df1[[cmnt]])
   # check for emails in the comments and replace them
  pattern <- "[^@\\s]*@[^@\\s]*\\.[^@\\s]*"
  replacement <- "[email removed]"
  df1[[cmnt]] <- gsub(pattern, replacement, df1[[cmnt]], perl = TRUE)
   # check for phone numbers and replace them too
  pattern <- "\\(?\\d{3}\\)?[ -]?\\d{3}[ -]?\\d{4}"
  replacement <- "[phone number removed]"
  df1[[cmnt]] <- gsub(pattern, replacement, df1[[cmnt]], perl = TRUE)
   # comments can only be 4000 characters so truncate them to ensure they fit.
  df1[[cmnt]] <- str_sub(df1[[cmnt]], 1, 4000)
  }

  return(df1)
}

#' filter columns base on list
#' Simple check to pull only the columns in the list cols
#' parameters are:
#' survey_data = data frame of interest
#' cols = list of column names to check for
filterColumns <- function(survey_data, cols) {
  df1 <- survey_data
  for (item in cols) {
    names(df1)[names(df1)==tolower(item)] <- item
  }
  return(df1[, names(df1) %in% cols])
}

#' check if a field exists and return its position
#' parameters are:
#' arry = the array to search
#' sval = the item to search for
#' srch = element to check rather than all the elements(maybe?)
#' skippos = list of element positions to skip over
checkfld <- function(arry, sval, srch, skippos) {
  mpos <- 0
  for (s in 1:length(arry)) {
    if (s %notin% skippos) {
      if (srch) {
        if (grepl(arry[s], sval)) {
          mpos <- s
          return(mpos)
        }
      } else {
        if (arry[s] == sval) {
          mpos <- s
          return(mpos)
        }
      }
    }
  }
  return(mpos)
}

#' check to see what a date format is
#' parameters are:
#' dteA = list of potential dates
#' year = year value to check for
#' month = month value to check for
checkDate <- function(dteA, year, month){
  dte<-0
  mpos <- 0
  ypos <- 0
  skippos <- c()
  mpos <- checkfld(dteA[[1]], month.abb[month], TRUE, skippos)
  if (mpos == 0) {
    mpos <- checkfld(dteA[[1]], month.name[month], TRUE, skippos)
  }
  if (mpos == 0) {
    mpos <- checkfld(dteA[[1]], month, FALSE, skippos)
  }
  if (mpos == 0) {
    mpos <- checkfld(dteA[[1]], paste0('0', month), FALSE, skippos)
  }
  skippos <- c(skippos, mpos)

  ypos <- checkfld(dteA[[1]], year, FALSE, skippos)
  if (ypos == 0) {
    ypos <- checkfld(dteA[[1]], substr(year,nchar(year)-1,nchar(year)), FALSE, skippos)
  }
  skippos <- c(skippos, ypos)
  dte <- 0

  for (d in 1:length(dteA[[1]])){
    if (d %notin% skippos){
      dte <- as.Date(paste0(year, '-', month, '-', dteA[[1]][d]))
      return(dte)
    }
  }
  return(NULL)
}

#' convert EKOS data format to curent format
#' taken from Nick's original analysis code - see his code for details
convertEKOSData <- function(ekos_data)  {
  UnspecifiedText <- "UN" #Unspecified"

  ekos_data <- ekos_data %>%
    as_tibble()

  ekos_data <-
    ekos_data %>%
    filter(!is.na(Licence_ID))


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

  ekos_data[,"datefished"] <- as.Date(paste(ekos_data$year,as.integer(ekos_data$month - 1000),ekos_data$day, sep = "-"), )

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
    bind_cols(ekos_data_strata, ekos_data_catch)

  ekos_data$completesurvey <- ekos_data$effort_days

  ekos_data$month_num <- as.integer(ekos_data$month - 1000)
  ekos_data <- ekos_data %>%  mutate_at(vars(area, method, month), labelText)

  #browser()
  if ("datecompleted" %notin% names(ekos_data)) {
    ekos_data$datecompleted <- "1900-01-01"
  }

  ekos_data <-dplyr::rename(ekos_data,
                            month_str = month,
                            month = month_num,
                            TotalJuveniles = juvepres,
                            surveykey = licence_id,
                            FishedFromLodge = lodge,
                            FishedWithGuide = guided) %>%
    mutate_if(is.labelled, zap_labels)

  ekos_data$did_not_fish[ekos_data$did_not_fish == TRUE] <- 1
  ekos_data$did_not_fish[ekos_data$did_not_fish != TRUE] <- 2
  #browser()

  if ("TYPE" %notin% names(ekos_data)) {
    ekos_data$TYPE <- paste("iREC", ekos_data$month_str, ekos_data$year," ")
  }


  ekos_data$startday <- 0
  ekos_data$endday <- 0
  ekos_data$startday[ekos_data$completesurvey == 1] <- 15
  ekos_data$endday[ekos_data$completesurvey == 1] <- 15

  return(ekos_data)
}

#' Function to build a connection to one of the oracle databases iREC references
#' parameter "S" indicates which server to attach to .
#' S = P means Production server for the OTOLITH_V1. schema (where CREST data (and iREC) are stored)
#' S = T  means Test server for the OTOLITH_V1. schema (where CREST data (and iREC) are stored)
#' S = L  means the ReadOnly LakeHose account to get data from FRIS and KREST
#' credentials are stored in a text file in the user's Documents folder in a subfolder named OracleCreds
setupOracleConn <- function(S) {
  db_conn <- NULL
  db_driver <- DBI::dbDriver("Oracle")

  hostT <- "vsbciosxd76.ent.dfo-mpo.ca"
  hostP <- "VSBCIOSXP75.ENT.DFO-MPO.CA"
  port <- 1523
  svcT = "OIOST01"
  svcP = "OIOSP01"


  if(S=='P'){
    host <- hostP
    svc <- svcP
    data <- read.table(file=paste0(path.expand('~'),"/OracleCreds/credsP.txt"), header = TRUE, sep = ",")
  }else if (S=='T') {
    host <- hostT
    svc <- svcT
    data <- read.table(file=paste0(path.expand('~'),"/OracleCreds/credsT.txt"), header = TRUE, sep = ",")
  } else if (S=='L') {
    host <- hostP
    svc <- svcP
    data <- read.table(file=paste0(path.expand('~'),"/OracleCreds/credsL.txt"), header = TRUE, sep = ",")
  } else {
    host <- 'HOST'
    svc <- 'SVC'
  }

  db_user_name <- data$USERNAME[1]
  db_pass = data$PSWD[1]
  auto_notify(paste0(db_user_name, ' - ' ,host, ' - ' , svc))
  db_conn_text <- paste0("(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=TCP)(HOST=",host,")(PORT=",port,")))(CONNECT_DATA=(SERVICE_NAME=",svc,")))")
  badconn <- FALSE
  for (i in 1:10) {
    if(i > 1) {
      auto_notify(paste0("Trying again ", i))
    }

    if (badconn) {
      break
    }
    goodcon <- tryCatch({
      goodcon <- FALSE
      db_conn <-
    DBI::dbConnect(
      db_driver,
      username = db_user_name,
      password = db_pass,
      dbname = db_conn_text
    )
    break
  }, error = function(e) {
    # 2. Handle connection failures gracefully
    auto_notify("Database connection failed!")
    auto_notify(paste0("System error message: ", e$message))
    if (grepl("ORA-12545", e$message, fixed = TRUE)){
      message("cannot find to database host - Not connected to correct network?")
      badconn <<- TRUE
    } else {
      Sys.sleep(10)
    }
  }, warning = function(w) {
    # 3. Handle connection warnings (optional)
    auto_notify("Connection warning: ", w$message)
    Sys.sleep(10)
  })
  }
    return(db_conn)
}


#' Function to retrieve data from a database connection (Oracle)
#' parameters are:
#' Kcon = database connection returned from "setupOracleConn"
#' qry = sql statement for the data required
#' chunks  = boolean to retrieve the data in blocks of 25 rows at a time.
#'   Once all the "chunks" are retieve the merged dataframe is returned.
#' Total time to retrieve the data is displayed in the console
#' @export
getdata <- function(Kcon, qry, chunks = FALSE) {
  Kfiles <- data.frame()
  time1 <- Sys.time()

  rs <- DBI::dbSendQuery(Kcon, qry)

  if (!chunks) {
       tryCatch(
        expr = {
          Kfiles <- DBI::dbFetch(rs)
          auto_notify("Data fetched successfully.")
        },
        error = function(e) {
          warning(paste("An error occurred during dbFetch:", conditionMessage(e)))
          return(data.frame())
        },
        warning = function(w) {
          auto_notify(paste("A warning occurred:", conditionMessage(w)))
        },
        finally = {
          DBI::dbClearResult(rs) # Ensure the result set is cleared
          auto_notify("dbFetch operation concluded.")
        }
      )

  } else
  {
    tryCatch(
      expr = {
        # Fetch data in chunks within a loop
        data_list <- list()
        chunk_size <- 25 # Define your desired chunk size

        while (!dbHasCompleted(rs)) {
          chunk <- DBI::dbFetch(rs, n = chunk_size)
          data_list[[length(data_list) + 1]] <- chunk
          auto_notify(nrow(chunk))
          if (length(shiny::getDefaultReactiveDomain()) > 0) {
            # incProgress <- shiny::incProgress
            withProgress <- shiny::withProgress
          } else {
            incProgress <- function(...) {
            }
            withProgress <- function(...)
              list(...)[["expr"]]
          }
        }
      },
      error = function(e) {
        warning(paste("An error occurred during dbFetch:", conditionMessage(e)))
        return(data.frame())
      },
      warning = function(w) {
        auto_notify(paste("A warning occurred:", conditionMessage(w)))
      },
      finally = {
        DBI::dbClearResult(rs) # Ensure the result set is cleared
        auto_notify("dbFetch operation concluded.")
      }
    )
    # Combine chunks into a single data frame
    Kfiles <- do.call(rbind, data_list)
  }
  time2 <- Sys.time()
  #dbClearResult(rs)
  auto_notify(nrow(Kfiles))
  auto_notify(difftime(time2, time1, units = c("secs")))
  return (Kfiles)
}

#' function to adjust a SQL where statement based on if there is already a where clause in the statement.
#' Parameters are:
#' sqlin = base sql statment
#' field = name of the field to add a where condition to
#' vals = the value(s) to select on
#' @export
buildsql <- function(sqlin, field, vals) {
  buildsql <- sqlin
  if (nchar(vals) > 0) {
    if (grepl("where", buildsql, ignore.case = TRUE)) {
      buildsql <- paste0(buildsql, " and ", field)
    } else {
      buildsql <- paste0(buildsql, " where ", field)
    }
    buildsql <- paste0(buildsql, " in (", vals, ")")
  }
  return(buildsql)
}

#' Function to calculate the mode from a list
#' parameters are:
#' x = list to calculate the modal value from
get_mode <- function(x) {
  unique_values <- unique(x)
  # Count the frequency of each unique value
  tabulate_counts <- tabulate(match(x, unique_values))
  # Return the unique value with the highest frequency
  mode_value <- unique_values[which.max(tabulate_counts)]
  return(mode_value)
}


#' Function to load look up tables from CREST
#' parameters are:
#' df_tbl_nmes = list of table names to load
load_CE_lus <- function(ccon, df_tbl_nmes) {
  rtn_list <- list()
  for (q in df_tbl_nmes) {
    q <- toupper(q)
    auto_notify(paste0(q, ' Starting '))
    qry <- (paste0("select * from OTOLITH_V1.", trim(q)))
    tmp <- getdata(ccon, qry, FALSE)

    tmp_lst <- list(tmp)
    tmp_lst <- setNames(tmp_lst, q)

    if (length(rtn_list) == 0) {
      rtn_list <- tmp_lst
    } else {
      rtn_list <- c(rtn_list, tmp_lst)
    }
    auto_notify(paste0(q, ' Done ', nrow(q)))
  }

  return(rtn_list)


}

#' Function to return pretty version of month
#' parameters are:
#' mIn = month to convert.  Can be a number or short or long version of the month name
Expmonth <- function (mIn) {
  return(
    case_when(
      mIn %in%(c("JAN", "1", "JANUARY")) ~ "January",
      mIn %in%(c("FEB", "2", "FEBRUARY")) ~ "February",
      mIn %in%(c("MAR", "3", "MARCH")) ~ "March",
      mIn %in%(c("APR", "4", "APRIL")) ~ "April",
      mIn %in%(c("MAY", "5", "MAY")) ~ "May",
      mIn %in%(c("JUN", "6", "JUNE")) ~  "June",
      mIn %in%(c("JUL", "7", "JULY")) ~ "July",
      mIn %in%(c("AUG", "8", "AUGUST")) ~ "August",
      mIn %in%(c("SEPT", "SEP", "9", "SEPTEMBER")) ~ "September",
      mIn %in%(c("OCT", "10", "OCTOBER")) ~ "October",
      mIn %in%(c("NOV", "11", "NOVEMBER")) ~ "November",
      mIn %in%(c("DEC", "12", "DECEMBER")) ~ "December",
      TRUE ~ "XXXXXXX")
  )
}

#' Function to read in a binary file
#' mostly to store files in an Oracle database
#' parameters are:
#' file_path = path + filename to the file to read
read_file_bytes <- function(file_path) {
  # 1. Get the file size
  file_info <- file.info(file_path)
  file_size <- file_info$size

  # 2. Establish a connection in "read binary" mode
  # The 'rb' mode is recommended for binary data
  con <- file(file_path, "rb")

  # 3. Read the data using readBin()
  # type = "raw" specifies we want a byte array
  # n = file_size specifies the number of bytes to read
  byte_array <- readBin(con, what = "raw", n = file_size)

  # 4. Close the connection
  close(con)

  # Return the byte array
  return(byte_array)
}

#' function to check if a character(string?) can be an integer value
#' parameters are:
#' chr = string to test if it can represent an integer/number
can_be_integer <- function(chr) {
  # Check if coercion to numeric results in NA
  !is.na(suppressWarnings(as.numeric(chr)))
}


#' function to extract the occurance of a value from a dataframe column - used in figuring out what a date is
#' parameters are:
#' df = the dataframe holding the date in question
#' val = the value we are looking for
#' frst = boolean - to limit the check to the first occasion (maybe?)
PurgeDte <- function(df, val, frst){
  if(frst) {
    df$d2[df$delim %notin% c('E','U','')] <- 1
  }
  df$d1[df$delim %notin% c('E','U','')] <- nchar(df$dte[df$delim %notin% c('E','U','')])
  df$dte[df$delim %notin% c('E','U','') & df$d2 != 0] <- str_remove(df$dte[df$delim %notin% c('E','U','') & df$d2 != 0],val)
  df$d2[df$delim %notin% c('E','U','') & df$d2 != 0] <- nchar(df$dte[df$delim %notin% c('E','U','') & df$d2 != 0])
  df$d2[df$d1 > df$d2] <- 0
  return(df)
}

#' function to check and fix if necessary the date formats in an adjust csv file
#' paramters are:
#' adj_df = the adjust csv file loaded into a dataframe
#' CSVfilename = the name of the adjust file
fixAdjDates <- function(adj_df, CSVfilename){
  # get the year and month of the adjust file
  dfDate <- GetYearMonth(adj_df)
  # add row numbers
  adj_df$rownumber <- seq.int(nrow(adj_df))
  # parse out the year and month
  yr <- year(dfDate)
  mn <- month(dfDate)
  message(dfDate)
  # figure out the delimiter in use for each row of the datefished column
  df <- adj_df %>%
    mutate(
      delim = case_when(
        grepl("-",adj_df$datefished) ~ "-",
        grepl("/",adj_df$datefished) ~ "/",
        grepl(" ",adj_df$datefished) ~ " ",
        TRUE ~ "U" # Default value if none of the above conditions are met
      )
    )
  # set the delim to "U" when it isn't a real date - blank or did not fish
  df <- df %>%
    mutate(
      delim = case_when(
        (delim == 'U' & datefished == "1900-01-01") ~ "",
        (delim == 'U' & is.na(datefished))  ~ "",
        (delim == 'U' & datefished == '') ~ "",
        (delim == 'U' & (tolower(did_not_fish) == 'no' | tolower(did_not_fish) == 'fished in the month')) ~ "E",
        TRUE ~ delim
      )
    )

  # set up the dataframe for day of month extraction
  df$d1 <- 0
  df$d2 <- 0
  df$dte <- ""
  df$dfished <- as.Date('1900-01-01')
  df$original_datefished <-  df$datefished
  df$dte[df$delim != 'E'] <- df$datefished[df$delim != 'E']

  # Find and remove the year as a 4 digit number - 2025
  df <- PurgeDte(df, as.character(yr), TRUE)
  # find and remove the year as a 2 digit number - 25
  df <- PurgeDte(df, substr(yr, nchar(yr) - 1, nchar(yr)), FALSE)
  # find and remove the month as a full month name - September
  df <- PurgeDte(df, month.name[mn], TRUE)
  # find and remove the month name as an abbreviation - Sept
  df <- PurgeDte(df, month.abb[mn], FALSE )
  # find and remove the month as a 2 digit month number - 09
  df <- PurgeDte(df, paste0('0', mn), FALSE)
  # find and remove the month as a 1 or 2 digit character number 9 or 12
  df <- PurgeDte(df, as.character(mn), FALSE)

  # remove the delimeters from the date should leave just the day of month
  df$dte[df$delim %notin% c('','E') ] <- trimws(str_remove_all(df$dte[df$delim %notin% c('','E')],df$delim[df$delim %notin% c('','E')]))
  df <- df %>%
    mutate(
      dte = case_when(
        (delim %notin% c('','E') & can_be_integer(dte) == TRUE) ~ dte,
        .default = "0")
    )
  # fix excel dates - days since 1899-12-30
  df$dfished[df$delim == 'E'] <- as.Date(as.numeric(df$datefished[df$delim == 'E']), origin = "1899-12-30")
  # use the day of month remaining to build a date with the known year and month
  df$dfished[df$delim %notin% c("E","")] <- parse_date_time(paste0(yr,"-",mn,"-",df$dte[df$delim %notin% c("E","")]), order = c("ymd"), quiet = TRUE)
  df$dfished[df$delim == ""] <- NA
  df$datefished <- as.Date(df$dfished)

  # rename licence_id to surveykey if found
  if ("licence_id" %in% names(df)){
    df <- df %>% dplyr::rename(surveykey = licence_id)
  }
  # pull the records with no delim and a NA for datefished - selecting a few key columns to identify a the issue
  tdf <- df[df$delim != '' & is.na(df$datefished), c("rownumber","surveykey","datefished","delim", "original_datefished")]

  # this seems to delete the CSV file we were using. Not sure about this.  Maybe an artifact of shiny loading?
  # I am going to remove this with comments
  # if (CSVfilename != '') {
  #   if (file.exists(CSVfilename)) {
  #     file.remove(CSVfilename)
  #   }
  # }
  # if there where dates not able to sort out then show them as an html in a new window
    if (nrow(tdf)>0) {
      df <- df[0,]
      print(tdf)
      for (s in 1:120){
        cat(".")
        Sys.sleep(1)
      }
      print("X")

      fileout <- gsub("\\.csv$", "\\.html", CSVfilename)
      print(xtable(tdf), type = "html", file = fileout)
      browseURL(fileout)
      for (s in 1:120){
        cat(".")
        Sys.sleep(1)
      }
      print("X")
    }

  return(df)
}

#' function to link look up tables to response data

linkTables <- function(survey_data, fullshellfish) {

  ctx_session <- shiny::getDefaultReactiveDomain()
  if (!is.null(ctx_session)) {
    data_pathA <- system.file("extdata", "AreasDFO.csv", package = "iRECUIpkg")
    data_pathM <- system.file("extdata", "Methods.csv", package = "iRECUIpkg")
    data_pathS <- system.file("extdata", "shellfishing.csv", package = "iRECUIpkg")
  } else {
    data_pathA <- "inst\\extdata\\AreasDFO.csv"
    data_pathM <- "inst\\extdata\\Methods.csv"
    data_pathS <- "inst\\extdata\\shellfishing.csv"
  }

  areas_df <- read.csv(data_pathA)
  methods_df <- read.csv(data_pathM)
  ccp_df <- read.csv(data_pathS)
  ccp_df$VALUE <- as.character(ccp_df$VALUE)

  df1 <- survey_data

  # change NA's to 0's for numeric cells
  df1 <- df1 |> mutate(across(where(is.numeric), ~replace_na(., 0)))

  df1 <- df1 %>% left_join(areas_df, join_by("AREA"=="TEXT"))
  names(df1)[names(df1)=='AREA'] <- 'AREASCODE'
  names(df1)[names(df1)=='TYPE_NAME'] <- 'AREA'

  df1 <- df1 %>% left_join(methods_df, join_by("METHOD"=="TEXT"))
  names(df1)[names(df1)=='METHOD'] <- 'METHODCODE'
  names(df1)[names(df1)=='KREST_CDE'] <- 'METHOD'

  if (!"CHECKCRABSPRAWNS" %in% names(df1)) {
    df1$CHECKCRABSPRAWNS <- NA
  }
  if (fullshellfish) {
    #browser()
    if (haven::is.labelled(df1$CHECKCRABSPRAWNS)){
      df1 <- haven::zap_labels(df1, user_na = TRUE)
    }
    if (class(df1$CHECKCRABSPRAWNS) != "character") {
      df1$CHECKCRABSPRAWNS <- as.character(df1$CHECKCRABSPRAWNS)
    }
    df1 <- df1 %>% left_join(ccp_df, join_by("CHECKCRABSPRAWNS"=="VALUE"))
    df1$CheckCrabsPrawnsTXT <- df1$CHECKCRABSPRAWNS
    names(df1)[names(df1)=='TEXT'] <- 'CHECKCRABSPRAWNS_TXT'
  } else {
    df1 <- df1 %>% left_join(ccp_df, join_by("CHECKCRABSPRAWNS"=="TEXT"))
    names(df1)[names(df1)=='CHECKCRABSPRAWNS'] <- 'CHECKCRABSPRAWNS_TXT'
    names(df1)[names(df1)=='VALUE'] <- 'CHECKCRABSPRAWNS'
  }
  #browser()

  if (!"CRAB_CENTREOFFISHING_LATITUDE" %in% names(df1)) {
    if ("CENTREOFFISHING_LATITUDE" %in% names(df1)){
      df1 <- df1 %>%
        mutate(CRAB_CENTREOFFISHING_LATITUDE = case_when(
          CHECKCRABSPRAWNSTXT  == "1" ~ CENTREOFFISHING_LATITUDE,
          CHECKCRABSPRAWNSTXT  == "2" ~ CENTREOFFISHING_LATITUDE,
          CHECKCRABSPRAWNSTXT  == "5" ~ CENTREOFFISHING_LATITUDE
        )) %>%
        mutate(Crab_CentreOfFishing_Longitude = case_when(
          CHECKCRABSPRAWNSTXT  == "1" ~ CENTREOFFISHING_LONGITUDE,
          CHECKCRABSPRAWNSTXT  == "2" ~ CENTREOFFISHING_LONGITUDE,
          CHECKCRABSPRAWNSTXT  == "5" ~ CENTREOFFISHING_LONGITUDE
        )) %>%
        mutate(PRAWN_CENTREOFFISHING_LATITUDE = case_when(
          CHECKCRABSPRAWNSTXT  == "1" ~ CENTREOFFISHING_LATITUDE,
          CHECKCRABSPRAWNSTXT  == "3" ~ CENTREOFFISHING_LATITUDE,
          CHECKCRABSPRAWNSTXT  == "5" ~ CENTREOFFISHING_LATITUDE
        )) %>%
        mutate(PRAWN_CENTREOFFISHING_LONGITUDE = case_when(
          CHECKCRABSPRAWNSTXT  == "1" ~ CENTREOFFISHING_LONGITUDE,
          CHECKCRABSPRAWNSTXT  == "3" ~ CENTREOFFISHING_LONGITUDE,
          CHECKCRABSPRAWNSTXT  == "5" ~ CENTREOFFISHING_LONGITUDE
        ))
    }}

  return (df1)
}

