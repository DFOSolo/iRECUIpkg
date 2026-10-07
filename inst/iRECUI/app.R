  library(shiny, warn.conflicts = FALSE)
  library(shinyauthr, warn.conflicts = FALSE)
  library(shinycssloaders, warn.conflicts = FALSE)
  library(shinyjs, warn.conflicts = FALSE)
  library(shinyWidgets, warn.conflicts = FALSE)
  library(dplyr, warn.conflicts = FALSE)
  library(dipsaus, warn.conflicts = FALSE)
  library(anytime, warn.conflicts = FALSE)
  library(dipsaus, warn.conflicts = FALSE)
  library(dplyr, warn.conflicts = FALSE)
  library(DT, warn.conflicts = FALSE)
  library(glue, warn.conflicts = FALSE)
  library(haven, warn.conflicts = FALSE)
  library(httr, warn.conflicts = FALSE)
  library(jsonlite, warn.conflicts = FALSE)
  library(magrittr, warn.conflicts = FALSE)
  library(openxlsx, warn.conflicts = FALSE)
  library(readr, warn.conflicts = FALSE)
  library(ROracle, warn.conflicts = FALSE)
  library(sentimentr, warn.conflicts = FALSE)
  library(stringr, warn.conflicts = FALSE)
  library(syuzhet, warn.conflicts = FALSE)
  library(textTools, warn.conflicts = FALSE)
  library(tibble, warn.conflicts = FALSE)
  library(tidyverse, warn.conflicts = FALSE)
  library(tools, warn.conflicts = FALSE)
  library(uuid, warn.conflicts = FALSE)
  library(DBI, warn.conflicts = FALSE)
  library(utils, warn.conflicts = FALSE)
  library(zip, warn.conflicts = FALSE)
  library(xml2, warn.conflicts = FALSE)
  library(future, warn.conflicts = FALSE)
  library(fs, warn.conflicts = FALSE)
  library(lubridate, warn.conflicts = FALSE)
  library(emoji, warn.conflicts = FALSE)
  library(stringi, warn.conflicts = FALSE)
  library(xtable, warn.conflicts = FALSE)
  library(base, warn.conflicts = FALSE)
  library(tidyr, warn.conflicts = FALSE)
  library(lexicon, warn.conflicts = FALSE)
  library(sodium, warn.conflicts = FALSE)
  library(progress, warn.conflicts = FALSE)
  library(iRECUIpkg, warn.conflicts = FALSE)
  options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
  options(java.parameters = "-Xmx1g") # increase heap size to 1gb so we can transfer/load larger files
  options(shiny.maxRequestSize=60*1024^2)
  message(lsf.str())

# ---------------------- UI ----------------------
# Define the User Interface
ui <- navbarPage(
  title = paste0("iREC interface ", utils::packageVersion("iRECUIpkg")),      # Title displayed on the top left
  id = "nav_id",                    # Optional ID to track the active tab in server
  collapsible = TRUE,               # Automatically collapses on mobile screens
  inverse = TRUE,                   # Uses a dark theme navbar with light text

  header=tags$head(
    tags$style(HTML("
    /* Browse buttons (lavender) */
    .btn-file {
      background-color:#c7a4d4;
      color:white;
      border:none;
    }
    .btn-file:hover {
      background-color:#b388c4;
      color:white;
    }

    /* Outlier family (peach) */
    #runOuts, #dl {
      background-color:#f9c6aa;
      color:black;
      border:none;
    }
    #runOuts:hover, #dl:hover {
      background-color:#f7a97c;
      color:black;
    }

    /* Adjust family (mint) */
    #checkAdjusts, #dlAdjust {
      background-color:#a8e6cf;
      color:black;
      border:none;
    }
    #checkAdjusts:hover, #dlAdjust:hover {
      background-color:#7cdcb5;
      color:black;
    }
  "))
  ),

  # ********************** UI for login Start ************************

  tabPanel("Login",
           useShinyjs(),
           div(class = "container",
               div(
                 class = "pull-right", shinyauthr::logoutUI(id = "logout")),
               shinyauthr::loginUI(id = "login"))
  ),
  # ********************** UI for login End ************************

  # ********************** UI for Outlier Start ************************
  tabPanel(
    title = "iRec Response Outlier Checker",
    icon = icon("cog"),           # Adds an icon next to the text
    fluidPage(
      #--------------------------------#
      fluidRow(
        column(
          4,
          fileInput(
            "savFile",
            "Choose SPSS .SAV File",
            multiple = FALSE,
            accept = c("sav", "SPSS File", ".sav"),
            placeholder = "No file selected"
          ),
          actionButton("runOuts", "Check Outliers"),
          textOutput("Out_message"),
          hr(),
          checkboxInput(
            "Areacheckbox",
            "Area Outliers return all repeating patterns",
            FALSE
          ),
          checkboxInput(
            "FRIScheckbox",
            "Use FRIS Data for Daily Bag Limits",
            FALSE
          )

        ),

        column(
          4,
          fileInput(
            "adjFile",
            "Choose adjustment File",
            multiple = FALSE,
            accept = c("csv", "Adjust File", ".csv"),
            placeholder = "survey_adj.csv"
          ),
          actionButton("checkAdjusts", "Check Adjust Values"),
          hr(),
          column(5,numericInput("FRIS_GRACE","Enter FRIS Limit+", min = 0 , max = 100, value = 1),
                 uiOutput("FRIS_Over"))
        ),

        column(
          4,
          fileInput(
            "excFILE",
            "Choose exclude File",
            multiple = FALSE,
            accept = c("csv", "Exclude File", ".csv"),
            placeholder = "..\\exclude.csv"
          ),
          div(style = "height: 30px;"),
          hr()
        )
      ),

      fluidRow(align = 'center',

               hr(),

               h4("Enter Limits if not using FRIS data"),

               column(1),

               column(3,numericInput("HABAG","Enter Halibut Bag Limit", min = 0 , max = 100, value = 2),
                      uiOutput("HA")),
               #    hr(),
               column(3,numericInput("CNBAG","Enter Chinook Bag Limit", min = 0 , max = 100, value = 3),
                      uiOutput("CN")),
               #    hr(),

               column(3,numericInput("COBAG","Enter Coho Bag Limit", min = 0 , max = 100, value = 5),
                      uiOutput("CO")),
               #    hr(),

               column(3,numericInput("ENCLIMIT","Enter Encounter Limit", min = 0 , max = 500, value = 20),
                      uiOutput("ENC")),

               column(3,
                      numericInput("ANN_CN","Chinook Annual Limit", min = 0, max = 100, value = 10),
                      uiOutput("ANN_CN_TEXT")),

               column(3,
                      numericInput("ANN_LC","Lindcod Annual Limit", min = 0, max = 100, value = 10),
                      uiOutput("ANN_LC_TEXT")),

               column(3,
                      numericInput("ANN_HA","Halibut Annual Limit", min = 0, max = 100, value = 10),
                      uiOutput("ANN_HA_TEXT")),

               column(1)
      ),

      hr(),
      fluidRow(
        tags$head(tags$script(src = "message-handler.js")),
        column(3, downloadButton("dl","Download Outlier")),
        column(3, downloadButton("dlAdjust", "Download Adjust Validation")),
        useShinyjs()

      ),
      fluidRow(
        DT::dataTableOutput("TBL_Catch") %>% withSpinner(type = 5, color = "#0dc5c7"),

        DT::dataTableOutput("TBL_Annual"),

        DT::dataTableOutput("TBL_Area"),

        DT::dataTableOutput("TBL_Comments"),

        DT::dataTableOutput("TBL_Responses"),

        DT::dataTableOutput("TBL_OFRIS")
      ),
      hr(),
      fluidRow(
        DT::dataTableOutput("TBL_AdjustCheck"),
        DT::dataTableOutput("TBL_AdjustProblems")
      )
    )
  ),
    # ********************** UI for Outlier End ************************

  # ********************** UI for quering database Start ************************
  tabPanel("Query Tool",
           useShinyjs(),
           mainPanel(
             fluidRow(
               column(2,selectInput("Qyrsel", "Select Year", choices = c("None"="", last5yrs), multiple = TRUE, selected = format(Sys.Date(),"%Y")),actionButton("clearQYRBtn", "Clear Selection")),
               column(2,selectInput("Qmnthsel", "Select Month", choices = c("None"="","All",month.name), multiple = TRUE, selected = "All"),actionButton("clearQMNTBtn", "Clear Selection")),
               column(2,selectInput("QAreasel", "Select Areas", choices = c("None"="","All", unique(Qareas_df$type_name)), multiple = TRUE, selected = "All"),actionButton("clearQAREABtn", "Clear Selection")),
               column(3,selectInput("Qsppgrpsel", "Select Species Group", choices = c("None"="","All", unique(df_spp$GROUPING)), multiple = TRUE, selected = "All"),actionButton("clearQSPPGBtn", "Clear Selection")),
               column(3,selectInput("Qsppsel", "Select Species", choices = c("None"="","All", unique(df_spp$COMMON_NAME)), multiple = TRUE, selected = "All"),actionButton("clearQSPPBtn", "Clear Selection"))
             ),
             hr(),
             fluidRow(
               column(5, selectInput("QQrysel", "Select Query", choices = c("None"="","Estimates with Proportions","Estimates with Proportions (BCF)", "Detailed Estimates"), multiple = FALSE, selected = character(0)))
             ),

             hr(),

             fluidRow(
               tags$head(tags$script(src = "message-handler.js")),
               actionButton("Fetch_ests", "Fetch Data")
             ),
             hr(),
             fluidRow(
               div(DT::dataTableOutput("TBL_QDATA") %>% withSpinner(type = 5, color = "#0dc5c7"), style = "font-size:70%;")
             ),
             width = 12
           )
  ),
  # ********************** UI for quering database End ************************

  # ********************** UI for quering FRIS data Start ************************
  tabPanel(
    title = "FRIS",
    icon = icon("table"),           # Adds an icon next to the text
    fluidPage(

      fluidRow(
        column(3,selectInput("Fyrsel", "Select Year", choices = c("None"="", last5yrs)), selected = format(Sys.Date(),"%Y")),
        column(3,selectInput("Fmnthsel", "Select Month", choices = c("None"="", month.name)), selected = "")
      ),
      hr(),
      fluidRow(
        column(5, selectInput("FQrysel", "Select Query", choices = c("None"="","Summary", "Detailed"), multiple = FALSE, selected = character(0)))
      ),
      fluidRow(
        tags$head(tags$script(src = "message-handler.js")),
        actionButton("Fetch_fris", "Fetch FRIS Data")
      ),
      fluidRow(
        div(DT::dataTableOutput("TBL_FRIS") %>% withSpinner(type = 5, color = "#0dc5c7"), style = "font-size:70%;")

      ),
      hr(),
      width = 12
    )
  ),
  # ********************** UI for quering FRIS data End ************************


  # ********************** UI for Loading Estimates to CREST / KREST Start ************************
  tabPanel("Load Estimates",
           sidebarPanel(
             # The sliderTextInput allows character vectors
             sliderTextInput(
               inputId = "dest_slider",
               label = "Select Destination Database:",
               choices = c("KREST", "CREST Test", "CREST Prod" ),
               selected = "CREST Test",
               grid = TRUE
             )
           ),
           mainPanel(
             div(textOutput("selected_database"), style = "font-size: 24px;"),
             fluidRow(
               column(4,
                      h3("Load Estimates"),
                      fancyFileInput('est_input', "Please upload .XLSX file", after_content = 'Drag & drop', size = 's', accept = ".xlsx"),
                      textOutput("Est_message"),
                      actionButton("Load_Ests", "Load Estimates")),
               fluidRow(column(4,
                   h3("Load Bias Correction Factors"),
                   fancyFileInput("BiasFile", "Please upload .CSV", after_content = 'Drag & drop', size = 's', accept = ".csv"),
                   textOutput("Bias_message"),
                   actionButton("FetchBIAS", "Load Corrections"),
                   actionButton("PostBIAS", "Post Corrections")
               )),
               hr(),
               fluidRow(div(DT::dataTableOutput("TBLBIAS"), style = "font-size:70%;") ),
               #verbatimTextOutput("code_box", placeholder = TRUE)
               #verbatimTextOutput("console_logs") # Monospaced text block for logs

             ),
             width = 12
           )
  ),
  # ********************** UI for Loading Estimates End ************************

  # ********************** UI for Loading Responses Start ************************
  tabPanel("Load Responses",
           sidebarPanel(
             # The sliderTextInput allows character vectors
             sliderTextInput(
               inputId = "R_dest_slider",
               label = "Select Destination Database:",
               choices = c("KREST", "CREST Test", "CREST Prod" ),
               selected = "CREST Test",
               grid = TRUE
             )
           ),
           mainPanel(
             div(textOutput("R_selected_database"), style = "font-size: 24px;"),
             fluidRow(
               column(4,
                      h3("Load Responses"),
                      fancyFileInput('resp_input', "Please upload .SAV file", after_content = 'Drag & drop', size = 's', accept = ".SAV"),
                      textOutput("Resp_message"),
                      actionButton("Load_Resp", "Load Responses")),
               column(4,
                      h3("Load Adjusts"),
                      fancyFileInput('adj_input', "Please upload .CSV file", after_content = 'Drag & drop', size = 's', accept = ".csv"),
                      textOutput("CSV_message"),
                      actionButton("load_adjs", "Load CSV")),

               column(4,
                      h3("Load Excludes"),
                      fancyFileInput('exc_input', "Please upload .CSV file", after_content = 'Drag & drop', size = 's', accept = c(".csv", ".xlsx")),
                      selectInput("yrsel", "Select Licence Year", choices = c("None"="", ly_df$LICENCE_YEAR)), selected = "",
                      textOutput("EXC_message"),
                      actionButton("load_excs", "Load CSV")),


               hr(),
               fluidRow(div(DT::dataTableOutput("TBLRESP"), style = "font-size:70%;") ),
               verbatimTextOutput("R_code_box", placeholder = TRUE)


             ),
             width = 12
           )
  ),
  tabPanel("Message Logs",
                                    verbatimTextOutput("console_logs") # Monospaced text block for logs
                        ),
  # ********************** UI for Loading Estimates to CREST End ************************


        navbarMenu(
    title = "More Options",
    icon = icon("info-circle"),

    tabPanel("Documentation", p("Add your user manuals here.")),
    "----",                         # Creates a horizontal separator line
    tabPanel("About", p("Created using the R Shiny navbarPage layout."))
  )
)

# Define Server Logic
server <- function(input, output, session) {
  #' function to push messages to web browser
  #' @export
  auto_notify <- function(text) {
    # Automatically grabs the active Shiny session
    ctx_session <- shiny::getDefaultReactiveDomain()

    if (!is.null(ctx_session)) {
      shiny::showNotification(text, session = ctx_session)

      message(text)
    } else {
      message(text)
    }
  }


  log_file <- tempfile(fileext = ".log")
  log_con <- file(log_file, open = "wt")
  system2("cmd.exe",
          args = c("/c", "start", "powershell", "-NoExit", "-Command",
                   sprintf("Get-Content '%s' -Wait -Tail 10", log_file)),
          wait = FALSE)
  # Divert console output to our temporary log file
 # sink(log_file, type = "output", split = TRUE)
  sink(log_con, type = "message")

  # Clean up the sinks when the user session terminates
  onSessionEnded(function() {
    sink(type = "message")
    close(log_con)
  })

  # Read the log file dynamically every 500ms
  log_reader <- reactiveFileReader(500, session, log_file, readLines)


  #####################**********************************##################################
  #####################*      Login Start
  #####################**********************************##################################
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  output$user_table <- renderTable({
    # use req to only render results when credentials()$user_auth is TRUE
    #req(credentials()$user_auth)
    credentials()$info
  })

  # The username (often an email address) will be in session$user
  username <- session$user

  # Ensure you are getting a non-NULL value, as it will be NULL for public apps
  if (!is.null(username)) {
    auto_notify("Logged in user:", username, "\n")
  } else {
    auto_notify("No authenticated user or app is public\n")
  }

  #####################**********************************##################################
  #####################*      Outlier Start
  #####################**********************************##################################
  trigger <- reactiveVal(0)
  df <- data.frame()
  auto_notify(Sys.time() %>% as.character)
  auto_notify(Sys.info()["user"])
  auto_notify(R.Version())
  output$HA <- renderUI({paste("Halibut Limit:", input$HABAG) })
  output$CN <- renderUI({paste("Chinook Limit:", input$CNBAG) })
  output$CO <- renderUI({paste("Coho Limit:", input$COBAG) })
  output$ENC <- renderUI({paste("Encounter Limit:", input$ENCLIMIT) })
  output$ANN_CN_TEXT  <- renderUI({ paste("Annual Chinook Limit:", input$ANN_CN) })
  output$ANN_LC_TEXT  <- renderUI({ paste("Annual Lindcod Limit:", input$ANN_LC) })
  output$ANN_HA_TEXT  <- renderUI({ paste("Annual Halibut Limit:", input$ANN_HA) })
  output$FRIS_Over <- renderUI({paste("Over Limit Grace:", input$FRIS_GRACE) })

  rv <- reactiveValues(
    Adata = NULL,
    Edata = NULL,
    Aclear = FALSE,
    Eclear = FALSE
  )

  observe({
    req(input$excFILE)
    req(!rv$Eclear)
    rv$Edata <- read.csv(input$excFILE$datapath)
  })

  observeEvent(input$excFILE, {
    rv$Eclear <- FALSE
  }, priority = 1000)

  observe({
    req(input$adjFile)
    req(!rv$Aclear)
    rv$Adata <- read.csv(
      input$adjFile$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character",
      na.strings = "NA"
    )
  })
  observeEvent(input$adjFile, {
    rv$Aclear <- FALSE
  }, priority = 1000)
  # ---------- NEW: Adjust validation workflow ----------
  AdjCheck <- eventReactive(input$checkAdjusts, {
    req(input$adjFile)
    iRECUIpkg::setupOAV()
    adjusts <- rv$Adata
    excludes <- NULL
    if (!is.null(input$excFILE)) excludes <- rv$Edata

    tryCatch(
      iRECUIpkg::validate_adjustments(adjusts, excludes = excludes),
      error = function(e) {
        list(validated = NULL, problems = NULL, error = conditionMessage(e))
      }
    )
  })


  observeEvent(input$checkAdjusts, {
    res <- AdjCheck()
    if (!is.null(res$error)) {
      showNotification(
        paste("Adjustment file error:", res$error),
        type = "error",
        duration = NULL,
        id = "adjust_error"
      )
    } else {
      removeNotification("adjust_error")
      showNotification(
        "Adjustment file validated successfully",
        type = "message",
        duration = 3,
        id = "adjust_ok"
      )
    }
  })

  output$TBL_AdjustCheck <- DT::renderDataTable({
    req(AdjCheck()$validated)
    AdjCheck()$validated
  },
  options = list(paging = TRUE, pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv','excel')),
  caption = "Adjustment file validated")

  output$TBL_AdjustProblems <- DT::renderDataTable({
    probs <- AdjCheck()$problems
    if (is.null(probs)) {
      data.frame(Message = "No problems found")
    } else {
      probs
    }
  },
  options = list(paging = TRUE, pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = c('csv','excel')),
  caption = "Adjust file issues")

  output$dlAdjust <- downloadHandler(
    filename = function() {
      "adjust_validation_results.xlsx"
    },
    content = function(file) {
      res <- AdjCheck()
      wb <- openxlsx::createWorkbook()

      openxlsx::addWorksheet(wb, "Validated")
      openxlsx::writeData(wb, "Validated", res$validated)

      openxlsx::addWorksheet(wb, "Problems")
      if (!is.null(res$problems)) {
        openxlsx::writeData(wb, "Problems", res$problems)
      } else {
        openxlsx::writeData(wb, "Problems", data.frame(Message = "No problems found"))
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  # ---------- END ----------


  NewDat <- eventReactive(input$runOuts, {
    req(input$savFile) ## ?req #  require that the input is available

    shinyjs::disable("runOuts")
    #source("R/outlierCode/Outliers.R")
    #source("R/outlierCode/adjust_validation_v2.R")
    #source("R/outlierCode/crazy8s.R")
    iRECUIpkg::setupOAV()
    inFile <- input$savFile
    baseName <- tools::file_path_sans_ext(inFile$name)
    xlsFile <- paste0(baseName,"_OLCR.xlsx")
    excludes <- rv$Edata
    adjusts <- rv$Adata
    df <- read_sav(inFile$datapath)
    auto_notify("Read SAV")
    flagged_df <- iRECUIpkg::flagged_8s(df)
    n_dfs <- processSav(df,xlsFile,input$HABAG, input$CNBAG, input$COBAG, input$ENCLIMIT, input$ANN_CN,input$ANN_LC, input$ANN_HA, excludes, adjusts,input$Areacheckbox, input$FRIScheckbox, input$FRIS_GRACE)
    auto_notify("Processed SAV")
    wrkbk <<- buildOutlierXLS(n_dfs, flagged_df)
    rtn_lst = list("responses" = n_dfs$responses,
                   "annual" = n_dfs$annual,
                   "areaD" = n_dfs$areaD,
                   "area" = n_dfs$area,
                   "comments" = n_dfs$comments,
                   "catch" = n_dfs$catch,
                   "crazy8s" = flagged_df,
                   "worksheet" = wrkbk,
                   "bookname" = n_dfs$bookname,
                   "fris" = n_dfs$fris)
    auto_notify("Done WorkBook")

    rv$Edata <- NULL
    rv$Eclear <- TRUE
    rv$Adata <- NULL
    rv$Aclear <- TRUE
    reset('savFile')
    reset('excFILE')
    reset('adjFile')

    output$Out_message <- renderText({
      'All Done!  Carry On!'
    })
    shinyjs::enable("runOuts")
    return(rtn_lst)

  }
  )


  output$dl <- downloadHandler(
    filename = function() { NewDat()$bookname},
    content = function(file)  {
      saveWorkbook(NewDat()$worksheet, file = file)
    }
  )


  output$TBL_Catch <- DT::renderDataTable(NewDat()$catch,selection=list(mode="single", target="cell"), filter = 'top',
                                     options = list(paging = TRUE,    ## paginate the output
                                                    pageLength = 10,  ## number of rows to output for each page
                                                    scrollX = TRUE,   ## enable scrolling on X axis
                                                    scrollY = TRUE,   ## enable scrolling on Y axis
                                                    #autoWidth = TRUE, ## use smart column width handling
                                                    server = FALSE,   ## use client-side processing
                                                    dom = 'Bfrtip',
                                                    buttons = c('csv', 'excel')),
                                     caption = "Catch Outliers",
                                     #extensions = 'Buttons',
                                     rownames = FALSE)

  hr()

  output$TBL_Annual <- DT::renderDataTable(NewDat()$annual,selection=list(mode="single", target="cell"), filter = 'top',
                                     options = list(paging = TRUE,    ## paginate the output
                                                    pageLength = 10,  ## number of rows to output for each page
                                                    scrollX = TRUE,   ## enable scrolling on X axis
                                                    scrollY = TRUE,   ## enable scrolling on Y axis
                                                    #autoWidth = TRUE, ## use smart column width handling
                                                    server = FALSE,   ## use client-side processing
                                                    dom = 'Bfrtip',
                                                    buttons = c('csv', 'excel')),
                                     caption = "Annual Outliers",
                                     #extensions = 'Buttons',
                                     rownames = FALSE)

  hr()
  output$TBL_Area <- DT::renderDataTable(NewDat()$areaD,selection=list(mode="single", target="cell"), filter = 'top',
                                     options = list(paging = TRUE,    ## paginate the output
                                                    pageLength = 10,  ## number of rows to output for each page
                                                    scrollX = TRUE,   ## enable scrolling on X axis
                                                    scrollY = TRUE,   ## enable scrolling on Y axis
                                                    #autoWidth = TRUE, ## use smart column width handling
                                                    server = FALSE,   ## use client-side processing
                                                    dom = 'Bfrtip',
                                                    buttons = c('csv', 'excel')),
                                     caption = "Area Outliers",
                                     #extensions = 'Buttons',
                                     rownames = FALSE)

  hr()
  output$TBL_Comments <- DT::renderDataTable(NewDat()$comments,selection=list(mode="single", target="cell"), filter = 'top',
                                     options = list(paging = TRUE,    ## paginate the output
                                                    pageLength = 10,  ## number of rows to output for each page
                                                    scrollX = TRUE,   ## enable scrolling on X axis
                                                    scrollY = TRUE,   ## enable scrolling on Y axis
                                                    #autoWidth = TRUE, ## use smart column width handling
                                                    server = FALSE,   ## use client-side processing
                                                    dom = 'Bfrtip',
                                                    buttons = c('csv', 'excel')),
                                     caption = "comments",
                                     #extensions = 'Buttons',
                                     rownames = FALSE)

  hr()
  output$TBL_OFRIS <- DT::renderDataTable(NewDat()$fris,selection=list(mode="single", target="cell"), filter = 'top',
                                        options = list(paging = TRUE,    ## paginate the output
                                                       pageLength = 10,  ## number of rows to output for each page
                                                       scrollX = TRUE,   ## enable scrolling on X axis
                                                       scrollY = TRUE,   ## enable scrolling on Y axis
                                                       #autoWidth = TRUE, ## use smart column width handling
                                                       server = FALSE,   ## use client-side processing
                                                       dom = 'Bfrtip',
                                                       buttons = c('csv', 'excel')),
                                        caption = "FRIS Limits",
                                        #extensions = 'Buttons',
                                        rownames = FALSE)


  hr()
  output$TBL_Responses <- DT::renderDataTable(NewDat()$responses,selection=list(mode="single", target="cell"), filter = 'top',
                                     options = list(paging = TRUE,    ## paginate the output
                                                    pageLength = 10,  ## number of rows to output for each page
                                                    scrollX = TRUE,   ## enable scrolling on X axis
                                                    scrollY = TRUE,   ## enable scrolling on Y axis
                                                    #autoWidth = TRUE, ## use smart column width handling
                                                    server = FALSE,   ## use client-side processing
                                                    dom = 'Bfrtip',
                                                    buttons = c('csv', 'excel')),
                                     caption = "Responses",
                                     #extensions = 'Buttons',
                                     rownames = FALSE)


  #####################**********************************##################################
  #####################*      Query Tool Start
  #####################**********************************##################################



  observeEvent(input$Qsppgrpsel, {
    #req(credentials()$user_auth)
    # #browser()
    if ('All' %in% input$Qsppgrpsel) {
      spps <- sort(unique(df_spp$COMMON_NAME))
    } else{
      spps <- sort(unique(df_spp[df_spp$GROUPING %in% input$Qsppgrpsel,]$COMMON_NAME))
    }
    updateSelectInput(session, "Qsppsel", choices = c("None"="","All", spps), selected = "All")

  })

  observeEvent(input$clearQYRBtn, {
    #req(credentials()$user_auth)
    updateSelectInput(session, "Qyrsel", selected = character(0))
  })

  observeEvent(input$clearQMNTBtn, {
    #req(credentials()$user_auth)
    updateSelectInput(session, "Qmnthsel", selected = character(0))
  })
  observeEvent(input$clearQAREABtn, {
    updateSelectInput(session, "QAreasel", selected = character(0))
  })
  observeEvent(input$clearQSPPGBtn, {
    #req(credentials()$user_auth)
    updateSelectInput(session, "Qsppgrpsel", selected = character(0))
  })
  observeEvent(input$clearQSPPBtn, {
    #req(credentials()$user_auth)
    updateSelectInput(session, "Qsppsel", selected = character(0))
  })

  c("None"="","Standard Estimates", "Detailed Estimates")

  observeEvent(input$QQrysel, {
    #req(credentials()$user_auth)
    qryR <- "Select 'no query found' as query from dual"
    if (!is.null(Lcon)) {
      if (input$QQrysel == 'Estimates with Proportions') {
        qryR <- getdata(
          Lcon,
          "select sql from otolith_V1.creel_queries where use = -12 and name = 'iREC - Estimates with % Lodge Guided Shellfish Residency'"
        )
      } else if (input$QQrysel == 'Estimates with Proportions (BCF)') {
        qryR <-  getdata(
          Lcon,
          "select sql from otolith_V1.creel_queries where use = -12 and name = 'iREC - Estimates with % Lodge Guided Shellfish Residency (BCF)'"
        )
      } else if (input$QQrysel == 'Detailed Estimates') {
        qryR <-  getdata(
          Lcon,
          "select sql from otolith_V1.creel_queries where use = -12 and name = 'iREC - Detailed estimates with variances'"
        )
      }
    }
    qry <<- qryR[[1]]
    qryName <<- input$QQrysel
  })

  Qrslt <- eventReactive(input$Fetch_ests, {
    #req(credentials()$user_auth)
    req(input$Qyrsel)
    req(input$Qmnthsel)
    req(input$QAreasel)
    req(input$Qsppgrpsel)
    req(input$Qsppsel)

    yrs <- paste(input$Qyrsel, collapse = ',')

    # These bits adjust the stock query to filer or not on months, areas, species_groups and species

    if ('All' %in% input$Qmnthsel){mnth <- 1:12} else {mnth <- match(input$Qmnthsel, month.name)}
    sql2 <- " Select * from data "
    sql2 <- buildsql(sql2, "YEAR", paste0(yrs, collapse = ','))
    sql2 <- buildsql(sql2, "MONTH", paste0(mnth, collapse = ','))


    if ('All' %notin% input$QAreasel) {
      areas <- paste0("'", gsub(',', "', '", paste0(input$QAreasel, collapse = ',')), "'")
      sql2 <- buildsql(sql2, "AREA", areas)
    }
    if ('All' %notin% input$Qsppsel) {
      spp <- paste0("'", gsub(',', "', '", paste0(input$Qsppsel, collapse = ',')), "'")
      sql2 <- buildsql(sql2, "ITEM", spp)
    }
    if ('All' %notin% input$Qsppgrpsel) {
      sppgrp <- paste0("'", gsub(',', "', '", paste0(input$Qsppgrpsel, collapse = ',')), "'")
      sql2 <- buildsql(sql2, "ITEM_GROUP", sppgrp)
    }

    qry <- paste0(qry, sql2)
    # clipr::write_clip(qry)

    auto_notify(paste0("Query string length is ",stringr::str_length(qry)))

    # send the query string to the database using the Lcon connection - Should be the readonly lakehouse credentials
    if (!is.null(Lcon)){
    df_qrslt <<- getdata(Lcon, qry)
    # reduce the decimal places to 5 - makes them less distracting
    df_qrslt <- df_qrslt %>% mutate_if(is.numeric, round, digits = 5)
    # squish the result and the query name together to be returned
    rtn_list <- list("df_qrslt" = df_qrslt, "qryName" = qryName)
    return(rtn_list)
    } else {
      auto_notify("No connection to database")
    }
  })

  hr()
  output$TBL_QDATA <- DT::renderDT(
    Qrslt()$df_qrslt,
    filter = 'top',
    selection=list(mode="single", target="cell"),
    options = list(
      paging = FALSE,   ## paginate the output
      pageLength = 10,  ## number of rows to output for each page
      scrollX = TRUE,   ## enable scrolling on X axis
      scrollY = '400px',  # Set the height for vertical scrolling
      scrollCollapse = TRUE,   # Optional: collapses the table if content is less than scrollY
      autoWidth = TRUE,   ## use smart column width handling
      server = FALSE, ## use client-side processing
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'csv',
          filename = paste0(Qrslt()$qryName, ' ', now())
        ),
        list(
          extend = 'excel',
          filename = paste0(Qrslt()$qryName, ' ', now()),
          title =  paste0(
            paste0(input$Qyrsel, collapse = ','),
            paste0(input$Qmnthsel, collapse = ','),
            paste0(input$QAreasel, collapse = ','),
            paste0(input$Qsppgrpsel, collapse = ','),
            paste0(input$Qsppsel, collapse = ',')
          )
        )
      )
    ),
    callback = JS(
      "
        var table = table.table().node();
        var $parent = $(table).closest('.dataTables_scrollBody');
        $parent.on('scroll', function() {
          localStorage.setItem('scrollLeft', $parent.scrollLeft());
        });
        if(localStorage.getItem('scrollLeft')) {
          $parent.scrollLeft(localStorage.getItem('scrollLeft'));
        }
      "
    ),
    caption = "Estimates",
    extensions = 'Buttons',
    rownames = FALSE
  )



  hr()
  #####################**********************************##################################
  #####################*      FRIS Start
  #####################**********************************##################################


  observeEvent(input$Fmnthsel, {
    #req(credentials()$user_auth)
    # #browser()
    req(input$Fyrsel)
    yr_name <- input$Fyrsel
    mn_name <- input$Fmnthsel
    month <-  match(mn_name, month.name)
  })

  Fris <- eventReactive(input$Fetch_fris, {
    if (oracredsL) {
    #req(credentials()$user_auth)
    req(input$Fyrsel)
    req(input$FQrysel)

    mn_name <- input$Fmnthsel
    month <-  match(mn_name, month.name)

    fris_yr <- input$Fyrsel
    fris_mn <- month
    data_path <- system.file("extdata", "FRIS_Limits.txt", package = "iRECUIpkg")
    fris_qry <- read_file(data_path) # load the base of the query to the FRIS database
    fris_qry <- gsub(':Year',fris_yr, fris_qry)
    fris_qry <- gsub(':Month',fris_mn, fris_qry)
    fris_qry <- gsub(':Day','15', fris_qry)
    if(input$FQrysel == 'Detailed') {
      data_path <- system.file("extdata", "FRIS_Detailed.txt", package = "iRECUIpkg")
      fris_qry <- paste0(fris_qry, read_file(data_path)) # add some SQL to extract the details of the limits.   Not sure why I did it this way and not include the sql in the original file.
    } else {
      fris_qry <- paste0(fris_qry, "  select * from sum3
order by cast(pfma as number), species") # pull from the summary part of the SQL.
    }
    fris_data <- getdata(Lcon, fris_qry)
return(fris_data)
  } else { auto_notify(" No Connection to Database!")}
    })

output$TBL_FRIS <- DT::renderDT(Fris(),selection=list(mode="single", target="cell"), filter = 'top',
                                options = list(paging = TRUE,    ## paginate the output
                                               #pageLength = 10,  ## number of rows to output for each page
                                               scrollX = TRUE,   ## enable scrolling on X axis
                                               scrollY = TRUE,   ## enable scrolling on Y axis
                                               autoWidth = TRUE, ## use smart column width handling
                                               server = FALSE,   ## use client-side processing
                                               dom = 'Bfrtip',
                                               buttons = c('csv', 'excel'),
                                               lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')) # Allow user to select 'All'
                                ),
                                caption = "FRIS Rules",
                                extensions = 'Buttons',
                                rownames = FALSE)

  # ---- Bias Start -------

observeEvent(input$PostBIAS, {
  #req(credentials()$user_auth)
  req(input$BiasFile)
  inFile <- input$BiasFile
  rws <- 0
  gtgC <- FALSE
  gtgK <- FALSE
  if (input$dest_slider == "CREST Test" & oracredsT) {
    gtgC <- TRUE
    dest <- "T"
  } else if (input$dest_slider == "CREST Prod" &
             oracredsP) {
    gtgC <- TRUE
    dest <- "P"
  } else if (input$dest_slider == "KREST") {
    gtgK <- TRUE
    dest <- 0
  }

  if (gtgC || gtgK) {
    tryCatch({
      # Main Expression
      if (gtgC) {
        rws <- Load_C_Bias(input$BiasFile$datapath, dest)
      } else {
        SetupKrest("P")
        rws <- Load_K_Bias(input$BiasFile$datapath)
      }},

      error = function(e) {
        # Error Handler
        auto_notify(paste("Load failed:", e$message))
        rws <- 0
      },

      warning = function(w) {
        # Warning Handler
        auto_notify(paste("Warning during write:", w$message))
        invokeRestart("muffleWarning")
      },

      finally = {
        # Cleanup / Log action
        auto_notify("Loading data finished.")
      })

  }
  output$Bias_message <- renderText({
    paste0('Loaded ', rws, ' Rows')
  })

})

BiasDat <- eventReactive(input$FetchBIAS, {
  req(input$BiasFile) ## ?req #  require that the input is available
  bias_df <- read.csv(input$BiasFile$datapath)
  return(bias_df)
})


output$TBLBIAS <- DT::renderDataTable(
  BiasDat(),
  selection = list(mode = "single", target = "cell"),
  options = list(
    paging = TRUE,
    ## paginate the output
    pageLength = 10,
    ## number of rows to output for each page
    scrollX = TRUE,
    ## enable scrolling on X axis
    scrollY = TRUE,
    ## enable scrolling on Y axis
    #autoWidth = TRUE, ## use smart column width handling
    server = FALSE,
    ## use client-side processing
    dom = 'Bfrtip',
    buttons = c('csv', 'excel')
  ),
  caption = "Bias Correction Factors",
  #extensions = 'Buttons',
  rownames = FALSE
)

#####################**********************************##################################
#####################*      Estimates Start
#####################**********************************##################################
output$selected_database <- renderText({
  paste("The selected Database is:", input$dest_slider)
})

output$console_logs <- renderPrint({
  logs <- log_reader()
  cat(paste(logs, collapse = "\n"))
})

observeEvent(input$Load_Ests, {
  #req(credentials()$user_auth)
  req(input$est_input)
  inFile <- input$est_input
  rws <- 0
  gtgC <- FALSE
  gtgK <- FALSE
  if (input$dest_slider == "CREST Test" & oracredsT) {
    gtgC <- TRUE
    dest <- "T"
  } else if (input$dest_slider == "CREST Prod" & oracredsP) {
    gtgC <- TRUE
    dest <- "P"
  } else if (input$dest_slider == "KREST") {
    gtgK <- TRUE
    dest <- 0
  }


  tryCatch({
    # Main Expression

    if (gtgC) {
      rws <- Load_C_Ests(inFile$datapath, inFile$name, dest, session = session)
    } else if (gtgK) {
      SetupKrest("P")
      rws <- Load_K_Ests(inFile$datapath, inFile$name, dest, session = session)
    }  else {
      output$Est_message <- renderText({
        'No credentials file found'
      })
    }


  }, error = function(e) {
    # Error Handler
    auto_notify(paste("Load failed:", e$message))
    rws <- 0
  }, warning = function(w) {
    # Warning Handler
    auto_notify(paste("Warning during write:", w$message))
    invokeRestart("muffleWarning")
  }, finally = {
    # Cleanup / Log action
    auto_notify("Loading data finished.")
  })


  output$Est_message <- renderText({
    paste0('Loaded ', rws, ' Rows')
  })

})

#####################**********************************##################################
#####################*      Responses Start
#####################**********************************##################################
output$R_selected_database <- renderText({
  paste("The selected Database is:", input$R_dest_slider)
})

observeEvent(input$Load_Resp, {
  #req(credentials()$user_auth)
  req(input$resp_input)
  inFile <- input$resp_input
  rws <- 0
  gtgC <- FALSE
  gtgK <- FALSE
  if (input$R_dest_slider == "CREST Test" & oracredsT) {
    gtgC <- TRUE
    dest <- "T"
  } else if (input$R_dest_slider == "CREST Prod" & oracredsP) {
    gtgC <- TRUE
    dest <- "P"
  } else if (input$R_dest_slider == "KREST") {
    gtgK <- TRUE
    dest <- 0
  }
  if (gtgC) {
    datapath <- inFile$datapath
    srcpth <- "R/iRECtoCREST/"
  } else if (gtgK) {
    SetupKrest("P")
    datapath = ''
    srcpth <- "R/iRECtoKREST/Responses/"
  }  else {
    output$Est_message <- renderText({
      'No credentials file found'
    })
  }
  if (gtgC || gtgK) {
    withCallingHandlers({
      df <- read_sav(inFile$datapath)
      if ("TYPE" %in% names(df)) {
        yr <- gsub("[^0-9.-]", "", df$TYPE[[1]])
        mnth_key <- tolower(df$TYPE[[1]])
      } else {
        for (y in 2012:year(now())) {
          if (grepl(y, inFile$name)) {
            yr <- y
            break
          }
        }
        mnth_key <- tolower(inFile$name)
      }

      for (m in month_nms) {
        if (grepl(tolower(m), mnth_key)) {
          mnth_str <- m
          break
        }
      }

      destpath <- dirname(inFile$datapath)
      datafile <- inFile$name

      if (mnth_str %in% c('January', 'February', 'March')) {
        yr_name <- paste0(as.numeric(yr) - 1, '-' , as.numeric(substr(yr, 3, 4)))
      } else {
        yr_name <- paste0(yr, '-' , as.numeric(substr(yr, 3, 4)) + 1)
      }
      if(gtgC) {
        rws <- Write_C_ResponsesIn(datapath, datafile, yr_name, yr,  mnth_str, df, "Response")
      } else if (gtgK){
      rws <- Write_K_ResponsesIn(srcpth, destpath, datapath, datafile, yr_name, df)
      }
    }, error = function(e) {
      # Error Handler
      auto_notify(paste("Load failed:", e$message))
      rws <- 0
    }, warning = function(w) {
      # Warning Handler
      auto_notify(paste("Warning during write:", w$message))
      invokeRestart("muffleWarning")
    }, finally = {
      # Cleanup / Log action
      auto_notify("Loading data finished.")
    })


    output$Resp_message <- renderText({
      paste0('Loaded ', rws, ' Rows')
    })
  }
})
observeEvent(input$load_adjs, {
  #req(credentials()$user_auth)

  req(input$adj_input)
  inFile <- input$adj_input
  adj_df <- read_csv(inFile$datapath,show_col_types = FALSE)
  dte <- GetYearMonth(adj_df)
  yr <- year(dte)
  mnth_str <- months.Date(dte)
  adjpath <- ''
  destpath <- dirname(inFile$datapath)
  adj_file <- inFile$name
  rws <- 0

  if (mnth_str %in% c('January', 'February', 'March')) {
    yr_name <- paste0(as.numeric(yr) - 1, '-' , as.numeric(substr(yr, 3, 4)))
  } else {
    yr_name <- paste0(yr, '-' , as.numeric(substr(yr, 3, 4)) + 1)
  }

  gtgC <- FALSE
  gtgK <- FALSE
  if (input$R_dest_slider == "CREST Test" & oracredsT) {
    gtgC <- TRUE
    dest <- "T"
  } else if (input$R_dest_slider == "CREST Prod" & oracredsP) {
    gtgC <- TRUE
    dest <- "P"
  } else if (input$R_dest_slider == "KREST") {
    gtgK <- TRUE
    dest <- 0
  }


  if(gtgC) {
    adjpath <- inFile$datapath
    rws <- Write_C_ResponsesIn(adjpath, adj_file, yr_name, yr,  mnth_str, adj_df, "Adjust")
  } else if (gtgK){
    SetupKrest("P")
    rws <-   write_K_AdjustsIn(sourcepath, destpath, adjpath, adj_file, yr_name, yr, mnth_str , adj_df)
  }

  output$CSV_message <- renderText({
    paste0('Loaded ', rws, ' Rows')
  })
})

observeEvent(input$load_excs, {
  #req(credentials()$user_auth)
  req(input$exc_input)
  req(input$yrsel)
  inFile <- input$exc_input
  yr_name <- input$yrsel

  gtgC <- FALSE
  gtgK <- FALSE
  if (input$R_dest_slider == "CREST Test" & oracredsT) {
    gtgC <- TRUE
    dest <- "T"
  } else if (input$R_dest_slider == "CREST Prod" & oracredsP) {
    gtgC <- TRUE
    dest <- "P"
  } else if (input$R_dest_slider == "KREST") {
    gtgK <- TRUE
    dest <- 0
  }
  excpath <- inFile$datapath
  excfile <- inFile$name
  auto_notify("reading exclude")
  if (endsWith(tolower(excfile), ".xlsx")) {
    exc_df <- openxlsx::read.xlsx(excpath, sheet = 1, detectDates = TRUE)
  } else {
    exc_df <- read_csv(excpath, show_col_types = FALSE)
  }
  auto_notify("done reading exclude")


  if(gtgC) {
    rws <- write_C_ExcludesIn(excpath, excfile, yr_name, exc_df)
  } else if (gtgK){
    SetupKrest("P")
    write_K_ExcludesIn(dirname(inFile$datapath), '', paste0(yr_name,'_',inFile$name), exc_df, yr_name)
  }



  output$EXC_message <- renderText({
    paste0('Loaded ', nrow(exc_df), ' Rows')
  })
})


}

# Run the Application
shinyApp(ui = ui, server = server)
#}
