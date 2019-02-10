library(shiny) 
library(ggplot2) 
library(stringr) 
# library(xtable) 
library(caret) 
library(lda)
library(randomForest)
library(gbm)
library(C50)
library(ada)
library(pROC)
library(e1071)
# source("support/structures.R")      # config moved to global.R
source("support/sources.R") 
source("support/commonfunctions.R") 
source("support/cleanfunctions.R") 
# source("support/datasets.R") 

shinyServer(function(input, output) { 

  # Global reactives
  ld <- reactiveValues(set = NULL,                      # the original train set
                       name = NULL,        
                       loadCleansed = FALSE,            # load original or cleaned?
                       loadCleansedName = "",           # (only in case of exttrain)
                       loadToScore = FALSE,
                       score = NULL,                    # the set to score (extscore)
                       scoreName = NULL, 
                       colnames = NULL, coltypes = NULL,# meta for validation?
                       message = "", 
                       exceptions = NULL) 
  cd <- reactiveValues(set = NULL,              # the cleansed train set 
                       name = NULL,             # NULL indicates loaded external set
                       # external = NULL,       # with no cleansed ('external' is alternative - not used)
                       seq = list(),            # the actual sequence of actions 
                       seqdf = data.frame(),    # for display reasons 
                       saved = FALSE,           # latest, in-memory ver of set saved?
                                                # affected by load, clean, save 
                       modelList = NULL,
                       score = NULL,            # the cleaned set to score (extscore)
                       scoreName = NULL,
                       message = NULL,          # for informing the user
                       statistics = NULL,       # column statistics 
                       meta = NULL,             # column metadata (type, etc)
                       summary = NULL)          # combines statistics & meta 
  
  model <- reactiveValues(formula = NULL, formulaText = NULL, 
                          fit = NULL,     # fit is best model from train/tune
                          name = NULL,    # fit contains all: (tune) results, bestTune, 
                          message = NULL, #     tuneGrid, response, predictors, testIndices
                          train = NULL, test = NULL, trainResponse = NULL, # for testing only 
                          tuneGrid = NULL, results = NULL, 
                          bestTune = NULL, finalModel = NULL,
                          exists = FALSE,
                          assessResponse = NULL,            # same response all models
                          assessResultsdf = data.frame(),   # assessment results
                          assess = NULL, assessName = NULL, # for testing only
                          assessPredictors = NULL,          # for each assessed model
                          score = NULL, scoreName = NULL, scorePredictors = NULL)


  # LOAD -- LOAD -- LOAD -- LOAD -- LOAD -- LOAD -- LOAD -- LOAD
 
  output$sourceControls <- renderUI({ 
    # for each data source render relative controls
    switch(input$source, 
           "working" = {                            # working set (original- cleansed)
             datasets <- filelist(path = 'data/ld',
                                 name = NULL)       # func filelist() from sources 
             list( 
               selectInput(inputId = "workingset", 
                           label = "Choose working set:", 
                           choices  = as.list(datasets)) 
             )
           },
           "exttrain" = {                           # external set -> working set
             list(
               selectInput(inputId  = "extsource", 
                           label    = "Load from data source:", 
                           choices  = 
                             list("Local text file (delimited/csv format)" = "file",
                             "Database (SQL-query)" = "db",
                             "Dataset in R package" = "package")) 
             )
           },
           "extscore" = {                           # external set to score
             list(
               selectInput(inputId  = "extsource", 
                           label    = "Load from data source:", 
                           choices  = list(
                             "Local text file (delimited/csv format)"= "file",
                             "Database (SQL-query)" = "db"))
             )  
           }) 
  }) 
 
  output$sourceControlsWorking <- renderUI({
    if (input$source == 'working') {
      datasets <- filelist(path = 'data/cd/set',
                          name = input$workingset)  # func filelist() from sources
      list( 
        selectInput(inputId = "cleansedset", 
                    label = "Choose cleansed version of working set:", 
                    choices  = as.list(datasets)), 
        checkboxInput(inputId = "loadCleansed", 
                      label = "Load only cleansed set? (useful for large
                               external sets, that are significantly 
                               sliced after being cleansed):", 
                      value = FALSE),
        
        br(), 
        fluidRow(
          column(width = 6, 
                 actionButton("load", "Load Working Set") 
          ),
          column(width = 2,
                 br()  # no action here
          )
        )
      )
    }
  }) 
 
  output$sourceControlsExternal <- renderUI({ 
    if (is.null(input$extsource) ) return(NULL)
    # otherwise: initially, when input$extsource IS NULL, if or swich gives an error
    # "Error in if (input$extsource == "package") { : argument is of length zero" OR 
    # "Error in switch(input$extsource, file = { : EXPR must be a length 1 vector"
    if (input$source != 'working') {
      switch(input$extsource, 
             file = {                               # external set -> working set
               list(
                 fileInput(inputId = "file", 
                           label = "Choose local csv file:"),
                 textOutput("fileSeparatorsText"),
                 br(), br(),
                 actionButton("load", "Load External Set")
               )
             },
             db = {                                 # external set -> working set
               list(
                 textInput(inputId = "sqlname", 
                           label = "Name of the obtained dataset:"),
                 textInput(inputId = "constring", 
                           label = "DB connection String:"), 
                 textInput(inputId = "sqlstring", 
                           label = "SQL Query (text):"), 
                 br(), br(),
                 actionButton("load", "Load External Set")
               )  
             },
             package = {                            # external set -> working set
               list(
                 textInput(inputId = "package", 
                           label = "Enter the name of the package 
                                  that contains the dataset:"),
                 textOutput("packageDatasetsText"), 
                 br(),
                 actionButton("loadPackage", "Load Package")
               )  
             })
    } 
  }) 
  output$fileSeparatorsText <- renderText({
    "The application accepts delimited text files. Accepted value separators 
    (delimeters) are the colon (,), the semicolon (;), the tab symbol and the   
    pipe (|). The application automatically detects any of the above symbols 
    as a value separator."}) 
  output$packageDatasetsText <- renderText({
    "Press the \'Load Package\' button to load the package. Given that the 
    package is installed on the computer/server and loads successfully, you 
    will be presented with a list of available datasets in the package, to 
    select from."}) 
    
  output$sourceControlsPackage <- renderUI({ 
    if (is.null(input$extsource) ) return(NULL)
    # otherwise: initially, when input$extsource IS NULL, if or swich gives an error
    # "Error in if (input$extsource == "package") { : argument is of length zero" OR 
    # "Error in switch(input$extsource, file = { : EXPR must be a length 1 vector"
    if (input$source == "exttrain") {
      if (input$extsource == 'package') {
        list( 
          selectInput(inputId = "packageDataset", 
                      label = "List of datasets in package:", 
                      choices  = as.list(packageDatasets())), 
          br(), 
          actionButton("load", "Load External Set")
        )
      }
    } 
  }) 
  packageDatasets <- eventReactive(input$loadPackage, { 
    # vector of package datasets - determined dynamically, once package is known 
    # NULL if package not given or if package can not be loaded, see ls.package()
    if(input$package=="") return(NULL)
    datasets <- ls.package(package = input$package, type = "data.frame")
    return(datasets)
  })
  
  fileInfo <-  eventReactive(input$file, {
    # file information - sep/header are determined dynamically
    # additionally use size to dynamically determine data mgmt (memory/disk)
    
    if(is.null(input$file)) return(NULL)
    
    fullname <- input$file$name                               # name with extension
    name <- unlist(strsplit(fullname, split='.', fixed=T))[1] # dataset name 
    size <- input$file$size                                   # (dataname)
    type <- input$file$type
    path <- input$file$datapath                               # path + fullname
    
    # detect separator
    con <- file(path, "r", blocking = FALSE)
    firstrow <- readLines(con=con, n=1)
    close(con)
    separators <- c(",", ";", "\t", "|")
    occurrences <- sapply(separators, 
                          function(sep) 
                            length(unlist(regmatches(firstrow, 
                                                     gregexpr(sep, firstrow, 
                                                              fixed=T))))
    )
    ind_max <- which.max(occurrences)
    ind_nonzero <- which(occurrences!=0, arr.ind = T)
    warn = ""
    if (length(ind_nonzero)>1) {
      warn <- "only one non zero"
    } else if (ind_max!=ind_nonzero) 
      warn <- "max must be equal non zero"
    sep <- separators[ind_max]
    
    # detect header existance
    nums_in_header <- as.numeric(unlist(strsplit(firstrow, split=sep, fixed=T)))
    decision <- sum(is.na(nums_in_header)) >= length(nums_in_header)
    if (decision == T) header <- T else header <- F
    
    # single file: return vector or list (not dataframe) 
    list(dataname = name, size = size, type = type, path = path, 
         sep = sep, header = header) 
  }) 
  
  observeEvent(input$load, {
    # load data to ld$set or ld$score (original set), cd$set (cleansed set) and
    # cd$seq (sequence of actions) - keep in sync with input$cleanSave - 
    # initialize models apropriately - both ld,cd sets are initialized with values
    
    # reset ld, cd set info, model structures (build, assess, score), messages, etc
    ld$message <- NULL
    cd$message <- NULL
    ld$loadCleansed <- FALSE; ld$loadCleansedName <- ""
    model$fit <- NULL
    # model$assessResultsdf <- data.frame()
    ld$loadToScore <- FALSE
    success <- FALSE
    
    # load from rds (working) or file or package (or sql query), depending on input
    if (input$source == 'working') {
        # reset model structures (assess results)
        model$assessResultsdf <- data.frame()
        # load cleansed set
        cd$name <- input$cleansedset
        filename <- paste0(input$workingset, "_", input$cleansedset)
        path <- "data/cd/set"
        cd$set  <- readRDS(file = paste0(path, "/", filename, ".rds"))
        path <- "data/cd/seq"
        cd$seq  <- readRDS(file = paste0(path, "/", filename, ".rds"))
        cd$seqdf <- convertSequenceToDf(cd$seq)
        # cd$set <- execSequence(ld$set, cd$seq) # !! for testing seq execution (OK)
        
        # load original, external set or cleansed set
        ld$name <- input$workingset
        if (input$loadCleansed) {
          ld$set <- cd$set       # possibly cd is a small sample (less than 1%)
          ld$loadCleansed <- TRUE
          ld$loadCleansedName <- cd$name
          ld$message <- 
            paste0("Success: Working set loaded.",
                   " Cleansed set (train/test set) ", 
                   "'", ld$name, ", ", cd$name, "'", " (", nrow(cd$set), " rows)",
                   " and clean history (sequence of clean actions) loaded.", 
                   " Original (external) set ", "'", ld$name, "'", " not loaded.")
        } else {
          path <- "data/ld"      # path <- paste0(getwd(), "/data/ld")
          ld$set  <- readRDS(file = paste0(path, "/", ld$name, ".rds"))
          ld$loadCleansed <- FALSE
          ld$loadCleansedName <- ""
          ld$message <- 
            paste0("Success: Working set loaded.",
                   " Original (external) set ", "'", ld$name, "'", 
                   " (", nrow(ld$set), " rows) loaded.",
                   " Cleansed set (train/test set) ", 
                   "'", ld$name, ", ", cd$name, "'", " (", nrow(cd$set), " rows)",
                   " and clean history (sequence of clean actions) loaded.")
        }
        success <- TRUE
        # in-memory cleansed set same as saved cleansed set 
        cd$saved <- TRUE
  
    } else if (input$source == 'exttrain') {
      # reset model structures (assess results)
      model$assessResultsdf <- data.frame()
      switch(input$extsource, 
         file = {
           if (is.null(input$file)) {
             ld$message <- "No file selected. Please select file." 
           } else {
             ld$set <- read.csv(file = fileInfo()$path, header = fileInfo()$header, 
                                sep = fileInfo()$sep, stringsAsFactors = FALSE)
             ld$name <- fileInfo()$dataname 
             success <- TRUE 
           }
         },
         package = {
           if (input$package=="" || input$packageDataset=="") {
             ld$message <- "No package name entered and/or no selected dataset."
           } else if (!input$packageDataset %in% 
                        ls.package(input$package, "data.frame")) {
             ld$message <- "Package - dataset list mismatch. 
                            Please enter/load correct package."
           } else {
             ld$set <- eval(parse(text = paste(input$package, '::', 
                                               input$packageDataset, sep="")))  
             ld$name <- input$packageDataset 
             success <- TRUE 
           } 

         },           
         db = {
           # ld$set <- execute sql
           # ld$name <- input$sqlname 
           success <- TRUE 
         }) 
      if (success) { 
        # external source, so, content/meta of cleansed set initialized appropriately
        cd$set <- ld$set
        cd$name <- NULL            # NULL name indicates set from external source
        cd$seq <- list(); cd$seqdf <- data.frame() 
        # in-memory cleansed set NOT saved yet 
        cd$saved <- FALSE
        ld$message <- 
          paste0("Success: External set ", "'", ld$name, "'", " loaded (",
                 nrow(ld$set), " rows and ", length(ld$set), " columns).",
                 " You need to prepare (cleanse) the set for model building 
                 and assessment (prepare the train/test set).")
      }
    
    } else {    # input$source == 'extscore'  # identical structure as matching ld$set
      # do not reset assessing model structures
      # model$assessResultsdf <- data.frame()
      if (is.null(cd$name)) {  # no cleansed set available (loaded or saved)
        ld$message <- paste0("No cleansed (working) set available. Please load 
                             a working set or clean a loaded, external set (and
                             save it) before trying to load a set to score.")  
      } else {
        switch(input$extsource, 
           file = {
             if (is.null(input$file)) {
               ld$message <- "No file selected. Please select file."
             } else {
               ld$score <- read.csv(file = fileInfo()$path, header = fileInfo()$header, 
                                    sep = fileInfo()$sep, stringsAsFactors = FALSE) 
               ld$scoreName <- fileInfo()$dataname
               success <- TRUE
             } 
           },           
           db = {
             # ld$score <- execute sql
             ld$scoreName <- input$sqlname
             success <- TRUE
           }) 
        if (success) {
          ld$message <- 
            paste0("Success: External set to score ", 
                   "'", ld$scoreName, "'", " loaded (", 
                   nrow(ld$score), " rows and ", length(ld$set), " columns - 
                   identical format as the original, uncleansed set).")
          ld$loadToScore <- TRUE
        }
      }
 
    }
 
    # reset global variabless, summarizations etc
    if (success) { 
      if (ld$loadToScore == FALSE) {
        ld$score <- NULL                  # NUll, if new train/test set is loaded
      } 
      cd$score <- NULL                    # will be populated duing scoring
      out <- summarize(cd$set) 
      cd$statistics <- out$statistics 
      cd$meta <- out$meta
      cd$summary <- out$summary 
    }

  }) 

  output$loadMessage <- renderText({
    ld$message
  }) 
  
  # LOAD main -- LOAD main -- LOAD main -- LOAD main -- LOAD main
  # in this and subsequent tabls use either ld or cd (both are loaded) to check
  # what to display
  
  output$loadTitle <- renderText({
    if(is.null(cd$set)) return(NULL)  # use either ld or cd (both are loaded)
    paste0("External set for model building: ", 
           "'", ld$name, "'",
           " Cleansed set (train/test set): ", 
           "'", ifelse(is.null(cd$name), "-", cd$name), "'",
           " External set to score: ",
           "'", ifelse(ld$loadToScore, ld$scoreName, "-"), "'")
  }) 
  
  output$deleteWorkingSet <- renderUI({
    # better move to tab clean - also better check cd$external
    
    if(is.null(cd$set)) return(NULL)  # use either ld or cd (both are loaded)
    if (input$source == 'working') {
      list(
        br(),
        actionButton("deleteWorkingSet", "Delete Working Set")
        )
    }
  }) 
  
  output$loadText1 <- renderText({
    if(is.null(cd$set)) return(NULL)
    "See below the content of the loaded, external dataset, which you first need 
    to prepare (cleanse) and save prior to using in exploratory and predictive 
    analysis. Once saved, the derived, cleansed version of the external set actually 
    becomes the working, train/test set that will be used for data exploration 
    and predictive modeling. Predictive modeling includes model building (on 
    training data) and model assessment (on test data). Both the train set and the  
    test set are partitions of the cleaned, working set. Go to the next tab to clean 
    the data and save the resulting cleansed set."
    })
  output$loadText2 <- renderText({  
    if(is.null(cd$set)) return(NULL)
    "Once you have a persistent working set loaded, the original, external set
    and the derived, cleansed set are both loaded, unless you choose to only 
    load (and display) the possibly much smaller, cleansed set. Data cleaning  
    (and data engineering) are essential data preparation steps in exploratory 
    and predictive analysis."
    })
  output$loadText3 <- renderText({ 
    if(is.null(cd$set)) return(NULL)
    "Data views like the following also appear in subsequent tabs. In such  
    tabular views of data, you can navigate across (pages of) observations,  
    sort observations by column(s), or apply filters to the entire dataset or  
    to particular column(s) (given that a particular functionality is available 
    for a specific data-view)."
    }) 
  
  output$loadSetTitle <- renderText({
    if(is.null(cd$set)) return(NULL)
    if (ld$loadToScore) {
      paste0("The external set to score: ", 
             "'", ld$scoreName, "'", 
             " (", nrow(ld$score), " observations, in total)")
    } else {
      if (ld$loadCleansed) {
        paste0("The cleansed set: ",
               "'", ld$name, ", ", ld$loadCleansedName, "'",
               " (", nrow(cd$set), " observations, in total)")
      } else {
        paste0("The original, external set: ", 
               "'", ld$name, "'", 
               " (", nrow(ld$set), " observations, in total)")
      }
    } 
  }) 
  output$loadSet <- DT::renderDataTable(expr = {
    if(is.null(cd$set)) return(NULL) # null table returns null, but here more control
    # return(DT::datatable(ld$set, options = list(lengthChange = FALSE)))
    
    if (ld$loadToScore) set <- ld$score else set <- ld$set
    # OR: set <- ifelse(ld$loadToScore, ld$score, ld$set)
    if(nrow(set) > cdRowsToDisplay) {
      DT::datatable(set[1:cdRowsToDisplay,], 
                    options = list(lengthChange = FALSE, autoWidth = FALSE))
    } else {
      DT::datatable(set, 
                    options = list(lengthChange = FALSE, autoWidth = FALSE))
    } 
  }) 
 
  
  # CLEAN -- CLEAN -- CLEAN -- CLEAN --CLEAN -- CLEAN CLEAN -- CLEAN
  
  output$cleanTitleSidebar <- renderText({ 
    if(is.null(cd$set)) return(NULL)
    paste0("Apply clean actions on set ", "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'") 
  }) 
  output$cleanText1Sidebar <- renderText({ 
    if(is.null(cd$set)) return(NULL)
    paste0("Prepare the loaded dataset for analysis, by applying cleaning actions  
           on it, one at a time. A cleansing action may either apply on column(s)  
           (such actions include factorization and scaling) or to rows (these are 
           actions like filtering or sampling), or to both (such as outlier 
           detection/handling).")
  }) 
  output$cleanText2Sidebar <- renderText({ 
    if(is.null(cd$set)) return(NULL)
    paste0("Applied changes, reflecting a successful cleaning action, directly
           affect the tabular views of set summary and content, to the right
           panel.")
  }) 
  
  output$cleanAction <- renderUI({ 
    if(is.null(cd$set)) return(NULL) 
    list( 
      selectInput(inputId = "cleanAction", 
                  label = "Select cleaning action:", 
                  choices  = cleanActionsList, 
                  selected = cleanActionsList[[1]]) 
    )
  }) 
 
  output$cleanActionControls <- renderUI({ 
    if(is.null(cd$set)) return(NULL)
    # colnames values are column names - colnames labels contain names + types
    # labels are displayed in widgets - values are used by the server 
    colnames <- colnames(cd$set)
    coltypes <- coltypes(cd$set)
    is.factorizable <- (coltypes %in% c('integer', 'character', 'logical')) 
    is.number <- (coltypes %in% c('integer', 'numeric')) # native: is.numeric()
    is.factor <- (coltypes %in% c('factor'))             # overrides native func
    is.character <- (coltypes %in% c('character')) 

    if(is.null(input$cleanAction)) return(NULL) 
    switch(input$cleanAction, 
           select = {
             list( 
               selectInput(inputId = "columns", 
                           label = "Select/rearrange or exclude columns:", 
                           choices  = as.list(colnames),
                           multiple = TRUE), 
               checkboxInput(inputId = "remove", 
                             label = "Remove (instead of selecting) columns?", 
                             value = FALSE) 
             )
           }, 
           filter = {
             list( 
               textInput(inputId = "exprText", 
                         label = "Enter filtering expression (logical expression 
                                  built on one or more columns):")
             )
           }, 
           sampleset = { 
             list( 
               sliderInput(inputId = "samplePercent", 
                           label = "Sample Percent:",
                           min = 0.00, max = 1.00, step = 0.01, 
                           value = 0.50), 
               checkboxInput(inputId = "stratified", 
                             label = "Stratified Sample?", FALSE),
               selectInput(inputId = "stratifyOnColumn", 
                           label = "Stratify according to column:", 
                           choices  = as.list(colnames[is.factor]))
             )
           }, 
           factorize = { 
             list( 
               selectInput(inputId = "columns", 
                           label = "Select columns to categorize (categorizable
                                   columns are those of character or integer type ):", 
                           choices  = as.list(colnames[is.factorizable]),
                           multiple = TRUE), 
               numericInput(inputId = "threshold", 
                            label = "Apply factorization only if all selected   
                                     columns have at most as many distinct values:", 
                            value = 5,
                            min = 2, max = 100, step = 1) 
             )
           }, 
           discretize = {
             list( 
               selectInput(inputId = "column", 
                           label = "Select column to discretize (i.e. group 
                                    column values into bins or intervals):", 
                           choices  = as.list(colnames[is.number])), 
               textInput(inputId = "breaks", 
                         label = "Enter breaks (that split the overall range of 
                                  values in  intervals) separated by ';':"), 
               textInput(inputId = "levels", 
                         label = "Enter names for the corresponding intervals  
                                  (lavels), separated by ';':"), 
               #h5(strong("Add a new column with the discretized data")),
               textInput(inputId = "newcolumn", 
                         label = "Discretized column name (give name of 
                                  existing variable to replace, or give new 
                                  name to add new column):")
             )
           }, 
           group = {
             list( 
               selectInput(inputId = "column", 
                           label = "Select (categorical) column to group:", 
                           choices  = as.list(colnames[is.factor])), 
               textInput(inputId = "groups", 
                         label = "Enter groupings of current values (values in 
                                  each group separated by ',', groups by ';':"), 
               textInput(inputId = "levels", 
                         label = "Enter group names, one for each group (lavels), 
                                 separated by ';':"), 
               textInput(inputId = "newcolumn", 
                         label = "New column name (with coarser grouping):")
             )
           }, 
           transform = { 
             list( 
               selectInput(inputId = "column", 
                           label = "Select column to transform:", 
                           choices  = as.list(colnames[is.number])), 
               selectInput(inputId = "transformation", 
                           label = "Select transformation function:", 
                           choices  = list("Logarithm (neperian)" = "log",
                                           "Square Root" = "sqrt"))
             )
           }, 
           derive = {
             list( 
               textInput(inputId = "exprText", 
                         label = "Enter expresion for derived variable calculation  
                                  (expression may combine many existing columns):"), 
               textInput(inputId = "newcolumn", 
                         label = "Name of derived variable (give name of 
                                  existing variable to replace, give new 
                                  name to add new column):")
             )
           }, 
           scale = {
             list( 
               selectInput(inputId = "columns", 
                           label = "Scale which columns:", 
                           choices  = as.list(colnames[is.number]),
                           multiple = TRUE) 
             )  
           }, 
           missing = {
             list( 
               selectInput(inputId = "columns", 
                           label = "Select columns to investigate for 
                                    missing values:", 
                           choices  = as.list(colnames),
                           multiple = TRUE), 
               br(),
               textInput(inputId = "indicators", 
                         label = "Search for specific strings or numbers:"),  
               p("Express missing-value indicators (strings or numbers) as 
                 sequences of indicators, separated by semicolons (like in 
                 \'?;999;-\' or in \'99;999\'"),
               br(),
               selectInput(inputId = "handling",                
                           label = "Missing value handling:", 
                           choices = list("Convert to NA (NA means 'no value')" 
                                          = "na",
                                          "Remove entire row" = "rmrow",
                                          "Substitute with column mean (if numeric)"
                                          = "mean")),
               selectInput(inputId = "convert", 
                           label = "Convert to numeric (if possible)?", TRUE)
             )
           }, 
           outliers = {
             list( 
             )  
           } 
    )
  }) 
  
  output$cleanActionButtons <- renderUI({
    if(is.null(cd$set)) return(NULL)
    list( 
      br(),
      fluidRow(
        column(width = 6, 
               actionButton("clean", "Apply Cleaning Action")
               ), 
        column(width = 2 
               #, actionButton("discard", "Discard Last Action")
               )
        )
    )
  }) 
  
  observeEvent(input$clean, { 
    # pay attention with input$cleanAction: can't be NULL
    # observe more than one event? Apply Cleaning Action or Apply Cleaning Seq?
    
    cd$message <- NULL
    
    # dispatch to clean function: composes 'arg=value' pairs & executes clean function
    cleanActionArgsNames <- cleanActionsArgs[[input$cleanAction]] # char vector
    # cleanActionArgsValues <- input[[cleanActionArgsNames]]      # not working 
    cleanActionArgsValues <- list()     # use loop instead: list of args values
    for ( arg in 1:length(cleanActionArgsNames) ) {
      cleanActionArgsValues[[arg]] <- input[[cleanActionArgsNames[arg]]]
    }
    # out is a list containing success, set, message, arg_value_pairs
    out <- execAction(set = cd$set, action = input$cleanAction, 
                      argsNames = cleanActionArgsNames, 
                      argsValues = cleanActionArgsValues)
    
    # use execution output to update messages and if success to update working set:
    #                         update set, update summary, increment action sequence               
    cd$message <- out$message
    
    if (out$success) {
      cd$set <- out$set 
      
      # update seq's ith action (notice the evaluation of the arglist expression
      ' arglistText <- paste0("list(", 
                            paste(out$arg_value_pairs, collapse = ", "), ")")
      arglist <- eval(parse(text = arglistText))
      arg list example: list(columns = input$columns, 
                               remove = input$remove) '
      cd$seq[[length(cd$seq)+1]] <- 
        list(action = input$cleanAction,
             input  = list(argsNames       = cleanActionArgsNames, 
                           argsValues      = cleanActionArgsValues, 
                           arg_value_pairs = out$arg_value_pairs), 
             result = list(message = out$message)) 
             # OR ... input  = arglist,
             # OR ... argument.names = cleanActionArgsNames, 
             #     argument.values   = cleanActionArgsValues, 
             #     arg_value_pairs   = out$arg_value_pairs, 
             # ... exclude      = input$exclude,

      # update seqdf's ith action
      seq <- list()
      seq[[1]] <- cd$seq[[length(cd$seq)]]
      ithrowdf <- convertSequenceToDf(seq) 
      cd$seqdf <- rbind(cd$seqdf, ithrowdf)
      
      # update summary: make improvements here (refactoring for reuse & performance
      #                 as well adding more meta like distinct vals & their freqs)
      out <- summarize(cd$set) 
      cd$statistics <- out$statistics 
      cd$meta <- out$meta
      cd$summary <- out$summary 
      # in-memory cleansed set NOT same as saved cleansed set 
      cd$saved <- FALSE
    } 
  }) 
  
  output$cleanMessage <- renderText({
    # informative (for the user) about the result of the cleansing action
    # conditional color: blue in case of success - red in case of failure 
    if(is.null(cd$set)) return(NULL)
    cd$message
  }) 
  
  # CLEAN main -- CLEAN main -- CLEAN main -- CLEAN main -- CLEAN main
  
  # 1st tab: Clean dataset
  output$cleanTitleSet <- renderText({
    if(is.null(cd$set)) return(NULL) 
    paste0("Cleansed dataset ", "'", ifelse(is.null(cd$name), "-", cd$name), "'",
           " from original set ", "'", ld$name, "'") 
  }) 
  output$cleanTextSet <- renderText({
    if(is.null(cd$set)) return(NULL)
    paste("The summary and detailed views below reflects the latest, cleaned  
          version of the original dataset. You can always save the cleansed  
          dataset, to both store the latest cleansing changes and incrementally 
          update the overall sequence of cleansing actions (always applied on  
          the original, external set). Check the sequence of successfully 
          applied cleaning actions in the next tab.")
  }) 
  
  output$cleanSaveName <- renderUI({ 
    if(is.null(cd$set)) return(NULL) 
    list( 
      textInput(inputId = "cleanSaveName", 
                label = "Enter name for cleansed set (if no name is entered, then
                         the loaded, cleansed set will be overwritten, - if 
                         already stored, otherwise you'll be asked for a name):") 
    )
  }) 
  
  output$cleanSave <- renderUI({
    if(is.null(cd$set)) return(NULL)
    actionButton("cleanSave", "Save Working Set")
  }) 
  
  cleanSave <-  eventReactive(input$cleanSave, {
    # keep in sync with input$load
    # save latest cleansed version: original ld$set (if !exists), cd$set, 
    # cd$seq (sequence, that if applied on ld$set gives cd$set) 
    # each object is stored sepaetely as an rds file
    
    # create directories (if not exist)
    if (!file.exists('data')) 
      dir.create(file.path(getwd(), 'data')) 
    if (!file.exists('data/ld')) 
      dir.create(file.path(getwd(), 'data/ld'))
    if (!file.exists('data/cd')) 
      dir.create(file.path(getwd(), 'data/cd'))
    if (!file.exists('data/cd/set')) 
      dir.create(file.path(getwd(), 'data/cd/set'))
    if (!file.exists('data/cd/seq')) 
      dir.create(file.path(getwd(), 'data/cd/seq')) 
    
    # check/validate input and set cleansed set name (cd$name)
    if (is.null(cd$name) &                # set loaded from external source 
          (input$cleanSaveName == ""))    # i.e. input$source != 'working'   
        return("Dataset storage failure: No name provided for cleansed set 
                of externally loaded dataqset. Please provide a name.") 
    if (input$cleanSaveName != "") 
        cd$name <- input$cleanSaveName 
    
    # save original set (ld), if not already saved 
    filename <- paste0(ld$name, ".rds")
    path <- "data/ld"
    if (!file.exists(paste0(path, "/", filename))) 
      saveRDS(object = ld$set, file = paste0(path, "/", filename)) 
    
    # save cleansed set (set & sequence)    
    filename <- paste0(ld$name, "_", cd$name, ".rds")
    path <- "data/cd/set" 
    saveRDS(object = cd$set, file = paste0(path, "/", filename))
    path <- "data/cd/seq"
    # path <- paste0(getwd(), "/data/cd/seq") 
    if (file.exists(paste0(path, "/", filename))) 
      saved <- " (overwritten) " 
    else 
      saved <- " " 
    saveRDS(object = cd$seq, file = paste0(path, "/", filename)) 
    
    # in-memory cleansed set same as saved cleansed set 
    cd$saved <- TRUE
    
    return(paste0("Success: Original & cleansed set as well as the action-sequence
                  of ", "'", ld$name, ", ", cd$name,  "'", 
                  " saved",  saved, "in ", "../data/", " on ", Sys.time())) 
  })
  
  output$cleanSaveMessage <- renderText({
    cleanSave()
  }) 

  output$cleanTitleSummary <- renderText({
    if(is.null(cd$set)) return(NULL)
    heading <- paste0(" (latest version, summary from all ", nrow(cd$set), 
                      " observations)")
    paste0("Summary of ", "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'", heading)  
  }) 
  output$cleanSummary = DT::renderDataTable(expr = {
    if(is.null(cd$set)) return(NULL)
    DT::datatable(cd$summary, options = list(lengthChange = FALSE, 
                                             autoWidth = FALSE, dom = 't'))
  }, server = TRUE) 
  
  output$cleanTitleData <- renderText({
    if(is.null(cd$set)) return(NULL)
    if(nrow(cd$set) > cdRowsToDisplay) 
      heading <- paste("' (latest version, top",  
                       cdRowsToDisplay, "observations)")
    else 
      heading <- paste0(" (latest version, all ", nrow(cd$set), " observations)")
    paste0("Content of ", "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'", heading)
  }) 
  output$cleanData <- DT::renderDataTable(expr = {
    if(is.null(cd$set)) return(NULL)
    if(nrow(cd$set) > cdRowsToDisplay) {
      DT::datatable(cd$set[1:cdRowsToDisplay,], 
                    filter = list(position = 'top', clear = FALSE), 
                    options = list(lengthChange = FALSE, autoWidth = FALSE))
    } else {
      DT::datatable(cd$set, 
                    filter = list(position = 'top', clear = FALSE), 
                    options = list(lengthChange = FALSE, autoWidth = FALSE))
    }
  }) 
  
  # 2nd tab: Clean actions sequence
  output$cleanTitleSequence <- renderText({
    if(is.null(cd$set)) return(NULL) 
    paste0("Sequence of cleansing actions, working set ", 
           "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'")  
  })
  output$cleanTextSequence <- renderText({
    if(is.null(cd$set)) return(NULL) 
    paste("View below the entire sequence of (succesfully) applied cleaning actions.
          That is the sequence been applied to the original, external dataset, 
          that one loaded from an external source like a text file or an R package
          and then cleansed to become the training/test set.
          That very same sequence will also be applied to an unlabeled external  
          set (one having identical format to the original, working set), which we 
          intend to score.")
  }) 
  output$cleanSequence = DT::renderDataTable(expr = {
    if(is.null(cd$set)) return(NULL)
    DT::datatable(cd$seqdf, options = list(lengthChange = FALSE, 
                                             autoWidth = FALSE, dom = 't'))
  }, server = TRUE) 

  
  # EXPLORE -- EXPLORE -- EXPLORE -- EXPLORE -- EXPLORE -- EXPLORE 
  # code for both sidebar & main
  
  output$exploreTitleSidebar <- renderText({
    if(is.null(cd$set)) return(NULL)
    paste0("Explore variables in set ", "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'") 
  })
  output$exploreTextSidebar <- renderText({
    if(is.null(cd$set)) return(NULL)
    "Explore the variables in the cleansed set. Plot variable distributions and  
    pairwise relationships between the variables, in order to gain an initial 
    understanding of the data." 
  }) 
  output$exploreTitleMain <- renderText({
    if(is.null(cd$set)) return(NULL)
    paste0("Exploring variables in set ", "'", ld$name, ", ", 
           ifelse(is.null(cd$name), "-", cd$name), "'") 
  })
  
  output$distTitleSidebar <- renderText({
    if(is.null(cd$set)) return(NULL)
    "Visualize/summarize variable distributions" 
  }) 
  output$distTitleMain <- renderText({
    if(is.null(cd$set)) return(NULL)
    "Visualizing/summarizing variable distributions" 
  })
  
  
  output$distVar <- renderUI({ 
    if(is.null(cd$set)) return(NULL)
    # colnames values are column names - colnames labels contain names + types
    # labels are displayed in widgets - values are used by the server 
    colnames <- colnames(cd$set) 
    is.char <- (coltypes(cd$set) %in% c('character')) 
    if(!is.null(is.char)) {  # otherwise object is.char not found (is NULL)
      list(
        selectInput(inputId = "var", 
                    label = "Select a variable to plot 
                           its distribution:", 
                    choices  = as.list(colnames[!is.char]))
      )
    }
  }) 
   
  output$distHistorBar <- renderPlot({ 
    # initially, for a few moments, input$var is null (if: argument is of length zero) 
    if (is.null(input$var)) return(NULL) 
    # whenever another dataset is loaded an instant mismatch happens (undefined columns selected)
    condition <- input$var %in% names(cd$set)
    #if (is.null(condition) | is.na(condition) | length(condition) != 1) return(NULL) 
    if (!condition) return(NULL) 
    
    if (is.numeric(cd$set[,input$var])) {
      hist(cd$set[,input$var], breaks = 50, freq=FALSE, 
           col='lightblue', border='white', 
           main = paste("Histogram of", input$var), 
           xlab = input$var)  
    } else if (is.factor(cd$set[,input$var])) {
      barplot(table(cd$set[,input$var]), 
              col='lightblue', border='white', 
              main = paste("Barplot of", input$var), 
              xlab = input$var, ylab = "Count")
    } else {
      return(NULL)
    } 
  }) 
  
  output$distBoxorBar <- renderPlot({ 
    # initially, for a few moments, input$var is null (if: argument is of length zero)
    if (is.null(input$var)) return(NULL) 
    # whenever another dataset is loaded an instant mismatch happens (undefined columns selected) 
    condition <- input$var %in% names(cd$set)
    #if (is.null(condition) | is.na(condition) | length(condition) != 1) return(NULL) 
    if (!condition) return(NULL) 
    
    if (is.numeric(cd$set[,input$var])) {
      boxplot(x = cd$set[,input$var], outline = TRUE, 
              plot = TRUE, col='lightblue', border = 'black', 
              main = paste("Boxplot of", input$var), 
              xlab = input$var, horizontal = TRUE)  
    } else if (is.factor(cd$set[,input$var])) {
      barplot(table(cd$set[,input$var]), 
              col='lightblue', border='white', 
              main = paste("Barplot of", input$var), 
              xlab = input$var, ylab = "Count") 
    } else { 
      return(NULL)   # through an exception character cols to factors
    } 
  }) 
  
  output$summary <- renderTable({
    if (var.type()=='character') {
      return(NULL)
    } else {
      xtable(as.table(summary(cd$set[,input$variable])))
    }
  }) 
  
  output$relTitleSidebar <- renderText({
    if(is.null(cd$set)) return(NULL)
    "Visualize/summarize pair-wise variable relationships" 
  }) 
  output$relTitleMain <- renderText({
    if(is.null(cd$set)) return(NULL)
    "Visualizing/summarizing pair-wise variable relationships" 
  })
  
  
  output$relVars <- renderUI({ 
    if(is.null(cd$set)) return(NULL)
    # colnames values are column names - colnames labels contain names + types
    # labels are displayed in widgets - values are used by the server 
    colnames <- colnames(cd$set)
    coltypes <- coltypes(cd$set)
    is.number <- (coltypes %in% c('integer', 'numeric')) # native: is.numeric()
    is.factor <- (coltypes %in% c('factor'))             # overrides native func
    is.char <- (coltypes %in% c('character')) 
    list(
      selectInput(inputId = "y", 
                  label = "Select the dependent variable (y):", 
                  choices  = as.list(colnames[is.number])),
      selectInput(inputId = "x", 
                  label = "Select the independent variable (x):", 
                  choices  = as.list(colnames[is.number | is.factor]))
    )
  }) 
  
  output$relControls <- renderUI({
    if(is.null(cd$set)) return(NULL)
    # initially, for a few moments, input$x and/or input$y is null (if: argument is of length zero)
    if ( is.null(input$x) | is.null(input$y) ) return(NULL) 
    # whenever another dataset is loaded an instant mismatch happens (undefined columns selected)
    condition <- input$x %in% names(cd$set) & input$y %in% names(cd$set)
    #if (is.null(!condition) | is.na(!condition) | length(!condition) != 1) return(NULL) 
    if (!condition) return(NULL) 
    
    if (is.numeric(cd$set[,input$x])) {
      list( 
        p(strong("A scatter plot will be displayed:")),
        p("(both variables (x & y) are numeric)"),
        radioButtons(inputId = "trend", 
                     label = "Estimated trend in data (estimated line)",
                     choices = list("Linear trend" = "lm", 
                                    "Curve" = "loess")), 
        sliderInput(inputId = "ci", 
                    label = "Confidence Interval (of estimated trend): 
                    select Confidence Level",
                    min = 0.90, max = 0.99, step = 0.01, value = 0.95) 
      )
    } else if (is.factor(cd$set[,input$x])) {
      list(
        p(strong("A box plot will be displayed:")), 
        p("(dependent variable (y) is numeric but 
          independent variable (x) is categorical)"), 
        checkboxInput("outliers", "Show outliers", FALSE)
      )
    } else {
      return(NULL)
    } 
  }) 
 
  output$relScatterorBox <- renderPlot({ 
    # initially, for a few moments, input$x and/or input$y is null (if: argument is of length zero)
    if ( is.null(input$x) | is.null(input$y) ) return(NULL) 
    # whenever another dataset is loaded an instant mismatch happens (undefined columns selected)
    condition <- input$x %in% names(cd$set) & input$y %in% names(cd$set)
    #if (is.null(!condition) | is.na(!condition) | length(!condition) != 1) return(NULL) 
    if (!condition) return(NULL) 
    
    if (is.numeric(cd$set[,input$x])) {
      title <- paste0(input$y, " vs ", input$x, ":",
                      " (conf. level of est. trend: ", input$ci, ")") 
      g <- ggplot(data=cd$set, aes_string(x=input$x, y=input$y))
      g <- g + geom_point(shape=19, size = 4, colour="blue")
      g <- g + stat_smooth(method=input$trend, size = 1, color = "red", 
                           se=T, level = input$ci) 
      g <- g + ggtitle(title)
      g
    } else if (is.factor(cd$set[,input$x])) {
      ggplot(cd$set, aes_string(x=input$x, y=input$y)) +
        geom_boxplot(aes_string(fill=input$x), 
                     outlier.size=ifelse(input$outliers,3,0)) +
        guides(fill=FALSE) 
    } else {
      return(NULL)
    } 
  }) 
 
  output$relCorMatrixorNothing <- renderTable({
    #if (is.null(x.type()) || is.null(y.type())) return(NULL)
    if (y.type()=='integer' || y.type()=='numeric') {
      if (x.type()=='integer' || x.type()=='numeric') {
        
      } else if (x.type()=='factor') {
        
      } else {
        return(NULL)
      }   
    } else {
      return(NULL)
    } 
    
  })
  
  # TRAIN/TUNE: Sidebar -- TRAIN/TUNE: Sidebar -- TRAIN/TUNE: Sidebar 
  # model training & tuning (model calibration or validation): we train the same  
  # type of model for several combinations of tuning parameters (tuning grid) and
  # select optimal model out of them - we can repeat this process for different
  # types of models of different complexities
  
  output$trainTitleSidebar <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Train/tune predictive models on set ", "'", ld$name, ", ", cd$name, "'")  
    
  })
  output$trainTextSidebar1 <- renderText({
    if(is.null(cd$name)) return(NULL)
    "Train variants of a particular training method (model) for different  
     combinations of values of the method's tuning parameters. All resulting  
     model variants predict the chosen response from the list of selected 
     predictors, each with different predictive performance."
  })
  output$trainTextSidebar2 <- renderText({
    if(is.null(cd$name)) return(NULL)
    "Model tuning (model calibration) selects the combination of parameter values 
     that delivers optimal performance, as indicated by the selected performance
     measure (which for the best value combination becomes minimum or maximum).  
     You may want to save the optimal model for further model assessment and  
     ultimately for making predictions (i.e. for scoring unlabeled data)."
  }) 
  
  output$taskText <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Predictive Task & held-out portion of data") 
  })
  
  output$task <- renderUI({ 
    if(is.null(cd$name)) return(NULL) 
    list( 
      selectInput(inputId = "task", 
                  label = "Prediction task:", 
                  choices  = taskList,
                  selected = taskList[[1]]), 
      sliderInput(inputId = "testPercent", 
                  label = "Portion of set held-out for model(s) assessment
                           (the test set):",
                  min = 0.00, max = 0.30, step = 0.05, 
                  value = 0.10)
    )
  }) 
  
  output$variablesText <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Involved variables (response & predictors)") 
  })
  
  output$response <- renderUI({ 
    # depends on task
    # ?initially, when input$task IS NULL, if or swich gives an error
    if ( is.null(cd$name) || is.null(input$task)) return(NULL) 
    
    # colnames values are column names - colnames labels contain names + types
    # labels are displayed in widgets - values are used by the server
    colnames <- colnames(cd$set) 
    coltypes <- coltypes(cd$set) 
    
    if (input$task == 'regr') {
      bool.response <- (coltypes %in% c('integer', 'numeric')) 
    } else {
      bool.response <- (coltypes %in% c('factor')) 
    } 
    
      selectInput(inputId = "response", 
                  label = "Response variable:", 
                  choices  = as.list(colnames[bool.response])) 
    
  })
  
  output$predictors <- renderUI({ 
    # depends on response
    if (is.null(cd$name)) return(NULL) 
    
    # colnames values are column names - colnames labels contain names + types
    # labels are displayed in widgets - values are used by the server
    bool.exclude.response <- !names(cd$set) %in% c(input$response)
    colnames <- colnames(cd$set[,bool.exclude.response]) 
    coltypes <- coltypes(cd$set[,bool.exclude.response]) 
    bool.select.predictors <- coltypes %in% c('integer', 'numeric', 
                                              'factor', 'logical') 
        
      selectInput(inputId = "predictors", 
                  label = "Predictors (predictor variables):", 
                  choices  = as.list(colnames[bool.select.predictors]),
                  multiple = TRUE)
  }) 
  
  output$metricsText <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Measures of predictive performance") 
  })
  
  output$metrics <- renderUI({ 
    # metric for final model selection - for tuning see trControl
    # depends on task
    # ?initially, when input$task IS NULL, if or swich gives an error
    if ( is.null(cd$name) || is.null(input$task) ) return(NULL) 
    
    # return nothing if no response selected (say, no factorizable var for class)
    if (is.null(input$response)) return(NULL)
    
    id <- "metrics"
    label <- "Summarizations (statistics) for measuring performance:"
    response.distinct.num <- length(levels(cd$set[[input$response]]))
    # less  efficient:    <- length(unique(cd$set[[input$response]]))
    # cd$summary doesn't work :  <- unlist(cd$summary["Distinct", input$response])
    # or cd$statistics, errors: "missing value where TRUE/FALSE needed" - 
    #                            "argument is of length zero"
    
    switch(input$task, 
           regr = {
             selectInput(inputId = id, 
                         label = label,
                         choices  = 
                           list("RMSE and R-Squared" = "defaultSummary"))
           },
           class = {
             if (response.distinct.num == 2L) 
               selectInput(inputId = id, 
                           label = label, 
                           choices  = 
                             list("Accuracy and Kappa statistic" = "defaultSummary", 
                                  "Sensitivity, Specificity, ROC-AUC" = 
                                    "twoClassSummary",
                                  "Precision, Recall, PR-AUC" = "prSummary",
                                  "Accuracy, Kappa statistic and more" = 
                                    "multiClassSummary")) 
             else
               selectInput(inputId = id, 
                           label = label, 
                           choices  = 
                             list("Accuracy and Kappa statistic" = "defaultSummary", 
                                  "Accuracy, Kappa statistic and more" = 
                                    "multiClassSummary",
                                  "Minus Log-likelihood of Multinomial Distribution" = 
                                    "mnLogLoss")) 
           })
  })
 
  output$metric <- renderUI({ 
    # metric for optimal model selection during tuning
    # depends on task
    # ?initially, when input$task IS NULL, if or swich gives an error
    if ( is.null(cd$name) || is.null(input$task) || is.null(input$response)) 
      return(NULL) 
    id <- "metric"
    label <- 
      "Performance measure for finding optimal tuning parameter values:"
    switch(input$task, 
           regr = {
             selectInput(inputId = id, 
                         label = label, 
                         choices  = 
                           list("Root  Mean Squared Error (RMSE)" = "RMSE", 
                                "R-Squared" = "Rsquared"))
           },
           class = {
             switch(input$metrics,
                    defaultSummary = {
                      selectInput(inputId = id, 
                                  label = label, 
                                  choices  = 
                                    list("Accuracy" = "Accuracy",
                                         "Kappa Statistic" = "Kappa"))
                    },
                    twoClassSummary = {
                      selectInput(inputId = id, 
                                  label = label, 
                                  choices  = 
                                    list("Sensitivity" = "Sens",
                                         "Specificity" = "Spec",
                                         "ROC-AUC (Area Under ROC Curve)" = "ROC"))
                    }, 
                    prSummary = {
                      selectInput(inputId = id, 
                                  label = label, 
                                  choices  = 
                                    list("Precision" = "Prec",
                                         "Recall" = "Rec",
                                         "PR-AUC" = "AUC"))
                    },
                    multiClassSummary = {
                      selectInput(inputId = id, 
                                  label = label, 
                                  choices  = 
                                    list("Accuracy" = "Accuracy",
                                         "Kappa Statistic" = "Kappa", 
                                         "Sensitivity" = "Sensitivity",
                                         "Specificity" = "Specificity",
                                         "ROC-AUC (Area Under ROC Curve)" = "ROC",
                                         "Positive Predictive Value" = 
                                           "Pos_Pred_Value",
                                         "Negative Predictive Value" = 
                                           "Neg_Pred_Value",
                                         "Minus Log-likelihood" = "logLoss",
                                         "Detection Rate" = "Detection_Rate",
                                         "Balanced Accuracy" = "Balanced_Accuracy"))
                    },
                    mnLogLoss = {
                      selectInput(inputId = id, 
                                  label = label, 
                                  choices  = 
                                    list("Minus Log-likelihood" = "logLoss"))
                    })
           })
  })   
  
  output$trainMethodParamsText <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Training method & tuning parameters (if any)", "") 
  })
  
  output$trainMethod <- renderUI({ 
    if ( is.null(cd$name) ) return(NULL) 
    # ?initially, when input$task IS NULL, if or swich gives an error 
    
    # determine trainMethods lists dynamically, based on input$task
    trainMethodsForTaskDf <- trainMethodsDf[trainMethodsDf$use %in% c("dual", 
                                                                      input$task),]
    trainMethodsForTaskList <- as.list(as.character(trainMethodsForTaskDf$id))
    names(trainMethodsForTaskList) <- trainMethodsForTaskDf$label
    ' produces a result like that produced by the following:
      trainMethodsForTaskList <- list("Ordinary Linear Regression (OLS)" = "lm", 
                              ..., 
                              "Generalized Boosted Models (GBM)" = "gbm",
                              ...) '
    list(     
      selectInput(inputId = "trainMethod", 
                  label = "Model training method (model):", 
                  choices  = trainMethodsForTaskList,
                  selected = trainMethodsForTaskList[[1]])
    ) 
  }) 
  
  output$trainParams <- renderUI(expr = quote({ 
    if ( is.null(cd$name) || is.null(input$trainMethod) ) return(NULL)
    # otherwise: initially, when input$tuneMethod IS NULL, if or swich gives an error
    # "Error in if (input$tuneMethod == "cv") { : argument is of length zero" OR 
    # "Error in switch(input$tuneMethod, cv = { : EXPR must be a length 1 vector"
    
    pName <- trainParamsList[[input$trainMethod]]$pName
    
    
    # return nothing if there are no params associated with the model
    if (length(pName)==0) return(NULL)
    
    # LL is the list of textInput expressions (tags) to be returned
    LL <- vector("list", length(pName))  
    for(i in 1:length(pName)) {
      LL[[i]] <- list(
          textInput(inputId = pName[i],
                    label = 
                      paste(trainParamsList[[input$trainMethod]]$methoddesc, 
                            "-", 
                            trainParamsList[[input$trainMethod]]$pDesc[i]), 
                    value = trainParamsList[[input$trainMethod]]$pValue[i])
        )  
    }
    return(LL)
 
  }), quoted=TRUE)   
  
  output$tuneMethodOptionsText <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    paste0("Tuning method & options", "") 
  })

  output$tuneMethod <- renderUI({ 
    if ( is.null(cd$name) ) return(NULL) 
    # ?initially, when input$task IS NULL, if or swich gives an error 
    list(     
      selectInput(inputId = "tuneMethod", 
                  label = "Model tuning method:", 
                  choices  = tuneMethodList, 
                  selected = tuneMethodList[[1]])
    ) 
  }) 

  output$tuneOptionsButton <- renderUI({ 
    if ( is.null(cd$name)  || is.null(input$tuneMethod) ) return(NULL)
    # otherwise: initially, when input$tuneMethod IS NULL, if or swich gives an error
    # "Error in if (input$tuneMethod == "cv") { : argument is of length zero" OR 
    # "Error in switch(input$tuneMethod, cv = { : EXPR must be a length 1 vector"
    
    switch(input$tuneMethod, 
           "cv" = { 
             list(
               sliderInput(inputId = "folds", 
                           label = "# of CV folds:",
                           min = 2, max = 10, step = 1, value = 5),
               br(),
               actionButton("tune", "Train/tune")
             )
           },
           "repeatedcv" = {
             list(
               sliderInput(inputId = "folds", 
                           label = "# of CV folds:",
                           min = 2, max = 10, step = 1, value = 5),
               sliderInput(inputId = "repeats", 
                           label = "# of repeats of a complete CV:",
                           min = 1, max = 10, step = 1, value = 5),
               br(),
               actionButton("tune", "Train/tune")
             )
           },
           "boot" = {
             list(
               sliderInput(inputId = "folds", 
                           label = "# of bootstrap samples:",
                           min = 10, max = 50, step = 5, value = 25),
               br(),
               actionButton("tune", "Train/tune")
             )
           },
           "LOOCV" = {
             list(
               br(), 
               actionButton("tune", "Train/tune")
             )
           }
    ) 
    
  }) 
  
  modelTune <-  eventReactive(input$tune, { 
    
    # check input and raise errors: if no predictors selected, 
    #                               if model built on unsaved set, etc
    # Warning: Unhandled error in observer: undefined columns selected
    if (is.null(input$predictors))   
      return("Model tuning failure: No predictor variables selected. Please select 
             the variables that will serve as the predictors of the response.") 
    if (!cd$saved)
      return("Model tuning failure: Training set (cleansed set) changed 
             but not saved. Please save set, then build model(s) on it.") 
 
    ' slice data, create formula object, etc
      slice: split data (obs) into train & test sets (apply stratified sampling)
      slice: slice sets vertically, keeping only response (1st col) & predictors 
    '
    model$formulaText <- paste0(input$response, " ~ ", 
                            paste(input$predictors, collapse = '+'))
    model$formula <- as.formula(model$formulaText)
    if (input$testPercent==0.0) { 
      testset <- NULL 
      trainset <- cd$set[, c(input$response, input$predictors)]
    } else {
      responseVec <- cd$set[, input$response] 
      testIndices <- testIndices(input$testPercent, responseVec, stratified = TRUE)
      testset <- cd$set[testIndices, c(input$response, input$predictors)]
      trainset <- cd$set[-testIndices, c(input$response, input$predictors)] 
    } 

    ' train/tune models on the train set (no tuning is not an option)
      tuneGrid/tuneLength, preProcess depends on trainMethod
      trControl arguments depends on tuneMethod 
    '
    # ?set up preprocess actions
        
    # set up tuneGrid (tuneLength=NULL): evaluate expression (parsed gridText)
    if (length(trainParamsList[[input$trainMethod]]$pName)==0) { 
      # tuning method has no tuning parameters
      tuneGrid <- NULL       
    } else if (length(trainParamsList[[input$trainMethod]]$pName)==1) { 
      # tuning method has tuning parameters: expand.grid(pname1=pvalue1, ...)
      gridText <- paste0("expand.grid(",
                         trainParamsList[[input$trainMethod]]$pName[1], "=",
                         input[[trainParamsList[[input$trainMethod]]$pName[1]]], 
                         ")") 
      tuneGrid <- eval(parse(text = gridText))
    } else { 
      gridText <- "expand.grid("
      for ( i in 1:(length(trainParamsList[[input$trainMethod]]$pName)-1) ) {
        gridText <- paste0(gridText, 
                           trainParamsList[[input$trainMethod]]$pName[i], "=",
                           input[[trainParamsList[[input$trainMethod]]$pName[i]]], 
                           ",")  
      } 
      gridText <- paste0(gridText, 
                         trainParamsList[[input$trainMethod]]$pName[i+1], "=",
                         input[[trainParamsList[[input$trainMethod]]$pName[i+1]]], 
                         ")")
      tuneGrid <- eval(parse(text = gridText))
    } 
    ' 
    more efficient code - to be deployed and tested
    if (length(trainParamsList[[input$trainMethod]]$pName)==0) { 
      # tuning method has no tuning parameters
      tuneGrid <- NULL       
    } else {
      # tuning method has one or more tuning parameters
      pnamevalue = vector(mode="character", 
                      length = length(trainParamsList[[input$trainMethod]]$pName))
      for ( i in 1:length(trainParamsList[[input$trainMethod]]$pName) ) {
        pnamevalue[i] <- paste0(trainParamsList[[input$trainMethod]]$pName[i], "=",
                            input[[trainParamsList[[input$trainMethod]]$pName[i]]])
      }
      gridText <- paste0("expand.grid(", paste(pnamevalue, collapse = ","), ")")
      tuneGrid <- eval(parse(text = gridText))
    }
    '
    # set up trControl: call trainControl() by evaluating the corresponding 
    # expression (parsed trControlText), instead of calling trainControl() directly 
    repeats <- ifelse(grepl("repeatedcv", input$tuneMethod), input$repeats, 1)
    if (input$metrics %in% c("twoClassSummary", "mnLogLoss", "multiClassSummary"))
      classProbs <- TRUE
    else 
      classProbs <- FALSE
    trControlText <- 
      paste0("trainControl(method = ", "\'", input$tuneMethod, "\'", ", ", # string
                          "number = ", input$folds, ", ",                  # number
                          "repeats = ", repeats, ", ",
                          "returnData = FALSE", ", ",
                          "returnResamp = ", "\'final\'", ", ",   # OR 'all' or 'none'
                          "summaryFunction = ", input$metrics, ", ", 
                          "selectionFunction = ", "best", ", ",            # default
                          "classProbs = ", classProbs, ", ",
                          "trim = TRUE", ", ",
                          "allowParallel = TRUE)") 
    trControl <- eval(parse(text = trControlText))
    ' 
    the less flexible direct call of trainControl()                        
    trainControl(method = input$tuneMethod,
                 number = input$folds, 
                 # repeats = input$repeats,
                 returnData = FALSE, 
                 returnResamp = "final", # OR all" or "none"
                 summaryFunction = defaultSummary, # default
                 selectionFunction = best,         # default
                 # classProbs = TRUE,
                 trim = TRUE,
                 allowParallel = TRUE), 
    '
    # train/tune the model  
    fit <- caret::train(x = as.data.frame(trainset[,-1]), 
                        y = as.vector(trainset[,1]),
                        method = input$trainMethod, 
                        preProcess = NULL, 
                        tuneGrid = tuneGrid, 
                        tuneLength = NULL, 
                        trControl = trControl, 
                        metric = input$metric) 
                        # maximize =  # summaryFunction: metrics to compute
                                      # metric: optimum model (selectionFunction)

    # select final model (metric, maximize) and evaluate it on test set 
    
    # fit: resulting model (including model tuning info), built on best parameters
    #      we store to list model$fit, and add response, predictors, testIndices
    model$fit <- fit                           # fitted model - can be stored
    model$fit$response <- input$response       # add more info to fitted model 
    model$fit$predictors <- input$predictors
    model$fit$testIndices <- testIndices
    model$fit$tuneGrid <- tuneGrid             # actually tuneGrid is part of results  
    # tuneGrid, results, bestTune, etc (see above) in fit object
    #model$tuneGrid <- tuneGrid        
    #model$results <- fit$results               
    #model$bestTune <- fit$bestTune
    # for testing purposes
    #model$test <- test 
    #model$train <- train
    #model$trainResponse <- train[,1]
    
    return(paste0("Success: Model built (tuned) on set ", 
                  "'", ld$name, ", ", cd$name, "'",
                  ", predicting  variable ", "'", model$fit$response, "'", "."))
  })
 
  output$modelTuneMessage <- renderText({
    if ( is.null(cd$name) || is.null(input$trainMethod) ) return(NULL)
    modelTune()
  }) 
  
  # TRAIN/TUNE Main -- TRAIN/TUNE Main -- TRAIN/TUNE Main 
  # ??? update each time you press button 'Train/Tune'?
    
  output$tuneTitle <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Tuning a model on set ", "'", ld$name, ", ", cd$name, "'") 
  }) 
 
  output$modelSaveName <- renderUI({ 
    if(is.null(model$fit)) return(NULL) 
    list( 
      textInput(inputId = "modelSaveName", 
                label   = "Enter name for optimal model:") 
    )
  }) 
  
  output$modelSave <- renderUI({
    if(is.null(model$fit)) return(NULL)
    actionButton("modelSave", "Save Optimal Model")
  }) 
  
  modelSave <-  eventReactive(input$modelSave, { 
    
    # check/validate model and input
    if (is.null(cd$name))        # no cleansed set (only external set loaded)   
      return("Model storage failure: No cleansed set available (only externally  
             loaded dataset). Please produce/save or load a cleansed set, then 
             build/save model on variables of that cleansed set.") 
    if (input$modelSaveName == "") 
      return("Model storage failure: No name provided for the model.
             Please provide a name.") 
    
    # Create model directory (if not exists) - possibly many models in it
    if (!file.exists('models')) 
      dir.create(file.path(getwd(), 'models')) 
    path <- paste0('models/', paste0(ld$name, "_", cd$name))
    if (!file.exists(path)) 
      dir.create(file.path(getwd(), path))
    path <- paste0('models/', paste0(ld$name, "_", cd$name), "/", model$fit$response)
    if (!file.exists(path)) 
      dir.create(file.path(getwd(), path))

    # save optimal model - internal: ld name - cd name - response var - model name
    #model$name <- paste0(paste0(ld$name, "_", cd$name), 
    #                     "_", model$fit$response, "_", input$modelSaveName)
    filename <- paste0(input$modelSaveName, ".rds")
    if (file.exists(paste0(path, "/", filename))) 
      saved <- " (overwritten) " 
    else 
      saved <- " "
    saveRDS(object = model$fit, file = paste0(path, "/", filename)) 
    
    return(paste0("Success: Optimal model named ", "'", input$modelSaveName, "'",
                 ", built on set ", "'", ld$name, ", ", cd$name, "'",
                 ", predicting  variable ", "'", model$fit$response, "'",
                 ", saved", saved, "in ", "../", path, " on ", Sys.time())) 
  })
  
  output$modelSaveMessage <- renderText({
    if(is.null(model$fit)) return(NULL)
    modelSave()
  }) 
 
  output$tuneResponse <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Response (variable to predict): ", model$fit$response) 
  }) 
  output$tunePredictors <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Predictors (predictor variables): ", 
           paste(model$fit$predictors, collapse=", ")) 
  }) 
  
  output$buildSet <- renderText({                     # the so-called training set
    if(is.null(model$fit)) return(NULL)
    paste0("# of training observations (for model building): ", 
           nrow(cd$set)-length(model$fit$testIndices)) 
  }) 
  output$assessSet <- renderText({                    # the so-called testing set
    if(is.null(model$fit)) return(NULL)
    paste0("# of held-out, test observations (for model assessment): ", 
           length(model$fit$testIndices)) 
  }) 
  
  output$trainingMethod <- renderText({               # the algorithm - hyp.set (model)
    if(is.null(model$fit)) return(NULL)
    paste0("Training method (training algorithm): ", 
           model$fit$method, " (", model$fit$modelType, ")") 
  }) 
  output$tuningMethod <- renderText({                 # cross-validation or other 
    if(is.null(model$fit)) return(NULL)               # put input values in model$fit
    if (grepl("cv", input$tuneMethod)) {
      moreinfo <- paste0(" (", input$folds, "-fold cv")
      if (grepl("repeatedcv", input$tuneMethod))
        moreinfo <- paste0(moreinfo, paste0(", ", input$repeats, " repeats)"))
      else 
        moreinfo <- paste0(moreinfo, ")")
    }
    else if (grepl("LOOCV", input$tuneMethod))
      moreinfo  <- "" 
    else 
      moreinfo <- paste0(" (", input$folds, "-repeat bootstrap)")
    paste0("Tuning method: ", input$tuneMethod, moreinfo)
  }) 
  
  output$modelMetric <- renderText({                  # cross-validation or other
    if(is.null(model$fit)) return(NULL)
    paste0("Metric used to select optimal model: ", model$fit$metric) 
  }) 
  
  output$timeElapsed <- renderText({                  # overall time elapsed (vector)
    if(is.null(model$fit)) return(NULL)
    paste0("Overall model tuning time: ", 
           paste(round(model$fit$times$everything[1], 2), collapse=","), " sec") 
  }) 
  
  output$tuneResultsTitle <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Tuning Grid & Model Tuning Results (Performance Measurements)") 
  })
  output$tuneResults <- DT::renderDataTable(          # tune-grid and metric values
    model$fit$results, options = list(lengthChange = FALSE, dom = 'tp') 
  ) 
  output$tuneBestTitle <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Best Tuning Parameter Values (based on selected performance measure ", 
           "'", model$fit$metric, "'", ")") 
  })
  output$tuneBestParameters <- DT::renderDataTable(expr = {
    if(is.null(model$fit)) return(NULL)
    DT::datatable(model$fit$bestTune, 
                  options = list(lengthChange = FALSE, dom = 't')) 
  }) 
  
  # move to model assessment
  output$tuneProfileTitle <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Tuning Profile") 
  }) 
  output$tuneDiagnosticsTitle <- renderText({
    if(is.null(model$fit)) return(NULL)
    paste0("Tuning Profile") 
  })

  # for testing purposes
  output$tuneGrid <- DT::renderDataTable(            
    model$fit$tuneGrid, options = list(lengthChange = FALSE) 
  ) 
  output$tblTest <- DT::renderDataTable(
    as.data.frame(model$test), options = list(lengthChange = FALSE)
  ) 
  output$tblTrain <- DT::renderDataTable(
    as.data.frame(model$train[,-1]), options = list(lengthChange = FALSE)
  ) 
  
  # for testing purposes 
  output$txtInput <- renderText({
    if(is.null(cd$name)) return(NULL)
    if(is.null(input$trainMethod)) return(NULL)
    paste("Row numbers: ", nrow(cd$set), 
          floor(input$testPercent*nrow(cd$set)), nrow(cd$test),   
          "Response input:", input$response, class(input$response), "--",
          "Predictors input:", length(input$predictors), class(input$predictors), 
          is.vector(input$predictors), "--", 
          "Test df, size:", nrow(model$test), length(model$test), "--", 
          "Train df, exclude response:", nrow(model$train[,-1]), 
          length(names(as.data.frame(model$train[,-1]))), "--", 
          "Response vector:", length(model$trainResponse), class(model$trainResponse),
          is.vector(model$trainResponse), "--",
          "Formula:", model$formulaText, class(model$formula), "--",
          "Methods:", paste(input$trainMethod, input$tuneMethod), 
          paste(class(input$trainMethod), class(input$tuneMethod)), 
          length(input$tuneMethod), 
          is.atomic(input$tuneMethod), is.vector(input$tuneMethod), "--", 
          "Params, p1, p2:", trainParamsList[[input$trainMethod]]$methoddesc,
          paste(trainParamsList[[input$trainMethod]]$pName,  collapse=";"),
          length(trainParamsList[[input$trainMethod]]$pName),
          input[[trainParamsList[[input$trainMethod]]$pName[1]]],
          input[[trainParamsList[[input$trainMethod]]$pName[2]]],
          class(input[[trainParamsList[[input$trainMethod]]$pName[1]]]),
          "TuneGrid, p1, p2:", 
          class(model$tuneGrid[[trainParamsList[[input$trainMethod]]$pName[1]]]),
          class(model$tuneGrid[[trainParamsList[[input$trainMethod]]$pName[2]]]),
          sep=" ") 
  }) 
  
  
  # ASSESS: Sidebar -- ASSESS: Sidebar -- ASSESS:: Sidebar -- ASSESS: Sidebar 
  # assessment/comparison (testing) of competitive final models of different 
  # types & complexities - a portion of cases is held out for this task 

  output$assessTitleSidebar <- renderText({ 
    if(is.null(cd$name)) return(NULL)
    path <- paste0('models/', paste0(ld$name, "_", cd$name))
    model$exists <- file.exists(path)
    if (!file.exists(path)) return("Please train & save model(s) on selected set! 
                               Then come back to assess.")
    return(paste0("Assess predictive models built on set ", 
                  "'", ld$name, ", ", cd$name, "'"))  
  })
  
  output$assessTextSidebar1 <- renderText({
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    "Assess a number of competitive models that predict the same response but 
    are different in the used algorithm, and/or the employed predictors and/or 
    the values of the tuning parameters."
  }) 
  output$assessTextSidebar2 <- renderText({
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    "It is possible for a model to be assessed only if a portion of cases was 
    held out during the construction of the model, for assessment (the held-out
    set is known as the test set). Among compared predictive models the goal
    is to find the simpler one with the optimal performance on the test set 
    (optimal, with respect to any of the preselected metrics)."
  })   
 
  output$assessResponses <- renderUI({ 
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    
    path <- paste0('models/', 
                   paste0(ld$name, "_", cd$name))
    responses <- filelist(path = path, name = NULL)  # func filelist(), sources
    responses <- as.list(responses) 
    
    list( 
      selectInput(inputId = "assessResponse", 
                  label = "Predicted variable (common, response 
                           variable for all models):", 
                  choices  = responses) 
    )
    
  }) 
  
  output$assessResponseLevels <- renderUI({ 
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    
    if (is.factor(cd$set[,input$assessResponse])) {     # task: classification
      selectInput(inputId = "assessResponseLevel", 
                  label = "Primary (important) response level:", 
                  choices  = levels(cd$set[[input$assessResponse]]), 
                  multiple = FALSE)
    } 
  }) 

  output$assessResponseModels <- renderUI({ 
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    
    path <- paste0('models/', 
                   paste0(ld$name, "_", cd$name), "/", 
                   input$assessResponse)
    modelsList <- filelist(path = path, name = NULL)  # func filelist() from sources
    names(modelsList) <- paste0(ld$name, ", ", cd$name, ", ", input$assessResponse, 
                                " - ", modelsList) 
    modelsList <- as.list(modelsList) 
    
    list( 
      selectInput(inputId = "assessModels", 
                  label = "Model(s) to assess:", 
                  choices  = modelsList, 
                  selected =  modelsList[[1]],
                  multiple = TRUE)
    )
  }) 
  
  output$assessMetrics <- renderUI({  
    if (is.null(cd$name)) return(NULL) 
    # return nothing if no response selected (say, no factorizable var for class)
    if (is.null(input$assessResponse)) return(NULL)
    if (!model$exists) return(NULL)
    
    id <- "assessMetrics"
    label <- "Summarization scheme (set of performance measures) to assess on:"

     if (is.factor(cd$set[,input$assessResponse])) {     # task: classification
       if ( length(levels(cd$set[[input$assessResponse]])) == 2L ) 
         selectInput(inputId = id, 
                     label = label, 
                     choices  = 
                       list("Accuracy and Kappa statistic" = "defaultSummary", 
                            "Sensitivity, Specificity, ROC-AUC" = 
                              "twoClassSummary",
                            "Precision, Recall, PR-AUC" = "prSummary")) 
       else
         selectInput(inputId = id, 
                     label = label, 
                     choices  = 
                       list("Accuracy and Kappa statistic" = "defaultSummary", 
                            "Accuracy, Kappa statistic and more" = 
                              "multiClassSummary",
                            "Minus Log-likelihood of Multinomial Distribution" = 
                              "mnLogLoss")) 
     } else {
       selectInput(inputId = id, 
                   label = label,
                   choices  = 
                     list("RMSE and R-Squared" = "defaultSummary"))
     } 
  })
  
  output$assessButton <- renderUI({  
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    actionButton(inputId = "assess", label = "Assess")
  })
  
  assess <-  eventReactive(input$assess, {
    # assess selected models, assessResponseModels, assess by applying each model
    # on the test set, assess on the basis of selected set of metrics, assessMetrics
    
    # initialize assessment dataframe (each model a row) and set response name
    model$assessResultsdf <- data.frame()
    model$assessResponse <- input$assessResponse
    
    # multiple models might have been selected; each assessed separately 
    modelToAssessCounter <- 0
    modelToAssessNames <- vector(mode = "character") # initialize (will be updated
                                                   # from input$assessModels) 
    observed <- NULL; predicted <- NULL
    
    for (i in 1:length(input$assessModels)) {
      
      # 1. load ith model from selected models for assessment
      path <- paste0('models/', 
                     paste0(ld$name, "_", cd$name), "/", 
                     input$assessResponse) 
      filename <- input$assessModels[i]
      # load the saved model (trained and kept initially in model$fit)
      modelToAssess <- readRDS(file = paste0(path, "/", filename, ".rds"))
      #modelToAssessName <- paste0(ld$name, "_", cd$name, "_", 
      #                            model$assessResponse, "_", filename)
      #                        OR  input$assessResponse - OR modelToAssess$response
      
      # 2. check whether model assessment is possible for the ith model
      #    i.e. if there are held out test cases for the ith model
      if (length(modelToAssess$testIndices) == 0) {
        # simply bypass modelToAssess if no test cases exist
        next
      } 
      # given that there are held out test cases (then, the model can be assessed)
      modelToAssessCounter <- modelToAssessCounter + 1
      modelToAssessNames[modelToAssessCounter] <- input$assessModels[i]
      #modelToAssessNames[modelToAssessCounter] <- names(input$assessModels)[i]
      
      # 3. get testset and vector of observed (response) values, by slicing cd$set 
      #    response variable remains fixed but testIndices may differ between
      #    models, thus, models are generally built/test on different train/test 
      #    sets - train/test sets may also differ between models in predictors 
      observed <- cd$set[modelToAssess$testIndices, modelToAssess$response]
      testset <- cd$set[modelToAssess$testIndices, modelToAssess$predictors] 
      
      # 4. compute predictions and data arg: compute vector predicted, then combine 
      #                         into df data then, if needed, add df probs to df data
      # probs are needed only for specific assess functions (assess metrics, say ROC)
      # note that class probabilities are not available for all classification models
      predicted <- predict(object = modelToAssess, newdata = testset, type = "raw")
      data <- data.frame(observed, predicted)       # avoid cbind (returns integers!)
      names(data)[1:2] <- c("obs", "pred")          # then as.factor? integers again
      if (input$assessMetrics %in% c("twoClassSummary", "mnLogLoss", 
                                     "multiClassSummary")) {
        probs <- predict(object = modelToAssess, newdata = testset, type = "prob")
        data <- data.frame(data, probs)
      }
        
      # 5. assessement: compute vector of performance estimates (selected metrics)
      #    first determine the important response level
      if (is.factor(data[,"obs"])) {                # task: classification
        levels <- levels(data[,"obs"])              # observed == data[,"obs"]
        primaryLevel <- input$assessResponseLevel
        reversed.levels <- c(primaryLevel, levels[-which(levels==primaryLevel)])
        data[,"obs"] <- as.character(data[,"obs"])
        data[,"obs"] <- factor(data[,"obs"], levels=reversed.levels)
        data[,"pred"] <- as.character(data[,"pred"])
        data[,"pred"] <- factor(data[,"pred"], levels=reversed.levels)
        levels <- reversed.levels
      } else {
        levels <- NULL
      }
      ' no need to use the entire response vector: testIndices by stratified sampling
      if (is.factor(cd$set[,input$assessResponse])) {    # task: classification
        levels <- levels(cd$set[[input$assessResponse]])
               or use:   cd$set[[model$assessResponse]]
      } 
      WRONG: 
      levels(observed) <- reversed.levels
      str(observed); as.integer(observed); observed 
      CORRECT: 
      observed = as.character(observed)  # ;str(observed); observed
      observed = factor(observed, levels=reversed.levels)
      str(observed); as.integer(observed); observed 
      '
      # compute model performance vector, named assessModelResults
      assessModelString <- paste0(input$assessMetrics, 
                               "(data = data",
                               ", lev = levels", 
                               # ", model = ", "\'", modelToAssess$method, "\'",
                               ")")
      assessModelExpr <- parse(text = assessModelString)
      assessModelResults <- eval(assessModelExpr)
      # OK - for testing/example: calling default summary for regression models
      #assessModelResults <- defaultSummary(data = data, lev = levels, 
      #                                    model =  modelToAssess$method)

      # add model performance vector to assessment dataframe (initially empty dataframe)
      model$assessResultsdf <- rbind(model$assessResultsdf, assessModelResults)
      names(model$assessResultsdf) <- names(assessModelResults)
      # OK - for testing: just a vector (the model performance vector)
      #model$assessResultsdf <- assessModelResults
    } 
    
    # add model name to assessment dataframe (better use data.frame instead of cbind)
    names <- names(model$assessResultsdf)
    model$assessResultsdf <- data.frame(modelToAssessNames, model$assessResultsdf)
    names(model$assessResultsdf) <- c("Model", names)
    
    return(paste0("Success: ", length(modelToAssessNames), "/", 
                  length(input$assessModels), 
                  " models assessed; all predicting variable ",  
                  "\'", input$assessResponse, "\'", "."
    ))
    
    ' for testing purposes only - use at least one model with held-out test cases
      4 f... errors made 1.  model var inside loop with same name as global var
                         2.  do not construct data arg using cbind - use data.frame
                             cbind turns level values yes/no to integers!!!
                         3a. to set observed once, check modelToAssessCounter, not i
                         3b. but, do not set observed once, diffrent models may have  
                             different test indices vector (test cases sets)
                         4.  attention: predict returns: a) vector of predictions OR 
                             b) DF with probs for each class (not both preds & probs)
      
    return(paste0("Success: ", length(modelToAssessNames), "/", length(input$assessModels), 
                  " models assessed; all predicting variable ",  
                  "\'", input$assessResponse, "\'", ".",
                  "assessment scheme, class: 
                  ", "\'", input$assessMetrics, "\'", ", ", 
                  class(observed), ", ", class(predicted), ", ", ", ", 
                  paste0(unique(observed), collapse = ","), ", ",
                  paste0(unique(predicted), collapse = ","), ", ",
                  " assessment call: ", "\'", assessModelString, "\'",
                  " assessment params (data, levels): ", 
                  nrow(data), ", ", 
                  paste0(names(data), collapse = ","), ", ", 
                  sum(observed == data[,"obs"]), ",", 
                  sum(predicted == data[,"pred"]), ", ",
                  paste0(lapply(data, class), collapse = ","), ", ",
                  paste0(levels(observed), collapse = ","), ", ",
                  paste0(levels(data[,"obs"]), collapse = ","), ", ",
                  paste0(levels(predicted), collapse = ","), ", ",
                  paste0(levels(data[,"pred"]), collapse = ","), ", ",
                  paste0(levels, collapse = ","), ", ",
                  paste0(names(assessModelResults), collapse = ","),
                  " - last model indices: ", length(modelToAssess$testIndices),
                  " - nrows/ncols: ", nrow(model$assessResultsdf), "-",
                  ncol(model$assessResultsdf), 
                  " content: ", paste0(model$assessResultsdf[1,], collapse = ","), "-",
                  paste0(model$assessResultsdf[2,], collapse = ","),
                  " - meta: ", class(model$assessResultsdf), "-",
                  paste0(names(model$assessResultsdf), collapse = ",")
  ))
  '
  })
 
  output$assessMessage <- renderText({
    if(is.null(cd$name)) return(NULL) 
    if (!model$exists) return(NULL)
    assess()
  }) 
  
  # ASSESS Main -- ASSESS Main -- ASSESS Main -- ASSESS Main 
  # display (multi-) model assessment results (also display scorings?)
  
  output$assessTitle <- renderText({ 
    if(length(model$assessResultsdf) == 0) return(NULL)   # both NULL or empty dataframe
    paste0("Assessing model(s) built on ", "'", ld$name, ", ", cd$name, "'",
           ", ", "predicting ", "'", model$assessResponse, "'")  
  }) 
  
  output$assessText1 <- renderText({
    if(length(model$assessResultsdf) == 0) return(NULL)   # both NULL or empty dataframe
    paste0("Selected models with no held out test cases can not be assessed and 
           therefore are not included in the following model assessment table.")
  }) 
  output$assessText2 <- renderText({
    if(length(model$assessResultsdf) == 0) return(NULL)   # both NULL or empty dataframe
    paste0("The compared models all predict the same response, ",
           "'", model$assessResponse, "',",
           " but are generally built on different methods (algorithms) and/or  
           predictors and/or tuning parameters. Model comparison can be performed 
           on the basis of estimations of the preselected metrics. Among the 
           compared models select the simpler one with the optimal metric-estimate 
           on the test set.")
  }) 
  
  output$assessTable <- DT::renderDataTable(expr = {
    if(length(model$assessResultsdf) == 0) return(NULL)   # both NULL or empty dataframe
    DT::datatable(model$assessResultsdf, 
                  options = list(lengthChange = FALSE, autoWidth = FALSE, dom = 't'))
  }, server = TRUE) 
  
  
  # SCORE: Sidebar -- SCORE: Sidebar -- SCORE: Sidebar -- SCORE: Sidebar 
  # Score loaded, unlabeled set, ld$score using one of the final models built 
  # and assessed on the orresponding training/test set, cd$set (and cd$seq)
  # ??? exclude any cleansing actions ? only sampling
  
  output$scoreTitleSidebar <- renderText({ 
    if(is.null(ld$score)) return(NULL) # !is.null(ld$score) implies !is.null(cd$name)
    path <- paste0('models/', paste0(ld$name, "_", cd$name))
    model$exists <- file.exists(path)
    if (!model$exists) return("Please train/assess & save model(s) on train/test
                               set. Then come back to score unlabeled data.")
    paste0("Score unlabeled observations in set ", "'",ld$scoreName, "'")  
  }) 
  
  output$scoreTextSidebar1 <- renderText({
    if(is.null(ld$score)) return(NULL)
    if (!model$exists) return(NULL)
    paste0("Select which variable to score (predict) and then select one or more 
           relative, predictive models. Available models for selection all predict  
           the same response variable, the variable choosen to be scored.")
  }) 
  output$scoreTextSidebar2 <- renderText({
    if(is.null(ld$score)) return(NULL)
    if (!model$exists) return(NULL)
    paste0("The scoring mechanism first applies on the unlabeled, external set ", 
           "'", ld$scoreName, "'", ",",
           " the sequence of cleansing actions that has been defined on the 
             original, external set ", "'", ld$name, "'",     
           " that corresponds to the training/test set ", "'", cd$name, "'", 
           " (both external sets must have identical formats). Then, it applies 
             each model, in turn, to predict (estimate) the labels (values) of 
             the common response, for the unlabeled observations in ", 
           "'", ld$scoreName, "'", ".")
  }) 
  
  output$scoreResponses <- renderUI({ 
    if(is.null(ld$score)) return(NULL)
    if (!model$exists) return(NULL)
    
    path <- paste0('models/', 
                   paste0(ld$name, "_", cd$name))
    responsesList <- filelist(path = path, name = NULL)  # func filelist(), sources
    responsesList <- as.list(responsesList) 
    
    list( 
      selectInput(inputId = "scoreResponse", 
                  label = 
                    "Select variable to score 
                    (common response variable for all selected models):", 
                  choices  = responsesList) 
    )
 
  })
 
  output$scoreModelsForResponse <- renderUI({ 
    if(is.null(ld$score)) return(NULL)
    if (!model$exists) return(NULL) 
    
    path <- paste0('models/', 
                   paste0(ld$name, "_", cd$name), "/", 
                   input$scoreResponse)
    modelsList <- filelist(path = path, name = NULL)  # func filelist() from sources
    names(modelsList) <- paste0(ld$name, ", ", cd$name, ", ", input$scoreResponse, 
                                " - ", modelsList) 
    modelsList <- as.list(modelsList) 
    
    list( 
      selectInput(inputId = "scoreModels", 
                  label = "Select model(s) for scoring the loaded, unlabeled data:", 
                  choices  = modelsList, 
                  selected =  modelsList[[1]],
                  multiple = TRUE), 
      br(), 
      actionButton("score", "Score")
    )
  }) 
  
  score <-  eventReactive(input$score, {
    # applies the selected model, model$score on set to score, ld$score, and makes
    # predictiions (computes estimates) for the labels of the response variable, 
    # that the model is constructed to predict
    
    # validate set to score (validate set name, column names/types)
    # ??? check structure (column names & types: ld$meta or cd$meta)
    if (length(grep(pattern = ld$name, x = ld$scoreName, fixed = TRUE)) == 0) 
      return(paste0("Set names do not match. Please load correct sets.")) 
    
    # ??? add (empty) response variable to ld$score prior to clenasing
    # cleanse ld$score (apply cd$seq on set) - ?exclude sampling actions?
    cd$score <- execSequence(set = ld$score, seq = cd$seq, 
                             score = TRUE, exclude = c("sampleset"))  # defaults
    
    # multiple models selected each will produce its own vector of predictions  
    # ??? check whether all predict the same response on same predictor set
    for (i in 1:length(input$scoreModels)) {
    
      # load selected model
      path <- paste0('models/', 
                     paste0(ld$name, "_", cd$name), "/", 
                     input$scoreResponse) 
      filename <- input$scoreModels[i]
      model$score <- readRDS(file = paste0(path, "/", filename, ".rds"))
      model$scoreName <- paste0(ld$name, "_", cd$name, "_", 
                                input$scoreResponse, "_", filename)
      predictions <- predict(object = model$score, 
                             newdata = cd$score[,model$score$predictors], 
                             type = "raw")
      cd$score[,paste0(model$score$response, ".", input$scoreModels[i])] <- 
        predictions
      
      # IMPOTANT NOTE:
      # I use my string 'model$score$predictors' in enhanced object from train
      # not the caret function predictors(), applied on finalModel (in train object)
      # model$scorePredictors <- predictors(model$score$finalModel) 
      # score (predict) response variable (apply input$scoreModels on cleansed, cd$score)
      # use the set of predictors used to train the model (for models like GBM, 
      # which requires the set to contain only the predictors used to fit the model)
    
    }
    
    return(paste0("Success: Scores (labels) predicted for ", nrow(cd$score), 
                  " observations. Scores estimated for variable ", 
                  "'", model$score$response, "'", " using the models ", 
                  "'", paste0(input$scoreModels, collapse = ", "), "'."))
  })
  
  output$scoreMessage <- renderText({
    if(is.null(ld$score)) return(NULL)
    if (!model$exists) return(NULL)
    score()
  }) 
    
  # SCORE Main -- SCORE Main -- SCORE Main -- SCORE Main 
  # display scored set with scorings
  
  output$scoreTitle <- renderText({ 
    if(is.null(cd$score)) return(NULL) 
    paste0("Scoring variable ", "'", model$score$response, "'", 
           " in unlabeled set ", "'", ld$scoreName, "'")  
  }) 
  
  output$scoreText1 <- renderText({ 
    if(is.null(cd$score)) return(NULL) 
    paste0("Models ", "'", paste0(input$scoreModels, collapse = ", "), "'",
           " are applied on unlabeled set ", 
           "'", ld$scoreName, "'", " (after been cleansed).",
           " All models score (predict) variable ", 
           "'", model$score$response, "'.")
  })  
    output$scoreText2 <- renderText({ 
      if(is.null(cd$score)) return(NULL) 
      paste0(" Models are built on training/test set ", 
             "'", ld$name, ", ", cd$name, "',", 
             " potentialy on different set of predictors and/or algorithms 
               and/or model parameters.", 
             # "'", paste0(model$score$predictors, collapse=","), "'.", 
             " Scorings (predictions) are added to the dataset as distinct columns, 
               one column for each applied predictive model.") 
  }) 
 
  output$scoreSave <- renderUI({
    if(is.null(cd$score)) return(NULL)
    actionButton("scoreSave", "Save Scoring")
  }) 
  
  scoreSave <-  eventReactive(input$score, { 
  })
  
  output$scoreSaveMessage <- renderText({
    if(is.null(cd$score)) return(NULL)
    ""  # scoreSave()
  }) 
 
  output$scoreSet <- DT::renderDataTable(expr = {
    if(is.null(cd$score)) return(NULL)
    predcols <- grep(pattern = model$score$response, 
                     x = names(cd$score), fixed = TRUE)
    if(nrow(cd$score) > cdRowsToDisplay) {
      DT::datatable(cd$score[1:cdRowsToDisplay, predcols], 
                    filter = list(position = 'top', clear = FALSE), 
                    options = list(lengthChange = FALSE, autoWidth = FALSE, 
                                   dom = 'tp')) 
    } else { 
      DT::datatable(cd$score[,predcols], 
                    filter = list(position = 'top', clear = FALSE), 
                    options = list(lengthChange = FALSE, autoWidth = FALSE, 
                                   dom = 'tp')) 
    }
  })

}) 