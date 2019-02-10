
# SESSION Globals

# By default, Shiny limits file uploads to 5MB per file. You can modify this limit by
# using the shiny.maxRequestSize option. 
# For example, adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R
# would increase the limit to 30MB

# if shiny runs locally (which means that SHINY_PORT or SHINY_SERVER_VERSION are 
# not set), then set maxRequestSize to a larger limit (here a few tens of MBs)
if (Sys.getenv('SHINY_PORT') == "") 
  options(shiny.maxRequestSize = 100*1024^2)
if (Sys.getenv('SHINY_PORT') != "") 
  options(shiny.maxRequestSize = 20*1024^2)

# datapaths & global variables
datapath <- "data"
ldpath <- "data/ld"
cdRowsToDisplay <- 100
cdRowsToSummarize <- "all"


# CLEANSING structures

# maintain cleansing actions lists, for tracking cleansing sequence 
# (or history or lineage)
# adding/removing a cleansig action: also update UI (controls), add/remove function 
cleanActionsList <- list(
  "Select/remove columns (variables)" = "select",
  "Filter rows (obdervations)" = "filter", 
  "Factorize (categorize) columns (variables)" = "factorize", 
  "Discretize numeric column (new variable)" = "discretize", 
  "Sample rows (obdervations)" = "sampleset", 
  "Derive column (new variable)" = "derive", 
  "Scale numeric columns (variables)" = "scale",
  # "Transform numeric column (new variable)" = "transform", 
  # "Group categorical column (new variable)" = "group", 
  "Identify/handle missing values" = "missing", 
  "Detect/handle outliers/errors" = "outliers"
) 
# args of clean functions - have 1-1 relation to controls of clean actions
# here we only track arg names: for each action/function a character vector
# args names also appear in renderUI server-function & individual action-functions
cleanActionsArgs <- list(
  "select"       =  c("columns", "remove"), 
  "filter"       =  c("exprText"),
  "factorize"    =  c("columns", "threshold"),
  "discretize"   =  c("column", "breaks", "levels", "newcolumn"),
  "group"        =  c("column", "groups", "levels", "newcolumn"), 
  "sampleset"    =  c("samplePercent", "stratified", "stratifyOnColumn"), 
  "transform"    =  c("column", "transformation"),
  "derive"       =  c("exprText", "newcolumn"),
  "scale"        =  c("columns"),
  "missing"      =  c("columns", "indicators", "handling", "convert"),
  "outliers"     =  c()
)
# cleanActionsHistList <- list() 
# not used yet
seq.init <- list(action       = "",
                 # exclude    = FALSE,
                 arguments    = list(),   
                 result       = list()) 

# TRAIN/TUNE structures

# task list
taskList <- list("Regression" = "regr", 
                 "Classification" = "class")

# dataframes/lists of training methods & their tuning params (load from csv file)
trainMethodsDf <- read.table(file = 'support/trainMethods.csv', header = TRUE, 
                             sep = ';', stringsAsFactors = FALSE)

# list of training methods tuning parameters
trainParamsList <- list()
for (i in 1:nrow(trainMethodsDf)) {
  trainParamsList[[trainMethodsDf[i, "id"]]] <-
    list(methoddesc = trainMethodsDf[i, "methoddesc"], 
         pName      = eval(parse(text = trainMethodsDf[i, "pName"])), 
         pDesc      = eval(parse(text = trainMethodsDf[i, "pDesc"])), 
         pValue     = eval(parse(text = trainMethodsDf[i, "pValue"]))
         )
    trainMethodsDf[i, "id"]
} 
# test: trainParamsList$glmnet$pValue == trainParamsList1$glmnet$pValue
#       trainParamsList[[5]]$pValue == trainParamsList1[[5]]$pValue

# list of tuning methods
tuneMethodList <- list("Cross-Validation (CV)" = "cv",
                       "Repeated Cross-Validation" = "repeatedcv",
                       "Bootstrap" = "boot",
                       "LOOCV (caution: too slow)" = "LOOCV") 
