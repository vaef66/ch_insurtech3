library(shiny) 
 
shinyUI(navbarPage(
  title = "EXPLORATORY & PREDICTIVE ANALYTICS",
  
  tabPanel( 
    title = "Get Data",

    sidebarLayout(
      
      sidebarPanel(
        h4(strong("Load a dataset to work on")),
        p("Make predictions using (predictive) models built on data in matrix 
          (tabular) format, where rows represent observations and columns 
          represent attributes (variables)."),
        p("Feed the application with a tabular dataset selected from one of
          the available sources: a set from an external source (for example, 
          a delimited/csv, local text file or an R package) or a set you work 
          with and possibly have already carried out a certain amount of 
          cleansing, exploration and modeling work on it. The term 'working set' 
          primarily refers to the cleansed version of the original, external   
          data, that serves as a training/test set on which predictive models 
          are built and assessed."),
        p("Once a working set is prepared, and models are built and assessed 
          on it then you may want to load a second, unlabeled set, with identical
          format as the uncleansed, original set, in order to make predictions, 
          i.e. estimate scores for observations in it."),
        p("After pressing the load button, a tabular view of the original 
          and cleansed versions of the loaded set are displayed in the right 
          panel of the current and next tab."), 
        br(), 
        radioButtons(inputId  = "source",
                     label    = "Select source/purpose of data loading:", 
                     choices  = list(
                       "Working dataset (train/test set)" = "working", 
                       "External dataset (train/test set)"= "exttrain", 
                       "External dataset (unlabeled data, to score)"= "extscore"), 
                     selected = "working", 
                     inline = FALSE), 
        br(),
        uiOutput("sourceControls"), 
        uiOutput("sourceControlsWorking"),
        uiOutput("sourceControlsExternal"),
        uiOutput("sourceControlsPackage"),
        br(), 
        div(textOutput("loadMessage"), style = "color:blue") 
      ),
      
      mainPanel(
        fluidRow(
          column(width = 10 
                 , h3(strong(textOutput("loadTitle"))) 
          ),
          column(width = 2
                 #, uiOutput("deleteWorkingSet")
                 
          )
        ),
        br(),
        strong(textOutput("loadText1")),
        strong(textOutput("loadText2")),
        strong(textOutput("loadText3")),
        br(), 
        h4(strong(textOutput("loadSetTitle"))),
        DT::dataTableOutput("loadSet")
      ) 
    )
    
  ),
  
  tabPanel(
    title = "Prepare (clean) Data",
    
    sidebarLayout(
    
      sidebarPanel(
        
        h4(strong(textOutput("cleanTitleSidebar"))), 
        p(textOutput("cleanText1Sidebar")),
        p(textOutput("cleanText2Sidebar")),
        br(),
        uiOutput("cleanAction"), 
        br(), 
        uiOutput("cleanActionControls"), 
        br(),
        uiOutput("cleanActionButtons"),
        br(),
        div(textOutput("cleanMessage"), style = "color:blue")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Cleaning Dataset", 
            fluidRow(
              column(width = 5, 
                     h3(strong(textOutput("cleanTitleSet")))
              ),
              column(width = 5,
                     br(),
                     uiOutput("cleanSaveName")
              ),
              column(width = 2,
                     br(),
                     uiOutput("cleanSave")
              )
            ), 
            fluidRow(
              column(width = 10, 
                     div(textOutput("cleanSaveMessage"), style = "color:blue")
              ),
              column(width = 2,
                     br()
              ) 
            ), 
            strong(textOutput("cleanTextSet")),
            br(),
            h4(strong(textOutput("cleanTitleSummary"))), 
            br(),
            DT::dataTableOutput("cleanSummary"),
            br(), br(),
            h4(strong(textOutput("cleanTitleData"))),
            br(),
            DT::dataTableOutput("cleanData")
          ), 
          tabPanel(
            title = "Sequence of Cleaning Actions", 
            fluidRow(
              column(width = 8, 
                     h3(strong(textOutput("cleanTitleSequence")))
              ),
              column(width = 2,
                     br(), 
                     uiOutput("cleanDiscardSequence")
              ),
              column(width = 2,
                     br(),
                     uiOutput("cleanApplySequence")
              )
            ),
            br(),
            strong(textOutput("cleanTextSequence")),
            br(), 
            DT::dataTableOutput("cleanSequence")
          )
        ) 
      )
      
    )
    
  ), 
  
  tabPanel(
    title = "Explore Variables",
    
    sidebarLayout(
      
      sidebarPanel(
        h4(strong(textOutput("exploreTitleSidebar"))),
        p(textOutput("exploreTextSidebar")),
        br(),
        h4(strong(textOutput("distTitleSidebar"))),
        uiOutput("distVar"),
        #uiOutput("distControls"),
        br(),
        h4(strong(textOutput("relTitleSidebar"))), 
        uiOutput("relVars"),
        uiOutput("relControls")
      ),
      
      mainPanel(
        h3(strong(textOutput("exploreTitleMain"))),
        br(),
        h4(strong(textOutput("distTitleMain"))),
        fluidRow(
          column(width = 6, 
                 plotOutput("distHistorBar")
          ),
          column(width = 6,
                 plotOutput("distBoxorBar")
          )
        ),
        # plot variable summary
        br(),
        h4(strong(textOutput("relTitleMain"))), 
        fluidRow(
          column(width = 6, 
                 plotOutput("relScatterorBox")
          ),
          column(width = 6,
                 br() # plot correlation matrix
          )
        )
      )
      
    )
    
  ),
  
  tabPanel(
    title = "Build Model(s)",
    
    sidebarLayout( 
      
      sidebarPanel(
        h4(strong(textOutput("trainTitleSidebar"))),
        p(textOutput("trainTextSidebar1")),
        p(textOutput("trainTextSidebar2")),
        br(), 
        h4(strong(textOutput("taskText"))),
        uiOutput("task"),   
        br(), 
        h4(strong(textOutput("variablesText"))),
        uiOutput("response"),
        uiOutput("predictors"),
        br(), 
        h4(strong(textOutput("metricsText"))),
        uiOutput("metrics"),
        uiOutput("metric"),
        br(),
        h4(strong(textOutput("trainMethodParamsText"))),
        uiOutput("trainMethod"),
        uiOutput("trainParams"),
        br(),
        h4(strong(textOutput("tuneMethodOptionsText"))),
        uiOutput("tuneMethod"),
        uiOutput("tuneOptionsButton"),
        br(),
        div(textOutput("modelTuneMessage"), style = "color:blue")
      ), 
      
      mainPanel( 
        fluidRow(
          column(width = 6,
                 h3(strong(textOutput("tuneTitle"))) 
          ),
          column(width = 4,
                 br(),
                 uiOutput("modelSaveName")
          ),
          column(width = 2,
                 br(), 
                 uiOutput("modelSave")
          ) 
        ),  
        fluidRow(
          column(width = 10,
                 div(textOutput("modelSaveMessage"), style = "color:blue")
          ),
          column(width = 2,
                 br()
          ) 
        ), 
        h4(textOutput("tuneResponse")),
        h4(textOutput("tunePredictors")), 
        h4(textOutput("buildSet")), 
        h4(textOutput("assessSet")),
        h4(textOutput("trainingMethod")),
        h4(textOutput("tuningMethod")), 
        h4(textOutput("modelMetric")),
        h4(textOutput("timeElapsed")),
        br(), 
        h4(strong(textOutput("tuneResultsTitle"))),
        DT::dataTableOutput("tuneResults"),
        br(),
        h4(strong(textOutput("tuneBestTitle"))),
        DT::dataTableOutput("tuneBestParameters"),
        br(),
        # testing
        #DT::dataTableOutput("tuneGrid"),
        # move to model assessment 
        #h4(strong(textOutput("tuneProfileTitle"))),
        br(),
        #h4(strong(textOutput("tuneDiagnosticsTitle"))),
        br(),
        fluidRow(
          column(width = 6,  
                 br()
                 #plotOutput("1")
          ),
          column(width = 6,
                 br()
                 #plotOutput("1") 
          )
        )
      )
      
    )  
  ), 

  tabPanel(
    title = "Assess Model(s)",
    
    sidebarLayout(
      
      sidebarPanel( 
        h4(strong(textOutput("assessTitleSidebar"))),
        p(textOutput("assessTextSidebar1")), 
        p(textOutput("assessTextSidebar2")),
        br(),
        uiOutput("assessResponses"),
        uiOutput("assessResponseLevels"),
        uiOutput("assessResponseModels"),
        uiOutput("assessMetrics"),
        br(),
        uiOutput("assessButton"),
        br(),
        div(textOutput("assessMessage"), style = "color:blue")
      ),
      
      mainPanel( 
        fluidRow(
          column(width = 10,
                 h3(strong(textOutput("assessTitle"))) 
                 #div(textOutput("assessSaveMessage"), style = "color:blue")
          ), 
          column(width = 2,
                 br() 
                 #uiOutput("assessSave")
          ) 
        ),  
        br(),
        strong(textOutput("assessText1")), 
        strong(textOutput("assessText2")), 
        br(),
        DT::dataTableOutput("assessTable")
      )
      
    )
    
  ),
  
  tabPanel(
    title = "Make Predictions",
    
    sidebarLayout(
      
      sidebarPanel(
        h4(strong(textOutput("scoreTitleSidebar"))),
        p(textOutput("scoreTextSidebar1")), 
        p(textOutput("scoreTextSidebar2")), 
        p(textOutput("scoreTextSidebar3")), 
        br(),
        uiOutput("scoreResponses"),
        uiOutput("scoreModelsForResponse"),
        br(),
        div(textOutput("scoreMessage"), style = "color:blue")
      ),
      
      mainPanel(
        fluidRow(
          column(width = 10,
                 h3(strong(textOutput("scoreTitle"))), 
                 div(textOutput("scoreSaveMessage"), style = "color:blue")
          ), 
          column(width = 2,
                 br(), 
                 uiOutput("scoreSave")
          ) 
        ),  
        br(),
        strong(textOutput("scoreText1")), 
        strong(textOutput("scoreText2")),
        br(),
        DT::dataTableOutput("scoreSet")
      )
 
    )
    
  )
  
))