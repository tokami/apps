## GUI for spictapp
## Tobias K. Mildenberger
## January 2020

## Load packages
##-----------------------------------------------------------------------------------
library(shiny)
##options(shiny.trace=TRUE)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
## library(shinyWidgets)
## library(markdown)
library(rmarkdown)
## library(googleAuthR)
library(htmltools)
library(spict)


## Load scripts
##-----------------------------------------------------------------------------------
source("../functions/spictappFuncs.R")



## UI
##-----------------------------------------------------------------------------------
shinyUI(
    fluidPage(

        useShinyjs(),

        ## style settings
        tags$head(
                 tags$style(
                          HTML("
                           .navbar-nav {
                           float: none !important;
                           }
                           .navbar-nav > li:nth-child(8) {
                           float: right;
                           right: 220px;
                           }
                           .navbar-nav > li:nth-child(9) {
                           float: right;
                           font-weight: bold;
                           color: black;
                           }

                           ")
                      )
             ),

        tags$style(
                 type="text/css",
                 "textarea {width:100%}"
             ),

        navbarPage(
            title = "spictapp",
            id = "tabset",
            theme = shinytheme("united"),
            ##                   position = "fixed-top",
            inverse = FALSE,
            collapsible = TRUE,

            tabPanel(
                "Home", id = "home",
                tagList(
                    HTML('<h1 style="text-align:center;float:center;line-height:50px;">spictapp</h1>
<h3 style="text-align:center;float:center;line-height:50px;"> - The Shiny app for the Stochastic surplus Production model in Continuous Time (SPiCT) - </h3>
<hr style="clear:both;"/>'), br(),
div(img(src="spictManageDemo.png"), style="text-align: center;"),
br(),
tags$hr(),
br(),
br(),
br()
)),

tabPanel(
    "Load data", id = "loaddat",
    ##-----------------------------------------------------------
    headerPanel(title = "Load input data"),
    br(),

    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),

    ## Sidebar panel for inputs
    sidebarPanel(
        div(style="display:inline-block;width:95%;text-align:center;",

            h3("Upload data file"),
            tags$hr(),
            ## Input: Select a file
            fileInput("file1", "Choose a csv/txt file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/x-csv",
                                 "text/tab-separated-values",
                                 "text/comma-separated-values",
                                 "text/x-comma-separated-values",
                                 "text/plain")),
            actionButton("reset", label = "Clear")
            ),
        br(),
        br(),
        "Your file must contain at least 3 columns: One vector with the times corresponding to the observations, one with the commercial catch observations, and one with either index observations or effort observations.",
        br(),
        br(),
        h3("File properties"),
        tags$hr(),
        wellPanel(
            fluidRow(

                ## Input: Select separator
                column(4, radioButtons("sep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t",
                                                   "White space"  = ""),
                                       selected = ",")),
                ## Input: Select quotes
                column(4, radioButtons("quote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"')),
                ## Input: Select number of rows to display
                column(4, radioButtons("disp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head"))),
            ## Input: Checkbox if file has header
            checkboxInput("header", "Header", TRUE)
        ),
        br(),
        br(),
        br(),
        h3("Assign variables"),
        tags$hr(),
        "If the columns in your data set have different names than the defaults, please enter the column names of the available observations.",
        br(),
        br(),
        "Commercial catch:",
        wellPanel(
            fluidRow(
                column(6,
                       ## Time catches
                       uiOutput("timeC_lab")
                       ),
                column(6,
                       ## Catch observations
                       uiOutput("obsC_lab")
                       )
            )
        ),
        br(),
        "Indices from scientific surveys:",
        wellPanel(
            fluidRow(
                column(6,
                       uiOutput("timeI_lab")
                       ),
                column(6,
                       uiOutput("obsI_lab")
                       )
            ),
            "If several indices are available (for example from different surveys), please enter the column names of all index timings and index observations separated by a comma."
        ),
        br(),
        "Effort information (optional):",
        wellPanel(
            fluidRow(
                column(6,
                       ## Time effort
                       uiOutput("timeE_lab")
                       ),
                column(6,
                       ## Effort observations
                       uiOutput("obsE_lab")
                       )
            ),
            "Effort observations are optional if indices are available and required otherwise."
        ),
        br(),
        "Scaling of uncertainty of observations (optional):",
        wellPanel(
            fluidRow(
                uiOutput("stdevfacC_lab"),
                uiOutput("stdevfacI_lab"),
                uiOutput("stdevfacE_lab")

            ),
            "If available information about the uncertainty of the observations can be provided."
        ),
        br(),
        br(),
        h3("Use example data"),
        tags$hr(),
        checkboxInput(inputId = "useExDat",
                      label = "Use example data set?",
                      value = FALSE),
        br(),
        conditionalPanel(
            condition = "input.useExDat",
            ## Input: Select example data set
            selectInput(inputId = "exdat",
                        "Example data sets",
                        choices = c("albacore",
                                    "hake",
                                    "lobster"),
                        width='35%'),
            ## Download example data
            conditionalPanel("input.exdat",
                             downloadLink('downloadExData', 'Download')))
    ),

    ## Main panel for displaying outputs
    mainPanel(
        br(),

        fluidRow(
            column(6,
                   h3("Uploaded file in raw format:"),
                   tags$hr()
                   ),
            column(6,
                   ),
            br(),
            br(),
            column(2,
                   ),
            column(8,
                   tableOutput("fileContentRaw"),
                   ),
            column(2,
                   ),
            column(6,
                   h3("Data with assigned columns:"),
                   tags$hr()
                   ),
            column(6,
                   ),
            br(),
            br(),
            column(2,
                   ),
            column(8,
                   tableOutput("fileContent"),
                   )
        )
    )
),

tabPanel(
    "Explore & modify data", id = "explodat",
    ##-----------------------------------------------------------
    headerPanel(title = "Explore & modify input data"),
    br(),

    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),

    sidebarLayout(
        sidebarPanel(
            id="sidebar",
            br(),
            h3("General settings"),
            tags$hr(),
            fluidRow(
                ## Number of seasons
                column(6,
                       selectInput("nseasons",
                                   "Number of seasons",
                                   c(1,2,4),
                                   selected = 1,
                                   width = "100%")
                       ),
                ## choose dteuler
                column(6,
                       selectInput(inputId = "dteuler",
                                   label = "dteuler",
                                   choices = c("1/64"=1/64,"1/32"=1/32,
                                               "1/16"=1/16,"1/8"=1/8,"1/4"=1/4,
                                               "1/2"=1/2,"1"=1),
                                   selected = 1/16,
                                   width = '100%'
                                   )
                       )
            ),
            br(),
            fluidRow(
                column(6,
                       ## Index timings
                       selectizeInput("timeIshift",
                                      "Index timing",
                                      choices = NULL,
                                      multiple = TRUE,
                                      options = list(create = TRUE),
                                      width = "100%"
                                      ),
                       "Adjust the timing of the index within the year (e.g. 0.25 for April)"
                       ),
                ## Catch unit
                column(6,
                       textInput(
                           "cunit",
                           "Catch unit",
                           "",
                           width = "100%")
                       )
            ),
            br(),
            ## choose time range of observations
            uiOutput("timerange"),
            br(),
            checkboxInput(
                "robflagc",
                "Should the robust estimation for catches be used?",
                value = FALSE
            ),
            br(),
            uiOutput("robflagi"),
            checkboxInput(
                "robflage",
                "Should the robust estimation for effort be used?",
                value = FALSE
            ),
            br(),
            ## Number of seasons
            selectInput("splineorder",
                        "Splineorder",
                        c(2,3),
                        selected = 2,
                        width = "20%"),
            br(),
            br(),
            h3("Management settings"),
            tags$hr(),

            ## management interval
            uiOutput("maninterval"),
            br(),
            ## management evaluation time
            uiOutput("maneval"),
            br(),

            h3("Priors"),
            tags$hr(),
            "Default priors",
            wellPanel(
                fluidRow(
                    column(
                        3,
                        checkboxInput(
                            "lognPrior",
                            "log(n)",
                            TRUE)
                    ),
                    column(
                        4,
                        numericInput(
                            "lognPriorMu",
                            "mu",
                            log(2))
                    ),
                    column(
                        3,
                        numericInput(
                            "lognPriorSd",
                            "sd",
                            2)
                    )
                ),
                ## logalpha
                fluidRow(
                    column(
                        3,
                        checkboxInput(
                            "logAlphaPrior",
                            "log(alpha)",
                            TRUE)
                    ),
                    column(
                        4,
                        numericInput(
                            "logAlphaPriorMu",
                            "mu",
                            1)
                    ),
                    column(
                        3,
                        numericInput(
                            "logAlphaPriorSd",
                            "sd",
                            2)
                    )
                ),
                ##logbeta
                fluidRow(
                    column(
                        3,
                        checkboxInput(
                            "logBetaPrior",
                            "log(beta)",
                            TRUE)
                    ),
                    column(
                        4,
                        numericInput(
                            "logBetaPriorMu",
                            "mu",
                            1)
                    ),
                    column(
                        3,
                        numericInput(
                            "logBetaPriorSd",
                            "sd",
                            2)
                    )
                )
            ),
            br(),
            "Additional priors",
            wellPanel(
                fluidRow(
                    column(
                        3,
                        checkboxInput(
                            "BmsyB0Prior",
                            "BmsyB0 prior",
                            FALSE),
                        ),
                    column(
                        4,
                        numericInput(
                            "BmsyB0PriorMu",
                            "mu",
                            0.5)
                    ),
                    column(
                        3,
                        numericInput(
                            "BmsyB0PriorSd",
                            "sd",
                            2)
                    )

                )
            ),
            br(),
            br(),
            h3("Display options"),
            tags$hr(),
            checkboxInput(inputId = "dataplotAdv",
                          label = "Plot the advanced data plot?",
                          value = FALSE),
            ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("SPiCT timeline"),
            tags$hr(),
            div(style="display:inline-block;width:100%;text-align:left;",
                verbatimTextOutput(outputId = "mantimeline")),
            br(),
            br(),
            h3("Input data"),
            tags$hr(),
            plotOutput(outputId = "dataplot",
                       width = "100%", height = "800px"),
            br(),
            br(),
            h3("Priors"),
            tags$hr(),
            plotOutput(outputId = "priorplot",
                       width = "100%", height = "500px"),
            br()
        )
    )
),
##-----------------------------------------------------------
tabPanel(
    "Fit SPiCT", id = "fitspict",
    shinyjs::useShinyjs(),
    br(),
    headerPanel(title = "Fit SPiCT"),

    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),

    br(),
    sidebarLayout(
        sidebarPanel(
            "Find more information about the assumptions of the model in the ",
            a("SPiCT handbook",href="https://github.com/tokami/spict/blob/manage4.0/spict/inst/doc/spict_handbook.pdf"),
            br(),
            br(),
            br(),
            div(style="display:inline-block;width:95%;text-align:center;",
                actionButton("fitspict",
                             label = "Fit SPiCT",
                             icon = icon("arrow-right")),
                actionButton("resetspict",
                             label = "Clear")),
            br(),
            br(),
            h3("Estimation settings"),
            tags$hr(),
            br(),
            ## seed
            numericInput(inputId = "seed",
                         label = "Seed value",
                         min = 1,
                         max = Inf,
                         value = 1234,
                         width = '20%'),
            br(),
            fluidRow(
                column(6,
                       ## optimiser
                       selectInput(
                           "optimiser",
                           "Optimiser",
                           c("nlminb","optim"),
                           "nlminb")
                       ),
                column(6,
                       ## optim method
                       selectInput(
                           "optimMethod",
                           "optim method",
                           c("BFGS","SANN"),
                           "BFGS")
                       )
            ),
            br(),
            fluidRow(
                column(6,
                       ## optimiser control - maxiter
                       numericInput(
                           "itermax",
                           "Optimiser control - iter.max",
                           1e4)
                       ),
                column(6,
                       ## optimiser control - evalmax
                       numericInput(
                           "evalmax",
                           "Optimiser control - eval.max",
                           1e4)
                       )
            ),
            br()
        ),
        mainPanel(
            h3("Main SPiCT plots"),
            tags$hr(),
            plotOutput("plot2",height="750px"),
            br(),
            br(),
            h3("Summary of model fit"),
            tags$hr(),
            verbatimTextOutput("fit"),
            br(),
            br(),
            h3("Priors"),
            tags$hr(),
            plotOutput("plotPrior",height="350px"),
            br(),
            br(),
            h3("Absolute trajectories"),
            tags$hr(),
            plotOutput("plotAbs",height="400px")
        ))
),
##-----------------------------------------------------------


tabPanel(
    "Diagnostics", id = "diag",
    ##-----------------------------------------------------------
    br(),
    headerPanel(title = "Model diagnostics"),
    br(),
    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),
    sidebarLayout(
        sidebarPanel(
            id="sidebar",
            h3("Retrospective analysis"),
            tags$hr(),
            div(style="display:inline-block;width:95%;text-align: center;",
                actionButton("runretro",
                             label = "Run retro",
                             icon = icon("arrow-right")),
                actionButton("resetretro",
                             label = "Clear")),
            br(),
            br(),
            ## Number of retro years
            uiOutput("nretroyear"),
            br(),
            br(),
            h3("Sensitivity analysis"),
            tags$hr(),
            div(style="display:inline-block;width:95%;text-align: center;",
                actionButton("runsensi",
                             label = "Run check.ini",
                             icon = icon("arrow-right")),
                actionButton("resetsensi",
                             label = "Clear")),
            br(),
            br(),
            ## Number of trials
            uiOutput("nsensi"),
            br()
        ),
        ## Show a plot of the generated distribution
        mainPanel(
            h3("SPiCT diagnostics plot"),
            tags$hr(),
            plotOutput("plotDiag",height="900px"),
            br(),
            br(),
            h3("Retrospective analysis"),
            tags$hr(),
            plotOutput("plotRetro",height="900px"),
            br(),
            br(),
            h3("Sensitivity analysis"),
            tags$hr(),
            verbatimTextOutput("sensi"),
            br()
        ))
),
##-----------------------------------------------------------

tabPanel(
    "Management scenarios", id = "management",
    ##-----------------------------------------------------------
    br(),
    headerPanel(title = "Explore management scenarios"),
    br(),
    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),
    sidebarLayout(
        sidebarPanel(id="sidebar",

                     br(),
                     div(style="display:inline-block;width:95%;text-align: center;",
                         actionButton("runmanage",
                                      label = "Run Manage",
                                      icon = icon("arrow-right")),
                         actionButton("resetmanage",
                                      label = "Clear")),
                     br(),
                     br(),
                     br(),
                     br(),
                     h3("Management settings"),
                     tags$hr(),
                     br(),
                     ## scenarios
                     selectInput(inputId = "scenarios",
                                 label = "Management scenarios",
                                 choices = c("currentCatch" = 1,
                                             "currentF" = 2,
                                             "Fmsy" = 3,
                                             "noF" = 4,
                                             "reduceF25" = 5,
                                             "increaseF25" = 6,
                                             "msyHockeyStick" = 7,
                                             "ices" = 8),
                                 selected = c(1,3),
                                 width = '50%'),
                     br(),
                     br(),
                     ## management interval
                     uiOutput("maninterval2"),
                     br(),
                     br(),
                     ## management evaluation time
                     uiOutput("maneval2"),
                     br(),
                     br(),
                     ## Intermediate period catch
                     numericInput(
                         inputId = "ipc",
                         label = "Catch during intermediate period",
                         value = NULL,
                         min = 0),
                     br(),
                     br()
                     ),
        ##
        mainPanel(
            h3("Management times"),
            tags$hr(),
            verbatimTextOutput(outputId = "mantimeline2"),
            br(),
            h3("Management plot"),
            tags$hr(),
            plotOutput("plotMana",height="900px"),
            br(),
            br(),
            h3("Management summary"),
            tags$hr(),
            verbatimTextOutput("mana"),
            br()
        ))
),
##-----------------------------------------------------------



tabPanel(
    "Summary", id = "overview",
    ##-----------------------------------------------------------
    br(),
    headerPanel(title = "All results"),
    br(),
    tags$style(
             HTML("hr{border-top: 2px solid #d35400;}")),
    sidebarLayout(
        ##               left = "20%", right = "auto", bottom = "auto",
        ##               width = "80%", height = "auto",
        sidebarPanel(id="sidebar",
                     br(),
                     "Generate an Rmarkdown-based assessment report summarising the results of the assessment of your data:",
                     br(),
                     br(),
                     actionButton("generateReport", "Generate Assessment Report",
                                  icon = icon("file")),
                     br(),
                     br(),
                     "Download the assessment report:",
                     br(),
                     br(),
                     conditionalPanel(condition = "output.reportbuilt",
                                      downloadButton("downloadReport",
                                                     "Download Assessment Report")),
                     br(),
                     br(),
                     br(),
                     br(),
                     'Download all estimated parameters as a "csv" file:',
                     br(),
                     br(),
                     downloadButton("allParameters",
                                    label = "Download all parameters"),
                     br(),
                     br(),
                     br(),
                     br(),
                     'Download all graphs as a pdf files in a "zip" archive:',
                     br(),
                     br(),
                     downloadButton("allGraphs",
                                    label = "Download all graphs"),
                     br(),
                     br(),
                     br(),
                     br(),
                     'Download all data as a "RData" file:',
                     br(),
                     br(),
                     downloadButton("allData",
                                    label = "Download all data"),
                     br(),
                     br(),
                     br()
                     ),
        mainPanel(
            br(),
            h3("Parameter estimates"),
            tags$hr(),
            br(),
            br(),
            br()
        )
    )
),
##-----------------------------------------------------------



navbarMenu("More",
           ##-----------------------------------------------------------
           tabPanel("References", id = "references",
                    br(),
                    h3("Scientific articles:"),
                    tags$hr(),
                    "Mildenberger, T. K., Berg, C. W., Pedersen, M. W., Kokkalis, A., & Nielsen, J. R. (2020). Time-variant productivity in biomass dynamic models on seasonal and long-term scales. ICES Journal of Marine Science, 77(1), 174-187.",a("Link",href="https://academic.oup.com/icesjms/article/77/1/174/5572245"),
                    br(),
                    br(),
                    "Pedersen, M. W., & Berg, C. W. (2017). A stochastic surplus production model in continuous time. Fish and Fisheries, 18(2), 226-243. ",a("Link",href="https://github.com/DTUAqua/spict/blob/master/spict/inst/spict.pdf"),
                    tags$hr(),
                    br(),
                    br(),
                    br(),
                    h3("Tutorials:"),
                    tags$hr(),
                    "Pedersen, MW, Kokkalis, A, Mildenberger, TK, Berg, CW. 2020. SPiCT handbook.",a("Link",href="https://github.com/DTUAqua/spict/blob/master/spict/inst/doc/spict_manual.pdf"),
                    br(),
                    br(),
                    "Mildenberger, TK, Kokkalis, A, Berg, CW. 2020. SPiCT guidelines. ",a("Link",href="https://github.com/DTUAqua/spict/blob/master/spict/inst/doc/spict_guidelines.pdf"),
                    br(),
                    br(),

                    tags$hr(),
                    br(),
                    br(),
                    br(),
                    br()
                    ),
           tabPanel("About", id = "about",
                    br(),
                    tags$h2("spictapp"),
                    tags$hr(),
                    'Shiny app for SPiCT...',
                    br(),
                    br(),
                    br(),
                    br(),
                    tags$h2("Version"),
                    tags$hr(),
                    "Version number: v0.1 (Beta)",
                    br(),
                    br(),
                    br(),
                    br(),
                    tags$h2("News"),
                    tags$hr(),
                    "This is the beta version of spictapp. You can find detailed descriptions of new features, bug fixes, other changes of specific package versions concerning", a("spictapp",href="https://github.com/tokami/apps/tree/master/spict
"), " and concerning", a("SPiCT",href="https://raw.githubusercontent.com/DTUAqua/spict/master/spict/NEWS"),
br(),
br(),
br(),
br(),
tags$h2("Installation for offline use"),
tags$hr(),
'This application can be used offline. Please download the package from', a("GitHub",href="https://github.com/tokami/apps/spictapp"), 'with devtools::install_github("tokami/apps/spictapp") and run the R commands: require(spictapp) and runApp("apps/").',
br(),
br(),
br(),
br(),
tags$h2("Citation"),
tags$hr(),
"Please cite this application as:",
"Mildenberger TK. 2020. spictapp - The click-based user interface for SPiCT. doi:...",a("COMING",href=""),
br(),
br(),
br(),
br(),
tags$h2("Questions/Issues"),
tags$hr(),
"In case you have questions or find bugs, please write an email to",
a("Tobias Mildenberger",href="mailto:t.k.mildenberger@gmail.com"), "or post on",
a("apps/issues",href="https://github.com/tokami/apps/issues"), ". If you want to be updated with the development of the application and underlying R package (spict) or want to discuss with spictapp and SPiCT users and developers, follow the project on", a("ResearchGate", href="https://www.researchgate.net/project/Stochastic-production-model-in-continuous-time-SPiCT"),".",
br(),
br(),
br(),
br(),
tags$h2("Developer team"),
tags$hr(),
box(title = "Tobias K. Mildenberger",
    status = "primary",
    solidHeader = TRUE,
    collapsible = FALSE,
    background="black",
    width = 3,
    height = 1,
    fluidRow(column(width = 4, align = "center", imageOutput("tobm")),
             column(width = 8, align = "left", h5("Creator, Author, Maintainer"),  HTML("DTU AQUA<br/>National Institute of Aquatic Resources<br/>Technical University of Denmark<br/>Kemitorvet<br/>2800 Kgs. Lyngby<br/>Denmark"))
             )),
br()

)),
tabPanel(
    textOutput("filename")
)
)
)
)
##-----------------------------------------------------------
