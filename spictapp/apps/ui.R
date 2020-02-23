## GUI for spictapp
## Tobias K. Mildenberger
## January 2020

## Load packages
##-----------------------------------------------------------------------------------
library(shiny)
options(shiny.trace=TRUE)
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
                 tags$style(HTML("
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

                           "))
             ),

        tags$style(
                 type="text/css",
                 "textarea {width:100%}"
             ),

        navbarPage(title = "spictapp",
                   id = "tabset",
                   theme = shinytheme("united"),
                   ##                   position = "fixed-top",
                   inverse = FALSE,
                   collapsible = TRUE,

                   tabPanel("Home", id = "home",
                            tagList(
                                HTML('<h1 style="text-align:center;float:center;line-height:50px;">spictapp</h1>
<h3 style="text-align:center;float:center;line-height:50px;"> - Apply the Stochastic surplus Production model in Continuous Time (SPiCT) without any R knowledge - </h3>
<hr style="clear:both;"/>'), br(),
div(img(src="spictManageDemo.png"), style="text-align: center;"),
br(),
##tags$hr(),
br(),
br(),
br()
)),

tabPanel("Load data", id = "loaddat",
         ##-----------------------------------------------------------
         headerPanel(title = "Load your data"),
         br(),

         tags$style(
                  HTML("hr{border-top: 2px solid #3891BA;}")),

         ## Sidebar panel for inputs
         sidebarPanel(

             div(style="display:inline-block;width:95%;text-align:center;",
                 ## Input: Select a file
                 fileInput("file1", "Choose your csv/txt file",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/x-csv",
                                      "text/tab-separated-values",
                                      "text/comma-separated-values",
                                      "text/x-comma-separated-values",
                                      "text/plain")),
                 actionButton("reset", label = "Clear")),

             br(),
             br(),

             "Please make sure that the headers of your data are called: 'timeC', 'obsC', 'timeI1', and 'obsI1', where the first two indicate the start of the catch interval (e.g. 1990, or 1990.25) and observations (e.g. 10333) of the commercial fleet(s), respectively, and the latter two indicate timing (e.g. 1990.5) and observations of the scientific fleet, respectively. If several fleets are available the the column names 'timeS2' and 'obsS2' (and 'timeS3' and 'obsS3' and so on) can be used.",

             br(),
             br(),

             wellPanel(
                 ## Input: Checkbox if file has header
                 checkboxInput("header", "Header", TRUE),
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
                                            selected = "head")))),

             ## tags$div(
             ##          HTML("<font size='4'>Example data</font>")),
             h3("Example data"),
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

             ## Output: Data file
             tableOutput("contents")

         )
         ),

tabPanel("Explore data", id = "explodat",
         ##-----------------------------------------------------------
         headerPanel(title = "Explore your data"),
         br(),
         sidebarLayout(
             sidebarPanel(id="sidebar",
                          br(),
                          ## tags$div(
                          ##          HTML("<font size='3'><b>LFQ restructuring</b></font>")),
                          h3("General settings"),
                          tags$hr(),

                          ## choose dteuler
                          selectInput(inputId = "dteuler",
                                      label = "Euler discretisation time step",
                                      choices = c("1/64"=1/64,"1/32"=1/32,
                                                  "1/16"=1/16,"1/8"=1/8,"1/4"=1/4,
                                                  "1/2"=1/2,"1"=1),
                                      selected = 1/16,
                                      width = '50%'),

                          ## choose time range of observations
                          uiOutput("timerange"),

                          br(),
                          br(),

                          h3("Management settings"),
                          tags$hr(),

                          ## management interval
                          uiOutput("maninterval"),

                          br(),
                          br(),

                          h3("Priors"),
                          tags$hr(),

                          br(),
                          br(),


                          h3("Display options"),
                          tags$hr()
                          ),

             # Show a plot of the generated distribution
             mainPanel(

                 ### element styles
                 br(),

                 tags$div(
                          HTML("<font size='4'>Plot of input data:</font>")),

                 ## data plot
                 plotOutput(outputId = "dataplot",
                            width = "100%", height = "700px"),

                 br(),


                 verbatimTextOutput(outputId = "mantimeline"),

                 br()
             )
         )
         ),
##-----------------------------------------------------------


tabPanel("Fit SPiCT", id = "fitspict",
         shinyjs::useShinyjs(),
         br(),
         headerPanel(title = "Fit the SPiCT model"),
         br(),
         sidebarLayout(
             sidebarPanel(
                 "Find more information and a user manual for SPiCT.",
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
                 br(),
                 numericInput(inputId = "seed",
                              label = "Seed value",
                              min = 1,
                              max = Inf,
                              value = 1234,
                              width = '20%'),
                 br(),
                 br()
             ),
             mainPanel(

                 tags$h4("SPiCT results"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 tags$h5("Plots"),
                 plotOutput("plot2",height="750px"),
                 br(),
                 br(),
                 tags$h5("Fit summary"),
                 verbatimTextOutput("fit"),
                 br(),
                 br(),
                 tags$h5("Diagnostics"),
                 plotOutput("plotDiag",height="900px"),
                 br(),
                 br(),
                 tags$h5("Priors"),
                 plotOutput("plotPrior",height="350px"),
                 br(),
                 br(),
                 tags$h5("Absolute trajectories"),
                 plotOutput("plotAbs",height="400px")
             ))
         ),
##-----------------------------------------------------------




tabPanel("Retrospective analysis", id = "retro",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "Analyse retrospective patterns"),
         br(),
         sidebarLayout(
             sidebarPanel(id="sidebar",
                          "The retro",
                          br(),
                          br(),
                          br(),
                          div(style="display:inline-block;width:95%;text-align: center;",
                              actionButton("runretro",
                                           label = "Run retro",
                                           icon = icon("arrow-right")),
                              actionButton("resetretro",
                                           label = "Clear")),
                          br(),
                          br(),
                          br(),
                          br()
                          ),
             ## Show a plot of the generated distribution
             mainPanel(
                 tags$h4("Plot"),
                 br(),
                 plotOutput(outputId = "LCCC_plot",
                            width = "100%"),
                 br(),
                 br()
             ))
         ),
##-----------------------------------------------------------

tabPanel("Management scenarios", id = "management",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "Explore management scenarios"),
         br(),
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
                          br(),
                          ## management interval
                          uiOutput("maninterval"),
                          br(),
                          br()
                          ),
             ##
             mainPanel(
                 tags$h4("Management interval"),
                 tags$hr(style = "border-top: dashed 2px #3891BA;"),
                 br(),
                 verbatimTextOutput(outputId = "mantimeline"),
                 br()
             ))
         ),
##-----------------------------------------------------------



tabPanel("Overview", id = "overview",
         ##-----------------------------------------------------------
         br(),
         headerPanel(title = "All results"),
         br(),
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
                 tags$h4("All parameters"),
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
