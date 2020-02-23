## Shiny app for TropFishR related data exploration
## 
##
## You can run the application by clicking the 'Run App' button above or
## sourcing this script. However, make sure you have instaled required
## packages (see below) and that the required data is in the same directory.
##



## Install required packages
##-----------------------------------------------------------------------------------
## install.packages("shiny")
## install.packages("shinydashboard")
## install.packages("devtools")
## devtools::install_github("tokami/TropFishR", ref="dev2.0")
##-----------------------------------------------------------------------------------



## Load packages
##-----------------------------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(TropFishR)
source("../functions/ELEFAN_GA_fun.R")
##-----------------------------------------------------------------------------------




data("alba")
dat <- alba
## dat <- synLFQ4




## Define UI for application
##-----------------------------------------------------------------------------------
ui <- fluidPage(


    ## Application title
    ## titlePanel("TropFishR - APP"),

    
    ## Style
    tags$head(tags$style(
                       
        HTML('
             #sidebar {
             background-color: #B9CEF7;
             }

             body, label, input, button, select { 
             font-family: "Arial";
             }')
      )),
    


    navbarPage("TropFishR-APP",
               tabPanel("Home",
                        
                        ),
               tabPanel("Load data"),
               tabPanel("Growth",

##                            headerPanel(title = "ELEFAN_GA"),
                        sidebarLayout(
                            
    sidebarPanel(
      submitButton("Re-start Genetic Algorithm"),
      numericInput(
        inputId = "seasonalized",
        label = "soVBGF (TRUE=1, FALSE=0)",
        min = 0, max = 1, value = 0, step = 1
      ),

      sliderInput(
        inputId = "MA",
        label = "Moving average (MA) span",
        value = 5, min = 5, max = round(length(dat$midLengths)/1.5),
        step = 2, round = TRUE
      ),

      sliderInput(
        inputId = "Linf",
        label = "Linf range",
        dragRange = TRUE,
        value = range(dat$midLengths), min = 0, max = max(dat$midLengths)*2,
        step = 1, round = TRUE
      ),

      sliderInput(
        inputId = "K",
        label = "K range",
        dragRange = TRUE,
        value = range(0.01, 3), min = 0.01, max = 3,
        step = 0.01
      ),

      sliderInput(
        inputId = "popSize",
        label = "Population size",
        value = 20, min = 10, max = 100,
        step = 5, round = TRUE
      ),

      sliderInput(
        inputId = "maxiter",
        label = "Maximum number of generations",
        value = 20, min = 10, max = 190, 
        step = 5, round = TRUE
      ),

      sliderInput(
        inputId = "run",
        label = "Stopping criteria (generations without improvement",
        value = 20, min = 10, max = 190, 
        step = 5, round = TRUE
      )
    ),

    mainPanel(
      tags$h4("GA fitness plot:"),
      plotOutput("ELEFAN_GA_score_plot"),
      tags$h4("LFQ plot:"),
      plotOutput("ELEFAN_GA_lfq_plot"),
      tags$h4("Fitted parameters:"),
      tableOutput("pars")
    ))

                        
                        ),
               tabPanel("Mortality/Selectivity"),
               tabPanel("Stock size"),
               tabPanel("Reference levels"),
               tabPanel("Information"))


    

    
  
)
##-----------------------------------------------------------------------------------






## Define server logic
##-----------------------------------------------------------------------------------
server <- function(input, output) {
  GA_res <- reactive({
    ELEFAN_GA_shiny(
      x=dat, seasonalised = input$seasonalized, MA = input$MA, parallel = FALSE,
      low_par = c(Linf=input$Linf[1], K=input$K[1], ta=0, C=0, ts=0),
      up_par = c(Linf=input$Linf[2], K=input$K[2], ta=1, C=1, ts=1),
      popSize=input$popSize, maxiter=input$maxiter, run = input$run,
      pmutation = 0.2
    )
  })
  
  PARS <- reactive({
    pars <- as.list(GA_res()@solution[1,])
    if(!input$seasonalized){
      names(pars) <- c("Linf", "K", "ta")
      pars <- pars[c("Linf", "K", "ta")]
    } else {
      names(pars) <- c("Linf", "K", "ta", "C", "ts")
    }
    pars$phiL <- log10(pars$K) + 2 * log10(pars$Linf)
    pars
  })
  
  lfq <- reactive({
    lfq <- dat
    lfq <- lfqRestructure(param = lfq, MA = input$MA)
    lfq <- lfqFitCurves(
      lfq = lfq, par=as.list(PARS())
    )
    lfq
  })

  output$pars <- renderTable({
    tmp <- PARS()
    tmp <- as.data.frame( c(PARS(), list(Rn_max = GA_res()@fitnessValue)) )
    names(tmp) <- replace(names(tmp), names(tmp)=="Rn_max", "Rn")
    names(tmp) <- replace(names(tmp), names(tmp)=="phiL", "phi'")
    tmp
  })
  
  
  output$ELEFAN_GA_score_plot <- renderPlot({ # should work but doesn't
    par(mar=c(5,5,2,1))
    plot(GA_res())
  })
  
  output$ELEFAN_GA_lfq_plot <- renderPlot({
    par(mar=c(5,5,2,1))
    plot(lfq(), Fname = "rcounts")
    # lfqFitCurves(lfq = lfq(), par=as.list(PARS()), draw = TRUE )
  })
  
}
##-----------------------------------------------------------------------------------











## Run the application
##-----------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
##-----------------------------------------------------------------------------------

