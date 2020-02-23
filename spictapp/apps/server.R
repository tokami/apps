## SERVER for spictapp
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


## Server script
##-----------------------------------------------------------------------------------
shinyServer(function(input, output, session) {

    ## GENERAL ##################################################################################################
    ## reactive buttons (when tabs changed or undo pressed)
    rv <- reactiveValues(doDatLoad = FALSE,
                         doSPICT = FALSE,
                         doRETRO = FALSE,
                         doMANA = FALSE)

    ## only run if action button used
    # observeEvent(input$file1, {
    #     if(!is.null(input$exdat) && input$useExDat){
    #         rv$doDatLoad <- TRUE
    #     }else if(!is.null(input$file1)){
    #         rv$doDatLoad <- TRUE
    #     }else{
    #         rv$doDatLoad <- FALSE
    #     }
    # })

    observe({
        input$useExDat
        input$file1
        if(input$useExDat){
            rv$doDatLoad <- TRUE
        }else if(!is.null(input$file1)){
            rv$doDatLoad <- TRUE
        }else{
            rv$doDatLoad <- FALSE
        }
    })

    observeEvent(input$exdat, {
        if(input$useExDat){
            rv$doDatLoad <- TRUE
        }else if(!is.null(input$file1)){
            rv$doDatLoad <- TRUE
        }else{
            rv$doDatLoad <- FALSE
        }
    })

    ## reset button
    observeEvent(input$reset, {
        ## reset file upload
        reset("file1")
        rv$doDatLoad <- FALSE
        rv$doSPICT <- FALSE
        rv$doRETRO <- FALSE
        rv$doMANA <- FALSE
        rv$dat <- NULL
        rv$inp <- NULL
        rv$inpORI <- NULL
        rv$filename <- ""
        rv$dteuler <- 1/16
        rv$timerange <- c(0,100)
        rv$lastCatchObs <- 100
        rv$maninterval <- c(100,101)
        ## reset example data
        rv$useExDat <- FALSE
        rv$exdat <- NULL
        updateCheckboxInput(session = session,
                            inputId = "useExDat",
                            value = FALSE)
        updateSelectInput(session = session,
                          inputId = "exdat",
                          selected = NULL)
    })


    ## DATA LOAD #############################################################################################

    datLoad <- function(){
        infile <- input$file1
        exdat <- as.character(input$exdat)
        if(input$useExDat){
            data(list="pol", package="spict")
            inpall <- get("pol")
            inp <- inpall[[which(names(inpall) == exdat)]]
            dat <- inp2dat(inp)
            rv$dat <- dat
            rv$inpORI <- inp
            rv$inp <- inp <- check.inp(inp)
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
        }else if(!is.null(infile)){
            dat <- read.csv(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            quote = input$quote)
            rv$dat <- dat
        }else{
            rv$dat <- NULL
        }
    }


    datPars <- function(){
        infile <- input$file1
        exdat <- as.character(input$exdat)
        topa <- "Data: "
        if(input$useExDat){
            rv$filename <- paste0(topa,as.character(exdat))
        }else if(!is.null(infile)){
            dat <- rv$dat
            rv$inpORI <- inp <- dat2inp(dat)
            rv$inp <- inp <- check.inp(inp)
            rv$dat <- dat
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
            ## filename
            tmp <- strsplit(infile[,1], ".csv")[[1]]
            tmp <- strsplit(tmp, ".txt")[[1]]
            filename <- paste0(topa,as.character(tmp))
            rv$filename <- filename
        }else{
            rv$inpORI <- NULL
            rv$inp <- NULL
            rv$dat <- NULL
            rv$filename <- ""
        }
    }

    output$downloadExData <- downloadHandler(
        filename = function() {
            paste0('spictdata_', input$exdat, '.csv')
        },
        content = function(con) {
            dat <- rv$dat
            # Wide to long
            ##           res <- setNames(res, c('length', 'dates', 'frequency'))
            write.csv(dat, con, row.names = FALSE)
        }
    )

    output$contents <- renderTable({
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            datLoad()
            if(ncol(rv$dat) < 2){
                stop("Something went wrong when uploading the data! Please try another separator or just try again!")
                return()
            }
            if(!input$useExDat && !any(colnames(rv$dat) == "timeC")){
                stop("Something went wrong when uploading the data! Please make sure that you have a column with the catch times in your file! This column must be called 'timeC'.")
                return()
            }
            if(!input$useExDat && !any(colnames(rv$dat) == "obsC")){
                stop("Something went wrong when uploading the data! Please make sure that you have a column with the catch observations in your file! This column must be called 'obsC'.")
                return()
            }
            if(!input$useExDat && !any(colnames(rv$dat) == "timeI1") && !any(colnames(rv$dat) == "timeE")){
                stop("Something went wrong when uploading the data! Please make sure that you have a column with either index or effort times in your file! This column must be called either 'timeI1' or 'timeE' for the index and effort, respectively.")
                return()
            }
            if(!input$useExDat && !any(colnames(rv$dat) == "obsI1") && !any(colnames(rv$dat) == "obsE")){
                stop("Something went wrong when uploading the data! Please make sure that you have a column with either index or effort observations in your file! This column must be called either 'obsI1' or 'obsE' for the index and effort, respectively.")
                return()
            }
            datPars()
            dat <- rv$dat
            if(input$disp == "head"){
                return(head(dat))
            }else{
                return(dat)
            }
        }
    })

    output$filename <- renderText({
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            return(rv$filename)
        }
    })


    ## EXPLORE DATA  #############################################################################################

    ## VARS:
    ## priors


    ## create timerange slider
    output$timerange <- renderUI({
        sliderInput(
            inputId = "timerange",
            label = "Time range of observations",
            dragRange = TRUE,
            value = rv$timerange,
            min = rv$timerange[1],
            max = rv$lastCatchObs,
            sep="",
            step=0.1
        )
    })

    ## create maninterval slider
    output$maninterval <- renderUI({
        sliderInput(
            inputId = "maninterval",
            label = "Management interval",
            dragRange = TRUE,
            value = rv$maninterval,
            min = rv$lastCatchObs,
            max = rv$lastCatchObs + 10,
            sep="",
            step=0.1
        )
    })

    update.inp <- reactive({
        req(input$dteuler)
        req(input$timerange)
        req(input$maninterval)
        if(is.null(rv$inp)){
            showNotification(
                paste("No data has been choosen. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                             type = "error",
                             duration = NULL,
                             closeButton = TRUE,
                             action = a(href = "javascript:location.reload();", "Reload page")
                             )
        }else{
            ## update rv elements (only if GUI depend on them/each other)
            ## update inp
            inp <- rv$inpORI
            inp$dteuler <- as.numeric(input$dteuler)
            inp <- shorten.inp(inp, mintime = input$timerange[1], maxtime = input$timerange[2])
            inp$maninterval <- input$maninterval
            inp <- check.man.time(inp)
            inp <- check.inp(inp)
            rv$inp <- inp
        }
    })


    output$dataplot <- renderPlot({
        req(rv$inp)
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            update.inp()
            inp <- rv$inp
            plotspict.data(inp)
        }
    })


    output$mantimeline <- renderPrint({
        req(rv$inp)
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            inp <- rv$inp
            man.timeline(inp)
        }
    })



    ## FIT SPICT  #############################################################################################

    observeEvent(input$tabset, {
        if(!is.null(rv$inp)){
            shinyjs::enable("fitspict")
            shinyjs::enable("resetspict")
        }else{
            shinyjs::disable("fitspict")
            shinyjs::disable("resetspict")
        }
    })

    ## only run if action button used
    observeEvent(input$fitspict, {
        rv$doSPICT <- input$fitspict
    })

    ## reset button
    observeEvent(input$resetspict, {
        rv$doSPICT <- FALSE
    })

    spict.res <- function(){
        if(is.null(rv$inp)){
            showNotification(
                paste("SPiCT requires an input list. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                type = "error",
                duration = NULL,
                closeButton = TRUE,
                action = a(href = "javascript:location.reload();", "Reload page")
            )
        }else{
            update.inp()
            inp <- rv$inp
            withProgress(message = "Running SPiCT", value = 0, {
                set.seed(input$seed)
                fit <- fit.spict(inp)
                fit <- calc.osa.resid(fit)
            })
            rv$fit <- fit
        }
    }

    output$fit <- renderPrint({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            isolate({
                spict.res()
            })
            summary(rv$fit)
        }
    })

    output$plot2 <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            plot2(rv$fit)
        }
    })

    output$plotPrior <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            nopriors <- get.no.active.priors(rv$fit$inp)
            par(mfrow = c(1,nopriors))
            plotspict.priors(rv$fit, automfrow = FALSE, do.plot=3)
        }
    })

    output$plotAbs <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            par(mfrow=c(1,2))
            plotspict.biomass(rv$fit)
            plotspict.f(rv$fit)
        }
    })

    output$plotDiag <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            plotspict.diagnostic(rv$fit)
        }
    })


})
