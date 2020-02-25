## SERVER for spictapp
## Tobias K. Mildenberger
## January 2020

## Load packages
##-----------------------------------------------------------------------------------
library(shiny)
## options(shiny.trace=TRUE)
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
                         doSENSI = FALSE,
                         doMANA = FALSE)

    ## Defaults
    rv.defaults <- function(){
        rv$doDatLoad <- FALSE
        rv$doSPICT <- FALSE
        rv$doRETRO <- FALSE
        rv$doSENSI <- FALSE
        rv$doMANA <- FALSE
        rv$datORI <- NULL
        rv$colNames <- NULL
        rv$dat <- NULL
        rv$inp <- NULL
        rv$inpORI <- NULL
        rv$fit <- NULL
        rv$retro <- NULL
        rv$mana <- NULL
        rv$filename <- "No Data"
        rv$dteuler <- 1/16
        rv$timerange <- c(0,100)
        rv$lastCatchObs <- 100
        rv$maninterval <- c(100,101)
        ## reset example data
        rv$useExDat <- FALSE
        rv$exdat <- NULL
    }
    rv.defaults()

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

    ## DATA LOAD #############################################################################################

    ## observe({
    ##     input$useExDat
    ##     input$file1
    ##     if(input$useExDat){
    ##         rv$doDatLoad <- TRUE
    ##     }else if(!is.null(input$file1)){
    ##         rv$doDatLoad <- TRUE
    ##     }else{
    ##         rv$doDatLoad <- FALSE
    ##     }
    ## })

    observe({
        input$useExDat
        input$file1
        if(input$useExDat || !is.null(input$file1)){
            rv$doDatLoad <- TRUE
        }else{
            rv$doDatLoad <- FALSE
        }
    })

    observeEvent(input$exdat, {
        if(input$useExDat || !is.null(input$file1)){
            rv$doDatLoad <- TRUE
        }else{
            rv$doDatLoad <- FALSE
        }
    })

    ## reset button
    observeEvent(input$reset, {
        ## reset file upload
        reset("file1")
        rv.defaults()
        updateCheckboxInput(session = session,
                            inputId = "useExDat",
                            value = FALSE)
        updateSelectInput(session = session,
                          inputId = "exdat",
                          selected = NULL)
    })

    ## Catches
    output$timeC_lab <- renderUI({
        ind <- which("timeC" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeC_lab",
                    "Times of catch observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })
    output$obsC_lab <- renderUI({
        ind <- which("obsC" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsC_lab",
                    "Catch observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })

    ## Indices
    output$timeI_lab <- renderUI({
        ind <- which("timeI" == rv$colNames | "timeI1" == rv$colNames)
        if(length(ind) > 0){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeI_lab",
                    "Times of index observations",
                    choices = c("Choose one"="",rv$colNames),
                    multiple = TRUE,
                    selected = selected)
    })
    output$obsI_lab <- renderUI({
        ind <- which("obsI" == rv$colNames | "obsI1" == rv$colNames)
        if(length(ind) > 0){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsI_lab",
                    "Index observations",
                    choices = c("Choose one"="",rv$colNames),
                    multiple = TRUE,
                    selected = selected)
    })

    ## Effort
    output$timeE_lab <- renderUI({
        ind <- which("timeE" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeE_lab",
                    "Times of effort observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })
    output$obsE_lab <- renderUI({
        ind <- which("obsE" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsE_lab",
                    "Effort observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })


    ## scaling catch uncertainty
    output$stdevfacC_lab <- renderUI({
        ind <- which("stdevfacC" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacC_lab",
                    "Scaling of uncertainty of catch observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })
    output$stdevfacI_lab <- renderUI({
        ind <- which("stdevfacI" == rv$colNames | "stdevfacI1" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacI_lab",
                    "Scaling of uncertainty of index observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected,
                    multiple = TRUE)
    })
    ## scaling catch uncertainty
    output$stdevfacE_lab <- renderUI({
        ind <- which("stdevfacE" == rv$colNames)
        if(length(ind) == 1){
            selected <- rv$colNames[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacE_lab",
                    "Scaling of uncertainty of effort observations",
                    choices = c("Choose one"="",rv$colNames),
                    selected = selected)
    })

    datLoad <- function(){
        infile <- input$file1
        exdat <- as.character(input$exdat)
        topa <- "Data: "
        if(input$useExDat){
            rv$filename <- paste0(topa,as.character(exdat))
            data(list="pol", package="spict")
            inpall <- get("pol")
            inp <- inpall[[which(names(inpall) == exdat)]]
            dat <- inp2dat(inp)
            rv$datORI <- dat
            rv$dat <- dat
            rv$colNames <- colnames(dat)
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
            rv$datORI <- dat
            rv$colNames <- colnames(dat)
            ## filename
            tmp <- strsplit(infile[,1], ".csv")[[1]]
            tmp <- strsplit(tmp, ".txt")[[1]]
            filename <- paste0(topa,as.character(tmp))
            rv$filename <- filename
        }else{
            rv$datORI <- NULL
            rv$dat <- NULL
            rv$colNames <- NULL
        }
    }

    update.dat <- function(){
        datORI <- rv$datORI
        if(!input$useExDat){
            datORI <- rv$datORI
            colNames = list("timeC" = input$timeC_lab,
                            "obsC" = input$obsC_lab,
                            "timeI" = input$timeI_lab,
                            "obsI" = input$obsI_lab,
                            "timeE" = input$timeE_lab,
                            "obsE" = input$obsE_lab,
                            "stdevfacC" = input$stdevfacC_lab,
                            "stdevfacI" = input$stdevfacI_lab,
                            "stdevfacE" = input$stdevfacE_lab)
            if(length(colNames$timeC) < 1 || colNames$timeC == "") stop("No times of the catch observations provided! Choose corresponding column names.")
            if(length(colNames$obsC) < 1 || colNames$obsC == "") stop("No catch observations provided! Choose corresponding column names")
            if((length(colNames$timeI) < 1 || colNames$timeI == "") && (length(colNames$timeE) < 1 || colNames$timeE == "")) stop("Neither times for the index nor effort observations provided! Choose corresponding column names.")
            if((length(colNames$obsI) < 1 || colNames$obsI == "") && (length(colNames$obsE) < 1 || colNames$obsE == "")) stop("Neither index nor effort observations provided! Choose corresponding column names")
            rv$dat <- checkDat(datORI, colNames)
            dat <- rv$dat
            rv$inpORI <- inp <- dat2inp(dat)
            rv$inp <- inp <- check.inp(inp)
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
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

    output$fileContentRaw <- renderTable({
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            datLoad()
            datORI <- rv$datORI
            if(input$disp == "head"){
                return(head(datORI))
            }else{
                return(datORI)
            }
        }
    })

    output$fileContent <- renderTable({
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            infile <- input$file1
            if(!input$useExDat && !is.null(infile)){
                update.dat()
            }
            dat <- rv$dat
            if(is.null(dat)){
                return()
            }else{
                if(input$disp == "head"){
                    return(head(dat))
                }else{
                    return(dat)
                }
            }
        }
    })


    output$filename <- renderText({
        return(rv$filename)
    })


    ## EXPLORE DATA  #############################################################################################

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

    ## create maneval
    output$maneval <- renderUI({
        sliderInput(
            inputId = "maneval",
            label = "Management evaluation time",
            value = rv$maninterval[2],
            min = rv$lastCatchObs,
            max = rv$lastCatchObs + 10,
            sep="",
            step=0.1
        )
    })

    ## create robflagi (dependent on number of indices)
    output$robflagi <- renderUI({
        selectizeInput("robflagi",
                       "Should the robust estimation for indices be used?",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(create = TRUE),
                       width = "100%"
                       )
    })

    update.inp <- reactive({
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
            ## dteuler
            inp$dteuler <- as.numeric(input$dteuler)
            ## timing indices
            if(!is.null(input$timeIshift)){
                if(length(input$timeIshift) == length(inp$timeI)){
                    if(inherits(inp$timeI, "list")){
                        for(i in 1:length(inp$timeI)){
                            inp$timeI[[i]] <- inp$timeI[[i]] + as.numeric(input$timeIshift)[i]
                        }
                    }else{
                        inp$timeI <- inp$timeI + as.numeric(input$timeIshift)
                    }
                }else{
                    stop(paste0("Please provide one timing per index!"))
                }
            }
            inp <- shorten.inp(inp, mintime = input$timerange[1], maxtime = input$timerange[2])
            ## forecast times
            inp$maninterval <- input$maninterval
            inp$maneval <- input$maneval
            ## nseasons
            inp$nseasons <- as.numeric(input$nseasons)
            ## catchunit
            inp$catchunit <- input$cunit
            ## robflags
            inp$robflagc <- ifelse(input$robflagc == TRUE, 1, 0)
            robflagi <- input$robflagi
            if(is.null(robflagi) || length(robflagi) == length(inp$timeI)){
                robflagi <- rep(0, length(inp$timeI))
            }
            inp$robflagi <- robflagi
            inp$robflage <- ifelse(input$robflage == TRUE, 1, 0)
            ## splineorder
            inp$splineorder <- as.numeric(input$splineorder)
            ## checkinp
            inp <- check.man.time(inp)
            inp <- check.inp(inp)
            ## priors
            if(input$lognPrior)
                inp$priors$logn <- c(input$lognPriorMu,input$lognPriorSd,1)
            else
                inp$priors$logn <- c(input$lognPriorMu,input$lognPriorSd,0)
            if(input$logAlphaPrior)
                inp$priors$logalpha <- c(input$logAlphaPriorMu,input$logAlphaPriorSd,1)
            else
                inp$priors$logalpha <- c(input$logAlphaPriorMu,input$logAlphaPriorSd,0)
            if(input$logBetaPrior)
                inp$priors$logbeta <- c(input$logBetaPriorMu,input$logBetaPriorSd,1)
            else
                inp$priors$logbeta <- c(input$logBetaPriorMu,input$logBetaPriorSd,0)
            if(input$BmsyB0Prior)
                inp$priors$BmsyB0 <- c(input$BmsyB0PriorMu,input$BmsyB0PriorSd,1)
            else
                inp$priors$BmsyB0 <- c(input$BmsyB0PriorMu,input$BmsyB0PriorSd,0)
            ## save
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
            if(input$dataplotAdv){
                plotspict.ci(inp)
            }else{
                plotspict.data(inp)
            }
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

    output$priorplot <- renderPlot({
        req(rv$inp)
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            inp <- rv$inp
            nopriors <- get.no.active.priors(inp)
            automfrow <- FALSE
            if(nopriors < 4)
                par(mfrow = c(1,3))
            else if(nopriors < 7)
                par(mfrow = c(2,3))
            else if(nopriors < 10)
                par(mfrow = c(3,3))
            else
                automfrow <- TRUE
            plotspict.priors.inp(inp, automfrow = automfrow, do.plot=nopriors)
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
            inp$optimiser <- input$optimiser
            inp$optim.method <- input$optimMethod
            inp$optimiser.control <- list(iter.max = input$itermax, eval.max = input$evalmax)
            set.seed(input$seed)
            showModal(modalDialog("Please wait while fitting SPiCT.", footer=NULL))
            fit <- fit.spict(inp)
            fit <- calc.osa.resid(fit)
            removeModal()
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



    ## MODEL DIAGNOSTICS  ################################################################################

    observeEvent(input$tabset, {
        if(!is.null(rv$fit)){
            shinyjs::enable("runretro")
            shinyjs::enable("resetretro")
        }else{
            shinyjs::disable("runretro")
            shinyjs::disable("resetretro")
        }
    })

    ## only run if action button used
    observeEvent(input$runretro, {
        rv$doRETRO <- input$runretro
    })

    ## reset button
    observeEvent(input$resetretro, {
        rv$doRETRO <- FALSE
    })

    observeEvent(input$tabset, {
        if(!is.null(rv$fit)){
            shinyjs::enable("runsensi")
            shinyjs::enable("resetsensi")
        }else{
            shinyjs::disable("runsensi")
            shinyjs::disable("resetsensi")
        }
    })

    ## only run if action button used
    observeEvent(input$runretro, {
        rv$doSENSI <- input$runretro
    })

    ## reset button
    observeEvent(input$resetsensi, {
        rv$doSENSI <- FALSE
    })

    ## create nretroyear slider
    output$nretroyear <- renderUI({
        numericInput(
            inputId = "nretroyear",
            label = "Number of years to remove in retrospective analysis",
            value = 5,
            min = 1,
            max = diff(rv$timerange) - 5,  ## give spict at least 5 years
            step=1, width="20%"
        )
    })

    ## create ntrials for check.ini
    output$nsensi <- renderUI({
        numericInput(
            inputId = "nsensi",
            label = "Number of trials for sensitivity analysis",
            value = 10,
            min = 1,
            step=1, width="20%"
        )
    })

    spict.retro <- function(){
        if(is.null(rv$fit)){
            showNotification(
                paste("The retrospective analysis requires a fitted SPiCT model. Please go to the tab 'Fit SPiCT' and fit the SPiCT model to your or example data."),
                type = "error",
                duration = NULL,
                closeButton = TRUE,
                action = a(href = "javascript:location.reload();", "Reload page")
            )
        }else{
            fit <- rv$fit
            showModal(modalDialog("Please wait while running the retrospective analysis.", footer=NULL))
            retro <- retro(fit, nretroyear = input$nretroyear)
            removeModal()
            rv$retro <- retro
        }
    }

    spict.sensi <- function(){
        if(is.null(rv$fit)){
            showNotification(
                paste("The sensitivity analysis requires a fitted SPiCT model. Please go to the tab 'Fit SPiCT' and fit the SPiCT model to your or example data."),
                type = "error",
                duration = NULL,
                closeButton = TRUE,
                action = a(href = "javascript:location.reload();", "Reload page")
            )
        }else{
            fit <- rv$fit
            showModal(modalDialog("Please wait while running the sensitivity analysis.", footer=NULL))
            sensi <- check.ini(fit, ntrials = input$nsensi)
            removeModal()
            rv$sensi <- sensi
        }
    }

    output$sensi <- renderPrint({
        if(rv$doSENSI == FALSE){
            return()
        }else{
            isolate({
                spict.sensi()
            })
            sumspict.ini(rv$sensi)
        }
    })

    output$plotDiag <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            plotspict.diagnostic(rv$fit)
        }
    })

    output$plotRetro <- renderPlot({
        if(rv$doRETRO == FALSE){
            return()
        }else{
            isolate({
                spict.retro()
            })
            plotspict.retro(rv$retro)
        }
    })

    ## MANAGEMENT  ##################################################################################

    observeEvent(input$tabset, {
        if(!is.null(rv$fit)){
            shinyjs::enable("runmana")
            shinyjs::enable("resetmana")
        }else{
            shinyjs::disable("runmana")
            shinyjs::disable("resetmana")
        }
    })

    ## only run if action button used
    observeEvent(input$runmana, {
        rv$doMANA <- input$runmana
    })

    ## reset button
    observeEvent(input$resetmana, {
        rv$doMANA <- FALSE
    })


    ## create maninterval slider
    output$maninterval2 <- renderUI({
        sliderInput(
            inputId = "maninterval2",
            label = "Management interval",
            dragRange = TRUE,
            value = rv$maninterval,
            min = rv$lastCatchObs,
            max = rv$lastCatchObs + 10,
            sep="",
            step=0.1
        )
    })

    ## create maneval
    output$maneval2 <- renderUI({
        sliderInput(
            inputId = "maneval",
            label = "Management evaluation time",
            value = rv$maninterval[2],
            min = rv$lastCatchObs,
            max = rv$lastCatchObs + 10,
            sep="",
            step=0.1
        )
    })

    ## deactive ipc if no intermediate period
    observeEvent(input$maninterval2,{
        manstart <- input$maninterval2[1]
        lastobs <- rv$lastCatchObs
        if(manstart - lastobs > 0){
            shinyjs::enable("ipc")
        }else{
            shinyjs::disable("ipc")
        }
    })

    ## manage function
    spict.mana <- function(){
        if(is.null(rv$fit)){
            showNotification(
                paste("The management functionality requires a fitted SPiCT model. Please go to the tab 'Fit SPiCT' and fit the SPiCT model to your or example data."),
                type = "error",
                duration = NULL,
                closeButton = TRUE,
                action = a(href = "javascript:location.reload();", "Reload page")
            )
        }else{
            fit <- rv$fit
            showModal(modalDialog("Please wait while running the management scenarios.", footer=NULL))
            mana <- manage(fit, scenarios = input$scenarios,
                           maninterval = input$maninterval2,
                           maneval = input$maneval,
                           intermediaterPeriodCatch = input$ipc)
            removeModal()
            rv$mana <- mana
        }
    }

    output$mana <- renderPrint({
        if(rv$doMANA == FALSE){
            return()
        }else{
            isolate({
                spict.mana()
            })
            sumspict.manage(rv$mana)
        }
    })

    output$mantimeline2 <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE){
            return()
        }else if(!is.null(rv$mana)){
            man.timeline(rv$mana)
        }else{
            man.timeline(rv$fit)
        }
    })

    output$plotMana <- renderPlot({
        if(rv$doMANA == FALSE){
            return()
        }else{
            plot2(rv$mana)
        }
    })

    ## OVERVIEW  ##################################################################################


})
