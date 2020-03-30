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
source("../funcs/serverFuncs.R")


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
        rv$colNamesORI <- NULL
        rv$colNames <- NULL
        rv$dat <- NULL
        rv$inp <- NULL
        rv$inpORI <- NULL
        rv$seed <- NULL
        rv$fit <- NULL
        rv$retro <- NULL
        rv$mana <- NULL
        rv$filename <- "Data: -"
        rv$dteuler <- 1/16
        rv$timerange <- c(0,100)
        rv$lastCatchObs <- 100
        rv$maninterval <- c(100,101)
        rv$nseasons <- 1
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


    observeEvent(input$useExDat, {
        rv$doDatLoad <- TRUE
    })

    observeEvent(input$file1, {
        rv$doDatLoad <- TRUE
    })

    ## reset button
    observeEvent(input$reset, {
        ## reset file upload
        reset("file1")
        rv.defaults()
        updateRadioButtons(session = session,
                           inputId = "sep",
                           selected = ",")
        updateRadioButtons(session = session,
                           inputId = "quote",
                           selected = "'")
        updateRadioButtons(session = session,
                           inputId = "header",
                           selected = "TRUE")
        updateRadioButtons(session = session,
                           inputId = "dispRaw",
                           selected = "head")
        updateRadioButtons(session = session,
                           inputId = "disp",
                           selected = "head")
        ## example data
        updateCheckboxInput(session = session,
                            inputId = "useExDat",
                            value = FALSE)
        updateSelectInput(session = session,
                          inputId = "exdat",
                          selected = NULL)
    })

    ## Catches
    output$timeC_lab <- renderUI({
        ind <- which("timeC" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeC_lab",
                    "Times of catch observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })
    output$obsC_lab <- renderUI({
        ind <- which("obsC" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsC_lab",
                    "Catch observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })

    ## Indices
    output$timeI_lab <- renderUI({
        ind <- which(rv$colNamesORI %in% c("timeI","timeI1","timeI2","timeI3"))
        if(length(ind) > 0){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeI_lab",
                    "Times of index observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    multiple = TRUE,
                    selected = selected)
    })
    output$obsI_lab <- renderUI({
        ind <- which(rv$colNamesORI %in% c("obsI","obsI1","obsI2","obsI3"))
        if(length(ind) > 0){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsI_lab",
                    "Index observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    multiple = TRUE,
                    selected = selected)
    })

    ## Effort
    output$timeE_lab <- renderUI({
        ind <- which("timeE" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("timeE_lab",
                    "Times of effort observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })
    output$obsE_lab <- renderUI({
        ind <- which("obsE" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("obsE_lab",
                    "Effort observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })


    ## scaling catch uncertainty
    output$stdevfacC_lab <- renderUI({
        ind <- which("stdevfacC" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacC_lab",
                    "Catch observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })
    output$stdevfacI_lab <- renderUI({
        ind <- which(rv$colNamesORI %in% c("stdevfacI","stdevfacI1","stdevfacI2","stdevfacI3"))
        if(length(ind) > 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacI_lab",
                    "Index observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected,
                    multiple = TRUE)
    })
    ## scaling catch uncertainty
    output$stdevfacE_lab <- renderUI({
        ind <- which("stdevfacE" == rv$colNamesORI)
        if(length(ind) == 1){
            selected <- rv$colNamesORI[ind]
        }else{
            selected <- NULL
        }
        selectInput("stdevfacE_lab",
                    "Effort observations",
                    choices = c("Choose one"="",rv$colNamesORI),
                    selected = selected)
    })

    load.dat <- function(){
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
            rv$colNamesORI <- colnames(dat)
            rv$colNames <- colnames(dat)
            rv$inpORI <- inp
            rv$inp <- inp <- check.inp(inp)
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
            rv$nseasons <- inp$nseasons
        }else if(!is.null(infile)){
            dat <- read.csv(input$file1$datapath,
                            header = as.logical(input$header),
                            sep = input$sep,
                            quote = input$quote)
            rv$datORI <- dat
            rv$colNamesORI <- colnames(dat)
            rv$dat <- NULL
            rv$colNames <- NULL
            rv$inpORI <- NULL
            rv$inp <- NULL
            ## filename
            tmp <- strsplit(infile[,1], ".csv")[[1]]
            tmp <- strsplit(tmp, ".txt")[[1]]
            filename <- paste0(topa,as.character(tmp))
            rv$filename <- filename
        }else{
            rv$datORI <- NULL
            rv$dat <- NULL
            rv$colNamesORI <- NULL
        }
    }

    match.cols <- function(){
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
            rv$colNames <- colNames
        }
    }

    update.dat <- function(){
        datORI <- rv$datORI
        colNames <- rv$colNames
        if(!input$useExDat && !is.null(colNames) && !is.null(datORI)){
            rv$dat <- checkDat(datORI, colNames)
            dat <- rv$dat
            rv$inpORI <- inp <- dat2inp(dat)
            rv$inp <- inp <- check.inp(inp)
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
            rv$nseasons <- inp$nseasons
        }
    }


    observe({
        if(rv$doDatLoad){
            load.dat()
        }
    })

    observe({
        if(!is.null(rv$datORI)){
            match.cols()
            colNames <- unlist(rv$colNames)
            if(any(colNames == "timeC") && any(colNames == "obsC") &&
               ((any(colNames == "timeI1") && any(colNames == "obsI1")) ||
                (any(colNames == "timeE") && any(colNames == "obsE")))){
                update.dat()
            }
        }
    })

    observe({
        if(!is.null(rv$datORI)){
            shinyjs::enable("datUpdate")
        }else{
            shinyjs::disable("datUpdate")
        }
    })

    ## only run if action button used
    observeEvent(input$datUpdate, {
        update.dat()
    })

    output$downloadExData <- downloadHandler(
        filename = function() {
            paste0('spictdata_', input$exdat, '.csv')
        },
        content = function(con) {
            dat <- rv$dat
            write.csv(dat, con, row.names = FALSE)
        }
    )

    output$fileContentRaw <- renderTable({
        if(rv$doDatLoad == FALSE){
            return()
        }else{
            datORI <- rv$datORI
            if(input$dispRaw == "head"){
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
            isolate({
                update.dat()
            })
            dat <- rv$dat
            if(input$disp == "head"){
                return(head(dat))
            }else{
                return(dat)
            }
        }
    })

    output$filename <- renderText({
        return(rv$filename)
    })

    observe({
        if(!is.null(rv$dat)){
            colNames <- colnames(rv$dat)
            if(any(colNames == "timeC") && any(colNames == "obsC") &&
               ((any(colNames == "timeI1") && any(colNames == "obsI1")) ||
                (any(colNames == "timeE") && any(colNames == "obsE")))){
                showNotification("The data meets the data requirements for SPiCT. You can move on to the next tab.",
                                 duration = 10)
            }
        }
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
                       "Should the robust estimation for indices be used? (logical for each index)",
                       choices = NULL,
                       multiple = TRUE,
                       options = list(create = TRUE),
                       width = "100%"
                       )
    })


    ## create nseasons (dependent in inp$dtc)
    output$nseasons <- renderUI({
        selectInput("nseasons",
                    "Number of seasons",
                    c(1,2,4),
                    selected = rv$nseasons,
                    width = "100%")
    })



    ## create seasointype (dependent in nseasons)
    output$seasontype <- renderUI({
        selectInput("seasontype",
                    "Season type",
                    c(0,1,2,3),
                    selected = ifelse(rv$nseasons == 1, 0, 1),
                    width = "100%")
    })

    update.inp <- reactive({
        if(is.null(rv$dat)){
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
            rv$inp <- inp <- check.inp(rv$inpORI)
            rv$dteuler <- inp$dteuler
            rv$timerange <- inp$timerange
            rv$lastCatchObs <- inp$lastCatchObs
            rv$maninterval <- inp$maninterval
            rv$nseasons <- inp$nseasons
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
            inp$ffac <- input$ffac
            inp$fcon <- input$fcon
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
            if(input$lognPrior){
                mu <- input$lognMu
                mu <- ifelse(input$lognLog,log(mu),mu)
                inp$priors$logn <- c(mu,input$lognSd,1)
            }else{
                mu <- input$lognMu
                mu <- ifelse(input$lognLog,log(mu),mu)
                inp$priors$logn <- c(mu,input$lognSd,0)
            }
            if(input$logAlphaPrior){
                mu <- input$logAlphaMu
                mu <- ifelse(input$logAlphaLog,log(mu),mu)
                inp$priors$logalpha <- c(mu,input$logAlphaSd,1)
            }else{
                mu <- input$logAlphaMu
                mu <- ifelse(input$logAlphaLog,log(mu),mu)
                inp$priors$logalpha <- c(input$logAlphaMu,input$logAlphaSd,0)
            }
            if(input$logBetaPrior){
                mu <- input$logBetaMu
                mu <- ifelse(input$logBetaLog,log(mu),mu)
                inp$priors$logbeta <- c(input$logBetaMu,input$logBetaSd,1)
            }else{
                mu <- input$logBetaMu
                mu <- ifelse(input$logBetaLog,log(mu),mu)
                inp$priors$logbeta <- c(input$logBetaMu,input$logBetaSd,0)
            }
            tmp <- ifelse(input$BmsyB0Prior,1,0)
            inp$priors$BmsyB0 <- c(input$BmsyB0Mu,input$BmsyB0Sd,tmp)

            tmp <- ifelse(input$logbkfracPrior,1,0)
            mu <- input$logbkfracMu
            mu <- ifelse(input$logbkfracLog,log(mu),mu)
            inp$priors$logbkfrac <- c(mu,input$logbkfracSd,tmp)

            tmp <- ifelse(input$logngammaPrior,1,0)
            mu <- input$logngammaMu
            mu <- ifelse(input$logngammaLog,log(mu),mu)
            inp$priors$logngamma <- c(mu,input$logngammaSd,tmp)

            tmp <- ifelse(input$logKPrior,1,0)
            mu <- input$logKMu
            mu <- ifelse(input$logKLog,log(mu),mu)
            inp$priors$logK <- c(mu,input$logKSd,tmp)

            tmp <- ifelse(input$logmPrior,1,0)
            mu <- input$logmMu
            mu <- ifelse(input$logmLog,log(mu),mu)
            inp$priors$logm <- c(mu,input$logmSd,tmp)

            tmp <- ifelse(input$logrPrior,1,0)
            mu <- input$logrMu
            mu <- ifelse(input$logrLog,log(mu),mu)
            inp$priors$logr <- c(mu,input$logrSd,tmp)

            tmp <- ifelse(input$logqfPrior,1,0)
            mu <- input$logqfMu
            mu <- ifelse(input$logqfLog,log(mu),mu)
            inp$priors$logqf <- c(mu,input$logqfSd,tmp)

            tmp <- ifelse(input$logsdbPrior,1,0)
            mu <- input$logsdbMu
            mu <- ifelse(input$logsdbLog,log(mu),mu)
            inp$priors$logsdb <- c(mu,input$logsdbSd,tmp)

            tmp <- ifelse(input$logsdiPrior,1,0)
            mu <- input$logsdiMu
            mu <- ifelse(input$logsdiLog,log(mu),mu)
            inp$priors$logsdi[[1]] <- c(mu,input$logsdiSd,tmp)

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 2){
                tmp <- ifelse(input$logsdiPrior2,1,0)
                mu <- input$logsdiMu2
                mu <- ifelse(input$logsdiLog2,log(mu),mu)
                inp$priors$logsdi[[2]] <- c(mu,input$logsdiSd2,tmp)
            }

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 3){
                tmp <- ifelse(input$logsdiPrior3,1,0)
                mu <- input$logsdiMu3
                mu <- ifelse(input$logsdiLog3,log(mu),mu)
                inp$priors$logsdi[[3]] <- c(mu,input$logsdiSd3,tmp)
            }

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 4){
                tmp <- ifelse(input$logsdiPrior4,1,0)
                mu <- input$logsdiMu4
                mu <- ifelse(input$logsdiLog4,log(mu),mu)
                inp$priors$logsdi[[4]] <- c(mu,input$logsdiSd4,tmp)
            }

            tmp <- ifelse(input$logqPrior,1,0)
            mu <- input$logqMu
            mu <- ifelse(input$logqLog,log(mu),mu)
            inp$priors$logq[[1]] <- c(mu,input$logqSd,tmp)

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 2){
                tmp <- ifelse(input$logqPrior2,1,0)
                mu <- input$logqMu2
                mu <- ifelse(input$logqLog2,log(mu),mu)
                inp$priors$logq[[2]] <- c(mu,input$logqSd2,tmp)
            }

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 3){
                tmp <- ifelse(input$logqPrior3,1,0)
                mu <- input$logqMu3
                mu <- ifelse(input$logqLog3,log(mu),mu)
                inp$priors$logq[[3]] <- c(mu,input$logqSd3,tmp)
            }

            if(ifelse(inherits(inp$obsI, "list"), length(inp$obsI), 1) >= 4){
                tmp <- ifelse(input$logqPrior4,1,0)
                mu <- input$logqMu4
                mu <- ifelse(input$logqLog4,log(mu),mu)
                inp$priors$logq[[4]] <- c(mu,input$logqSd4,tmp)
            }

            tmp <- ifelse(input$logsdfPrior,1,0)
            mu <- input$logsdfMu
            mu <- ifelse(input$logsdfLog,log(mu),mu)
            inp$priors$logsdf <- c(mu,input$logsdfSd,tmp)

            tmp <- ifelse(input$logsdePrior,1,0)
            mu <- input$logsdeMu
            mu <- ifelse(input$logsdeLog,log(mu),mu)
            inp$priors$logsde <- c(mu,input$logsdeSd,tmp)

            tmp <- ifelse(input$logsdcPrior,1,0)
            mu <- input$logsdcMu
            mu <- ifelse(input$logsdcLog,log(mu),mu)
            inp$priors$logsdc <- c(mu,input$logsdcSd,tmp)

            ## save
            rv$inp <- inp
        }
    })

    output$noSurv <- renderText({
        if(any(!is.null(rv$inp)))
            ifelse(inherits(rv$inp$obsI, "list"), length(rv$inp$obsI), 1) else 1
    })
    outputOptions(output, 'noSurv', suspendWhenHidden=FALSE)

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

    output$inpsum <- renderPrint({
        req(rv$inp)
        if(rv$doDatLoad == FALSE){
            writeLines("No data loaded. Upload your data or use example data.")
        }else{
            rv$inp
        }
    })

    output$mantimeline <- renderPrint({
        req(rv$inp)
        if(rv$doDatLoad == FALSE){
            writeLines("No data loaded. Upload your data or use example data.")
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
            tmp <- try(update.inp(),silent=TRUE)
            if(inherits(tmp, "try-error")){
                rv$inp <- check.inp(rv$inpORI)
            }
            inp <- rv$inp
            inp$optimiser <- input$optimiser
            inp$optim.method <- input$optimMethod
            inp$optimiser.control <- list(iter.max = input$itermax, eval.max = input$evalmax)
            set.seed(input$seed)
            rv$seed <- input$seed

            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Fitting SPiCT.",
                         detail = "This may take a while. This window will disappear
                     automatically when done.", value = 1)
            fit <- fit.spict(inp)
            try(fit <- calc.osa.resid(fit),silent=TRUE)
            rv$fit <- fit
        }
    }

    output$fit <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            isolate({
                spict.res()
            })
            summary(rv$fit)
        }
    })

    output$convergence <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            if(rv$fit$op$convergence == 0){
                writeLines("Model converged.")
            }else{
                writeLines("Model did not converge!")
            }
        }
    })

    output$sumPriors <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            sumspict.priors(rv$fit)
        }
    })

    output$plot2 <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            plot2(rv$fit)
        }
    })

    output$plotPriors <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            nopriors <- get.no.active.priors(rv$fit$inp)
            if(is.numeric(nopriors) && nopriors > 0){
                par(mfrow = c(1,nopriors))
                plotspict.priors.inp(rv$fit, automfrow = FALSE, do.plot=nopriors, stamp='')
            }
        }
    })

    output$plotAdd <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            par(mfrow=c(2,2))
            plotspict.biomass(rv$fit, stamp='')
            plotspict.f(rv$fit, stamp='')
            plotspict.production(rv$fit, stamp='')
            if (rv$inp$nseasons > 1 && rv$inp$seasontype %in% c(1,3)){
                plotspict.season(rv$fit, stamp='')
            }else if (rv$inp$nseasons == 1 && !(rv$inp$timevaryinggrowth || rv$inp$logmcovflag)){
                plotspict.tc(rv$fit, stamp='')
            }else plot.new()
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
    observeEvent(input$runsensi, {
        rv$doSENSI <- input$runsensi
    })

    ## reset button
    observeEvent(input$resetsensi, {
        rv$doSENSI <- FALSE
    })

    ## create nretroyear slider
    output$nretroyear <- renderUI({
        numericInput(
            inputId = "nretroyear",
            label = "# years",
            value = 5,
            min = 1,
            max = diff(rv$timerange) - 5,  ## give spict at least 5 years
            step=1, width="15%"
        )
    })

    ## create ntrials for check.ini
    output$nsensi <- renderUI({
        numericInput(
            inputId = "nsensi",
            label = "# trials",
            value = 10,
            min = 1,
            step=1, width="15%"
        )
    })

    spict.retro <- function(){
        if(is.null(rv$fit) ){
            showNotification(
                paste("The retrospective analysis requires a fitted SPiCT model. Please go to the tab 'Fit SPiCT' and fit the SPiCT model to your or example data."),
                type = "error",
                duration = NULL,
                closeButton = TRUE,
                action = a(href = "javascript:location.reload();", "Reload page")
            )
        }else{
            fit <- rv$fit
            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Running the retrospective analysis.",
                         detail = "This may take a while. This window will disappear
                     automatically when done.", value = 1)
            retro <- retro(fit, nretroyear = input$nretroyear)
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
            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Running the sensitivity analysis.",
                         detail = "This may take a while. This window will disappear
                     automatically when done.", value = 1)
            sensi <- check.ini(fit, ntrials = input$nsensi)
            rv$sensi <- sensi
        }
    }

    output$sensi <- renderPrint({
        if(rv$doSENSI == FALSE){
            writeLines("No results of the sensitivity analysis. Run 'Run check.ini'.")
        }else{
            isolate({
                spict.sensi()
            })
            sumspict.ini(rv$sensi)
        }
    })

    output$diag <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            sumspict.diagnostics(rv$fit)
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
            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Evaluating the management scenarios.",
                         detail = "This may take a while. This window will disappear
                     automatically when done.", value = 1)
            mana <- manage(fit, scenarios = as.numeric(input$scenarios),
                           maninterval = input$maninterval2,
                           maneval = input$maneval2,
                           intermediatePeriodCatch = input$ipc)
            rv$mana <- mana
        }
    }

    output$mana <- renderPrint({
        if(rv$doMANA == FALSE){
            writeLines("No management scenarios. Run 'Run manage'.")
        }else{
            isolate({
                spict.mana()
            })
            sumspict.manage(rv$mana)
        }
    })

    output$tacs <- renderPrint({
        req(rv$mana)
        if(rv$doMANA){
            lapply(man.tac(rv$mana, input$fractileCatch),round,3)
        }else{
            writeLines("No management scenarios. Run 'Run manage'.")
        }
    })

    output$mantimeline2 <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE){
            writeLines("No management scenarios. Run 'Run manage'.")
        }else if(!is.null(rv$mana)){
            man.timeline(rv$mana)
        }else{
            man.timeline(rv$fit)
        }
    })

    output$plotMana <- renderPlot({
        req(rv$mana)
        if(rv$doMANA){
            plot2(rv$mana)
        }else{
            return()
        }
    })


    ## SUMMARY  ##################################################################################

    output$sumParest <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.parest(rv$fit),3)
        }
    })

    output$sumSrefpoints <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.srefpoints(rv$fit),3)
        }
    })

    output$sumDrefpoints <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.drefpoints(rv$fit),3)
        }
    })

    output$sumStates <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.states(rv$fit),3)
        }
    })

    output$sumPredictions <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.predictions(rv$fit),3)
        }
    })

    output$sumDiag <- renderPrint({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            sumspict.diagnostics(rv$fit)
        }
    })

    output$sumIni <- renderPrint({
        req(rv$sensi)
        if(rv$doSENSI == FALSE || rv$doDatLoad == FALSE){
            writeLines("No results from the sensitivity analysis. Run 'Run check.ini'.")
        }else{
            round(sumspict.ini(rv$sensi),3)
        }
    })

    output$sumMana <- renderPrint({
        req(rv$mana)
        if(rv$doMANA == FALSE || rv$doDatLoad == FALSE){
            writeLines("No management scenarios. Run 'Run manage'.")
        }else{
            sumspict.manage(rv$mana)
        }
    })

    output$sumTACs <- renderPrint({
        req(rv$mana)
        if(rv$doMANA == FALSE || rv$doDatLoad == FALSE){
            writeLines("No management scenarios. Run 'Run manage'.")
        }else{
            lapply(man.tac(rv$mana, input$fractileCatch),round,3)
        }
    })

    output$plotAll <- renderPlot({
        req(rv$fit)
        if(rv$doSPICT == FALSE || rv$doDatLoad == FALSE){
            return()
        }else{
            plot(rv$fit)
        }
    })

    output$plotMana2 <- renderPlot({
        req(rv$mana)
        if(rv$doMANA == FALSE || rv$doDatLoad == FALSE){
            return()
        }else{
            plot(rv$mana)
        }
    })

    output$plotRetroSum <- renderPlot({
        req(rv$retro)
        if(rv$doRETRO == FALSE || rv$doDatLoad == FALSE){
            return()
        }else{
            plotspict.retro(rv$retro)
        }
    })


    ## RDATA
    output$allData <- downloadHandler(
        filename = function(){
            filename = strsplit(rv$filename,"Data: ")[[1]][2]
            paste0("spictapp_RData_",filename,"_",Sys.Date(),".RData")
        },
        content = function(con){
            save(reactiveValuesToList(rv), file = con)
        }
    )

    ## CSV
    output$allTables <- downloadHandler(
        filename = function(){
            filename = strsplit(rv$filename,"Data: ")[[1]][2]
            paste0("spictapp_tables_",filename,"_",Sys.Date(),".zip")
        },
        content = function(con){
            tables <- c("parest","states","predictions","drefpoints","srefpoints","manage")
            fs <- c()
            tmpdir <- tempdir()
            ## parest
            i = 1
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.parest(rv$fit),3)
                write.csv(tab, path)
            }
            ## states
            i = 2
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.states(rv$fit),3)
                write.csv(tab, path)
            }
            ## predictions
            i = 3
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.predictions(rv$fit),3)
                write.csv(tab, path)
            }
            ## drefs
            i = 4
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.drefpoints(rv$fit),3)
                write.csv(tab, path)
            }
            ## srefs
            i = 5
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.srefpoints(rv$fit),3)
                write.csv(tab, path)
            }
            ## man
            i = 6
            if(!is.null(rv$mana)){
                path <- paste0(tmpdir,"/",tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.manage(rv$mana),3)
                write.csv(tab, path)
            }
            ##
            zip(zipfile=con, files=fs)
        },
        contentType = "application/zip"
    )


    ## GRAPHS

    output$allGraphs <- downloadHandler(
        filename = function(){
            filename = strsplit(rv$filename,"Data: ")[[1]][2]
            paste0("spictapp_graphs_",filename,"_",Sys.Date(),".zip")
        },
        content = function(con){
            graphs <- c("data", "priors","plot2","retro","manage","plotAll","plotAll_manage")
            fs <- c()
            tmpdir <- tempdir()
            ## data plot
            i = 1
            if(!is.null(rv$inp)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 10, height = 11)
                plotspict.data(rv$inp)
                dev.off()
            }
            ## priors
            i = 2
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 11)
                plotspict.priors(rv$fit)
                dev.off()
            }else if(!is.null(rv$inp)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 11)
                plotspict.priors.inp(rv$inp)
                dev.off()
            }
            ## plot2
            i = 3
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 10)
                plot2(rv$fit)
                dev.off()
            }
            ## retro
            i = 4
            if(!is.null(rv$retro)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 10)
                plotspict.retro(rv$retro)
                dev.off()
            }
            ## manage
            i = 5
            if(!is.null(rv$mana)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 10)
                plot2(rv$mana)
                dev.off()
            }
            ## plot all
            i = 6
            if(!is.null(rv$fit)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 11)
                plot(rv$fit)
                dev.off()
            }
            ## plot all manage
            i = 7
            if(!is.null(rv$mana)){
                path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path, width = 12, height = 11)
                plot(rv$mana)
                dev.off()
            }
            ##
            zip(zipfile=con, files=fs)
        },
        contentType = "application/zip"
    )


    ## REPORT

    report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath

    observeEvent(input$generateReport, {

            ## check if tex distribution installed
            texAvail <- try(Sys.which('pdflatex'), silent=TRUE)
            ## check if pandoc installed
            pandocAvail <- rmarkdown::pandoc_available()
            ## system2('pdflatex', '--version')
            if(input$reportFormat == "pdf" && (is.null(texAvail) || inherits(texAvail, "try-error"))){
                showNotification("No TeX distribution found. Install the required TeX distribution on your computer or generate the report in 'html' format.",
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE,
                                 action = a(href = "javascript:location.reload();", "Reload page")
                                 )
            }else if(input$reportFormat == "docx" && !pandocAvail){
                showNotification("The software 'pandoc' not found. Install pandoc on your computer or generate the report in 'html' format.",
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE,
                                 action = a(href = "javascript:location.reload();", "Reload page")
                                 )
            }else {

                progress <- shiny::Progress$new()

                ## Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Building report.",
                             detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 1)

                if(is.null(rv$inp))
                    showNotification(paste("The minimum requirements for the report is input data. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                     type = "error",
                                     duration = 30,
                                     closeButton = TRUE,
                                     action = a(href = "javascript:location.reload();", "Reload page")
                                     )

                params <- list(rv = rv)

                td <- tempdir()
                tmp_file <- tempfile(fileext = paste0(".",input$reportFormat))
                tmp_file2 <- tempfile(fileext = ".Rmd")

                file.copy("report/spictapp.bib", td,
                          overwrite = TRUE)
                file.copy("report/assessmentReport.Rmd", tmp_file2, overwrite = TRUE)

                if(input$reportFormat == "html"){
                    output_format <- "html_document"
                }else if(input$reportFormat == "pdf"){
                    output_format <- "pdf_document"
                }else if(input$reportFormat == "docx"){
                    output_format <- "word_document"
                }

                rmarkdown::render(tmp_file2,
                                  output_format = output_format,
                                  output_file = tmp_file,
                                  output_dir = td,
                                  intermediates_dir = td,
                                  knit_root_dir = td,
                                  clean = TRUE,
                                  params = params,
                                  envir = new.env())

                report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
            }
            showNotification("The report was generated successfully. You can download with the 'Download Assessment Report' button on the left.",
                             duration = 10)
    })

    ## Hide download button until report is generated
    output$reportbuilt <- reactive({
        return(!is.null(report$filepath))
    })
    outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

    ## Download report
    output$downloadReport <- downloadHandler(

        ## This function returns a string which tells the client
        ## browser what name to use when saving the file.
        filename = function() {
            paste0("spictapp_report_", Sys.Date(), ".", input$reportFormat)
            ##     filename = strsplit(rv$filename,"Data: ")[[1]][2]
            ##     paste0("STF_report_",filename,"_",Sys.Date(),".pdf")
        },

        ## This function should write data to a file given to it by
        ## the argument 'file'.
        content = function(con){
            file.copy(report$filepath, con)
        }
    )


    ## OTHER  ##################################################################################

    observe({
        if (input$tabset == "stop"){
            js$closeWindow();
            stopApp(message("App stopped"))
        }
    })

    output$tobm <- renderImage({
        return(list(
            src = "www/tobm.JPG",
            contentType = "image/jpeg",
            width = "100%",
            alt = "Tobias K. Mildenberger",
            caption = "Tobias K. Mildenberger"
        ))
    }, deleteFile = FALSE)

    output$alko <- renderImage({
        return(list(
            src = "www/alko.JPG",
            contentType = "image/jpeg",
            width = "100%",
            alt = "Alexandros Kokkalis",
            caption = "Alexandros Kokkalis"
        ))
    }, deleteFile = FALSE)

})
