## Shiny app for TropFishR related data exploration
## server script

library(shiny)
library(shinydashboard)
##library(markdown)
library(TropFishR)
library(rmarkdown)
##library(jsonlite)
library(shinyjs)
library(reshape2)
library(flextable)
library(pander)


## Load scripts
##-----------------------------------------------------------------------------------
source("../funcs/serverFuncs.R")
source("../funcs/ELEFAN_GA_fun.R")
source("../funcs/LCCC_fun.R")
source("../funcs/natM_fun.R")
##source("../funcs/VPA_fun.R")
source("../funcs/YPR_fun.R")



shinyServer(
    function(input, output, session){

        ## GENERAL
        ##-----------------------------------------------------------
        ## reactive buttons (when tabs changed or undo pressed)
        rv <- reactiveValues()

        ## Defaults
        rv.defaults <- function(){
            rv$doDatLoad = FALSE
            rv$doDatUp = FALSE
            rv$doELEFAN = FALSE
            rv$doLCCC = FALSE
            rv$doGOTCHA = FALSE
            rv$doYPR = FALSE
            rv$datORI <- NULL
            rv$colNamesORI <- NULL
            rv$colNames <- NULL
            rv$dat <- NULL
            rv$lfq <- NULL
            rv$lfqORI <- NULL
            rv$lfqOrig <- NULL
            rv$seed <- NULL
            rv$filename <- "Data: -"
            rv$Lrange <- c(NaN,NaN)
            rv$binSize <- 4
            rv$ma <- 5
            rv$addlSqrt <- FALSE
            rv$years <- NA
            rv$yearsRefLev <- NA
            rv$agg <- "month"
            rv$plusGroup <- FALSE
            ## reset example data
            rv$useExDat <- FALSE
            rv$exdat <- NULL
        }
        rv.defaults()

        ## LOAD DATA
        ##-----------------------------------------------------------
        ## switch on DatLoad if ex data used or data uploaded
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
            updateRadioButtons(session = session,
                                inputId = "sep",
                               selected = ",")
            updateRadioButtons(session = session,
                                inputId = "quote",
                               selected = '"')
            updateRadioButtons(session = session,
                                inputId = "header",
                               selected = "TRUE")
            updateRadioButtons(session = session,
                                inputId = "dispRaw",
                               selected = "head")
            updateRadioButtons(session = session,
                                inputId = "disp",
                               selected = "head")
            updateRadioButtons(session = session,
                                inputId = "isLFQ",
                                selected = "raw")
            updateSelectInput(session = session,
                              inputId = "lengthCol",
                              selected = NULL)
            updateSelectInput(session = session,
                              inputId = "dateCol",
                              selected = NULL)
            updateSelectInput(session = session,
                              inputId = "freqCol",
                              selected = NULL)
            updateCheckboxInput(session = session,
                                inputId = "aggDates",
                                value = FALSE)
            updateTextInput(session=session,
                            inputId = "dateFormat",
                            value = "%Y-%m-%d")
            ## example data
            updateCheckboxInput(session = session,
                                inputId = "useExDat",
                                value = FALSE)
            updateSelectInput(session = session,
                              inputId = "exdat",
                              selected = NULL)
            rv.defaults()
            errCode <- NULL
        })

        output$lengthCol <- renderUI({
            ind <- which("length" == rv$colNamesORI)
            if(length(ind) == 1){
                selected <- rv$colNamesORI[ind]
            }else{
                selected <- NULL
            }
            selectInput("lengthCol",
                        "Length measurements / length classes",
                        choices = c("Choose one"="",rv$colNamesORI),
                        selected = selected)
        })

        output$dateCol <- renderUI({
            ind <- which("dates" == rv$colNamesORI)
            if(length(ind) == 1){
                selected <- rv$colNamesORI[ind]
            }else{
                selected <- NULL
            }
            selectInput("dateCol",
                        "Sampling dates",
                        choices = c("Choose one"="",rv$colNamesORI),
                        selected = selected)
        })

        output$freqCol <- renderUI({
            ind <- which("frequency" == rv$colNamesORI)
            if(length(ind) == 1){
                selected <- rv$colNamesORI[ind]
            }else{
                selected <- NULL
            }
            selectInput("freqCol",
                        "Frequencies",
                        choices = c("Choose one"="",rv$colNamesORI),
                        selected = selected)
        })

        output$freqColsLFQ1 <- renderUI({
            collength <- length(rv$colNamesORI)
            if(collength > 1){
                selected <- 2
            }else{
                selected <- NULL
            }
            numericInput(inputId = "freqColsLFQ1",
                         label = "From column",
                         min = 1, max = collength,
                         value = selected)
        })
        output$freqColsLFQ2 <- renderUI({
            collength <- length(rv$colNamesORI)
            numericInput(inputId = "freqColsLFQ2",
                         label = "To column",
                         min = 2, max = collength,
                         value = collength)
        })

        load.dat <- function(){
            infile <- input$file1
            exdat <- as.character(input$exdat)
            topa <- "Data: "
            ## Defaults
            rv$datORI <- NULL
            rv$dat <- NULL
            rv$colNamesORI <- NULL
            rv$lfqORI <- NULL
            rv$lfq <- NULL
            rv$filename <- ""
            rv$Lrange <- c(NaN,NaN)
            rv$binSize <- 4
            rv$ma <- 5
            rv$addlSqrt <- FALSE
            rv$years <- NA
            rv$yearsRefLev <- NA
            rv$agg <- "month"
            rv$plusGroup <- FALSE
            errCode <<- NULL
            if(input$useExDat){
                rv$filename <- paste0(topa,as.character(exdat))
                data(list=exdat, package="TropFishR")
                lfq <- get(exdat)
                dat <- setNames(data.frame(lfq$catch), lfq$dates)
                dat <- cbind(midLengths = lfq$midLengths, dat)
                rv$datORI <- lfq2dat(lfq)
                rv$colNamesORI <- colnames(rv$datORI)
                rv$dat <- dat
                rv$colNames <- colnames(rv$datORI)
                rv$lfq <- rv$lfqORI <- lfq
                rv$Lrange <- range(rv$dat$midLengths)
                rv$binSize <- diff(rv$dat$midLengths)[1]
                rv$ma <- 5
                rv$addlSqrt <- FALSE
                rv$years <- unique(format(rv$lfq$dates, "%Y"))
                rv$agg <- "month"
                rv$plusGroup <- FALSE
                rv$doDatUp <- TRUE
            }else if(!is.null(infile)){
                dat <- try(read.csv(input$file1$datapath,
                                header = as.logical(input$header),
                                sep = input$sep,
                                quote = input$quote,
                                stringsAsFactors = FALSE),silent = TRUE)
                if(!inherits(dat,"try-error")){
                    rv$datORI <- dat
                    rv$colNamesORI <- colnames(dat)
                    rv$dat <- NULL
                    rv$colNames <- NULL
                    rv$lfqORI <- NULL
                    rv$lfq <- NULL
                    ## filename
                    tmp <- strsplit(infile[,1], ".csv")[[1]]
                    tmp <- strsplit(tmp, ".txt")[[1]]
                    filename <- paste0(topa,as.character(tmp))
                    rv$filename <- filename
                    rv$doDatUp <- FALSE
                }
            }
        }

        match.cols <- function(){
            if(!input$useExDat){
                datORI <- rv$datORI
                if(input$isLFQ == "raw"){
                    colNames = list("length" = input$lengthCol,
                                    "dates" = input$dateCol,
                                    "frequency" = input$freqCol)
                }else{
                    colNames = list("midLengths" = input$lengthCol,
                                    "freqColsLFQ1" = input$freqColsLFQ1,
                                    "freqColsLFQ2" = input$freqColsLFQ2)
                }
                rv$colNames <- colNames
            }
        }

        update.dat <- function(){
            datORI <- rv$datORI
            colNames <- rv$colNames
            if(!input$useExDat && !is.null(colNames) && !is.null(datORI)){
                if(input$isLFQ == "raw"){
                    dat <- try(checkDat(datORI, colNames),silent=TRUE)
                }else{
                    dat <- try(lfqdat2dat(datORI,rv$colNames,input$dateFormat),silent=TRUE)
                }
                if(inherits(dat,"try-error")){
                    errCode <<- c(errCode,1)
                }else if(any(is.na(as.character(dat$dates)))){
                    errCode <<- c(errCode,2)
                }else{
                    errCode <<- errCode[-which(errCode %in% c(1,2))]
                    dat$length <- as.numeric(dat$length)
                    dat$frequency <- as.numeric(dat$frequency)
                    lfq <- try(dat2lfq(dat,
                                       dateFormat = ifelse(input$isLFQ == "lfq","%Y-%m-%d",
                                                           input$dateFormat),
                                       aggDates = input$aggDates),silent = TRUE)
                    if(inherits(lfq,"try-error")){
                        errCode <<- c(errCode,3)
                    }else{
                        errCode <<- errCode[-which(errCode %in% c(3))]
                        rv$lfqORI <- rv$lfq <- lfq
                        lfqdat <- setNames(data.frame(lfq$catch), lfq$dates)
                        lfqdat <- cbind(lfq$midLengths, lfqdat)
                        colnames(lfqdat) <- c("midLengths",colnames(lfqdat)[-1])
                        rv$dat <- lfqdat
                        ## length range
                        rv$Lrange <- range(dat$length,na.rm=TRUE)
                        ## bin size
                        rv$binSize <- diff(dat$length)[1]  # ifelse(min(dat$length) %% 1 > 0, 0.5, 1)
                        rv$ma <- 5
                        rv$addlSqrt <- FALSE
                        rv$years <- unique(format(lfq$dates, "%Y"))
                        rv$agg <- "month"
                        rv$plusGroup <- FALSE
                    }
                }
            }
        }

        observe({
            if(!is.null(rv$datORI)){
                shinyjs::enable("datUpdate")
            }else{
                shinyjs::disable("datUpdate")
            }
        })

        ## only run if action button used
        observeEvent(input$datUpdate, {
            rv$doDatUp <- input$datUpdate
        })

        output$downloadExData <- downloadHandler(
            filename = function() {
                paste0('data-', input$exdat, '.csv')
            },
            content = function(con) {
                dat <- rv$dat
                # Wide to long
                res <- expand.grid(dat[, "midLengths"], names(dat)[-1], stringsAsFactors = FALSE)
                res$frequency <- sapply(seq(nrow(res)),
                                        function(i) dat[, -1][dat[, 1] == res[i, 1], names(dat)[-1] == res[i, 2]])
                res <- setNames(res, c('length', 'dates', 'frequency'))
                write.csv(res, con, row.names = FALSE)
            }
        )

        output$fileContentRaw <- renderTable({
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                load.dat()
                datORI <- rv$datORI
                if(input$dispRaw == "head"){
                    return(head(datORI))
                }else{
                    return(datORI)
                }
            }
        })

        output$fileContent <- renderTable({
            if(is.null(rv$datORI)){
                return()
            }else{
                if(rv$doDatUp){
                    isolate({
                        match.cols()
                        update.dat()
                        if(!input$useExDat){
                            ## informative error messages  ## reset button doesn't work anymore!
                            if(any(errCode == 1)){
                                showNotification(paste("Something went wrong with the recognition and/or assignment of column names. Did you choose the correct settings for the seperator, quote, header and the correct column names?"),
                                                 type = "error",
                                                 duration = 30,
                                                 closeButton = TRUE
                                                 )
                                return()
                            }else if(any(errCode == 2)){
                                showNotification(paste("Something went wrong with the date conversion. Did you provide the correct format for the date column/header with date information (e.g. %Y.%m.%d)?"),
                                                 type = "error",
                                                 duration = 30,
                                                 closeButton = TRUE
                                                 )
                                return()
                            }else if(any(errCode == 3)){
                                showNotification(paste("There was an error in the creation of the LFQ table. Please check: Did you choose the correct data format? Are the column names assigned correctly? Is the date format correct?"),
                                                 type = "error",
                                                 duration = 30,
                                                 closeButton = TRUE
                                                 )
                                return()
                            }
                        }
                    })
                    if(!is.null(rv$dat)){
                        dat <- rv$dat
                        if(input$disp == "head"){
                            return(head(dat))
                        }else{
                            return(dat)
                        }
                    }else return()
                }else if(!is.null(rv$dat)){
                    dat <- rv$dat
                    if(input$disp == "head"){
                        return(head(dat))
                    }else{
                        return(dat)
                    }
                }else return()
            }
        })

        output$filename <- renderText({
            return(rv$filename)
        })

        observe({
            if(!is.null(rv$dat)){
                showNotification("The data meets the data requirements for TropFishR. You can move on to the next tab.",
                                     duration = 10)
            }
        })


        ## EXPLORE DATA
        ##-----------------------------------------------------------

        ## create bin size slider
        output$binSize <- renderUI({
            req(rv$lfqORI)
            sliderInput(inputId = "binSize",
                        label = "Bin size",
                        min = isolate(rv$lfqORI$midLengths[2] - rv$lfqORI$midLengths[1]),
                        max = 20,
                        step = 0.5,
                        value = isolate(rv$binSize),
                        round = TRUE)
        })

        ##         observeEvent(input$tabset,{
        ##             if(is.null(input$binSize) & is.null(rv$binSize)){
        ##                 binSizeVal <- 1
        ##             }else if(is.null(input$binSize) & !is.null(rv$binSize)){
        ##                 binSizeVal <- rv$binSize
        ##             }else{
        ##                 binSizeVal <- input$binSize
        ##             }
        ## ##            rv$binSize <- binSizeVal
        ##             updateSliderInput(session = session,
        ##                               inputId = "binSize",
        ##                               value = binSizeVal)
        ##         })

        output$selYears <- renderUI({
            selectInput(inputId = "selYears",
                        label = "Select years (multiple possible):",
                        choices = unique(format(rv$lfqORI$dates, "%Y")),
                        selected = unique(format(rv$lfqORI$dates, "%Y")),
                        multiple = TRUE,
                        width ='60%')
        })

        ##         observeEvent(input$tabset,{
        ##             if(is.null(rv$lfq)){
        ## ##                years <- NA
        ##                 yearsSEL <- rv$years
        ##             }else{
        ## ##                years <- unique(format(rv$lfqORI$dates, "%Y"))
        ##                 yearsSEL <- rv$years
        ##             }
        ## ##            rv$years <- years
        ##             updateSelectInput(session = session,
        ##                               inputId = "selYears",
        ##                               selected = yearsSEL)
        ##         })

        output$plusGroup <- renderUI({
            if(!is.null(rv$lfq)){
                potPGs <- c("FALSE", rv$lfq$midLengths[2:length(rv$lfq$midLengths)])
            }else{
                potPGs <- c("FALSE")
            }
            selectInput(inputId = "plusGroup",
                        label = "Plus group?",
                        choices = potPGs,
                        selected = "FALSE",
                        width ='30%')
        })
        ## observeEvent(input$tabset,{
        ##     if(is.null(rv$lfq)){
        ##         potPGs <- c("FALSE")
        ##     }else{
        ##         potPGs <- c("FALSE", rv$lfq$midLengths[2:length(rv$lfq$midLengths)])
        ##     }
        ##     updateSelectInput(session = session,
        ##                       inputId = "plusGroup",
        ##                       choices = potPGs)
        ## })

        lfqUp <- reactive({
            req(input$selYears)
            if(is.null(rv$lfq)){
                showNotification(paste("No length-frequency data set has been chosen. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                 type = "error",
                                 duration = NULL,
                                 closeButton = TRUE
                                 )
            }else{
                ## update rv elements
                rv$binSize <- input$binSize
                rv$ma <- input$ma
                rv$addlSqrt <- input$addlSqrt
                rv$years <- input$selYears
                rv$agg <- input$agg
                rv$plusGroup <- input$plusGroup
                ## update lfq
                lfq <- lfqModify(rv$lfqORI,
                                 bin_size = rv$binSize,
                                 years = rv$years,
                                 aggregate = rv$agg,
                                 plus_group = rv$plusGroup)
                lfqre <- lfqRestructure(lfq, MA = rv$ma, addl.sqrt = rv$addlSqrt)
                rv$lfq <- lfq
                rv$lfqre <- lfqre
            }
        })


        output$sampPermonth <- renderTable({
            if(is.null(rv$dat)){
                return()
            }else{
                ##
                lfqUp()
                lfqre <- rv$lfqre
                tab <- data.frame(Date = paste0(format(lfqre$dates, "%b")," ",
                                                format(lfqre$dates, "%Y")),
                                  "Samples" = as.integer(colSums(as.matrix(lfqre$catch))))
            }
        })

        output$lfqPlot <- renderPlot({
            req(rv$lfqre)
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                lfqre <- rv$lfqre
                opar <- par(mfrow=c(2,1), mar=c(2,4,2,3), oma=c(2,0,0,0))
                plot(lfqre, Fname = "catch", rel = input$relLFQ)
                plot(lfqre, Fname = "rcounts")
                par(opar)
            }
        })

        output$sampSize <- renderText({
            req(rv$lfqre)
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                lfqre <- rv$lfqre
                paste0("Number of length measurements: ",
                       as.integer(sum(colSums(as.matrix(lfqre$catch)))))
            }
        })

        output$sampYears <- renderText({
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                paste0(c("Years in data: ", paste0(rv$years,collapse=", ")))
            }
        })


        ## output$lfqPlotDetailed <- renderPlot({
        ##     if(rv$doDatLoad == FALSE){
        ##         return()
        ##     }else{
        ##         x <- lfqUp()
        ##         years <- unique(format(x$dates,"%Y"))
        ##         nyears <- length(years)
        ##         opar <- par(mfrow=c(nyears,1), mar=c(1,4,1,3), oma=c(4,0,1,2))
        ##         for(i in 1:nyears){
        ##             xi <- x
        ##             xi$catch <- as.matrix(x$catch[,which(format(x$dates,"%Y") == years[i])])
        ##             xi$dates <- x$dates[which(format(x$dates,"%Y") == years[i])]
        ##             try(plot(xi, Fname = input$catchVSrcounts,
        ##                      date.axis = ifelse(i == nyears, "modern","no"),
        ##                      date.format = "%b",
        ##                      rel = input$relLFQ),silent=TRUE)
        ##             mtext(text = as.character(years[i]),font = 2, line=0.25, side=3)
        ##         }
        ##         par(opar)
        ##     }
        ## })

        output$minL <- renderText({
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                lrange <- rv$Lrange
                return(paste0("Minimum length: ", min(lrange)))
            }
        })

        output$maxL <- renderText({
            if(rv$doDatLoad == FALSE){
                return()
            }else{
                lrange <- rv$Lrange
                return(paste0("Maximum length: ", max(lrange)))
            }
        })

        ##-----------------------------------------------------------



        ## GROWTH
        ##-----------------------------------------------------------
        observeEvent(input$tabset, {
            if(!is.null(rv$lfq)){
                shinyjs::enable("runELEFAN")
                shinyjs::enable("resetELEFAN")
            }else{
                shinyjs::disable("runELEFAN")
                shinyjs::disable("resetELEFAN")
            }
        })

        ## only run if action button used
        observeEvent(input$runELEFAN, {
            rv$doELEFAN <- input$runELEFAN
        })

        ## reset button
        observeEvent(input$resetELEFAN, {
            updateCheckboxInput(session = session,
                                inputId = "seasonalized",
                                value = FALSE)
            lrange <- rv$Lrange
            updateSliderInput(session = session,
                              "Linfrange",
                              value = round(c(0.8,1.2) * lrange[2]))

            updateSliderInput(session = session,
                              inputId = "Krange",
                              value = range(0.01, 1)
                              )

            updateSliderInput(session = session,
                              inputId = "tarange",
                              value = range(0, 1)
                              )

            updateSliderInput(session = session,
                              inputId = "Crange",
                              value = c(0,1)
                              )

            updateSliderInput(session = session,
                              inputId = "tsrange",
                              value = range(0, 1)
                              )

            updateSliderInput(session = session,
                              inputId = "popSize",
                              value = 50
                              )

            updateSliderInput(session = session,
                              inputId = "pmutation",
                              value = 0.2
                              )

            updateSliderInput(session = session,
                              inputId = "maxiter",
                              value = 20
                              )

            updateSliderInput(session = session,
                              inputId = "run",
                              value = 20,
                              )

            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeGrowth",
                              value = bsVal)

            if(is.null(input$ma)){
                maVal <- 5
            }else{
                maVal <- input$ma
            }
            rv$ma <- maVal
            updateSliderInput(session = session,
                              inputId = "maGrowth",
                              value = maVal)

            rv$doELEFAN <- FALSE
            rv$resGA <- NULL
            rv$parsGrowth <- NULL
        })

        observeEvent(input$binSize,{
            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeGrowth",
                              value = bsVal)
        })

        observeEvent(input$ma,{
            if(is.null(input$ma)){
                maVal <- 5
            }else{
                maVal <- input$ma
            }
            rv$ma <- maVal
            updateSliderInput(session = session,
                              inputId = "maGrowth",
                              value = maVal)
        })

        output$Linfrange <-renderUI({
            req(rv$Lrange)
            lrange <- rv$Lrange
            sliderInput("Linfrange", label = "Linf range",
                        min = floor(lrange[1]),
                        max = ceiling(lrange[2] * 1.5),
                        value = round(c(0.8,1.2) * lrange[2]),
                        step = 1,
                        dragRange = TRUE, round = TRUE)
        })

        ## inactive C and ts sliders if not seasonal
        observeEvent(input$seasonalized, {
            if(input$seasonalized){
                shinyjs::enable("Crange")
                shinyjs::enable("tsrange")
            }else{
                shinyjs::disable("Crange")
                shinyjs::disable("tsrange")
            }
        })

        GA_res <- function(){
            if(is.null(rv$lfq)){
                showNotification(paste("ELEFAN requires a length-frequency data set. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            }else{
                ## use all info from datExplo tab
                tmp <- try(lfqUp(),silent=TRUE)
                rv$binSize <- input$binSizeGrowth
                rv$ma <- input$maGrowth
                ## use growth tab info
                lfq <- lfqModify(rv$lfqORI,
                                 bin_size = input$binSizeGrowth)
                lfqre <- lfqRestructure(lfq, MA = input$maGrowth)
                rv$lfq <- lfq
                rv$lfqre <- lfqre
                withProgress(message = "Running ELEFAN", value = 0, {
                    resGA <- ELEFAN_GA_shiny(
                        x=lfq, seasonalised = input$seasonalized, MA = input$maGrowth, parallel = FALSE,
                        low_par = c(Linf=input$Linfrange[1],
                                    K=input$Krange[1],
                                    ta=input$tarange[1],
                                    C=input$Crange[1],
                                    ts=input$tsrange[1]),
                        up_par = c(Linf=input$Linfrange[2],
                                   K=input$Krange[2],
                                   ta=input$tarange[2],
                                   C=input$Crange[2],
                                   ts=input$tsrange[2]),
                        popSize=input$popSize, maxiter=input$maxiter, run = input$run,
                        pmutation = input$pmutation, monitor = shinyMonitor,
                        seed = input$seed
                    )
                })
                rv$resGA <- resGA
                ## growth parameters
                pars <- as.list(resGA@solution[1,])
                if(!input$seasonalized){
                    names(pars) <- c("Linf", "K", "ta")
                    pars <- pars[c("Linf", "K", "ta")]
                } else {
                    names(pars) <- c("Linf", "K", "ta", "C", "ts")
                }
                pars$phiL <- log10(pars$K) + 2 * log10(pars$Linf)
                rv$parsGrowth <- pars
                ## fit curves
                lfqre <- rv$lfqre
                lfqre$par <- as.list(pars)
                lfqFC <- lfqFitCurves(lfq = lfqre,
                                      par=as.list(pars))
                lfqFC$par <- as.list(pars)
                rv$lfqFC <- lfqFC
            }
        }

        output$pars <- renderTable({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                isolate({
                    GA_res()
                })
                tmp <- as.data.frame( c(rv$parsGrowth,
                                        list(Rn_max = rv$resGA@fitnessValue)) )
                names(tmp) <- replace(names(tmp), names(tmp)=="Rn_max", "Rn")
                names(tmp) <- replace(names(tmp), names(tmp)=="phiL", "phi'")
                tmp
            }
        })

        output$ELEFAN_GA_score_plot <- output$ELEFAN_GA_score_plot_ov <- renderPlot({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                par(mar=c(5,5,2,1))
                plot(rv$resGA)
            }
        })

        output$ELEFAN_GA_lfq_plot <- output$ELEFAN_GA_lfq_plot_ov <- renderPlot({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                par(mar=c(5,5,2,1))
                plot(rv$lfqFC, Fname = "rcounts")
                lfqFC <- rv$lfqFC
                lfqFitCurves(lfq = lfqFC,
                             par=as.list(rv$parsGrowth),
                             draw = TRUE)
            }
        })

        output$ELEFAN_GA_cohort_plot <- output$ELEFAN_GA_cohort_plot_ov <- renderPlot({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                par(mar=c(5,5,2,1))
                lfqc <- lfqCohort(rv$lfqFC, calc_dt = FALSE)
                plot(lfqc, Fname = "catch",
                     ylim = c(0, max(rv$lfq$midLengths)), image.col=NA)
                pal <- colorRampPalette(c(4,5,7,2))
                with(lfqc, image(x = dates, y = midLengths, z = t(cohort),
                                 col = adjustcolor(pal(max(cohort, na.rm = TRUE)), 0.75),
                                 add=TRUE
                                 ))
            }
        })

        output$recruitPars <- output$recruitPars_ov <- renderTable({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                lfq <- rv$lfq
                lfq$par <- rv$parsGrowth
                rv$parsRecruit <- recruitment(lfq, plot=FALSE)
                tmp <- data.frame("relative month" = as.integer(rv$parsRecruit$mids),
                                  counts = rv$parsRecruit$counts,
                                  density = round(rv$parsRecruit$density,3))
                tmp
            }
        })

        output$recruit_plot <- output$recruit_plot_ov <- renderPlot({
            if(rv$doELEFAN == FALSE){
                return()
            }else{
                lfq <- rv$lfq
                lfq$par <- rv$parsGrowth
                par(mar=c(5,5,2,1))
                tmp <- recruitment(lfq, plot=TRUE)
            }
        })

        ##-----------------------------------------------------------


        ## MORTALITY
        ##-----------------------------------------------------------
        observeEvent(input$tabset, {
            if(!is.null(rv$lfq) && !is.null(rv$parsGrowth)){
                shinyjs::enable("runLCCC")
                shinyjs::enable("resetLCCC")
                shinyjs::enable("runGOTCHA")
                shinyjs::enable("resetGOTCHA")
            }else{
                shinyjs::disable("runLCCC")
                shinyjs::disable("resetLCCC")
                shinyjs::disable("runGOTCHA")
                shinyjs::disable("resetGOTCHA")
            }
        })

        ## only run if action button used
        observeEvent(input$runLCCC, {
            rv$doLCCC <- input$runLCCC
        })

        ## observe ELEFAN
        observeEvent(input$runELEFAN, {
            rv$doLCCC <- FALSE
            rv$doGOTCHA <- FALSE
        })

        ## only run if action button used
        observeEvent(input$runGOTCHA, {
            rv$doLCCC <- TRUE
            rv$doGOTCHA <- input$runGOTCHA
        })

        ## reset button
        observeEvent(input$resetLCCC, {
            updateSelectInput(session=session,
                        inputId = "selYearsLCCC",
                        selected = rv$years)
            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeMort",
                              value = bsVal)
            tmp <- est.regInt()
            updateSliderInput(session=session,
                              inputId="regInt",
                              value=tmp$opt)
            updateSelectInput(session=session,
                              inputId = "natM",
                              selected = "Then_growth")
            rv$doLCCC <- FALSE
            rv$parsMort <- NULL
            rv$parsMortGOTCHA <- NULL
            rv$parsMortFinal <- NULL
            rv$resLCCC <- NULL
            rv$resnatM <- NULL
        })

        ## reset button
        observeEvent(input$resetGOTCHA, {
            updateCheckboxInput(session=session,
                                inputId="gotchaEst",
                                value=FALSE)
            tmp <- est.regIntGOTCHA()
            updateSliderInput(session=session,
                              inputId="regIntGOTCHA",
                              value=tmp$opt)
            rv$doGOTCHA <- FALSE
            rv$parsMortGOTCHA <- NULL
            rv$resGOTCHA <- NULL
        })

        getNumYears <- function(){
            lfq <- rv$lfq
            lfqY <- lfqModify(lfq, aggregate = "year")
            return(ncol(lfqY$catch))
        }

        ## update and create sliders
        observeEvent(input$binSize,{
            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeMort",
                              value = bsVal)
        })

        output$selYearsLCCC <- renderUI({
            selectInput(inputId = "selYearsLCCC",
                        label = "Year(s) for LCCC, GOTCHA, and YPR:",
                        choices = unique(format(rv$lfqORI$dates, "%Y")),
                        selected = rv$years,
                        multiple = TRUE,
                        width ='80%')
        })

        est.regInt <- function(){
            lfq <- rv$lfq
            lfq$par <- rv$parsGrowth
            binSize <- ifelse(is.null(input$binSizeMort), input$binSize, input$binSizeMort)
            selYearsLCCC <- ifelse(is.null(input$selYearsLCCC),rv$years,input$selYearsLCCC)
            lfqU <- lfqModify(lfq,
                              vectorise_catch = TRUE,
                              bin_size = binSize)
            lfqU$catch <- as.matrix(lfqU$catch)
            years <- unique(format(lfqU$dates, "%Y"))
            catchCol <- which(years %in% selYearsLCCC)
            rv$yearsRefLev <- years[catchCol]
            tmp <- LCCC_shiny(lfqU, returnRegInt = TRUE,
                              catch_columns = as.numeric(catchCol))
            if(is.numeric(tmp[[1]]) && length(tmp[[1]]) == 2){
                opt <- tmp[[1]]
            }else{
                opt <- c(1,4)
            }
            if(is.numeric(tmp[[2]]) && length(tmp[[2]]) == 2){
                min <- tmp[[2]][1]
                max <- tmp[[2]][2]
            }else{
                min <- 1
                max <- 40
            }
            res <- list(opt = opt, max = max, min = min)
            return(res)
        }


        output$regInt <- renderUI({
            req(rv$lfq)
            req(rv$parsGrowth)
            tmp <- est.regInt()
            sliderInput(
                inputId = "regInt",
                label = "Regression points",
                dragRange = TRUE,
                value = tmp$opt,
                min = tmp$min,
                max = tmp$max,
                step = 1
            )
        })

        est.regIntGOTCHA <- function(){
            lfq <- rv$lfq
            lfq$par <- rv$parsGrowth
            lfqc <- lfqCohort(lfq, calc_dt = FALSE)
            df <- data.frame(
                length = rep(lfqc$midLengths, times = length(lfqc$dates)),
                rel.age = c(lfqc$rel.age),
                bday = c(lfqc$bday),
                cohort = c(lfqc$cohort),
                n = c(lfqc$catch))
            lfqU <- c(lfqc, list(df=df))
            tmp <- LCCC_shiny(lfqU, returnRegInt = TRUE,
                              calc_ogive = FALSE,
                              gotcha = TRUE)
            if(is.numeric(tmp[[1]]) && length(tmp[[1]]) == 2){
                opt <- tmp[[1]]
            }else{
                opt <- c(1,4)
            }
            if(is.numeric(tmp[[2]]) && length(tmp[[2]]) == 2){
                min <- tmp[[2]][1]
                max <- tmp[[2]][2]
            }else{
                min <- 1
                max <- 40
            }
            res <- list(opt = opt, max = max, min = min)
            return(res)
        }

        output$regIntGOTCHA <- renderUI({
            req(rv$lfq)
            req(rv$parsGrowth)
            tmp <- est.regIntGOTCHA()
            sliderInput(
                inputId = "regIntGOTCHA",
                label = "Regression points for GOTCHA",
                dragRange = TRUE,
                value = tmp$opt,
                min = tmp$min,
                max = tmp$max,
                step = 1
            )
        })

        ## inactive temp, schooling and tmax
        observeEvent(input$natM, {
            if(input$natM == "Then_growth"){
                shinyjs::disable("schooling")
                shinyjs::disable("temp")
                shinyjs::disable("tmax")
            }else if(input$natM == "Pauly_Linf"){
                shinyjs::enable("schooling")
                shinyjs::enable("temp")
                shinyjs::disable("tmax")
            }else if(input$natM == "Then_tmax"){
                shinyjs::disable("schooling")
                shinyjs::disable("temp")
                shinyjs::enable("tmax")
            }
        })


        LCCC_res <- function(){
            if(is.null(rv$lfq))
                showNotification(paste("The length-converted catch curve analysis requires a length-frequency data set. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            if(is.null(rv$parsGrowth))
                showNotification(paste("The length-converted catch curve analysis requires estimated growth parameters. Please go to the tab 'Growth' and run ELEFAN."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )

            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Running LCCC.",
                         detail = "This may take a while. This window will disappear
                     automatically.", value = 1)

            lfq <- rv$lfq
            lfq$par <- rv$parsGrowth

            lfqU <- lfqModify(lfq, vectorise_catch = TRUE,
                              bin_size = input$binSizeMort) ## , plus_group = input$plusGroup)
            lfqU$catch <- as.matrix(lfqU$catch)
            years <- unique(format(lfqU$dates, "%Y"))
            catchCol <- which(years %in% input$selYearsLCCC)
            resLCCC <- LCCC_shiny(
                x = lfqU, reg_int = c(input$regInt[1], input$regInt[2]),
                catch_columns = as.numeric(catchCol), calc_ogive = TRUE)
            rv$resLCCC <- resLCCC
        }

        GOTCHA_res <- function(){
            if(is.null(rv$lfq))
                showNotification(paste("GOTCHA requires a length-frequency data set. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            if(is.null(rv$parsGrowth))
                showNotification(paste("GOTCHA requires estimated growth parameters. Please go to the tab 'Growth' and run ELEFAN."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )

            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Running GOTCHA.",
                         detail = "This may take a while. This window will disappear
                     automatically.", value = 1)

            lfq <- rv$lfq
            lfq$par <- rv$parsGrowth

            ## GOTCHA
            lfqc <- lfqCohort(lfq, calc_dt = FALSE)
            df <- data.frame(
                length = rep(lfqc$midLengths, times = length(lfqc$dates)),
                rel.age = c(lfqc$rel.age),
                bday = c(lfqc$bday),
                cohort = c(lfqc$cohort),
                n = c(lfqc$catch))
            lfqU <- c(lfqc, list(df=df))

            ## relages <- unique(as.numeric(lfqc$rel.age))
            ## catch <- rep(NA, length(relages))
            ## for(i in 1:length(relages)){
            ##     catch[i] <- sum(lfqc$catch[lfqc$rel.age == relages[i]])
            ## }
            ## lfqU <- list(catch=catch, relages=relages,
            ##              par = rv$parsGrowth)

            resGOTCHA <- LCCC_shiny(
                x = lfqU, reg_int = c(input$regIntGOTCHA[1],
                                      input$regIntGOTCHA[2]),
                calc_ogive = FALSE, gotcha = TRUE)
            rv$resGOTCHA <- resGOTCHA
        }

        natM_res <- function(){
            resnatM <- natM_shiny(Linf = rv$parsGrowth$Linf, K_l = rv$parsGrowth$K,
                                  temp=input$temp, tmax = input$tmax,
                                  schooling=input$schooling,
                                  method = input$natM)
            rv$resnatM <- resnatM
        }

        observeEvent(input$gotchaEst,{
            rv$doYPR  <- FALSE
            if(input$gotchaEst && is.null(rv$resGOTCHA)){
                showNotification(paste("No GOTCHA estimates found. Please run GOTCHA to use the GOTCHA estimates for further analysis (press 'Run GOTCHA')."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            }
        }, ignoreInit = TRUE)

        observe({
            if(input$gotchaEst){
                pars <- list(Z=rv$resGOTCHA$par$Z,
                             M=rv$resnatM,
                             F=rv$resGOTCHA$par$Z - rv$resnatM,
                             E=(rv$resGOTCHA$par$Z- rv$resnatM) / rv$resGOTCHA$par$Z,
                             L50=rv$resLCCC$L50,
                             L75=rv$resLCCC$L75)
                rv$parsMortFinal <- unlist(pars)
            }else{
                pars <- list(Z=rv$resLCCC$par$Z,
                             M=rv$resnatM,
                             F=rv$resLCCC$par$Z - rv$resnatM,
                             E=(rv$resLCCC$par$Z- rv$resnatM) / rv$resLCCC$par$Z,
                             L50=rv$resLCCC$L50,
                             L75=rv$resLCCC$L75)
                rv$parsMortFinal <- unlist(pars)
            }
        })

        output$mortPars <- renderTable({
            if(rv$doLCCC == FALSE){
                data.frame()
            }else{
                isolate({
                    LCCC_res()
                })
                if(is.null(rv$resLCCC$L50))
                    showNotification(paste("The selectivity parameters could not be estimated. Try another regression interval."),
                                     type = "error",
                                     duration = 30,
                                     closeButton = TRUE
                                     )
                natM_res()
                pars <- list(Z=rv$resLCCC$par$Z,
                             M=rv$resnatM,
                             F=rv$resLCCC$par$Z - rv$resnatM,
                             E=(rv$resLCCC$par$Z- rv$resnatM) / rv$resLCCC$par$Z,
                             L50=rv$resLCCC$L50,
                             L75=rv$resLCCC$L75)
                rv$parsMort <- unlist(pars)
                tmp <- as.data.frame(t(as.matrix(rv$parsMort)))
                tmp
            }
        })

        output$LCCC_plot <- output$LCCC_plot_ov <- renderPlot({
            if(rv$doLCCC == FALSE){
                return()
            }else{
                par(mar=c(5,5,2,1))
                plotLCCC(rv$resLCCC)
            }
        })

        output$LCCC_sel_plot <- output$LCCC_sel_plot_ov <- renderPlot({
            if(rv$doLCCC == FALSE){
                return()
            }else{
                par(mar=c(5,5,2,1))
                plotLCCC_sel(rv$resLCCC)
            }
        })

        output$mortParsGOTCHA <- renderTable({
            if(rv$doGOTCHA == FALSE){
                data.frame()
            }else{
                isolate({
                    GOTCHA_res()
                })
                natM_res()
                pars <- c(rv$resGOTCHA$Z,
                          rv$resnatM,
                          rv$resGOTCHA$Z - rv$resnatM,
                          (rv$resGOTCHA$Z - rv$resnatM ) / rv$resGOTCHA$Z)
                names(pars) <- c("Z", "M", "F", "E")
                rv$parsMortGOTCHA <- pars
                tmp <- as.data.frame(t(as.matrix(rv$parsMortGOTCHA)))
                tmp
            }
        })

        output$GOTCHA_plot <- output$GOTCHA_plot_ov <- renderPlot({
            if(rv$doGOTCHA == FALSE){
                return()
                }else{
            par(mar=c(5,5,2,1))
            plotLCCC(rv$resGOTCHA)
            }
        })

        ##-----------------------------------------------------------



        ## REF LEVELS
        ##-----------------------------------------------------------
        observeEvent(input$tabset, {
            if(!is.null(rv$lfq) && !is.null(rv$parsGrowth) && !is.null(rv$parsMortFinal)){
                shinyjs::enable("runYPR")
                shinyjs::enable("resetYPR")
            }else{
                shinyjs::disable("runYPR")
                shinyjs::disable("resetYPR")
            }
        })

        ## only run if action button used
        observeEvent(input$runYPR, {
            rv$doYPR <- input$runYPR
        })

        ## reset button
        observeEvent(input$resetYPR, {
            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeYPR",
                              value = bsVal)
            updateSliderInput(session = session,
                              inputId = "l50",
                              value = round(rv$resLCCC$L50,1),
                              )
            updateNumericInput(session = session,
                              inputId = "lr",
                              value = lrVal,
                              )
            updateNumericInput(session = session,
                              inputId = "LWa",
                              value = 0.001,
                              )
            updateNumericInput(session = session,
                              inputId = "LWb",
                              value = 3,
                              )
            updateSliderInput(session = session,
                              inputId = "l50",
                              value = round(rv$resLCCC$L50,1),
                              )
            updateSliderInput(session = session,
                              inputId = "wqs",
                              value = round((rv$resLCCC$L75 - rv$resLCCC$L50)*2,1),
                              )
            lc <- round(rv$resLCCC$L50)
            updateSliderInput(session = session,
                              inputId = "lcChange",
                              value = range(0.6*lc, 1.4*lc),
                              )

            updateSliderInput(session = session,
                              inputId = "fmChangeAbs",
                              value = range(0, 10),
                              )
            updateSliderInput(session = session,
                              inputId = "fmChangeRel",
                              value = range(0, (rv$parsMortFinal[3] * 10)),
                              )
            ##
            rv$doYPR <- FALSE
            rv$resYPR <- NULL
            rv$resYPR_Lc <- NULL
            rv$parsRef <- NULL
        })

        ## observe ELEFAN and LCCC
        observeEvent(input$runELEFAN, {
            rv$doYPR <- FALSE
        })
        observeEvent(input$runLCCC, {
            rv$doYPR <- FALSE
        })

        ## update and create sliders
        observeEvent(input$binSize,{
            if(is.null(input$binSize)){
                bsVal <- 2
            }else{
                bsVal <- input$binSize
            }
            updateSliderInput(session = session,
                              inputId = "binSizeYPR",
                              value = bsVal)
        })

        output$lr <- renderUI({
            req(rv$Lrange)
            req(rv$parsGrowth)
            numericInput(
                inputId = "lr",
                label = "Length at recruitment to fishery (Lr)",
                value = min(rv$Lrange, na.rm=TRUE),
                min = 0,
                width = '50%'
            )
        })

        output$l50 <- renderUI({
            req(rv$resLCCC)
            req(rv$parsGrowth)
            numericInput(
                inputId = "l50",
                label = "Length at which probability of selection = 50% (L50)",
                value = round(rv$resLCCC$L50,1),
                min = 0
            )
        })

        output$wqs <- renderUI({
            req(rv$resLCCC)
            req(rv$parsGrowth)
            numericInput(
                inputId = "wqs",
                label = "Width of selectivity curve (L75 - L25)",
                value = round((rv$resLCCC$L75 - rv$resLCCC$L50)*2,1),
                min = 0
            )
        })

        output$lcChange <- renderUI({
            req(rv$resLCCC)
            req(rv$parsGrowth)
            lc <- round(rv$resLCCC$L50)
            sliderInput(
                inputId = "lcChange",
                label = "Selectivity (L50; absolute)",
                dragRange = TRUE,
                value = range(0.6*lc, 1.4*lc),
                min = 0,
                max = round(rv$parsGrowth$Linf),
                step = 1
            )
        })

        output$fmChangeRel <- renderUI({
            req(rv$parsMortFinal)
            vals <- range(0, (rv$parsMortFinal[3] * 10))
            sliderInput(
                inputId = "fmChangeRel",
                label = "Fishing mortality (relative to current F)",
                dragRange = TRUE,
                value = vals,
                min = 0,
                max = 20,##ifelse((rv$parsMort[3] * 10) < 10, 10, round(rv$parsMort[3] * 10)),
                step = 1
            )
        })

        observeEvent(input$fmChangeAbs,{
            vals <- round(input$fmChangeAbs * rv$parsMortFinal[3])
            updateSliderInput(session = session,
                              inputId = "fmChangeRel",
                              value = vals)
        })

        observeEvent(input$fmChangeRel,{
            vals <- round(input$fmChangeRel / rv$parsMortFinal[3])
            updateSliderInput(session = session,
                              inputId = "fmChangeAbs",
                              value = vals)
        })

        YPR_res <- function(){
            if(is.null(rv$lfq))
                showNotification(paste("The yield per recruit model requires a length-frequency data set. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            if(is.null(rv$parsGrowth))
                showNotification(paste("The yield per recruit model requires estimated growth parameters. Please go to the tab 'Growth' and run ELEFAN."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )
            if(is.null(rv$resLCCC) || is.null(rv$resnatM))
                showNotification(paste("The yield per recruit model requires estimated total and natural mortality rates. Please go to the tab 'Mortality' and run the length-converted catch curve (LCCC)."),
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE
                                 )

            progress <- shiny::Progress$new()
            ## Make sure it closes when we exit this reactive, even if there's an error
            on.exit(progress$close())
            progress$set(message = "Running YPR.",
                         detail = "This may take a while. This window will disappear
                     automatically.", value = 1)

            lfq <- rv$lfq
            lfq <- lfqModify(lfq, vectorise_catch = TRUE,
                             bin_size = input$binSizeYPR)
            lfq$par <- rv$parsGrowth
            lfq$par$a <- as.numeric(input$LWa)
            lfq$par$b <- as.numeric(input$LWb)
            lfq$par$M <- rv$resnatM
            lfq$par$FM <- rv$resLCCC$Z - rv$resnatM ## rep(1,length(rv$lfq$midLengths))
            lfq$par$Lr <- input$lr
            lfq$par$Lc <- input$l50
            lfq$par$Lmat <- as.numeric(input$Lmat)
            lfq$par$wmat <- as.numeric(input$wmat)
            if(input$yprSel == "KE"){
                selecType = 'knife_edge'
            }else if(input$yprSel == "TL"){
                selecType = 'trawl_ogive'
            }
            l75 <- input$l50 + input$wqs/2
            select.list <- list(selecType = selecType,
                                L50 = input$l50, L75 = l75)
            resYPR <- YPR_shiny(
                lfq = lfq, type = "ThompBell",
                s_list = select.list,
                FM_change = seq(min(input$fmChangeAbs), max(input$fmChangeAbs),
                                length.out = input$fmLengthOut),
                stock_size_1 = input$stockSize,
                curr.E = rv$parsMortFinal[4],
                curr.Lc = rv$resLCCC$L50)

            resYPR_Lc <- YPR_shiny(
                lfq = lfq, type = "ThompBell",
                s_list = select.list,
                FM_change = seq(min(input$fmChangeAbs), max(input$fmChangeAbs),
                                length.out = input$fmLengthOut),
                Lc_change = seq(min(input$lcChange), max(input$lcChange),
                                length.out = input$lcLengthOut),
                stock_size_1 = input$stockSize,
                curr.E = rv$parsMortFinal[4],
                curr.Lc = rv$resLCCC$L50)

            rv$resYPR <- resYPR
            rv$resYPR_Lc <- resYPR_Lc
            pars <- resYPR$df_Es
            names(pars) <- names(resYPR$df_Es)
            rv$parsRef <- pars
        }

        output$yprPars <- renderTable({
            if(rv$doYPR == FALSE){
                data.frame()
            }else{
                isolate({
                    YPR_res()
                })
                tmp <- as.data.frame(rv$parsRef)
                tmp
            }
        })

        output$YPR_plot <- output$YPR_plot_ov <- renderPlot({
            if(rv$doYPR == FALSE)
                return()
            par(mar=c(5,5,2,4))
            plotYPR(rv$resYPR, mark=TRUE)
        })

        output$YPR_Lc_plot <- output$YPR_Lc_plot_ov <- renderPlot({
            if(rv$doYPR == FALSE)
                return()
            par(mar=c(5,5,2,1))
            plotYPR(rv$resYPR_Lc, mark=TRUE,xaxis1 = "FM", yaxis_iso = "L50")
        })

        output$YPR_Lc2_plot <- output$YPR_Lc2_plot_ov <- renderPlot({
            if(rv$doYPR == FALSE)
                return()
            par(mar=c(5,5,2,1))
            plotYPR(rv$resYPR_Lc, mark=TRUE, yaxis1 = "B_R",xaxis1 = "FM", yaxis_iso = "L50")
        })

        ##-----------------------------------------------------------



        ## SUMMARY
        ##-----------------------------------------------------------
        output$growthPars_ov <-renderTable({
            if(rv$doELEFAN == FALSE){
                data.frame()
            }else{
                tmp <- as.data.frame( c(rv$parsGrowth,
                                        list(Rn_max = rv$resGA@fitnessValue)) )
                names(tmp) <- replace(names(tmp), names(tmp)=="Rn_max", "Rn")
                names(tmp) <- replace(names(tmp), names(tmp)=="phiL", "phi'")
                tmp
            }
        })

        output$mortPars_ov <- renderTable({
            if(rv$doLCCC == FALSE){
                data.frame()
            }else{
                as.data.frame(t(rv$parsMort))
            }
        })

        output$mortParsGOTCHA_ov <- renderTable({
            if(rv$doGOTCHA == FALSE){
                data.frame()
            }else{
                as.data.frame(t(rv$parsMortGOTCHA))
            }
        })

        output$yprPars_ov <- renderTable({
            if(rv$doYPR == FALSE){
                data.frame()
            }else{
                as.data.frame(rv$parsRef)
            }
        })

        output$allpars_ov <- renderTable({
            if(rv$doYPR){
               return(cbind(rv$parsGrowth, t(rv$parsMortFinal),rv$parsRef))
            }else if(rv$doLCCC){
               return(cbind(rv$parsGrowth, t(rv$parsMortFinal)))
            }else if(rv$doELEFAN){
               return(rv$parsGrowth)
            }else
                return(data.frame())
        })

        report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath

        observeEvent(input$generateReport, {

            ## check if tex distribution installed
            texAvail <- try(Sys.which('pdflatex'), silent=TRUE)
            ## check if pandoc installed
            pandocAvail <- rmarkdown::pandoc_available()
            ## system2('pdflatex', '--version')
            if(input$reportFormat == "pdf" &&
               (is.null(texAvail) || inherits(texAvail, "try-error") || texAvail == "")){
                report$filepath <- NULL
                showNotification("No TeX distribution found. Install the required TeX distribution on your computer or generate the report in 'html' format.",
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE,
                                 action = a(href = "https://www.latex-project.org/get/#tex-distributions",
                                            "TeX webpage",
                                            target="_blank")
                                 )
            }else if(input$reportFormat == "docx" && !pandocAvail){
                report$filepath <- NULL
                showNotification("The software 'pandoc' was not found. Install pandoc on your computer or generate the report in 'html' format.",
                                 type = "error",
                                 duration = 30,
                                 closeButton = TRUE,
                                 action = a(href = "https://pandoc.org/installing.html",
                                            "Pandoc webpage",
                                            target="_blank")
                                 )
            }else {
                progress <- shiny::Progress$new()

                ## Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                progress$set(message = "Building report.",
                             detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 1)

                if(is.null(rv$lfq)){
                    report$filepath <- NULL
                    showNotification(paste("The minimum requirements for the report is a length-frequency data set. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                                     type = "error",
                                     duration = 30,
                                     closeButton = TRUE
                                     )
                    }

                params <- list(shinyRes = rv)

                td <- tempdir()
                tmp_file <- tempfile(fileext = paste0(".",input$reportFormat))
                tmp_file2 <- tempfile(fileext = ".Rmd")

                file.copy("report/TropFishR.bib", td,
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
                showNotification("The report was generated successfully. You can download with the 'Download Assessment Report' button on the left.",
                                 duration = 10)
            }
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
                paste0("ShinyTropFish_report_", Sys.Date(), ".", input$reportFormat)
                ##     filename = strsplit(rv$filename,"Data: ")[[1]][2]
                ##     paste0("STF_report_",filename,"_",Sys.Date(),".pdf")
            },

            ## This function should write data to a file given to it by
            ## the argument 'file'.
            content = function(con){
                file.copy(report$filepath, con)
            }
        )

        output$allParameters <- downloadHandler(
            filename = function(){
                filename = strsplit(rv$filename,"Data: ")[[1]][2]
                paste0("ShinyTropFish_allpars_",filename,"_",Sys.Date(),".csv")
            },
            content = function(con){
                write.csv(rv$allpars, con)
            }
        )

        output$allGraphs <- downloadHandler(
            filename = function(){
                filename = strsplit(rv$filename,"Data: ")[[1]][2]
                paste0("ShinyTropFish_allgraphs_",filename,"_",Sys.Date(),".zip")
            },
            content = function(con){
                graphs <- c("lfq","ELEFANfit","ELEFANscore",
                            "cohorts","recruitment","catchcurve",
                            "ypr")
                fs <- c()
                tmpdir <- tempdir()
                ## lfq data plot
                i = 1
                if(!is.null(rv$lfqre)){
                    path <- paste0(graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    lfqre <- rv$lfqre
                    opar <- par(mfrow=c(2,1), mar=c(2,4,2,3), oma=c(2,0,0,0))
                    plot(lfqre, Fname = "catch", rel = input$relLFQ)
                    plot(lfqre, Fname = "rcounts")
                    par(opar)
                    dev.off()
                }
                ## ELEFAN fit
                i = 2
                if(!is.null(rv$lfqFC) & !is.null(rv$parsGrowth)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    par(mar=c(5,5,2,1))
                    plot(rv$lfqFC, Fname = "rcounts")
                    lfqFC <- rv$lfqFC
                    lfqFitCurves(lfq = lfqFC,
                                 par=as.list(rv$parsGrowth),
                                 draw = TRUE)
                    dev.off()
                }
                ## ELEFAN score plot
                i = 3
                if(!is.null(rv$resGA)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    par(mar=c(5,5,2,1))
                    plot(rv$resGA)
                    dev.off()
                }
                ## cohort plot
                i = 4
                if(!is.null(rv$lfqFC)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    par(mar=c(5,5,2,1))
                    lfqc <- lfqCohort(rv$lfqFC, calc_dt = FALSE)
                    plot(lfqc, Fname = "catch",
                         ylim = c(0, max(rv$lfq$midLengths)), image.col=NA)
                    pal <- colorRampPalette(c(4,5,7,2))
                    with(lfqc, image(x = dates, y = midLengths, z = t(cohort),
                                     col = adjustcolor(pal(max(cohort, na.rm = TRUE)), 0.75),
                                     add=TRUE
                                     ))
                    dev.off()
                }
                ## recruitment plot
                i = 5
                if(!is.null(rv$parsGrowth)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    lfq <- rv$lfq
                    lfq$par <- rv$parsGrowth
                    par(mar=c(5,5,2,1))
                    tmp <- recruitment(lfq, plot=TRUE)
                    dev.off()
                }
                ## catch curve plot
                i = 6
                if(!is.null(rv$resLCCC)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    par(mar=c(5,5,2,1))
                    plot(rv$resLCCC)
                    dev.off()
                }
                ## yield per recruit plot
                i = 7
                if(!is.null(rv$resYPR)){
                    path <- paste0(tmpdir,"/",graphs[i], ".pdf")
                    fs <- c(fs, path)
                    pdf(path)
                    par(mar=c(5,5,2,1))
                    plot(rv$resYPR)
                    dev.off()
                }
                ##
                zip(zipfile=con, files=fs)
            },
            contentType = "application/zip"
        )

        output$allData <- downloadHandler(
            filename = function(){
                filename = strsplit(rv$filename,"Data: ")[[1]][2]
                paste0("ShinyTropFish_alldata_",filename,"_",Sys.Date(),".RData")
            },
            content = function(con){
                shinyRes <- reactiveValuesToList(rv)
                save(shinyRes, file = con)
            }
        )

        ##-----------------------------------------------------------




        ## ABOUT
        ##-----------------------------------------------------------
        output$tobm <- renderImage({
            return(list(
                src = "images/tobm.JPG",
                contentType = "image/jpeg",
                width = "100%",
                alt = "Tobias K. Mildenberger",
                caption = "Tobias K. Mildenberger"
            ))
        }, deleteFile = FALSE)


        output$mht <- renderImage({
            return(list(
                src = "images/mht2.JPG",
                contentType = "image/jpeg",
                width = "100%",
                alt = "Marc H. Taylor",
                caption = "Marc H. Taylor"
            ))
        }, deleteFile = FALSE)


        output$alko <- renderImage({
            return(list(
                src = "images/alko.JPG",
                contentType = "image/jpeg",
                width = "100%",
                alt = "Alexandros Kokkalis",
                caption = "Alexandros Kokkalis"
            ))
        }, deleteFile = FALSE)

        output$dp <- renderImage({
            return(list(
                src = "images/dp.JPG",
                contentType = "image/jpeg",
                width = "100%",
                alt = "Daniel Pauly",
                caption = "Daniel Pauly"
            ))
        }, deleteFile = FALSE)
        ##-----------------------------------------------------------

        ## OTHER  ##################################################################################

        observe({
            if (input$tabset == "stop"){
                js$closeWindow();
                stopApp(message("App stopped"))
            }
        })


    }
)
