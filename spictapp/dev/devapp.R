

            if(length(colNames$timeC) < 1 || colNames$timeC == "") stop("No times of the catch observations provided! Choose corresponding column names.")
            if(length(colNames$obsC) < 1 || colNames$obsC == "") stop("No catch observations provided! Choose corresponding column names")
            if((length(colNames$timeI) < 1 || colNames$timeI[1] == "") && (length(colNames$timeE) < 1 || colNames$timeE == "")) stop("Neither times for the index nor effort observations provided! Choose corresponding column names.")
if((length(colNames$obsI) < 1 || colNames$obsI[1] == "") && (length(colNames$obsE) < 1 || colNames$obsE == "")) stop("Neither index nor effort observations provided! Choose corresponding column names")




div(style="display:inline-block;width:95%;text-align: center;",
                         actionButton("stop", "Quit shiny", class = "btn-danger", onclick = "setTimeout(function(){window.close();}, 100);")),

require(spict)

inp <- check.inp(pol$albacore)
inp$priors$BmsyB0 <- c(0.5,2,1)



get.no.active.priors(inp)

plotspict.priors.inp(inp)

fit <- fit.spict(inp)
fit <- calc.osa.resid(fit)

retro <- retro(fit,1)

fit <- check.ini(fit,1)




## ADD:
round(sumspict.parest(fit),3)

## ADD:
round(sumspict.srefpoints(fit),3)
round(sumspict.drefpoints(fit),3)

## ADD:
round(sumspict.states(fit),3)
round(sumspict.predictions(fit),3)

## ADD:
fit <- check.ini(fit)
sumspict.ini(fit) ## list

## ADD: this
sumspict.diagnostics(fit)


mana <- manage(fit,1)

rr <- (sumspict.manage(mana))

rr

a

## older
####

        #sidebarLayout(
        #sidebarPanel(
        #tags$textarea(id = 'input_text', placeholder = 'Type here', rows = 8, ""),
        #tag$textarea(id="foo", rows=3, cols=40, "Default value"),
        column(8,
               fluidRow(column(8, h3('Inputs')), column(4, actionButton("runspict", 'Run SPiCT', col='green'))),
               tabsetPanel(type = "tabs",
                           tabPanel("Data", wellPanel(
                                                radioButtons("radio", label = h5('Source'), choices = list("Own data" = 'own', "Demo data" = 'demo'), selected = 'own'),
                                                fileInput("file", label = h5("File input (csv)"), accept=c('text/csv','text/comma-separated-values,text/plain','.csv')))),
                           tabPanel("Read options", wellPanel(
                                                        fluidRow(
                                                            column(3, checkboxInput("inphead", label = "Header?", value = TRUE)),
                                                            column(2, selectInput('inpsep', 'Separator', choices=c(',', 'blank', ';'))),
                                                            column(1, span('')),
                                                            column(2, numericInput("inpskip", label = "Skip lines", value = 0))
                                                        ),
                                                        fluidRow(
                                                            column(2, numericInput("timecol", label = "Time col", value = 1)),
                                                            column(1, span('')),
                                                            column(2, numericInput("catchcol", label = "Catch col", value = 2)),
                                                            column(1, span('')),
                                                            column(2, numericInput("indexcol", label = "Index col", value = 3)))
                                                    )),

                           tabPanel("Set input options", wellPanel(
                                                             fluidRow(
                                                                 column(4, tags$textarea(id = 'input_text', placeholder = 'Type here', rows = 8, "")),
                                                                 column(6, verbatimTextOutput("output_text"))
                                                                 #column(4, verbatimTextOutput("inp"))
                                                             )
                                                         ))
                           ),

               tabsetPanel(type = "tabs",
                           tabPanel("Uploaded data", tableOutput("contents")),
                           tabPanel("Data plot", wellPanel( fluidRow( column(6,plotOutput("dataplot"))))),
                           tabPanel("Input list", verbatimTextOutput('inp')),
                           tabPanel("Hide input")
                           )
               ),

        # Show a plot of the generated distribution
        #mainPanel(
        column(9,
               h3('Ouputs'),
               tabsetPanel(type='tabs',
                           tabPanel('Plots',
                                    column(4, plotOutput("bplot")),
                                    column(4, plotOutput("fplot")),
                                    column(4, plotOutput("catchplot")),
                                    column(4, plotOutput("bbplot")),
                                    column(4, plotOutput("ffplot")),
                                    column(4, plotOutput("fbplot")),
                                    #column(4, plotOutput("osarplot")),
                                    column(4, plotOutput("prodplot")),
                                    column(4, plotOutput("tcplot"))
                                    ),
                           tabPanel('Summary', verbatimTextOutput('summary'))
                           ))



## old in server:
    # Upload file
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    dataInput <- reactive({
        inFile <- input$file
        if(is.null(inFile)){
            return(NULL)
        } else {
            sep <- input$inpsep
            if(sep=='blank') sep <- ' '
            return(read.csv(inFile$datapath, header=input$inphead, sep=sep, skip=input$inpskip))
        }
    })



    # Make inp list
    make.inp <- reactive({
        inp <- make.inpout()
        if(input$radio == 'own'){
            dat <- dataInput()
            if(is.null(dat)){
                return(NULL)
            } else {
                #inp <- list()
                timecol <- as.numeric(input$timecol)
                if(timecol>0){
                    inp$timeC <- dat[, timecol]
                    inp$timeI <- dat[, timecol]
                }
                inp$obsC <- dat[, as.numeric(input$catchcol)]
                inp$obsI <- dat[, as.numeric(input$indexcol)]
            }
        }
        if(input$radio == 'demo'){
            nms <- names(pol$albacore)
            for(i in 1:length(nms)) inp[[nms[i]]] <- pol$albacore[[nms[i]]]
            #inp <- pol$albacore
        }
        return(inp)
    })

    # Translate input inp list to verbatim output
    output$output_text <- renderPrint({
        make.inpout()
    })

    # Translate current inp list to verbatim output
    output$inp <- renderPrint({
        make.inp()
    })

    # Translate input file to a table
    output$contents <- renderTable({
        dataInput()
    })

    # Fit spict
    spict <- reactive({
        inp <- make.inp()
        if(!is.null(inp)){
            rep <- try(fit.spict(inp))
            return(rep)
        } else {
            return(NULL)
        }
    })

    # Capture summary of results
    output$summary <- renderPrint({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            #sink(file='deleteme.txt')
            msg.trap <- capture.output(rep <- spict())
            #sink()
            if(class(rep)=='try-error'){
                cat(paste('There was an Error fitting the model!', rep, '\n'))
            } else {
                if(!is.null(rep)){
                    summary(rep)
                }
            }
        })
    })

    ### --- PLOTS --- ###
    # Data plot
    output$dataplot <- renderPlot({
        inp <- make.inp()
        if(!is.null(inp)){
            # dev.new(width=7, height=9) # This makes the plot a pop-up (may not work if R is not installed)
            plotspict.ci(inp)
        }
    })

    # Result plots
    output$bplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        # Isolate to ensure spict only runs if button is pressed.
        # For isolate to work it is important to fix the extends of the output such as setting the height and width of a plot.
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.biomass(rep)
            }
        })
    })
    output$fplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.f(rep)
            }
        })
    })
    output$catchplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.catch(rep, qlegend=FALSE)
            }
        })
    })
    output$bbplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.bbmsy(rep, qlegend=FALSE)
            }
        })
    })
    output$ffplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.ffmsy(rep)
            }
        })
    })
    output$fbplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.fb(rep)
            }
        })
    })
    output$osarplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            osar <- calc.osa.resid(rep)
            if(!is.null(osar)){
                plotspict.osar(osar)
            }
        })
    })
    output$prodplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.production(rep)
            }
        })
    })
    output$tcplot <- renderPlot({
        # Take dependency on action button
        if(input$runspict == 0) return()
        isolate({
            rep <- spict()
            if(!is.null(rep)){
                plotspict.tc(rep)
            }
        })
    })



## old readme

The click-based Shiny App for fittng surplus production models in
continuous-time to fisheries catch data and biomass indices (either scientific
or commercial). Main advantages of spict are:

1. All estimated reference points (MSY, Fmsy, Bmsy) are reported with uncertainties.

2. The model can be used for short-term forecasting and management strategy evaluation.

3. The model is fully stochastic in that observation error is included in catch and index observations, and process error is included in fishing and stock dynamics.

4. The model is formulated in continuous-time and can therefore incorporate arbitrarily sampled data.

## Help files

A vignette for the package is available
[`here`](https://github.com/DTUAqua/spict/raw/master/spict/vignettes/vignette.pdf),
and serves as an introduction to the package and its functionality. The vignette
also contains description of the more advanced features of the package.

A document with technical guidelines for using SPiCT is available
[here](https://github.com/DTUAqua/spict/raw/master/spict/vignettes/spict_guidelines.pdf).
This is a living document that has a list of things to check before accepting an
assessment and some options to deal with more dificult data sets.

The package also contains reasonable documentation in the form of help texts
associated with each function (some may not be fully up-to-date). These can be
accessed in the usual R manner by writing e.g. ```?check.inp```. A good place to
start (in addition to reading the vignette) is to read ```?check.inp``` and
```?fit.spict```.

## Citation

The underlying model used in the package is described in a published [`paper`](http://onlinelibrary.wiley.com/doi/10.1111/faf.12174/full). This paper is included in the package in the [`inst`](https://github.com/DTUAqua/spict/tree/master/spict/inst) folder. To get citation information write `citation(spict)` in the command line.

## Package requirements

The package requires [`TMB`](http://www.tmb-project.org) to be installed. TMB is now a part of CRAN and can therefore be installed using the install.packages() command. For more information about TMB click [`here`](https://github.com/kaskr/adcomp).

## Installing the spict package

To install spict from GitHub use

```
library(remotes)
install_github("DTUAqua/spict/spict")            # master branch
```

Windows
-------
The above procedure using install_github() should now work on Windows (make sure to remove spict before trying to reinstall). If it doesn't work the old, but tedious, procedure can be used:

1. Start 64 bit R and change working directory to the (cloned or unzipped) ```spict``` folder.

2. From R run: ```source("install_windows.R")```

This requires that Rtools is installed. Rtools can be obtained [`here`](https://cran.r-project.org/bin/windows/Rtools/). When running install_windows.R remember to set your working directory to the spict directory containing install_windows.R.























#####
    output$sumParest <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            sumspict.parest(rv$fit)
        }
    })


    output$sumParest <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.parest(rv$fit),3)
        }
    })

    output$sumSrefpoints <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.srefpoints(rv$fit),3)
        }
    })

    output$sumDrefpoints <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.drefpoints(rv$fit),3)
        }
    })

    output$sumStates <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.states(rv$fit),3)
        }
    })

    output$sumPredictions <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.predictions(rv$fit),3)
        }
    })

    output$sumDiag <- renderPrint({
        if(rv$doSPICT == FALSE){
            writeLines("No model fitted. Run 'Fit SPiCT'.")
        }else{
            round(sumspict.diagnostics(rv$fit),3)
        }
    })

    output$sumIni <- renderPrint({
        if(rv$doSENSI == FALSE){
            writeLines("No results from the sensitivity analysis. Run 'Run check.ini'.")
        }else{
            round(sumspict.ini(rv$fit),3)
        }
    })

    output$plotAll <- renderPlot({
        if(rv$doSPICT == FALSE){
            return()
        }else{
            plot(rv$fit)
        }
    })

    output$plotMana2 <- renderPlot({
        if(rv$doMANA == FALSE){
            return()
        }else{
            plot(rv$mana)
        }
    })


    ## RDATA
    output$allData <- downloadHandler(
        filename = function(){
            filename = strsplit(rv$filename,"Data: ")[[1]][2]
            paste0("spictapp_RData_",filename,"_",Sys.Date(),".RData")
        },
        content = function(con){
            save(rv, file = con)
        }
    )

    ## CSV
    output$allTables <- downloadHandler(
        filename = function(){
            filename = strsplit(rv$filename,"Data: ")[[1]][2]
            paste0("spictapp_tables_",filename,"_",Sys.Date(),".zip")
        },
        content = function(con){
            tables <- c("parest","states","predictions")
            fs <- c()
            tmpdir <- tempdir()
            ## parest
            i = 1
            if(!is.null(rv$fit)){
                path <- paste0(tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.parest(rv$fit),3)
                write.csv(tab, path)
            }
            ## states
            i = 2
            if(!is.null(rv$fit)){
                path <- paste0(tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.states(rv$fit),3)
                write.csv(tab, path)
            }
            ## predictions
            i = 3
            if(!is.null(rv$fit)){
                path <- paste0(tables[i], ".csv")
                fs <- c(fs, path)
                tab <- round(sumspict.predictions(rv$fit),3)
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
            ##                setwd(tempdir())
            ## data plot
            i = 1
            if(!is.null(rv$inp)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plotspict.data(rv$inp)
                dev.off()
            }
            ## priors
            i = 2
            if(!is.null(rv$fit)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plotspict.priors(rv$fit)
                dev.off()
            }else if(!is.null(rv$inp)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plotspict.priors.inp(rv$inp)
                dev.off()
            }
            ## plot2
            i = 3
            if(!is.null(rv$fit)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plot2(rv$fit)
                dev.off()
            }
            ## retro
            i = 4
            if(!is.null(rv$retro)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plotspict.retro(rv$retro)
                dev.off()
            }
            ## manage
            i = 5
            if(!is.null(rv$mana)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plot2(rv$mana)
                dev.off()
            }
            ## plot all
            i = 6
            if(!is.null(rv$fit)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
                plot(rv$fit)
                dev.off()
            }
            ## plot all manage
            i = 7
            if(!is.null(rv$mana)){
                path <- paste0(graphs[i], ".pdf")
                fs <- c(fs, path)
                pdf(path)
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

        progress <- shiny::Progress$new()

        ## Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Building report.",
                     detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 1)

        if(is.null(rv$inp))
            showNotification(paste("The minimum requirements for the report is input data. Please go to the tab 'Load data' and upload your own data or choose an example data set."),
                             type = "error",
                             duration = NULL,
                             closeButton = TRUE,
                             action = a(href = "javascript:location.reload();", "Reload page")
                             )

            params <- list(rv = rv)

            td <- tempdir()
            tmp_file <- tempfile(fileext = ".pdf")
            tmp_file2 <- tempfile(fileext = ".Rmd")

            file.copy("report/spictapp.bib", td,
                      overwrite = TRUE)
            file.copy("report/assessmentReport.Rmd", tmp_file2, overwrite = TRUE)

            rmarkdown::render(tmp_file2,
                              output_format = "pdf_document",
                              output_file = tmp_file,
                              output_dir = td,
                              intermediates_dir = td,
                              knit_root_dir = td,
                              clean = TRUE,
                              params = params,
                              envir = new.env())

            report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above

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
            paste0("spictapp_report_", Sys.Date(), ".pdf")
            ##     filename = strsplit(rv$filename,"Data: ")[[1]][2]
            ##     paste0("STF_report_",filename,"_",Sys.Date(),".pdf")
        },

        ## This function should write data to a file given to it by
        ## the argument 'file'.
        content = function(con){
            file.copy(report$filepath, con)
        }
    )
