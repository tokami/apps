
require(spict)

inp <- check.inp(pol$albacore)

fit <- fit.spict(inp)
fit <- calc.osa.resid(fit)

retro <- retro(fit)

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
sumspict.ini(fit)

## ADD: this
sumspict.diagnostics(fit)

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
