## spictapp
##
##
## Short manual:
## You can run the application by clicking the 'Run App' button above or
## sourcing this script. However, make sure you have instaled required
## packages (see below) and that the required data is in the same directory.
##


## Install required packages
##-----------------------------------------------------------------------------------
## install.packages("shiny")
## install.packages("shinydashboard")
## install.packages("devtools")
## install.packages("shinyjs")
## install.packages("shinythemes")
## install.packages("shinyWidgets")
## devtools::install_github("tokami/spict/spict",ref="manage4.0")
##-----------------------------------------------------------------------------------

## Install packages

installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown", "htmltools", "remotes")

install.packages(needed[! needed %in% installed], repos="https://cloud.r-project.org/")
remotes::install_github("tokami/spict/spict", ref = "manage4.0")

library(shiny)

## data(pol)
## polalb <- check.inp(pol$albacore)
## anms <- names(polalb)


## Run the application
runApp("apps/")
