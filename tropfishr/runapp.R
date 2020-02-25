## Shiny app for TropFishR related data exploration

## spictapp
## --------

## Install packages
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown", "htmltools", "remotes")

install.packages(needed[! needed %in% installed], repos="https://cloud.r-project.org/")
remotes::install_github("tokami/TropFishR", ref="master")

## load shiny
library(shiny)

## Run the application
runApp("apps/")
