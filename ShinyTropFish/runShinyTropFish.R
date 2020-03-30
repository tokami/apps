## ShinyTropFish
## --------
## Tobias K. Mildenberger
## April 2020


## Set working directory to script location (only working for windows yet)
## -------------------------
tried <- try(setwd(getSrcDirectory()[1]), silent=TRUE)
if(inherits(tried, "try-error"))
    try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent=TRUE)


## Install packages
## -------------------------
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard",
            "rmarkdown", "htmltools","remotes","ks", "flextable")
## install only missing packages
install.packages(needed[! needed %in% installed],
                 repos="https://cloud.r-project.org/")
## download the development version of TropFishR if not downloaded already
if(packageVersion("TropFishR") != "1.7.0")
    remotes::install_github("tokami/TropFishR@dev")


## Run the application
## -------------------------
shiny::runApp("apps/")
