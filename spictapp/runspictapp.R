## spictapp
## --------
## Tobias K. Mildenberger
## April 2020


## Set working directory to script location (check for ubuntu)
## -------------------------
tried <- try(setwd(getSrcDirectory()[1]), silent=TRUE)
if(inherits(tried, "try-error"))
    trie <- try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent=TRUE)


## Install packages
## -------------------------
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown",
            "htmltools", "remotes", "ellipse", "flextable", "pander")
install.packages(needed[! needed %in% installed],
                 repos="https://cloud.r-project.org/")
## download the development version of spict if not downloaded already
if(packageVersion("spict") != "1.3.0")
    remotes::install_github("tokami/spict/spict@FIDEA")


## Run the application
## -------------------------
shiny::runApp("apps/", launch.browser = TRUE)
