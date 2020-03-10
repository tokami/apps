## spictapp
## --------
## Tobias K. Mildenberger
## March 2020



## Set working directory to script location (check for ubuntu)
## -------------------------
tried <- try(setwd(getSrcDirectory()[1]), silent=TRUE)
if(inherits(tried, "try-error"))
    trie <- try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent=TRUE)


## Run the application
## -------------------------
shiny::runApp("apps/", launch.browser = TRUE)
