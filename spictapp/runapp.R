## spictapp
## --------
## Tobias K. Mildenberger
## March 2020

## Install packages
## -------------------------
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown", "htmltools", "remotes")
install.packages(needed[! needed %in% installed], repos="https://cloud.r-project.org/")

## Operating system
## -------------------------
os <- .Platform$OS.type
if(os == "unix"){ ## linux + mac
    ## remotes::install_github("tokami/spict/spict@Zanzibar2020")
    install.packages("spict_1.3.0.tar.gz",repos=NULL)  ## package needs to be in path!
}else if(os == "windows"){ ## windows
    install.packages("spict_1.3.0.zip",repos=NULL)  ## package needs to be in path!
}else{
    stop("Operating System not known!")
}

## load shiny
## -------------------------
library(shiny)

## Run the application
## -------------------------
runApp("apps/", launch.browser = TRUE)
