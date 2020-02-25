## spictapp
## --------

## Install packages
## -------------------------
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown", "htmltools", "remotes")
install.packages(needed[! needed %in% installed], repos="https://cloud.r-project.org/")

## Operating system
## -------------------------
os <- .Platform$OS.type
if(os == "unix"){ ## linux + mac
    remotes::install_github("tokami/spict/spict", ref = "manage4.0")
}else if(os == "windows"){ ## windows
    install.packages("spict_v1.3.0.zip")
}else{
    stop("Operating System not known!")
}

## load shiny
## -------------------------
library(shiny)

## Run the application
## -------------------------
runApp("apps/", launch.browser = TRUE)
