## spictapp
## --------
## Tobias K. Mildenberger
## April 2020


## Install packages
## -------------------------
installed <- rownames(installed.packages())
needed <- c("shiny", "shinyjs", "shinythemes", "shinydashboard", "rmarkdown",
            "htmltools", "remotes", "ellipse", "flextable", "pander", "R.utils")
install.packages(needed[! needed %in% installed],
                 repos="https://cloud.r-project.org/")
## download the development version of spict if not downloaded already
if(packageVersion("spict") != "1.3.0")
    remotes::install_github("tokami/spict/spict@FIDEA")


## Set working directory to script location
## -------------------------
tried <- try(setwd(getSrcDirectory()[1]), silent=TRUE)
if(inherits(tried, "try-error")){
    tried <- try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent=TRUE)
}
if(inherits(tried, "try-error")){
    tried <- try(setwd(dirname(parent.frame(2)$ofile)), silent=TRUE)
}
if(inherits(tried, "try-error")){
    tried <- try(setwd(dirname(parent.frame(1)$ofile)), silent=TRUE)
}
if(inherits(tried, "try-error")){
    Dirname <- "ShinyTropFish"
    Dirs <- R.utils::withTimeout(list.dirs(path=file.path("~/.."),recursive=T),
                                 timeout=120, onTimeout = "silent")
    tried <- try(setwd(names(unlist(sapply(Dirs,grep,pattern=Dirname))[1])), silent=TRUE)
    rm(Dirs)
    gc()
}
writeLines(paste0("Setting working directory to script location ",
             ifelse(!inherits(tried,"try-error"),"successfull.","not successfull. Please set the working directory to script location manually.")))


## Run the application
## -------------------------
shiny::runApp("apps/", launch.browser = TRUE)
