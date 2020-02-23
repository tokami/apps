## Shiny app for TropFishR related data exploration
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
## devtools::install_github("tokami/TropFishR", ref="dev2.0")
## devtools::install_github("tokami/TropFishR", ref="dev")
##-----------------------------------------------------------------------------------


require(shiny)


## Run the application
runApp("apps/")


a





## development area

## older for Tobias
## runApp("~/Documents/DrTokami/R packages/ShinyTropFish/ShinyTropFish/apps/")
## runApp("~/Documents/DrTokami/rpackages/shinyTropFishR/ShinyTropFishR/apps/")


## create example dataset
if(FALSE){
set.seed(1)
dat <- data.frame(length = round(rnorm(1e3,50,15)),
                  dates = seq.Date(as.Date("2017-01-15"),as.Date("2017-12-15"), length.out = 1e3),
                  frequency = rep(1,100))

write.csv(dat, "../exampleDataset.csv")

set.seed(1)
dat2 <- data.frame(length = round(c(rnorm(100,50,15),rnorm(100,100,25))),
                  dates = seq.Date(as.Date("2018-01-15"),as.Date("2018-12-15"), length.out = 100),
                  frequency = round(abs(rnorm(100,20,5))))

lfqT <- lfqCreate(dat2, Lname = "length", Fname = "frequency", Dname = "dates",
                  bin_size = 2, aggregate_dates = T)
plot(lfqT)

write.table(dat2, "../exampleDataset2.txt")


tmp <- read.csv("../exampleDataset2.txt", sep = "\t")
tmp <- read.table("../exampleDataset2.txt")
head(tmp)

read.table

}





