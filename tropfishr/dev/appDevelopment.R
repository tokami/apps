




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



## for development
rm(list=ls())

## devtools::install_github("tokami/TropFishR", ref="dev2.0")
require(TropFishR)
require(fishdynr)

source("~/Documents/DrTokami/R\ packages/ShinyTropFish/ShinyTropFish/functions/ELEFAN_GA_fun.R")
source("~/Documents/DrTokami/R\ packages/ShinyTropFish/ShinyTropFish/functions/LCCC_fun.R")
source("~/Documents/DrTokami/R\ packages/ShinyTropFish/ShinyTropFish/functions/natM_fun.R")
source("~/Documents/DrTokami/R\ packages/ShinyTropFish/ShinyTropFish/functions/VPA_fun.R")
source("~/Documents/DrTokami/R\ packages/ShinyTropFish/ShinyTropFish/functions/YPR_fun.R")


data(alba)

res <- ELEFAN_GA_shiny(alba)

a












low_par = c(Linf=8, K=0.1, ta=0, C=0, ts=0)
up_par = c(Linf=20, K=1, ta=1, C=1, ts=1)
resGA <- ELEFAN_GA_shiny(alba, low_par = low_par, up_par = up_par)

resGA

pars <- as.list(resGA@solution[1,])
names(pars) <- c("Linf", "K", "ta")
pars <- pars[c("Linf", "K", "ta")]

alba2 <- lfqModify(alba, vectorise_catch = TRUE)

alba2$par <- pars

resLCCC <- LCCC_shiny(alba2, reg_int = c(5,12))

resnatM <- natM_shiny(Linf=pars$Linf, K_l = pars$K, method = "Then_growth")

tmp <- as.list(c(resLCCC$Z,resnatM, resLCCC$Z-resnatM))
names(tmp) <- c("Z","M","F")
as.data.frame(tmp)

plot(resLCCC)

resF <- resLCCC$Z - resnatM
alba2$a <- 0.005
alba2$b <- 3
alba2$M <- resnatM

resVPA <- VPA_shiny(alba2, terminalF = resF, catch_columns = 1, plus_group = TRUE)


resVPA$FM_calc
sum(resVPA$meanBiomassTon)
sum(resVPA$annualMeanNr)

alba2$catch <- as.numeric(alba2$catch[,1])
alba2$dates <- alba2$dates[1]
alba2$FM <- resVPA$FM_calc
alba2$Lc <- 5
alba2$Lr <- 2

select.list <- list(selecType = 'knife_edge',
                    L50 = 5)
resYPR <- YPR_shiny(alba2, type = "ThompBell", s_list = select.list)


resYPR$df_Es

plot(resYPR)
