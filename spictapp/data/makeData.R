## Make example data sets for spictapp
## Tobias K. Mildenberger
## March 2020

library(spict)

source("../funcs/serverFuncs.R")


## albacore (csv)
## ----------------------------------------------
dat <- inp2dat(pol$albacore)

## arrange
dat <- dat[c("timeC","obsC","timeI1","obsI1")]

## save
write.csv(dat, file = "albacore.csv")


## albacore (csv + 2 Indices)
## ----------------------------------------------
set.seed(123)
inp <- list(timeC=pol$albacore$timeC, obsC=pol$albacore$obsC)
inp$timeI <- list(pol$albacore$timeI, pol$albacore$timeI[10:23]+0.25)
inp$obsI <- list()
inp$obsI[[1]] <- pol$albacore$obsI * exp(rnorm(23, sd=0.1)) # Index 1
inp$obsI[[2]] <- 10*pol$albacore$obsI[10:23] # Index 2
dat <- inp2dat(inp)

## save
write.csv(dat, file = "albacore_2indices.csv")


## lobster (txt + different sep)
## ----------------------------------------------
dat <- inp2dat(pol$lobster)

## arrange
dat <- dat[c("timeC","obsC","timeI1","obsI1")]

##save
write.table(dat, file = "lobster.txt", sep=";")


## lobster (csv + stdevfac)
## ----------------------------------------------
dat <- inp2dat(pol$lobster)

## arrange
dat$timeI1 <- dat$timeI1 + 0.5
dat$stdevfacC[1:8] <- rep(1.5,8)
dat$stdevfacI1[1:4] <- rep(2,4)

## save
write.csv(dat, file = "lobster_stdevfac.csv")



## hake (txt + different column names)
## ----------------------------------------------
dat <- inp2dat(pol$hake)

## arrange
dat <- dat[c("timeC","obsC","timeI1","obsI1")]

## change column names
colnames(dat) <- c("Time","Catch commercial fleet","Survey time","Catch survey fleet")

## save
write.table(dat, file = "hake.txt", sep=",")



## hake (effort)
## ----------------------------------------------
inpeff <- list(timeC=pol$hake$timeC, obsC=pol$hake$obsC,
               timeE=pol$hake$timeC, obsE=pol$hake$obsC/pol$hake$obsI)
dat <- inp2dat(inpeff)

## arrange
dat <- dat[c("timeC","obsC","timeE","obsE")]

## save
write.table(dat, file = "hake_effort.csv")



## seasonal data (standard)
## ----------------------------------------------
set.seed(455)
nt <- 25
inp <- list(nseasons = 4, splineorder = 3)
inp$timeC <- seq(0, nt - 1 / inp$nseasons, by = 1 / inp$nseasons)
inp$nindex <- 2
inp$nobsI <- c(24,16)
inp$timeI <- list()
inp$timeI[[1]] <- seq(0.12, inp$nobsI[1] + 0.12, 1)
inp$timeI[[2]] <- seq(2.65, inp$nobsI[2] + 0.65, 1)
inp$ini <- list(logK = log(1000), logm=log(800), logq = c(log(1),log(0.04)), logn = log(2),
                logbkfrac = log(0.9), logsdf = log(0.3), logF0 = log(0.8),
                logphi = log(c(0.3, 0.5, 1.8)))
inpsim <- sim.spict(inp)
dat <- inp2dat(inpsim)

## arrange
dat <- dat[c("timeC","obsC","timeI1","obsI1","timeI2","obsI2")]

## save
write.csv(dat, file = "seasonalCatches.csv")



## really data limited data (catch + 3 biomass indices)
## ----------------------------------------------
inp <- pol$albacore

catch <- inp$obsC[17:23]
timeC <- 2013:2019
index <- inp$obsI[c(20,22,23)]
timeI <- c(2016,2018,2019)

inp <- list()
inp$timeC <- timeC
inp$obsC <- catch
inp$timeI <- timeI
inp$obsI <- index

inp <- check.inp(inp)
dat <- inp2dat(inp)

dat <- dat[c("timeC","obsC","timeI1","obsI1")]

## save
write.csv(dat, file = "datlim.csv")


## really data limited data (catch + effort)
## ----------------------------------------------
inp <- pol$albacore

catch <- inp$obsC[17:23]
timeC <- 2013:2019
effort <- inp$obsI[17:23] / catch
timeE <- timeC

inp <- list()
inp$timeC <- timeC
inp$obsC <- catch
inp$timeE <- timeE
inp$obsE <- effort

inp <- check.inp(inp)
dat <- inp2dat(inp)

dat <- dat[c("timeC","obsC","timeE","obsE")]

## save
write.csv(dat, file = "datlim2.csv")
