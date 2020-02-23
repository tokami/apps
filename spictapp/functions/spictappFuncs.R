## GUI for spictapp
## Tobias K. Mildenberger
## January 2020

## dat <- inp2dat(pol$hake)

## write.csv(dat, file = "albacore.csv")
## write.table(dat, file = "lobster.txt", sep=";")
## write.table(dat, file = "hake.txt", sep=",")
## getwd()
## write.csv


## function that makes inp from dat
## ------------------------------------------------------------------
dat2inp <- function(dat){

    ## checks
    if(!any(colnames(dat) == "timeC")) stop("Your data is missing a column with the timing of the commercial catches labelled 'timeC'!")
    if(!any(colnames(dat) == "obsC")) stop("Your data is missing a column with the commercial catch observations labelled 'obsC'!")
    if(!any(colnames(dat) == "timeI1") && !any(colnames(dat) == "timeE")) stop("Your data is missing a column with the timing of the index or effort data labelled 'timeI1' or 'timeE'!")
    if(!any(colnames(dat) == "obsI1")  && !any(colnames(dat) == "obsE")) stop("Your data is missing a column with the index or effort observations labelled 'obsI1' or 'obsE'!")

    ncols <- ncol(dat)
    nsurv <- (ncols - 2) / 2
    n <- nrow(dat)


    ## spict input list
    inp <- list()
    inp$timeC <- dat$timeC
    inp$obsC <- dat$obsC

    if(any(colnames(dat) == "timeI1")){
        if(nsurv > 1){
            inp$timeI <- list()
            inp$obsI <- list()
            for(i in 1:nsurv){
                inp$timeI[[i]] <- dat[,which(colnames(dat) == paste0("timeI",i))]
                inp$obsI[[i]] <- dat[,which(colnames(dat) == paste0("obsI",i))]
            }
        }else{
            inp$timeI <- dat$timeI1
            inp$obsI <- dat$obsI1
        }
    }else{
        inp$timeE <- dat$timeE
        inp$obsE <- dat$obsE
    }

    inp <- check.inp(inp)  ## CHECK: not check here, because some arguments want to be set with GUI before check.inp?

    return(inp)
}


## function that makes dat from inp (for data download of example data)
## ------------------------------------------------------------------
inp2dat <- function(inp){

    ## check inp
    inp <- check.inp(inp)

    ## catches
    timeC <- inp$timeC
    obsC <- inp$obsC

    ## indices
    timeI <- list()
    obsI <- list()
    if(class(inp$timeI) == "list"){
        nsurv <- length(inp$timeI)
        for(i in 1:nsurv){
            timeI[[i]] <- inp$timeI[[i]]
            obsI[[i]] <- inp$obsI[[i]]
        }
    }else{
        nsurv <- 1
        timeI[[1]] <- inp$timeI
        obsI[[1]] <- inp$obsI
    }

    nc <- length(timeC)
    nis <- lapply(timeI, length)
    n <- max(c(nc,unlist(nis)))

    timeC <- c(timeC, rep(NA, n-nc))
    obsC <- c(obsC, rep(NA, n-nc))
    ret <- cbind(timeC,obsC)

    for(i in 1:nsurv){
        tmp <- c(timeI[[i]],rep(NA, n-nis[[i]]))
        ret <- cbind(ret, tmp)
        colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
        tmp <- c(obsI[[i]],rep(NA, n-nis[[i]]))
        ret <- cbind(ret, tmp)
        colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
    }

    ret <- as.data.frame(ret)

    return(ret)
}
