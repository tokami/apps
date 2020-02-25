## GUI for spictapp
## Tobias K. Mildenberger
## January 2020

## dat <- inp2dat(pol$hake)

## write.csv(dat, file = "albacore.csv")
## write.table(dat, file = "lobster.txt", sep=";")
## write.table(dat, file = "hake.txt", sep=",")
## getwd()
## write.csv

## function to check consistency and find matching column names
## ------------------------------------------------------------------
checkDat <- function(dat, colNames){

    colNamesRaw <- colnames(dat)

    timeCInd <- which(colNames$timeC == colNamesRaw)
    obsCInd <- which(colNames$obsC == colNamesRaw)

    ## catch matched?
    if(length(timeCInd) != 1) stop(paste0("Column ",colNames$timeC," for the times of the catch observations not found in the uploaded file."))
    if(length(obsCInd) != 1) stop(paste0("Column ",colNames$obsC," for the catch observations not found in the uploaded file."))

    timeIInd <- list()
    obsIInd <- list()
    if(length(colNames$timeI) > 0){
        for(i in 1:length(colNames$timeI)){
            timeIInd[[i]] <- which(colNames$timeI[[i]] == colNamesRaw)
            obsIInd[[i]] <- which(colNames$obsI[[i]] == colNamesRaw)
        }
    }

    timeEInd <- which(colNames$timeE == colNamesRaw)
    obsEInd <- which(colNames$obsE == colNamesRaw)

    ## index or effort matched?
    if(length(timeIInd) != 1 && length(timeEInd) != 1) stop(paste0("Neither a column for the times of the index nor effort found in the uploaded file."))
    if(length(obsIInd) != 1 && length(obsEInd) != 1) stop(paste0("Neither a column for the index nor effort observations found in the uploaded file."))

    ## scaling of uncertainty of observations
    stdevfacCInd <- which(colNames$stdevfacC == colNamesRaw)
    stdevfacEInd <- which(colNames$stdevfacE == colNamesRaw)
    stdevfacIInd <- list()
    if(length(colNames$stdevfacI) > 0 && colNames$stdevfacI != ""){
        for(i in 1:length(colNames$stdevfacI)){
            stdevfacIInd[[i]] <- which(colNames$stdevfacI[[i]] == colNamesRaw)
        }
    }


    timeC <- dat[,timeCInd]
    obsC <- dat[,obsCInd]

    nsurv <- length(timeIInd)
    if(nsurv > 0){
        timeI <- list()
        obsI <- list()
        for(i in 1:nsurv){
            timeI[[i]] <- dat[,timeIInd[[i]]]
            obsI[[i]] <- dat[,obsIInd[[i]]]
        }
    }else{
        timeI <- NA
        obsI <- NA
    }

    if(length(timeEInd) > 0){
        timeE <- dat[,timeEInd]
        obsE <- dat[,obsEInd]
    }else{
        timeE <- NA
        obsE <- NA
    }

    if(length(stdevfacCInd) > 0){
        stdevfacC <- dat[,stdevfacCInd]
    }else{
        stdevfacC <- NA
    }

    if(length(stdevfacEInd) > 0){
        stdevfacE <- dat[,stdevfacEInd]
    }else{
        stdevfacE <- NA
    }

    if(length(stdevfacIInd) > 0){
        stdevfacI <- list()
        for(i in 1:length(stdevfacIInd)){
            stdevfacI[[i]] <- dat[,stdevfacIInd[[i]]]
        }
    }else stdevfacI <- NA


    ## combine to dataframe
    nc <- length(timeC)
    nis <- lapply(timeI, length)
    ne <- length(timeE)
    nsc <- length(stdevfacC)
    nsis <- lapply(stdevfacI, length)
    nse <- length(stdevfacE)
    nmax <- max(c(nc,unlist(nis),ne))

    ## checks
    if(length(stdevfacCInd) && nsc > nc){
        warning("stdevfacC is longer than the catch observations. Cutting the variable to the same length as the catch observations.")
        stdevfacC <- stdevfacC[1:nc]
    }
    if(length(stdevfacEInd) && nse > ne){
        warning("stdevfacE is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
        stdevfacE <- stdevfacE[1:ne]
    }
    if(length(stdevfacIInd) && any(sapply(1:nis,function(x) nsis[[x]] > nis[[x]]))){
        warning("stdevfacI is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
        for(i in 1:length(stdevfacI)){
            stdevfacI[[i]] <- stdevfacI[[i]][1:nsis[[i]]]
        }

    }

    timeC <- c(timeC, rep(NA, nmax-nc))
    obsC <- c(obsC, rep(NA, nmax-nc))
    ret <- cbind(timeC,obsC)

    if(nsurv > 0){
        for(i in 1:nsurv){
            tmp <- c(timeI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
            tmp <- c(obsI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
        }
    }

    if(length(timeEInd) > 0){
        timeE <- c(timeE, rep(NA, nmax-ne))
        obsE <- c(obsE, rep(NA, nmax-ne))
        ret <- cbind(ret, timeE, obsE)
    }

    if(length(stdevfacCInd) > 0){
        stdevfacC <- c(stdevfacC, rep(NA, nmax-nc))
        ret <- cbind(ret, stdevfacC)
    }
    if(length(stdevfacIInd) > 0){
        for(i in 1:length(stdevfacIInd)){
            tmp <- c(stdevfacI[[i]], rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("stdevfacI",i))
        }
    }
    if(length(stdevfacEInd) > 0){
        stdevfacE <- c(stdevfacE, rep(NA, nmax-ne))
        ret <- cbind(ret, stdevfacE)
    }


    ret <- as.data.frame(ret)

    return(ret)
}


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
    nmax <- nrow(dat)

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
    }

    if(any(colnames(dat) == "timeE")){
        inp$timeE <- dat$timeE
        inp$obsE <- dat$obsE
    }

    ## uncertainty scaling
    if(any(colnames(dat) == "stdevfacC")){
        inp$stdevfacC <- dat$stdevfacC
    }
    if(any(colnames(dat) == "stdevfacI1")){
        if(length(dat$stdevfacI1) > 1){
            inp$stdevfacI <- list()
            for(i in 1:length(dat$stdevfacI1)){
                inp$stdevfacI[[i]] <- dat[,which(colnames(dat) == paste0("stdevfacI",i))]
            }
        }else{
            inp$stdevfacI <- dat$stdevfacI1
        }
    }
    if(any(colnames(dat) == "stdevfacE")){
        inp$stdevfacE <- dat$stdevfacE
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
    if(length(inp$timeI) > 0){
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
    }else{
        nsurv <- 0
    }

    ## effort
    timeE <- inp$timeE
    obsE <- inp$obsE

    ## uncertainty scaling
    stdevfacC <- inp$stdevfacC
    stdevfacI <- inp$stdevfacI
    stdevfacE <- inp$stdevfacE

    nc <- length(timeC)
    nis <- lapply(timeI, length)
    ne <- length(timeE)
    nmax <- max(c(nc,unlist(nis)))

    timeC <- c(timeC, rep(NA, nmax-nc))
    obsC <- c(obsC, rep(NA, nmax-nc))
    stdevfacC <- c(stdevfacC, rep(NA, nmax-nc))
    ret <- cbind(timeC,obsC,stdevfacC)

    if(nsurv > 0){
        for(i in 1:nsurv){
            tmp <- c(timeI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
            tmp <- c(obsI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
            tmp <- c(stdevfacI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("stdevfacI",i))
        }
    }

    if(length(timeE) > 0){
        timeE <- c(timeE, rep(NA, nmax-ne))
        obsE <- c(obsE, rep(NA, nmax-ne))
        stdevfacE <- c(stdevfacE, rep(NA, nmax-ne))
        ret <- cbind(ret, timeE, obsE, stdevfacE)
    }

    ret <- as.data.frame(ret)

    return(ret)
}
