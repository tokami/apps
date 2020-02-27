## Addtional server functions for spictapp
## Tobias K. Mildenberger
## March 2020


if(FALSE){

    dat <-  read.csv("data/albacore_2indices.csv")

    dat

    colNames <- list("timeC"="timeC","obsC"="obsC",
                     "timeI"=c("timeI1","timeI2"),
                     "obsI"=c("obsI1","obsI2")
                     )

    colnames(dat)

    dat2 <- checkDat(dat, colNames)

    dat2

    inp <- dat2inp(dat2)

    inp <- check.inp(inp)

    plotspict.data(inp)

    plotspict.priors.inp(inp)
}

## function to check consistency and find matching column names
## ------------------------------------------------------------------
checkDat <- function(dat, colNames){

    colNamesRaw <- colnames(dat)

    timeCInd <- which(colNames$timeC == colNamesRaw)
    obsCInd <- which(colNames$obsC == colNamesRaw)

    ## catch matched?
    if(length(timeCInd) == 0) stop(paste0("Column ",colNames$timeC," for the times of the catch observations not found in the uploaded file."))
    if(length(obsCInd) == 0) stop(paste0("Column ",colNames$obsC," for the catch observations not found in the uploaded file."))

    ## matched to too many columns?
    if(length(timeCInd) > 1) stop(paste0("Several columns match to the times of the catch observations: ",paste0(colNamesRaw[timeCInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(obsCInd) > 1) stop(paste0("Several columns match to the catch observations: ",paste0(colNamesRaw[obsCInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))

    timeIInd <- list()
    obsIInd <- list()
    if(length(colNames$timeI) > 0){
        for(i in 1:length(colNames$timeI)){
            timeIInd[[i]] <- which(colNames$timeI[[i]] == colNamesRaw)
        }
    }
    if(length(colNames$obsI) > 0){
        for(i in 1:length(colNames$obsI)){
            obsIInd[[i]] <- which(colNames$obsI[[i]] == colNamesRaw)
        }
    }

    timeEInd <- which(colNames$timeE == colNamesRaw)
    obsEInd <- which(colNames$obsE == colNamesRaw)

    ## index or effort matched?
    if(length(timeIInd) < 1 && length(timeEInd) < 1) stop(paste0("Neither a column for the times of the index nor effort found in the uploaded file."))
    if(length(obsIInd) < 1 && length(obsEInd) < 1) stop(paste0("Neither a column for the index nor effort observations found in the uploaded file."))

    ## matched to too many columns?
    if(length(timeEInd) > 1) stop(paste0("Several columns match to the times of the effort observations: ",paste0(colNamesRaw[timeEInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(obsEInd) > 1) stop(paste0("Several columns match to the catch observations: ",paste0(colNamesRaw[obsEInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))

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

    if(length(timeIInd) > 0){
        timeI <- list()
        for(i in 1:length(timeIInd)){
            timeI[[i]] <- dat[,timeIInd[[i]]]
        }
    }else{
        timeI <- NA
    }
    if(length(obsIInd) > 0){
        obsI <- list()
        for(i in 1:length(obsIInd)){
            obsI[[i]] <- dat[,obsIInd[[i]]]
        }
    }else{
        obsI <- NA
    }

    if(length(timeEInd) > 0){
        timeE <- dat[,timeEInd]
    }else{
        timeE <- NA
    }
    if(length(obsEInd) > 0){
        obsE <- dat[,obsEInd]
    }else{
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
    ## if(length(stdevfacCInd) > 0 && nsc > nc){
    ##     warning("stdevfacC is longer than the catch observations. Cutting the variable to the same length as the catch observations.")
    ##     stdevfacC <- stdevfacC[1:nc]
    ## }
    ## if(length(stdevfacEInd) > 0 && nse > ne){
    ##     warning("stdevfacE is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
    ##     stdevfacE <- stdevfacE[1:ne]
    ## }
    ## if(length(stdevfacIInd) > 0){
    ##     if(any(sapply(1:nsis,function(x) nsis[[x]] > nis[[x]]))){
    ##         warning("stdevfacI is longer than the effort observations. Cutting the variable to the same length as the effort observations.")
    ##         for(i in 1:length(stdevfacI)){
    ##             stdevfacI[[i]] <- stdevfacI[[i]][1:nsis[[i]]]
    ##         }
    ##     }
    ## }

    timeC <- c(timeC, rep(NA, nmax-nc))
    obsC <- c(obsC, rep(NA, nmax-nc))
    ret <- cbind(timeC,obsC)

    if(length(timeIInd) > 0){
        for(i in 1:length(timeIInd)){
            tmp <- c(timeI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("timeI",i))
        }
    }
    if(length(obsIInd) > 0){
        for(i in 1:length(obsIInd)){
            tmp <- c(obsI[[i]],rep(NA, nmax-nis[[i]]))
            ret <- cbind(ret, tmp)
            colnames(ret) <- c(colnames(ret)[-ncol(ret)], paste0("obsI",i))
        }
    }

    if(length(timeEInd) > 0){
        timeE <- c(timeE, rep(NA, nmax-ne))
        ret <- cbind(ret, timeE)
    }
    if(length(obsEInd) > 0){
        obsE <- c(obsE, rep(NA, nmax-ne))
        ret <- cbind(ret, obsE)
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

    colna <- colnames(dat)
    nmax <- nrow(dat)

    ## spict input list
    inp <- list()
    inp$timeC <- dat$timeC
    inp$obsC <- dat$obsC

    if(any(colnames(dat) == "timeI1")){
        nsurv <- length(which(sapply(strsplit(colna,"timeI"),function(x) length(x) > 1)))
        if(nsurv > 1){
            inp$timeI <- list()
            for(i in 1:nsurv){
                inp$timeI[[i]] <- as.numeric(na.omit(dat[,which(colnames(dat) == paste0("timeI",i))]))
            }
        }else{
            inp$timeI <- dat$timeI1
        }
    }

    if(any(colnames(dat) == "obsI1")){
        nsurv <- length(which(sapply(strsplit(colna,"obsI"),function(x) length(x) > 1)))
        if(nsurv > 1){
            inp$obsI <- list()
            for(i in 1:nsurv){
                inp$obsI[[i]] <- as.numeric(na.omit(dat[,which(colnames(dat) == paste0("obsI",i))]))
            }
        }else{
            inp$obsI <- dat$obsI1
        }
    }

    if(any(colnames(dat) == "timeE")){
        inp$timeE <- dat$timeE
    }
    if(any(colnames(dat) == "obsE")){
        inp$obsE <- dat$obsE
    }

    ## uncertainty scaling
    if(any(colnames(dat) == "stdevfacC")){
        inp$stdevfacC <- dat$stdevfacC
    }
    if(any(colnames(dat) == "stdevfacI1")){
        nsurv <- length(which(sapply(strsplit(colna,"stdevfacI"),function(x) length(x) > 1)))
        if(nsurv > 1){
            inp$stdevfacI <- list()
            for(i in 1:nsurv){
                inp$stdevfacI[[i]] <- as.numeric(na.omit(dat[,which(colnames(dat) == paste0("stdevfacI",i))]))
            }
        }else{
            inp$stdevfacI <- dat$stdevfacI1
        }
    }
    if(any(colnames(dat) == "stdevfacE")){
        inp$stdevfacE <- dat$stdevfacE
    }

##    inpORI <- inp

    inp <- try(check.inp(inp))  ## CHECK: not check here, because some arguments want to be set with GUI before check.inp?
    if(inherits(inp,"try-error")) browser() ## REMOVE:

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


## function that makes dat from inp (for data download of example data)
## ------------------------------------------------------------------
plotspict.priors.inp <- function(x, do.plot=NULL, stamp=get.version(), automfrow=TRUE){
    ## if check.inp has not been run yet
    if(!inherits(x, "spictcls")){
        x <- try(check.inp(x, verbose = FALSE, mancheck = FALSE), silent=TRUE)
        if(inherits(x, "try-error")){
            stop("Provided object 'x' needs to be an input list or a fitted spict object!")
        }
    }
    ## if fitted object
    if(inherits(x, "spictcls") && any(names(x) == "opt")){
        inp <- x$inp
        rep <- x
        isRep <- TRUE
    }else{
        inp <- check.inp(x, verbose = FALSE, mancheck = FALSE)
        rep <- NULL
        isRep <- FALSE
    }
    useflags <- inp$priorsuseflags
    inds <- which(useflags == 1)
    ninds <- length(inds)
    if(!is.null(do.plot)) automfrow <- FALSE
    if(automfrow) {
        nopriors <- get.no.active.priors(inp)
        op <- par(mfrow=n2mfrow(nopriors))
        on.exit(par(op))
    }
    counter <- 0
    nused <- sum(useflags)
    if (ninds > 0){
        for (i in 1:ninds){
            j <- inds[i]
            priorvec <- inp$priors[[j]]
            nm <- names(inp$priors)[j]
            isGamma <- FALSE
            nmpl <- sub('log', '', nm)
            nmpl <- sub('gamma','',nmpl)
            if(isRep){
                par <- get.par(nm, rep, exp=FALSE)
            }
            if (length(grep('gamma', nm)) == 1){
                isGamma <- TRUE
                if(nm=="logngamma" && isRep) par <- get.par("logn",rep,exp=FALSE)
            }

            repriors <- c('logB', 'logF', 'logBBmsy', 'logFFmsy')
            if (nm %in% repriors){
                if(isRep) par <- par[priorvec[5], , drop=FALSE]
                nmpl <- paste0(nmpl, fd(priorvec[4]))
                if (nm == 'logB'){
                    nmpl <- add.catchunit(nmpl, inp$catchunit)
                }
            }
            if(isRep){
                nrowPar <- nrow(par)
            }else
                nrowPar <- 1
            for (rr in 1:nrowPar){
                if (nrowPar > 1){
                    nmpl <- paste0(nmpl, rr)
                }
                prvec <- priorvec
                if(is.list(priorvec)) prvec <- priorvec[[rr]]
                if(isRep){
                    mu <- ifelse(is.na(par[rr, 4]), prvec[1], par[rr, 2])
                    sd <- ifelse(is.na(par[rr, 4]), prvec[2], par[rr, 4])
                }else{
                    mu <- prvec[1]
                    sd <- prvec[2]
                }
                if(isRep){
                    if(isGamma && is.na(par[rr, 4])){
                        xmin <- 1e-12
                        xmax <- qgamma(0.99,shape=mu,rate=sd)
                    } else {
                        xmin <- mu - 3*sd
                        xmax <- mu + 3*sd
                    }
                }else{
                    xmin <- mu - 3*sd
                    xmax <- mu + 3*sd
                }
                xpr <- xpo <- seq(xmin, xmax, length=200)
                if(!isGamma) {
                    priorvals <- dnorm(xpr, prvec[1], prvec[2])
                }  else  {
                    priorvals <- dgamma(xpr, prvec[1], prvec[2])
                }

                if(isRep){
                    if (is.na(par[rr, 4])){
                        posteriorvals <- NULL
                    } else {
                        if(isGamma) xpo <- seq(mu - 3*sd, mu + 3*sd, length=200)
                        posteriorvals <- dnorm(xpo, par[rr, 2], par[rr, 4])
                    }
                }else posteriorvals <- NULL
                plot(exp(xpr), priorvals, typ='l', xlab=nmpl, ylab='Density', log='x',
                     lwd=1.5, ylim=c(0, max(priorvals, posteriorvals)*1.3))
                if(isRep){
                    if (is.na(par[rr, 4])){
                        if (!is.na(par[rr, 2])){
                            abline(v=exp(par[rr, 2]), lty=2, col=3, lwd=1.5)
                        }
                        legend('topright', legend=c('Prior', 'Post. Mean'), lty=1:2,
                               col=c(1, 3), lwd=1.5)
                    } else {
                        lines(exp(xpo), posteriorvals, col=3, lwd=1.5)
                        legend('topright', legend=c('Prior', 'Post.'), lty=1,
                               col=c(1, 3), lwd=1.5)
                    }
                }
                box(lwd=1.5)
                if (isRep && rep$opt$convergence != 0){
                    warning.stamp()
                }
                counter <- counter + 1
                if(!is.null(do.plot) && counter >= do.plot) {
                    txt.stamp(stamp)
                    return()
                }
            }
        }
        txt.stamp(stamp)
    }
}
