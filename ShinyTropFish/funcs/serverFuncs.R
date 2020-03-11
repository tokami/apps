checkDat <- function(dat, colNames){

    colNamesRaw <- colnames(dat)

    lengthInd <- which(colNames$length == colNamesRaw)
    dateInd <- which(colNames$dates == colNamesRaw)
    freqInd <- which(colNames$frequency == colNamesRaw)

    ## cols matched?
    if(length(lengthInd) == 0) stop(paste0("Column ",colNames$length," for the length measurements not found in the uploaded file."))
    if(length(dateInd) == 0) stop(paste0("Column ",colNames$dates," for the dates not found in the uploaded file."))

    ## matched to too many columns?
    if(length(lengthInd) > 1) stop(paste0("Several columns match to the times of the length measurements: ",paste0(colNamesRaw[lengthInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(dateInd) > 1) stop(paste0("Several columns match to the times of the dates: ",paste0(colNamesRaw[dateInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))
    if(length(freqInd) > 1) stop(paste0("Several columns match to the times of the frequency: ",paste0(colNamesRaw[freqInd], collapse = ", "),". The column names in the uploaded file have to be unique! Please change them."))

    lengths <- as.numeric(as.character(dat[,lengthInd]))
    dates <- as.character(dat[,dateInd])

    if(length(freqInd) > 0){
        freqs <- as.numeric(as.character(dat[,freqInd]))
    }else{
        freqs <- NA
    }

    ## combine to dataframe
    nl <- length(lengths)
    nd <- length(dates)
    nf <- length(freqs)
    nmax <- max(c(nl,nd,nf))

    lengths <- c(lengths, rep(NA, nmax-nl))
    dates <- c(dates, rep(NA, nmax-nd))
    ret <- cbind(lengths, dates)

    if(length(freqInd) > 0){
        freqs <- c(freqs, rep(NA, nmax-nf))
        ret <- cbind(ret, freqs)
    }

    ret <- as.data.frame(ret, stringsAsFactors = FALSE)
    colnames(ret) <- c("length","dates","frequency")

    return(ret)
}


## function that makes lfq from dat
## ------------------------------------------------------------------
dat2lfq <- function(dat, dateFormat, aggDates){

    ## checks
    if(!any(colnames(dat) == "length")) stop("Your data is missing a column with the length measurements labelled 'length'!")
    if(!any(colnames(dat) == "dates")) stop("Your data is missing a column with the dates labelled 'dates'!")

    colna <- colnames(dat)
    nmax <- nrow(dat)

    ## make sure that length and freq are numeric
    dat$length <- as.numeric(as.character(dat$length))

    ## lfq data set
    binSize <- diff(dat$length)[1]  # ifelse(min(dat$length) %% 1 > 0, 0.5, 1)
    ## create lfq
    dat$dates <- as.Date(as.character(dat$dates), dateFormat)
    ## create frequencies if don't exist
    if(!any(colnames(dat)=="frequency")){
        dat$frequency <- 1
    }else{
        dat$frequency <- as.numeric(as.character(dat$frequency))
    }

    ## arrange LFQ data
    lfq <- lfqCreate(data = dat,
                     Lname = "length",
                     Dname = "dates",
                     Fname = "frequency",
                     plot = FALSE,
                     bin_size = binSize,
                     aggregate_dates = aggDates)

    return(lfq)
}


## function that makes dat from lfq (for data download of example data)
## ------------------------------------------------------------------
lfq2dat <- function(lfq){

    ## lengths
    lengths = lfq$midLengths

    ## dates
    dates = lfq$dates

    ## catchmat
    catch = as.data.frame(lfq$catch,stringsAsFactors = FALSE)

    dat = cbind(lengths,catch)
    colnames(dat) = c("length",dates)

    ret <- reshape2::melt(dat, id.vars = "length")
    colnames(ret) <- c("length","dates","frequency")

    ret <- as.data.frame(ret, stringsAsFactors = FALSE)

    return(ret)
}


## function that makes dat from lfqdat (for case that LFQ table provided)
## ------------------------------------------------------------------
lfqdat2dat <- function(lfqdat, colNames, dateFormat){

    colNamesRaw <- colnames(lfqdat)

    lengthInd <- which(colNames$midLengths == colNamesRaw)

    ind1 <- colNames$freqColsLFQ1
    ind2 <- colNames$freqColsLFQ2

    ## lengths
    lengths = lfqdat[,lengthInd]

    ## catchmat
    catchmat <- lfqdat[,ind1:ind2]
    catch = as.data.frame(catchmat,stringsAsFactors = FALSE)

    dat = cbind(lengths,catch)
    colnames(dat) = c("length",colnames(catch))

    ret <- reshape2::melt(dat, id.vars = "length")
    colnames(ret) <- c("length","dates","frequency")

    ## dates
    dates <- unlist(lapply(strsplit(as.character(ret$dates),"X"),"[[",2))
    ## add days if missing
    tmp <- list(strsplit(dates,"-")[[1]],strsplit(dates,"_")[[1]],
                strsplit(dates," ")[[1]],strsplit(dates,"[.]")[[1]])
    if(!any(sapply(tmp,length) > 2)){
        dates <- paste0(dates,"-15")
        dateFormat <- paste0(dateFormat,"-%d")
    }
    ret$dates <- as.Date(as.character(dates), dateFormat)

    ret <- as.data.frame(ret, stringsAsFactors = FALSE)

    return(ret)
}
