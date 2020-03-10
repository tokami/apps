LCCC_shiny <- function(x,
                       catch_columns = NA,
                       calc_ogive = FALSE,
                       reg_int = NULL,
                       returnRegInt = FALSE,
                       gotcha = FALSE
                       ){
    res <- x
    par <- res$par

    ## Length converted catch curve
    Linf <- res$par$Linf
    K <- res$par$K
    ta <- res$par$ta    
    t0 <- ifelse("t0" %in% names(res$par), res$par$t0, 0)
    C <- ifelse("C" %in% names(res$par), res$par$C, 0)
    ts <- ifelse("ts" %in% names(res$par), res$par$ts, 0)
    

    if(!gotcha){
        
        catch_columns <- as.numeric(catch_columns)
        
        classes <- as.character(res$midLengths)
        ## create column without plus group (sign) if present
        classes.num <- do.call(rbind,strsplit(classes, split="\\+"))
        classes.num <- as.numeric(classes.num[,1])
        
        constant_dt <- FALSE
        
        ## non cumulative catch curve
        if(is.na(catch_columns[1]) & constant_dt == FALSE) catch <- res$catch
        if(!is.na(catch_columns[1])){
            catchmat <- res$catch[,(catch_columns)]
            if(length(catch_columns) > 1){
                catch <- rowSums(catchmat, na.rm = TRUE)
            }else catch <- catchmat
        }
        
        ## calculate size class interval
        midLengths <- classes.num
        interval <- midLengths[2] - midLengths[1]
        
        ## L and t of lower length classes
        lowerLengths <- midLengths - (interval / 2)
        t_L1 <- VBGF(pars = list(Linf = Linf, K = K, ta = ta), L = lowerLengths)
        ## t0 - (1/K) * log(1 - (lowerLengths / Linf))
        
        ## delta t
        dt <- rep(NA,length(midLengths))
        for(x1 in 1:(length(dt)-1)){
            dt[x1] <- t_L1[x1+1] - t_L1[x1]
        }
        ## x varaible
        ## ln (Linf - L)
        ln_Linf_L <- log(Linf - lowerLengths)
        ## t of midlengths
        t_midL <- VBGF(pars = list(Linf = Linf, K = K, ta = ta), L = midLengths)
        ## t0 - (1/K) * log(1 - (midLengths / Linf))

        lnC <- log(catch)
        lnC_dt <- log(catch / dt)
        lnC_dt[which(lnC_dt == -Inf)] <- NA   ### OR zero???
        
        xvar = t_midL
        yvar = lnC_dt
        xname = "t_midL"
        yname = "lnC_dt"
        xlabel = "Relative age [yrs]"
        ylabel = "ln(C/dt)"        
        
    }else{
        ## GOTCHA sum method
        ## t_midL <- sort(x$relages)        
        ## catch <- x$catch[order(x$relages)]
        ## dt <- c(diff(t_midL),NA)
        ##        dt <- rep(1, length(catch))
        
        ## GOTCHA Marc
        agg <- aggregate(n ~ cohort, x$df, sum, na.rm = TRUE)
        tmp <- aggregate(bday ~ cohort, x$df, mean)
        agg$rel.age <- ceiling(max(tmp$bday)) - tmp$bday
        agg <- agg[order(agg$rel.age),]

        t_midL <- agg$rel.age
        catch <- agg$n

        lnC <- log(catch)

        xvar = t_midL
        yvar = lnC
        xname = "t_midL"
        yname = "lnC"
        xlabel = "Relative age [yrs]"
        ylabel = "ln(C)"        
    }

    
    
    ## remove all NAs and Infs
    temp <- cbind(xvar,yvar)
    temp <- as.matrix(na.exclude(temp))
    temp <- temp[(!(temp[,1] == Inf | temp[,1] == -Inf)),]
    temp <- temp[(!(temp[,2] == Inf | temp[,2] == -Inf)),]
    xvar <- temp[,1]
    yvar <- temp[,2]
    
    
    minY <- min(yvar,na.rm=TRUE)
    maxY <- max(yvar,na.rm=TRUE) + 1
    xlims <- c(0, max(xvar,na.rm=TRUE))    


    ## USE THAT FOR FIRST SELECTION OF slider values
    if(returnRegInt){
        yvar2 <- as.numeric(yvar)
        xvar2 <- xvar[which(yvar2 > 0.5)]
        cutter <- c(which(yvar2 == max(as.numeric(yvar2),na.rm=TRUE))+1,
                    which(xvar2 == max(xvar2,na.rm=TRUE)))
        return(list(regInt = as.numeric(cutter),
                    maxRegInt = as.numeric(c(which(xvar == min(xvar, na.rm = TRUE)),
                                             which(xvar == max(xvar, na.rm = TRUE))))))
    }    
    
    cutter <- reg_int
    cutterList <- list(cutter)
    
    ## calculations + model
    df.CC <- as.data.frame(cbind(xvar,yvar))
    df.CC.cut <- df.CC[cutter[1]:cutter[2],]
    lm1 <- lm(yvar ~ xvar, data = df.CC.cut)
    sum_lm1 <- summary(lm1)
    r_lm1 <- sum_lm1$r.squared
    intercept_lm1 <- sum_lm1$coefficients[1]
    slope_lm1 <- sum_lm1$coefficients[2]
    se_slope_lm1 <- sum_lm1$coefficients[4]
    
    ## fit of regression line
    lm1.fit <- sum_lm1$r.squared
    Z_lm1 <- abs(slope_lm1)
    SE_Z_lm1 <- abs(se_slope_lm1)
    confi <-  abs(se_slope_lm1) * qt(0.975,sum_lm1$df[2])
    conf_Z_lm1 <- Z_lm1 + c(-confi,confi)
    
    
    ## save results to lists
    lm1List <- lm1
    Z_lm1List <- Z_lm1
    SE_Z_lm1List <- SE_Z_lm1
    conf_Z_lm1List <- conf_Z_lm1
    intercept_lm1List <- intercept_lm1
    
    
    ##save all in list

    ret <- c(res,list(
                     xvar = xvar,
                     yvar = yvar,
                     reg_int = unlist(cutterList),
                     linear_mod = lm1List,
                     Z = unlist(Z_lm1List),
                     Z_se = unlist(SE_Z_lm1List),
                     confidenceInt = unlist(conf_Z_lm1List)))
    
    names(ret)[names(ret) == "xvar"] <- xname
    names(ret)[names(ret) == "yvar"] <- yname
    
    class(ret) <- "catchCurve"
    
    
                                        # Calculate selection ogive from catch curve and add to ret
    if(calc_ogive){

        ## Assumption that Z of smallest selected individuals is most appropriate
        mini <- min(unlist(cutterList))
        temp <- lapply(cutterList, function(x) grep(mini,x))
        ind <- sapply(temp, function(x) length(x) > 0)
        cutter <- unlist(cutterList[ind])

        ## only use part of catch and t which is not fully exploited by the gear
        t_ogive <- xvar[1:(cutter[1]-1)]
        dt_ogive <- dt[1:(cutter[1]-1)]
        catch_ogive <- catch[1:(cutter[1]-1)]

        ## calculate observed selection ogive
        Sobs <- catch_ogive/(dt_ogive * exp(unlist(intercept_lm1List[ind]) -
                                            unlist(Z_lm1List[ind]) * t_ogive))
        
        ## dependent vairable in following regression analysis
        ln_1_S_1 <- log((1/Sobs) - 1)
        
        ## get rid of Inf
        ln_1_S_1[which(ln_1_S_1 == Inf)] <- NA
        t_ogive[which(t_ogive == Inf)] <- NA
        
        ##regression analysis to caluclate T1 and T2
        mod_ogive <- lm(ln_1_S_1 ~ t_ogive, na.action = na.omit)
        sum_lm_ogive <- summary(mod_ogive)
        T1 <- sum_lm_ogive$coefficients[1]
        T2 <- abs(sum_lm_ogive$coefficients[2])
        
        ## calculate estimated selection ogive
        Sest <- 1/(1+exp(T1 - T2*xvar))
        
        ## selection parameters
        t50 <- T1/T2
        t75 <- (T1 + log(3))/T2
        t95 <-  (T1 - log((1 / 0.95) - 1)) / T2
        if((!is.null(res$Linf) & !is.null(res$K)) | ("par" %in% names(res) &&
                                                     (!is.null(res$par$Linf) & !is.null(res$par$K)))){
            if(is.null(res$t0)) t0 = 0
            L50 <- Linf*(1-exp(-K*(t50-t0)))
            L75 <- Linf*(1-exp(-K*(t75-t0)))
            L95 <- Linf*(1-exp(-K*(t95-t0)))
        }
        
        ret2 <- c(ret,list(
                          intercept = intercept_lm1,
                          linear_mod_sel = mod_ogive,
                          Sobs = Sobs,
                          ln_1_S_1 = ln_1_S_1,
                          Sest = Sest,
                          t50 = t50,
                          t75 = t75,
                          t95 = t95))
        if(exists("L50")) ret2$L50 = L50
        if(exists("L75")) ret2$L75 = L75
        if(exists("L95")) ret2$L95 = L95
        if(exists("L50")) names(ret2)[which(ret2 %in% L50)] <- "L50"
        if(exists("L75")) names(ret2)[which(ret2 %in% L75)] <- "L75"
        if(exists("L95")) names(ret2)[which(ret2 %in% L95)] <- "L95"

        par$Z <- unlist(Z_lm1List)
        ret2$par <- par
        
        class(ret2) <- "catchCurve"
        return(ret2)
    }else{
        par$Z <- unlist(Z_lm1List)
        ret$par <- par        
        return(ret)
    }
}


## plotting function
plotLCCC <- function(x, xaxis = 'age', plot_selec = FALSE,
                     col=c('blue',"darkgreen","orange","darkred"),
                     cex = 1.5, xlim = NULL, ylim = NULL,
                     xlab = "default", ylab = "default", ...){
    pes <- x

    ## growth parameters
    if("midLengths" %in% names(pes) | xaxis == "length"){
        if("par" %in% names(pes)){
            Linf <- pes$par$Linf
            K <- pes$par$K
            t0 <- ifelse("t0" %in% names(pes$par), pes$par$t0, 0)
            C <- ifelse("C" %in% names(pes$par), pes$par$C, 0)
            ts <- ifelse("ts" %in% names(pes$par), pes$par$ts, 0)
        }else{
            Linf <- pes$Linf
            K <- pes$K
            t0 <- ifelse("t0" %in% names(pes), pes$t0, 0)
            C <- ifelse("C" %in% names(pes), pes$C, 0)
            ts <- ifelse("ts" %in% names(pes), pes$ts, 0)
        }

        if((is.null(Linf) | is.null(K))) stop(noquote(
                                             "You need to assign values to Linf and K for the catch curve based on length-frequency data!"))
    }


    if(xaxis == 'age'){
        xlabel <- "Age [yrs]"
        if("t_midL" %in% names(pes)){
            xplot <- pes$t_midL
            xlabel <- "Relative age [yrs]"
            xplotAGE <- pes$t_midL
        }else if("tplusdt_2" %in% names(pes)){
            xplot <- pes$tplusdt_2
        }else if("ln_Linf_L" %in% names(pes)){
            xplot <- pes$ln_Linf_L
            xlabel <- "ln(Linf - L)"
        }else if("classes.num" %in% names(pes)) xplot <- pes$classes.num
    }
    if(xaxis == 'length'){
        xplot <- pes$midLengths
        xlabel <- "Length [cm]"
        if("t_midL" %in% names(pes)){
            xplotAGE <- pes$t_midL
        }
    }

    if("lnC_dt" %in% names(pes)){
        yplot <- pes$lnC_dt
        ylabel <- "ln(C / dt)"
    }else if("lnC" %in% names(pes)){
        yplot <- pes$lnC
        ylabel <- "ln(C)"
    }else if("ln_C" %in% names(pes)){
        yplot <- pes$ln_C
        ylabel <- "ln C(L,Linf)"
    }
    if("ln_C" %in% names(pes) & "tplusdt_2" %in% names(pes)) ylabel <- "ln C(t, inf)"


    lm1List <- pes$linear_mod
    Z_lm1List <- pes$par$Z
    SE_Z_lm1List <- pes$Z_se
    reg_intList <- pes$reg_int
    ## Assumption that Z of smallest selected individuals is most appropriate
    mini <- min(unlist(reg_intList))
    temp <- lapply(reg_intList, function(x) grep(mini,x))
    ind <- sapply(temp, function(x) length(x) > 0)
    cutter <- unlist(reg_intList[ind])


    ##for final plot
    minyplot <- ifelse(min(yplot,na.rm=TRUE) < 0, min(yplot,na.rm=TRUE),0)
    maxyplot <- max(yplot,na.rm=TRUE) + 1

    if(is.null(xlim)){
        xlims <- c(min(xplot[which(yplot > 0)], na.rm = TRUE)-0.5,
                   max(xplot[which(yplot > 0)], na.rm = TRUE)+0.5)
    }else xlims <- xlim

    if(class(Z_lm1List) == "list"){
        reg_num <- length(Z_lm1List)
    }else{
        reg_num <- 1
    }

    if(is.null(ylim)){
        ylims <- c(minyplot, maxyplot)
    }else ylims <- ylim

    if (dev.cur()==1){ ## If plot is not open
        opar <- par(mfrow = c(1,1),
                    mar = c(7, 5, 4, 5) + 0.3)
        on.exit(par(opar))
    }
    if (dev.cur()==2){ ## If plot is open, check if it is a 1x1 plot
        if (all(par()$mfrow == c(1, 1))){
            opar <- par(mfrow = c(1,1),
                        mar = c(7, 5, 4, 5) + 0.3)
            on.exit(par(opar))
        }
    }


    ## use user defined labels if given
    if(xlab != "default") xlabel = xlab
    if(ylab != "default") ylabel = ylab

    ##final plot
    plot(x = xplot, y = yplot, ylim = ylims,
         xlab = xlabel, ylab = ylabel, xlim = xlims,
         cex = cex)
    par(new=T)

    for(I in 1:reg_num){
        if(reg_num > 1){
            lm1 <- lm1List[[I]]
            reg_int <- reg_intList[[I]]
            Z_lm1 <- Z_lm1List[[I]]
            SE_Z_lm1 <- SE_Z_lm1List[[I]]
        }else{
            if(class(lm1List)=="list"){
                lm1 <- lm1List[[I]]
            }else{
                lm1 <- lm1List
            }
            if(class(reg_intList)=="list"){
                reg_int <- reg_intList[[I]]
            }else{
                reg_int <- reg_intList
            }
            if(class(Z_lm1List)=="list"){
                Z_lm1 <- Z_lm1List[[I]]
            }else{
                Z_lm1 <- Z_lm1List
            }
            if(class(SE_Z_lm1List)=="list"){
                SE_Z_lm1 <- SE_Z_lm1List[[I]]
            }else{
                SE_Z_lm1 <- SE_Z_lm1List
            }
        }

        points(x = xplot[reg_int[1]:reg_int[2]], y = yplot[reg_int[1]:reg_int[2]],
               pch = 19, col = col[I], cex = cex)
        lines(xplot[reg_int[1]:reg_int[2]],fitted(lm1), col=col[I], lwd=1.7)
        text(x = xplot,##+0.02*max(xplot,na.rm=TRUE),
             y = yplot+0.04*max(yplot,na.rm=TRUE),
             labels = seq(xplot))            

        pusr <- par("usr")
        text(x = pusr[2]*0.85, y = pusr[4]-(pusr[4]/(12*(1/I))),
             labels = paste("Z =",round(Z_lm1,2),"+/-", round(SE_Z_lm1,2)), col = col[I])
        ## mtext(side = 3,line=(reg_num-I+0.3), text = paste("Z =",round(Z_lm1,2),"+/-",
        ##                                                  round(SE_Z_lm1,2)), col = col[I])
    }
}



## plotting function
plotLCCC_sel <- function(x, xaxis = 'age', plot_selec = FALSE,
                         col=c('blue',"darkgreen","orange","darkred"),
                         cex = 1.5, xlim = NULL, ylim = NULL,
                         xlab = "default", ylab = "default", ...){
    pes <- x

    ## growth parameters
    if("midLengths" %in% names(pes) | xaxis == "length"){
        if("par" %in% names(pes)){
            Linf <- pes$par$Linf
            K <- pes$par$K
            t0 <- ifelse("t0" %in% names(pes$par), pes$par$t0, 0)
            C <- ifelse("C" %in% names(pes$par), pes$par$C, 0)
            ts <- ifelse("ts" %in% names(pes$par), pes$par$ts, 0)
        }else{
            Linf <- pes$Linf
            K <- pes$K
            t0 <- ifelse("t0" %in% names(pes), pes$t0, 0)
            C <- ifelse("C" %in% names(pes), pes$C, 0)
            ts <- ifelse("ts" %in% names(pes), pes$ts, 0)
        }

        if((is.null(Linf) | is.null(K))) stop(noquote(
                                             "You need to assign values to Linf and K for the catch curve based on length-frequency data!"))
    }


    if(xaxis == 'age'){
        xlabel <- "Age [yrs]"
        if("t_midL" %in% names(pes)){
            xplot <- pes$t_midL
            xlabel <- "Relative age [yrs]"
            xplotAGE <- pes$t_midL
        }else if("tplusdt_2" %in% names(pes)){
            xplot <- pes$tplusdt_2
        }else if("ln_Linf_L" %in% names(pes)){
            xplot <- pes$ln_Linf_L
            xlabel <- "ln(Linf - L)"
        }else if("classes.num" %in% names(pes)) xplot <- pes$classes.num
    }
    if(xaxis == 'length'){
        xplot <- pes$midLengths
        xlabel <- "Length [cm]"
        if("t_midL" %in% names(pes)){
            xplotAGE <- pes$t_midL
        }
    }

    if("lnC_dt" %in% names(pes)){
        yplot <- pes$lnC_dt
        ylabel <- "ln(C / dt)"
    }else if("lnC" %in% names(pes)){
        yplot <- pes$lnC
        ylabel <- "ln(C)"
    }else if("ln_C" %in% names(pes)){
        yplot <- pes$ln_C
        ylabel <- "ln C(L,Linf)"
    }
    if("ln_C" %in% names(pes) & "tplusdt_2" %in% names(pes)) ylabel <- "ln C(t, inf)"


    lm1List <- pes$linear_mod
    Z_lm1List <- pes$par$Z
    SE_Z_lm1List <- pes$Z_se
    reg_intList <- pes$reg_int
    ## Assumption that Z of smallest selected individuals is most appropriate
    mini <- min(unlist(reg_intList))
    temp <- lapply(reg_intList, function(x) grep(mini,x))
    ind <- sapply(temp, function(x) length(x) > 0)
    cutter <- unlist(reg_intList[ind])


    ##for final plot
    minyplot <- ifelse(min(yplot,na.rm=TRUE) < 0, min(yplot,na.rm=TRUE),0)
    maxyplot <- max(yplot,na.rm=TRUE) + 1

    if(is.null(xlim)){
        xlims <- c(min(xplot[which(yplot > 0)], na.rm = TRUE)-0.5,
                   max(xplot[which(yplot > 0)], na.rm = TRUE)+0.5)
    }else xlims <- xlim

    if(class(Z_lm1List) == "list"){
        reg_num <- length(Z_lm1List)
    }else{
        reg_num <- 1
    }


    maxyplot <- ceiling(pes$intercept)

    if(is.null(ylim)){
        ylims <- c(minyplot, maxyplot)
    }else ylims <- ylim

    opar <- par(mfrow = c(1,1),
                mar = c(7, 5, 4, 5) + 0.3)
    on.exit(par(opar))


    ## use user defined labels if given
    if(xlab != "default") xlabel = xlab
    if(ylab != "default") ylabel = ylab

    ## final plot
    plot(pes$Sest ~ xplot, type ='o', xlab = xlabel, xlim = xlims,
         ylab = "Probability of capture")

    if(xaxis == 'length'){
        points(y = 0.5, x=pes$L50,col='red',pch=16)
        segments(x0 = pes$L50, y0 = 0.5, x1 = pes$L50, y1 = 0, col='red',lty=2)
        par(xpd=TRUE)
        title(xlab = xlabel, outer = TRUE, line = 2)
        ##text(y=-0.12, x=pes$t50, labels = "t50", col = 'red', xpd=TRUE)
        mtext(text = "L50",side = 1, at = pes$L50,line = 0.3, col = 'red', xpd=TRUE)
    }else{
        points(y = 0.5, x=pes$t50,col='red',pch=16)
        segments(x0 = pes$t50, y0 = 0.5, x1 = pes$t50, y1 = 0, col='red',lty=2)
        par(xpd=TRUE)
        title(xlab = xlabel, outer = TRUE, line = 2)
        ##text(y=-0.12, x=pes$t50, labels = "t50", col = 'red', xpd=TRUE)
        mtext(text = "t50",side = 1, at = pes$t50,line = 0.3, col = 'red', xpd=TRUE)
    }
}

