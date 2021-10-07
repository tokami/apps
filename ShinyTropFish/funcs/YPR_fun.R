YPR_shiny <- function(
  lfq, type, FM_change = NA,
  E_change = NA,
  FM_relative = FALSE,
  Lc_change = NULL,
  tc_change = NULL,
  s_list = NA,
  stock_size_1 = NA, age_unit = 'year', curr.E = NA,
  curr.Lc = NA,
  plus_group = FALSE, Lmin = NA, Lincr = NA,
  LW_unit = "g",mark = TRUE,
  hide.progressbar = FALSE
){

    res <- lfq
    if(!"par" %in% names(lfq)) stop(noquote("Please provide the required parameters in lfq$par!"))
    par <- lfq$par
    res$FM_relative <- FM_relative

    # Beverton and Holt's ypr
    if(type == "ypr"){
        Winf <- ifelse("Winf" %in% names(res$par), res$par$Winf, NA)
        Linf <- ifelse("Linf" %in% names(res$par), res$par$Linf, NA)
        K <- res$par$K
        t0 <- ifelse("t0" %in% names(res$par), res$par$t0, 0)
        C <- ifelse("C" %in% names(res$par), res$par$C, 0)
        ts <- ifelse("ts" %in% names(res$par), res$par$ts, 0)


      M <- res$par$M
      a <- res$par$a  # might be NULL
      b <- res$par$b  # might be NULL
      ## this makes sure that a is always in kg/cm3 = and therefore meanBodyWeight is always in kg
      if(LW_unit == "g"){
        a <- a / 1000
      }
      if(!is.na(Linf) & is.na(Winf) & "a" %in% names(par) & "b" %in% names(par)){
        Winf <- a * (Linf ^ b)
      }
      #Linf <- ifelse(!is.null(res$Linf),res$Linf, exp(log(Winf/a)/b))
      # REALLY ? maybe without Linf: then message that Winf has to exist
      #if(is.null(Linf) | is.na(Linf)) stop("Either Linf or Winf with a and b has to be provided!")
      #if(is.null(Winf)) Winf <-  a * Linf ^ b  ###exp((log(Linf) - a)/b) # might still be NULL
      # or              Winf <- exp(log(Linf-a)/b)

      if(length(FM_change) == 1 & is.na(FM_change[1]) &
         length(E_change) == 1 & is.na(E_change[1])){
        FM_change <- seq(0,10,0.1)
        print(noquote("No fishing mortality (FM_change) or exploitation rate (E_change) was provided, a default range for fishing mortality of 0 to 10 is used."))
      }

      # transfer E_change into F_change if provided
      # if(length(FM_change) == 1 & is.na(FM_change[1]) & length(E_change) != 1 & !is.na(E_change[1])){
      #   FM_change <- (E_change * M) / (1 - E_change)
      #   FM_change[FM_change == Inf] <- (0.9999 * M) / (1 - 0.9999)
      # }
      if(length(FM_change) == 1 & is.na(FM_change[1]) &
         length(E_change) != 1 & !is.na(E_change[1])){
        E_change <- E_change[E_change <= 0.9]
        FM_change <- (E_change * M) / (1 - E_change)
      }

      # Recruitment  - knife edge
      tr <- res$par$tr   # might be NULL
      Lr <- res$par$Lr   # might be NULL
      if(is.null(tr) & is.null(Lr)) stop("Either the age or the length at recruitment (tr or Lr) has to be provided in lfq$par!")

      if(!is.na(Linf)){
        if(is.null(tr)) tr <- VBGF(L=Lr,pars = list(Linf=Linf,K=K,t0=t0,C=C,ts=ts))
        ## VBGF(L=Lr,Linf=Linf,K=K,t0=t0)
        if(is.null(Lr)) Lr <- VBGF(t=tr,pars = list(Linf=Linf,K=K,t0=t0,C=C,ts=ts))
        ## VBGF(t=tr,Linf=Linf,K=K,t0=t0)
      }

      # Selectivity - knife edge or with selctivtiy ogive
      tc <- res$par$tc   # might be NULL
      Lc <- res$par$Lc   # might be NULL
      if(is.null(tc) & is.null(Lc)){
        if("L50" %in% s_list) Lc <- s_list$L50
        if("Lc" %in% s_list) Lc <- s_list$Lc
        #if(!("Lc" %in% s_list) & !("L50" %in% s_list))stop("Either the age or the length at first capture (tc or Lc) has to be provided in lfq$par! \n Or provide a Lc value in s_list!")
      }
      if(!is.na(Linf)){
        if(is.null(tc) & !is.null(Lc)) tc <- VBGF(L=Lc, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(L=Lc,Linf=Linf,K=K,t0=t0)
        if(is.null(Lc) & !is.null(tc)) Lc <- VBGF(t=tc, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(t=tc,Linf=Linf,K=K,t0=t0)
        if(is.null(tc_change) & !is.null(Lc_change)) tc_change <- VBGF(L=Lc_change, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(L=Lc_change,Linf=Linf,K=K,t0=t0)
        if(is.null(Lc_change) & !is.null(tc_change)) Lc_change <- VBGF(t=tc_change, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(t=tc_change,Linf=Linf,K=K,t0=t0)
      }
      tc <- c(tc,tc_change)
      Lc <- c(Lc,Lc_change)

      if(length(s_list) > 1){
        selecType <- s_list$selecType
      }else{
        selecType <- "knife_edge"
      }


      # HEART
      list_Lc_runs <- vector("list", length(Lc))
      list_Es <- vector("list", length(Lc))

      # show progress bar only if the loop has more than 1 runs
      if (!hide.progressbar) {
        nlk <- length(Lc)
        if(nlk > 1){
          pb <- txtProgressBar(min=1, max=nlk, style=3)
          counter <- 1
        }
      }

      if(is.null(Lc)) Lc_tc <- tc else Lc_tc <- Lc
      for(i in 1:length(Lc_tc)){

        Lci <- Lc[i]
        tci <- tc[i]

        Z <- (M + FM_change)
        E <- FM_change/Z

        # KNIFE EDGE
        if(length(s_list) == 1 ){#| selecType == "knife_edge"){
          input <- list(Linf=Linf,
                        Winf = Winf,
                        K = K,
                        M = M,
                        t0 = t0,
                        tr = tr,
                        tc = tci)
          output <- ypr(input, FM_change)
          B_R <- output$br
          Y_R <- output$yr
          B_R.rel <- output$rbr
          Y_R.rel <- output$ryr
          deri <- output$derivative
        }

        # SELECTION OGIVE
        if(length(s_list) > 1 ){#& selecType != "knife_edge"){
          if("midLengths" %in% names(res)){
            classes <- as.character(res$midLengths)
            # create column without plus group (sign) if present
            classes.num <- do.call(rbind,strsplit(classes, split="\\+"))
            classes.num <- as.numeric(classes.num[,1])
            Lt <- classes.num
            # Lincr <- res$midLengths[2] - res$midLengths[1]
            # Lmin <- res$midLengths[1] - (Lincr/2)
          }
          if(!"midLengths" %in% names(res)){
            if(is.na(Lmin) | is.na(Lincr)) writeLines("No midpoints of length classes are provided. This can be done using Lmin and Lincr or by a \n midLengths element in lfq. A standard range from 1cm to Linf * 0.98 by 2cm is assumed")
            Lmin <- ifelse(is.na(Lmin), 1, Lmin)
            Lincr <- ifelse(is.na(Lincr), 2, Lincr)
            # mid length vector
            Lt <- seq(Lmin,(Linf*0.98),Lincr)
          } # make Lt fixed except depending on Linf and as detailed as possible, e.g. Lincr = 0.5 (takes a long time)

          # selectivity
          P <- select_ogive(s_list, Lt =  Lt, Lc = Lci)

          input <- list(Linf = ifelse(length(Linf) > 0, Linf, NA),
                        Winf = ifelse(length(Winf) > 0, Winf, NA),
                        K = K,
                        M = M,
                        t0 = t0,
                        tr = tr,
                        tc = tci)
          output <- ypr_sel(input, FM_change, Lt, P)

          # relative yield and biomass per recruit
          B_R.rel <- output$rbr
          Y_R.rel <- output$ryr
          # derivative
          deri <- output$derivative

          #test
          Y_R <- Y_R.rel * Winf * exp(M * (tr - t0))
          B_R <- B_R.rel * Winf * exp(M * (tr - t0))

          # biased because only prints P for largest Lc value
          #if(i == length(Lc)) plot(Lt, P, type = 'l', ylab = 'Prob of capture',
          #                         main = 'Selectivity function')
        }


        # virgin biomass
        if(0 %in% FM_change){
          Bv_R <- B_R[FM_change == 0]
        }else{
          Bv_R <- B_R[FM_change == min(FM_change,na.rm = TRUE)]
          writeLines(paste0("Biomass was not estimated for a fishing mortality (FM) of 0, thus the virgin biomass corresponds to a FM of ",min(FM_change,na.rm = TRUE)))
        }

        #biomass in percetage of virgin biomass
        B_R.percent <- round((B_R / Bv_R ) * 100, digits = 1)

        #mean age in annual yield
        Ty <- (1 / Z) + tci

        #mean length in the annual yield
        S <- exp(-K * (tci - t0))         # the same:    S <- 1 - (Lci/Linf)
        Ly <- Linf * (1 - ((Z*S)/(Z+K)))

        #mean weight in annual yield
        Wy <- (Z) * Winf *
          ((1/Z) - ((3*S)/(Z+K)) +
             ((3*(S^2))/(Z+(2*K))) - ((S^3)/(Z + (3*K))))


        results.PBH <- data.frame(FM = FM_change,
                                  E = E)
        if(length(Ty) > 0) results.PBH$Ty <- Ty
        if(length(Ly) > 0) results.PBH$Ly <- Ly
        if(length(Wy) > 0) results.PBH$Wy <- Wy

        results.PBH$Y_R.rel <- Y_R.rel
        results.PBH$B_R.rel <- B_R.rel

        # WHY NECESSARY???
        if(length(Y_R) > 0) results.PBH$Y_R = Y_R
        if(length(B_R) > 0) results.PBH$B_R = B_R
        if(length(B_R.percent) > 0) results.PBH$B_R.percent = B_R.percent

        list_Lc_runs[[i]] <- results.PBH

        # reference points
        Nmax <- which.max(Y_R.rel)  #  should be the same as which.min(abs(deri)) which is also labelled Nmax
        deri_pot <- deri[1:Nmax]
        N01 <- which.min(abs(deri_pot - (deri[1] * 0.1)))
        N05 <- which.min(abs(B_R.percent - 50))  #which.min(abs(deri - (deri[1] * 0.5)))

        df_loop_Es <- data.frame(Lc = ifelse(!is.null(Lci),Lci,NA),
                                 tc = ifelse(!is.null(tci),tci,NA),
                                 F01 = FM_change[N01],
                                 Fmax = FM_change[Nmax])
        if(length(B_R.percent) > 0) df_loop_Es$F05 <- FM_change[N05]   # WHY NECESSARY????
        # df_loop_Es$Fmax <- FM_change[Nmax]
        df_loop_Es$E01 <- E[N01]
        df_loop_Es$Emax <- E[Nmax]
        if(length(B_R.percent) > 0) df_loop_Es$E05 <- E[N05]    # WHY NECESSARY????
        # df_loop_Es$Emax <- E[Nmax]

        list_Es[[i]] <- df_loop_Es

        # update counter and progress bar
        if (!hide.progressbar) {
          if(nlk > 1){
            setTxtProgressBar(pb, counter)
            counter <- counter + 1
          }
        }
      }

      df_Es <- do.call(rbind,list_Es)

      names(list_Lc_runs) <- paste0("Lc_", Lc_tc)   # names(list_tc_runs) <- tc
      ret <- c(res,list(FM_change = FM_change,
                        Lc = Lc,
                        tc = tc,
                        list_Lc_runs = list_Lc_runs,   #   list_tc_runs = list_tc_runs,
                        df_Es = df_Es))   #   df_Es = df_Es,


      if(!is.na(curr.E) & !is.na(curr.Lc)){
        curr.tc <- VBGF(L=curr.Lc, pars = list(Linf=Linf,K=K,t0=t0))
        # current exploitation rate
        curr.F = (M * curr.E)/(1-curr.E)  # curr.F <- (M * curr.E)/(1-curr.E)
        tmpList <- list(Linf=Linf,
                        Winf = Winf,
                        K = K,
                        M = M,
                        t0 = t0,
                        tr = tr,
                        tc = curr.tc)
        if(length(s_list) == 1 | selecType == "knife_edge"){
          tmpRES <- ypr(pars = tmpList, FM_change = curr.F)
        }
        if(length(s_list) > 1 & selecType != "knife_edge"){
          P <- select_ogive(s_list, Lt =  Lt, Lc = curr.Lc)
          tmpRES <- ypr_sel(pars = tmpList, FM_change = curr.F, Lt, P)
          tmpRES$yr <- tmpRES$ryr * Winf * exp(M * (tr - t0))
          tmpRES$br <- tmpRES$rbr * Winf * exp(M * (tr - t0))
        }

        df_currents <- data.frame(curr.Lc = curr.Lc,
                                  curr.tc = curr.tc,
                                  curr.E = curr.E,
                                  curr.F = curr.F,
                                  curr.YPR = tmpRES$yr,        #ypr(curr.F, curr.Lc_tc)       #, type = "length"),           # curr.YPR = ypr(curr.F, curr.Lc_tc, type = "age"),
                                  curr.YPR.rel = tmpRES$ryr,     #ypr.rel(curr.F, curr.Lc_tc),   #, type = "length"),   # curr.YPR.rel = ypr.rel(curr.F, curr.Lc_tc, type = "age"),
                                  curr.BPR = tmpRES$br,         #bpr(curr.F, curr.Lc_tc),           #, type = "length"),           # curr.BPR = bpr(curr.F, curr.Lc_tc, type = "age"),
                                  curr.BPR.rel = tmpRES$rbr)     #bpr.rel(curr.F, curr.Lc_tc))   #, type = "length"))   # curr.BPR.rel = bpr.rel(curr.F, curr.Lc_tc, type = "age"))
        ret$currents <- df_currents
      }
    }

    # Thompson and Bell model
    if(type == "ThompBell"){
      meanWeight <- res$meanWeight
      meanValue <- res$meanValue

      #mortalities
      FM <- par$FM
      if(!is.null(par$M)){
        nM <- par$M
        Z <- FM + nM
      }else{
        Z <- par$Z
        nM <- Z - FM
      }

        Winf <- ifelse("Winf" %in% names(par), res$par$Winf, NA)
        Linf <- ifelse("Linf" %in% names(par), res$par$Linf, NA)
        K <- res$par$K
        t0 <- ifelse("t0" %in% names(par), res$par$t0, 0)
        C <- ifelse("C" %in% names(par), res$par$C, 0)
        ts <- ifelse("ts" %in% names(par), res$par$ts, 0)

        a <- par$a  # might be NULL
      b <- par$b  # might be NULL

      Lmat <- par$Lmat
      wmat <- par$wmat


      # Selectivity - knife edge or with selctivtiy ogive
      tc <- par$tc   # might be NULL
      Lc <- par$Lc   # might be NULL
      if(is.null(tc) & is.null(Lc)){
        if("L50" %in% s_list) Lc <- s_list$L50
        if("Lc" %in% s_list) Lc <- s_list$Lc
        #if(!("Lc" %in% s_list) & !("L50" %in% s_list))stop("Either the age or the length at first capture (tc or Lc) has to be provided in lfq$par! \n Or provide a Lc value in s_list!")
      }
      if(!is.null(Linf)){
        if(is.null(tc) & !is.null(Lc)) tc <- VBGF(L=Lc, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(L=Lc,Linf=Linf,K=K,t0=t0)
        if(is.null(Lc) & !is.null(tc)) Lc <- VBGF(t=tc, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(t=tc,Linf=Linf,K=K,t0=t0)
        if(is.null(tc_change) & !is.null(Lc_change)) tc_change <- VBGF(L=Lc_change, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(L=Lc_change,Linf=Linf,K=K,t0=t0)
        if(is.null(Lc_change) & !is.null(tc_change)) Lc_change <- VBGF(t=tc_change, pars = list(Linf=Linf,K=K,t0=t0)) # VBGF(t=tc_change,Linf=Linf,K=K,t0=t0)
      }
      if(is.null(tc)){
          tc <- c(tc,tc_change)
      }else{
          tc <- tc_change
      }
      if(is.null(Lc)){
          Lc <- c(Lc,Lc_change)
      }else{
          Lc <- Lc_change
      }


      # age based
      if('age' %in% names(res)) classes <- as.character(res$age)
      # length based
      if('midLengths' %in% names(res)) classes <- as.character(res$midLengths)

      # create column without plus group (sign) if present
      classes.num <- do.call(rbind,strsplit(classes, split="\\+"))
      classes.num <- as.numeric(classes.num[,1])

      if(length(FM_change) == 1 & is.na(FM_change[1]) &
         length(E_change) == 1 & is.na(E_change[1])){
        FM_change <- seq(0,10,0.1)
        print(noquote("No fishing mortality (FM_change) or exploitation rate (E_change) \nwas provided, a default range for the absolute \nfishing mortality of 0 to 10 is used."))
      }

      # transfer E_change into F_change if provided
      if(length(FM_change) == 1 & is.na(FM_change[1]) &
         length(E_change) != 1 & !is.na(E_change[1])){
        E_change <- E_change[E_change <= 0.9]
        FM_change <- (E_change * nM) / (1 - E_change)
      }
      if(length(E_change) == 1 & is.na(E_change[1])){
        E_change <- FM_change / (FM_change + nM)
      }

      Lt <- classes.num  # if age in names(res) Lt here is age because classes.num = age


      # Only FM change provided without Lc_tc change
      if((is.null(tc_change) & is.null(Lc_change))){  #  | length(s_list) == 1){

        #if(is.null(res$FM) | length(res$FM) == 1) stop(noquote("Please provide fishing mortality FM (in 'lfq$par') as a vector per size class!"))

        if(is.null(par$FM)) stop(noquote("Please provide fishing mortality FM (in 'lfq$par')!"))
        if(length(par$FM) == 1){
          if(length(s_list) > 1 | !is.null(Lc[1])){
            ##print(noquote("Fishing mortality per length class not povided, using selectivity information to derive fishing mortality per length class."))
            if(length(s_list) == 1){
              s_list <- list(selecType = "knife_edge", L50 = Lc[1])
            }
            sel <- select_ogive(s_list, Lt = Lt)
            FM <- par$FM * sel
          }else{
            stop(noquote("Please provide either fishing mortality FM (in 'lfq$par') per length class or a Lc value!"))
          }
        }

        #prediction based on f_change
        if(!FM_relative){
          pred_mat <- as.matrix(FM/max(FM, na.rm = TRUE)) %*% FM_change
        }
        if(FM_relative){
          pred_mat <- as.matrix(FM) %*% FM_change
        }

        pred_res_list <- list()
        for(x7 in 1:length(FM_change)){
          lfq$par$Z <- pred_mat[,x7] + nM
          lfq$par$FM <- pred_mat[,x7]
          resL <- stock_sim(lfq, age_unit = age_unit,
                            stock_size_1 = stock_size_1, plus_group = plus_group)
          pred_res_list[[x7]] <- resL$totals
        }

          ## SSB0 for SPR
          lfq$par$FM <- pred_mat[,1] * 0.0
          lfq$par$Z <- lfq$par$FM + nM
          tmp <- stock_sim(lfq, age_unit = age_unit,
                           stock_size_1 = stock_size_1, plus_group = plus_group)
          SSB0 <- tmp$totals$meanSSB

        ## pred_res_df <- do.call(rbind, pred_res_list)
        ## pred_res_df$FM_change <- FM_change
        ## pred_res_df$E_change <- E_change

        ## res2 <- pred_res_df
        ## res3 <- c(res,res2)


        ## reference points
        ## ## Fmax
        ## Nmax <- which.max(pred_res_df$totY)
        ## ## F01 (proxy for Fmsy in ICES)
        ## slopeOrg <- (pred_res_df$totY[2] - pred_res_df$totY[1]) / (FM_change[2] - FM_change[1])
        ## slope01 <- round(0.1*slopeOrg, 2)
        ## slopes <- rep(NA, length(FM_change))
        ## slopes[1] <- slopeOrg
        ## for(i in 3:length(FM_change)){
        ##   slopes[i-1] <- round((pred_res_df$totY[i] - pred_res_df$totY[i-1]) /
        ##                          (FM_change[i] - FM_change[i-1]),2)
        ## }
        ## dif <- abs(slopes - slope01)
        ## dif[is.na(dif)] <- 1e+11
        ## difpot <- dif[1:Nmax]
        ## N01 <- which.min(difpot)
        ## ## F05
        ## Bper <- rep(NA,length(pred_res_df$meanB))
        ## Bper[1] <- 100
        ## for(ix in 2:length(Bper)){
        ##   Bper[ix] <- pred_res_df$meanB[ix]/pred_res_df$meanB[1] * 100
        ## }
        ## N05 <- which.min(abs(Bper - 50))

        ## if(!is.null(Lc[1]) & !is.null(tc[1])){
        ##   df_Es <- data.frame(Lc = Lc,
        ##                       tc = tc,
        ##                       F01 = FM_change[N01],
        ##                       Fmax = FM_change[Nmax],
        ##                       F05 = FM_change[N05],
        ##                       E01 = E_change[N01],
        ##                       Emax = E_change[Nmax],
        ##                       E05 = E_change[N05])
        ## }else{
        ##   df_Es <- data.frame(F01 = FM_change[N01],
        ##                       Fmax = FM_change[Nmax],
        ##                       F05 = FM_change[N05],
        ##                       E01 = E_change[N01],
        ##                       Emax = E_change[Nmax],
        ##                       E05 = E_change[N05])
          ## }
pred_res_df <- do.call(rbind, pred_res_list)
        if(!is.null(SSB0) && !is.null(pred_res_df$meanSSB)){
            pred_res_df$SPR <- pred_res_df$meanSSB/SSB0
        }
        pred_res_df$FM_change <- FM_change
        pred_res_df$E_change <- E_change

        res2 <- pred_res_df
        res3 <- c(res,res2)


          ## reference points  NEW:
        spr <- pred_res_df$SPR
        ypr <- pred_res_df$totY
        bpr <- pred_res_df$meanB
        splSPR <- try(smooth.spline(x = FM_change, y = spr, spar = 0.4), silent = TRUE)
        splYPR <- smooth.spline(x = FM_change, y = ypr, spar = 0.4)
        splBPR <- smooth.spline(x = FM_change, y = bpr, spar = 0.4)
        newdat <- data.frame(F=seq(0,20,length.out = 1e3))
          newdat$YPR <- predict(splYPR, x=newdat$F)$y
          ## newdat$YPR[1] <- ypr[1]  ## NEW: because predict YPR for F=0 doesn't need to be 0, but in theory yes
        newdat$YPR.d1 <- predict(splYPR, x=newdat$F, deriv = 1)$y
        newdat$BPR <- predict(splBPR, x=newdat$F)$y
        newdat$SPR <- try(predict(splSPR, x=newdat$F)$y, silent = TRUE)
        Nmax <- which.max(newdat$YPR)
        ##                Fmax <- newdat[Nmax,1]
        ##                Emax <- Fmax/Z
        N01 <- which.min(((newdat$YPR.d1/newdat$YPR.d1[1])-0.1)^2)
        ##   F01 <- newdat[N01,1]
        ##                E01 <- F01/Z
        N05 <- which.min((newdat$BPR - newdat$BPR[1]/2)^2)
        ##                F05 <- newdat[N05,1]
        ##                E05 <- F05/Z
        N04 <- try(which.min((newdat$SPR - 0.4)^2), silent = TRUE)
        ##                F04 <- newdat[N04,1]
        ##                E04 <- F04/Z

        ## current SPR
        ## SPR <- newdat$SPR[which.min((newdat$F - FM)^2)]

        ## ref level
        N01 <- ifelse(is.integer(N01),N01,NA)
        Nmax <- ifelse(is.integer(Nmax),Nmax,NA)
        N05 <- ifelse(is.integer(N05),N05,NA)
        N04 <- ifelse(is.integer(N04),N04,NA)
        ##
        F01 <- ifelse(is.integer(N01),newdat[N01,1],NA)
        Fmax <- ifelse(is.integer(Nmax),newdat[Nmax,1],NA)
        F05 <- ifelse(is.integer(N05),newdat[N05,1],NA)
        F04 <- ifelse(is.integer(N04),newdat[N04,1],NA)
        ## E
        newdat$E <- newdat$F/mean(Z, na.rm = TRUE)
        E01 <- ifelse(is.integer(N01),newdat$E[N01],NA)
        Emax <- ifelse(is.integer(Nmax),newdat$E[Nmax],NA)
        E05 <- ifelse(is.integer(N05),newdat$E[N05],NA)
        E04 <- ifelse(is.integer(N04),newdat$E[N04],NA)
        ## for y axis
        F01_ypr <- ifelse(is.integer(N01),newdat$YPR[N01],NA)
        Fmax_ypr <- ifelse(is.integer(Nmax),newdat$YPR[Nmax],NA)
        F05_ypr <- ifelse(is.integer(N05),newdat$YPR[N05],NA)
        F04_ypr <- ifelse(is.integer(N04),newdat$YPR[N04],NA)
        F01_bpr <- ifelse(is.integer(N01),newdat$BPR[N01],NA)
        Fmax_bpr <- ifelse(is.integer(Nmax),newdat$BPR[Nmax],NA)
        F05_bpr <- ifelse(is.integer(N05),newdat$BPR[N05],NA)
        F04_bpr <- ifelse(is.integer(N04),newdat$BPR[N04],NA)
        if(is.numeric(Lmat) && is.numeric(wmat)){
            F01_spr <- try(ifelse(is.integer(N01),newdat$SPR[N01],NA),silent=TRUE)
            Fmax_spr <- try(ifelse(is.integer(Nmax),newdat$SPR[Nmax],NA),silent=TRUE)
            F05_spr <- try(ifelse(is.integer(N05),newdat$SPR[N05],NA),silent=TRUE)
            F04_spr <- try(ifelse(is.integer(N04),newdat$SPR[N04],NA),silent=TRUE)
        }else{
            F01_spr <- NA
            Fmax_spr <- NA
            F05_spr <- NA
            F04_spr <- NA
        }

##   ## reference points
      ##   Bper <- rep(NA,length(pred_res_df$meanB))
      ##   Bper[1] <- 100
      ##   for(ix in 2:length(Bper)){
      ##       Bper[ix] <- pred_res_df$meanB[ix]/pred_res_df$meanB[1] * 100
      ##   }
      ##   N05 <- which.min(abs(Bper - 50))
      ##   Nmax <- which.max(pred_res_df$totY)

        if(!is.null(Lc[1]) & !is.null(tc[1])){
            df_Es <- data.frame(Lc = Lc,
                                tc = tc,
                                F01 = F01,
                                Fmax = Fmax,
                                F05 = F05,
                                F04 = F04,
                                E01 = E01,
                                Emax = Emax,
                                E05 = E05,
                                E04 = E04,
                                YPR_F01 = F01_ypr,
                                YPR_Fmax = Fmax_ypr,
                                YPR_F05 = F05_ypr,
                                YPR_F04 = F04_ypr,
                                YPR_F01 = F01_bpr,
                                BPR_Fmax = Fmax_bpr,
                                BPR_F05 = F05_bpr,
                                BPR_F04 = F04_bpr,
                                SPR_F01 = F01_spr,
                                SPR_Fmax = Fmax_spr,
                                SPR_F05 = F05_spr,
                                SPR_F04 = F04_spr
                                )
        }else{
            df_Es <- data.frame(F01 = F01,
                                Fmax = Fmax,
                                F05 = F05,
                                F04 = F04,
                                E01 = E01,
                                Emax = Emax,
                                E05 = E05,
                                E04 = E04,
                                YPR_F01 = F01_ypr,
                                YPR_Fmax = Fmax_ypr,
                                YPR_F05 = F05_ypr,
                                YPR_F04 = F04_ypr,
                                YPR_F01 = F01_bpr,
                                BPR_Fmax = Fmax_bpr,
                                BPR_F05 = F05_bpr,
                                BPR_F04 = F04_bpr,
                                SPR_F01 = F01_spr,
                                SPR_Fmax = Fmax_spr,
                                SPR_F05 = F05_spr,
                                SPR_F04 = F04_spr
                                )
        }


        ret <- c(res3, list(df_Es = df_Es), list(newdat = newdat))


        if(!is.na(curr.E)){
          if(!is.na(curr.Lc)){
            curr.tc <- VBGF(L=curr.Lc, pars = list(Linf=Linf, K=K, t0=t0))
          }else curr.tc <- NA
          # current exploitation rate
          curr.F = (nM * curr.E)/(1-curr.E)

          if(is.na(curr.Lc)){
            sel <- (FM / max(FM,na.rm=TRUE))
          }else if(!is.na(curr.Lc)){
            s_list <- list(selecType = "knife_edge", L50 = curr.Lc)
            Lt <- res$midLengths
            sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
          }
          if(length(s_list) != 1){
            Lt <- res$midLengths
            sel <- select_ogive(s_list, Lt = Lt)
          }

          mati <- sel * curr.F
          parloop <- res
          parloop$par$FM <- mati
          parloop$par$Z <- mati + nM
          res2 <- stock_sim(lfq = parloop, age_unit = age_unit,
                            stock_size_1 = stock_size_1, plus_group=plus_group)
          mati2 <- res2$totals

          ## NEW:
          currYPR <- newdat$YPR[which.min((newdat$F - curr.F)^2)]


          ## SPR
        parloop <- res
        parloop$FM <- sel * 0.0
        parloop$Z <- parloop$FM + nM
        res2 <- stock_sim(parloop, age_unit = age_unit,
                          stock_size_1 = stock_size_1, plus_group=plus_group)
        SSB0 <- res2$totals$meanSSB
        SPR <- mati2$meanSSB/SSB0

          df_currents <- data.frame(curr.Lc = curr.Lc,
                                    curr.tc = curr.tc,
                                    curr.E = curr.E,
                                    curr.F = curr.F,
                                    curr.C = mati2$totC,
                                    curr.Y = currYPR, ## mati2$totY,
                                    curr.V = mati2$totV,
                                  curr.B = mati2$meanB,
                                  curr.SSB = mati2$meanSSB,
                                  curr.SPR = SPR)
          ret$currents <- df_currents
        }
      }

      # FM and Lc_tc change provided
      if(!is.null(tc_change) | !is.null(Lc_change)){
        # instead of s_list the outcome of one of the other select functions?

        if(length(s_list) == 1){
          s_list <- list(selecType = "knife_edge", L50 = Lc[1])
        }
        sel <- select_ogive(s_list, Lt = Lt) #classes.num

        sel.list <- list()
        for(x19 in 1:length(Lc)){
          sel.list[[x19]] <- select_ogive(s_list, Lt = Lt, Lc = Lc[x19]) #classes.num
        }
        Lc_mat <- do.call(cbind,sel.list)
        colnames(Lc_mat) <- Lc

        Lc_mat_FM <- Lc_mat   #max(FM, na.rm=TRUE)  # with one it should correspond to actual fishing mortality not to change in mortality (x factor)

        #list with FM_Lc_matrices per FM_change
        FM_Lc_com_mat.list <- list()
        if(!FM_relative){
          for(x20 in 1:length(colnames(Lc_mat_FM))){
            FM_Lc_com_mat.list[[x20]] <- as.matrix(Lc_mat_FM[,x20]) %*% FM_change
            colnames(FM_Lc_com_mat.list[[x20]]) <- FM_change
          }
        }
        if(FM_relative){
          for(x20 in 1:length(colnames(Lc_mat_FM))){
            FM_Lc_com_mat.list[[x20]] <- as.matrix(Lc_mat_FM[,x20] * FM) %*% FM_change
            colnames(FM_Lc_com_mat.list[[x20]]) <- FM_change
          }
        }

        parloop <- res

        ## pred.FM_Lc_com_res_loopC_list <- vector("list",length(FM_Lc_com_mat.list))
        ## pred.FM_Lc_com_res_loopY_list <- vector("list",length(FM_Lc_com_mat.list))
        ## pred.FM_Lc_com_res_loopB_list <- vector("list",length(FM_Lc_com_mat.list))
        ##   pred.FM_Lc_com_res_loopV_list <- vector("list",length(FM_Lc_com_mat.list))

    pred.FM_Lc_com_res_loopC_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopY_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopB_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopV_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopSSB_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopSPR_list <- vector("list",length(FM_Lc_com_mat.list))

        if (!hide.progressbar) {
          nlk <- prod(length(FM_Lc_com_mat.list),dim(FM_Lc_com_mat.list[[1]])[2])
          pb <- txtProgressBar(min=1, max=nlk, style=3)
          counter <- 1
        }

        for(x21 in 1:length(FM_Lc_com_mat.list)){  #loop for length of list == Lc changes
          mati <- FM_Lc_com_mat.list[[x21]]

          pred.FM_Lc_com_res_loop1_list <- list()
          for(x22 in 1:dim(mati)[2]){
            parloop$par$FM <- mati[,x22]
            parloop$par$Z <- mati[,x22] + nM
            res2 <- stock_sim(lfq = parloop, age_unit = age_unit,
                              stock_size_1 = stock_size_1, plus_group=plus_group)
            pred.FM_Lc_com_res_loop1_list[[x22]] <- res2$totals

            # update counter and progress bar
            if (!hide.progressbar) {
              setTxtProgressBar(pb, counter)
              counter <- counter + 1
            }
          }

## SSB0 for SPR
        parloop$FM <- 0.0 * FM_Lc_com_mat.list[[1]][,1]
        parloop$Z <- parloop$FM + nM
        tmp <- stock_sim(parloop, age_unit = age_unit,
                         stock_size_1 = stock_size_1, plus_group=plus_group)
          SSB0 <- tmp$totals$meanSSB

          prev_mat <- do.call(rbind, pred.FM_Lc_com_res_loop1_list)
          prev_matC <- prev_mat[,'totC']
          prev_matY <- prev_mat[,'totY']
          prev_matB <- prev_mat[,'meanB']
           prev_matSSB <- prev_mat[,'meanSSB']
          prev_matV <- prev_mat[,'totV']
           prev_matSPR <- prev_mat[,'meanSSB'] / SSB0

          pred.FM_Lc_com_res_loopC_list[[x21]] <- prev_matC
          pred.FM_Lc_com_res_loopY_list[[x21]] <- prev_matY
          pred.FM_Lc_com_res_loopB_list[[x21]] <- prev_matB
          pred.FM_Lc_com_res_loopSSB_list[[x21]] <- prev_matSSB
          pred.FM_Lc_com_res_loopV_list[[x21]] <- prev_matV
          pred.FM_Lc_com_res_loopSPR_list[[x21]] <- prev_matSPR
        }

        #for catch
        mat_FM_Lc_com.C <- do.call(rbind, pred.FM_Lc_com_res_loopC_list)
        rownames(mat_FM_Lc_com.C) <- Lc
        colnames(mat_FM_Lc_com.C) <- FM_change

        #for yield
        mat_FM_Lc_com.Y <- do.call(rbind, pred.FM_Lc_com_res_loopY_list)
        rownames(mat_FM_Lc_com.Y) <- Lc
        colnames(mat_FM_Lc_com.Y) <- FM_change

        #for biomass
        mat_FM_Lc_com.B <- do.call(rbind, pred.FM_Lc_com_res_loopB_list)
        rownames(mat_FM_Lc_com.B) <- Lc
           colnames(mat_FM_Lc_com.B) <- FM_change

                                        #for SSB
        mat_FM_Lc_com.SSB <- do.call(rbind, pred.FM_Lc_com_res_loopSSB_list)
        rownames(mat_FM_Lc_com.SSB) <- Lc
        colnames(mat_FM_Lc_com.SSB) <- FM_change

                                        #for SPR
        mat_FM_Lc_com.SPR <- do.call(rbind, pred.FM_Lc_com_res_loopSPR_list)
        rownames(mat_FM_Lc_com.SPR) <- Lc
        colnames(mat_FM_Lc_com.SPR) <- FM_change

        #for value
        mat_FM_Lc_com.V <- do.call(rbind, pred.FM_Lc_com_res_loopV_list)
        rownames(mat_FM_Lc_com.V) <- Lc
        colnames(mat_FM_Lc_com.V) <- FM_change

        ## # transvers matrices for plotting (the opposite arrangement from book)
        ## mat_FM_Lc_com.C <- t(mat_FM_Lc_com.C)
        ## mat_FM_Lc_com.Y <- t(mat_FM_Lc_com.Y)
        ## mat_FM_Lc_com.B <- t(mat_FM_Lc_com.B)
          ## mat_FM_Lc_com.V <- t(mat_FM_Lc_com.V)

## transvers matrices for plotting (the opposite arrangement from book)
        mat_FM_Lc_com.C <- t(mat_FM_Lc_com.C)
        mat_FM_Lc_com.Y <- t(mat_FM_Lc_com.Y)
        mat_FM_Lc_com.B <- t(mat_FM_Lc_com.B)
        mat_FM_Lc_com.SSB <- t(mat_FM_Lc_com.SSB)
        mat_FM_Lc_com.SPR <- t(mat_FM_Lc_com.SPR)
        mat_FM_Lc_com.V <- t(mat_FM_Lc_com.V)


        ## reference points
        ## Fmax
        Nmax <- apply(mat_FM_Lc_com.Y, MARGIN = 2, FUN = which.max)

        ## F01 (proxy for Fmsy in ICES)
        slopes <- matrix(NA,ncol=dim(mat_FM_Lc_com.Y)[2],
                         nrow=dim(mat_FM_Lc_com.Y)[1])
        slopeOrg <- (mat_FM_Lc_com.Y[2,] - mat_FM_Lc_com.Y[1,]) /
            (rep(FM_change[2],dim(mat_FM_Lc_com.Y)[2]) -
             rep(FM_change[1],dim(mat_FM_Lc_com.Y)[2]))
        slope01 <- round(0.1*slopeOrg, 2)
        slopes[1,] <- slopeOrg
        for(i in 3:length(FM_change)){
            slopes[i-1,] <- round((mat_FM_Lc_com.Y[i,] - mat_FM_Lc_com.Y[i-1,]) /
                                  (rep(FM_change[i],dim(mat_FM_Lc_com.Y)[2]) -
                                   rep(FM_change[i-1],dim(mat_FM_Lc_com.Y)[2])),2)
        }
        dif <- t(apply(slopes,1,function(x) abs(x - slope01)))
        dif[is.na(dif)] <- 1e+11
        difpot <- dif
        for(i in 1:ncol(dif)){
            difpot[(Nmax[i]:nrow(difpot)),i] <- 1e+11
        }
        N01 <- apply(difpot, MARGIN = 2, FUN = which.min)
        ## F05
        mat_FM_Lc_com.Bper <- matrix(NA,ncol=dim(mat_FM_Lc_com.B)[2],
                                     nrow=dim(mat_FM_Lc_com.B)[1])
        mat_FM_Lc_com.Bper[1,] <- 100
        for(ix in 2:dim(mat_FM_Lc_com.B)[1]){
            mat_FM_Lc_com.Bper[ix,] <- mat_FM_Lc_com.B[ix,]/mat_FM_Lc_com.B[1,] *100
        }
        N05 <- apply(mat_FM_Lc_com.Bper, MARGIN = 2,
                     FUN = function(x) which.min(abs(x - 50)))

        if((!is.null(Lc[1]) & !is.null(tc[1])) | (!is.na(Lc[1]) & !is.na(tc[1])) ){
            df_Es <- data.frame(Lc = Lc,
                                tc = tc,
                                F01 = FM_change[N01],
                                Fmax = FM_change[Nmax],
                                F05 = FM_change[N05],
                                E01 = E_change[N01],
                                Emax = E_change[Nmax],
                                E05 = E_change[N05])
        }else{
            df_Es <- data.frame(
                F01 = FM_change[N01],
                Fmax = FM_change[Nmax],
                F05 = FM_change[N05],
                E01 = E_change[N01],
                Emax = E_change[Nmax],
                E05 = E_change[N05])
        }


        ## ## reference points
        ## ## Fmax
        ## Nmax <- apply(mat_FM_Lc_com.Y, MARGIN = 2, FUN = which.max)

        ## ## F01 (proxy for Fmsy in ICES)
        ## slopes <- matrix(NA,ncol=dim(mat_FM_Lc_com.Y)[2],
        ##                  nrow=dim(mat_FM_Lc_com.Y)[1])
        ## slopeOrg <- (mat_FM_Lc_com.Y[2,] - mat_FM_Lc_com.Y[1,]) /
        ##   (rep(FM_change[2],dim(mat_FM_Lc_com.Y)[2]) -
        ##      rep(FM_change[1],dim(mat_FM_Lc_com.Y)[2]))
        ## slope01 <- round(0.1*slopeOrg, 2)
        ## slopes[1,] <- slopeOrg
        ## for(i in 3:length(FM_change)){
        ##   slopes[i-1,] <- round((mat_FM_Lc_com.Y[i,] - mat_FM_Lc_com.Y[i-1,]) /
        ##                           (rep(FM_change[i],dim(mat_FM_Lc_com.Y)[2]) -
        ##                              rep(FM_change[i-1],dim(mat_FM_Lc_com.Y)[2])),2)
        ## }
        ## dif <- t(apply(slopes,1,function(x) abs(x - slope01)))
        ## dif[is.na(dif)] <- 1e+11
        ## difpot <- dif
        ## for(i in 1:ncol(dif)){
        ##   difpot[(Nmax[i]:nrow(difpot)),i] <- 1e+11
        ## }
        ## N01 <- apply(difpot, MARGIN = 2, FUN = which.min)
        ## ## F05
        ## mat_FM_Lc_com.Bper <- matrix(NA,ncol=dim(mat_FM_Lc_com.B)[2],
        ##                              nrow=dim(mat_FM_Lc_com.B)[1])
        ## mat_FM_Lc_com.Bper[1,] <- 100
        ## for(ix in 2:dim(mat_FM_Lc_com.B)[1]){
        ##   mat_FM_Lc_com.Bper[ix,] <- mat_FM_Lc_com.B[ix,]/mat_FM_Lc_com.B[1,] *100
        ## }
        ## N05 <- apply(mat_FM_Lc_com.Bper, MARGIN = 2,
        ##              FUN = function(x) which.min(abs(x - 50)))

        ## if((!is.null(Lc[1]) & !is.null(tc[1])) | (!is.na(Lc[1]) & !is.na(tc[1])) ){
        ##   df_Es <- data.frame(Lc = Lc,
        ##                       tc = tc,
        ##                       F01 = FM_change[N01],
        ##                       Fmax = FM_change[Nmax],
        ##                       F05 = FM_change[N05],
        ##                       E01 = E_change[N01],
        ##                       Emax = E_change[Nmax],
        ##                       E05 = E_change[N05])
        ## }else{
        ##   df_Es <- data.frame(
        ##     F01 = FM_change[N01],
        ##     Fmax = FM_change[Nmax],
        ##     F05 = FM_change[N05],
        ##     E01 = E_change[N01],
        ##     Emax = E_change[Nmax],
        ##     E05 = E_change[N05])
        ## }


        ## ret <- c(res,
        ##          list(FM_change = FM_change,
        ##               # FM_relative = FM_relative,
        ##               E_change = E_change,
        ##               Lc_change = Lc_change,
        ##               tc_change = tc_change,
        ##               Lt = Lt,
        ##               sel = sel,
        ##               mat_FM_Lc_com.C = mat_FM_Lc_com.C,
        ##               mat_FM_Lc_com.Y = mat_FM_Lc_com.Y,
        ##               mat_FM_Lc_com.V = mat_FM_Lc_com.V,
        ##               mat_FM_Lc_com.B = mat_FM_Lc_com.B,
        ##               df_Es = df_Es))

## # reference points
      ## mat_FM_Lc_com.Bper <- matrix(NA,ncol=dim(mat_FM_Lc_com.B)[2],
      ##                                      nrow=dim(mat_FM_Lc_com.B)[1])
      ## mat_FM_Lc_com.Bper[1,] <- 100
      ## for(ix in 2:dim(mat_FM_Lc_com.B)[1]){
      ##   mat_FM_Lc_com.Bper[ix,] <- mat_FM_Lc_com.B[ix,]/mat_FM_Lc_com.B[1,] *100
      ## }
      ## N05 <- apply(mat_FM_Lc_com.Bper, MARGIN = 2,
      ##              FUN = function(x) which.min(abs(x - 50)))

      ## Nmax <- apply(mat_FM_Lc_com.Y, MARGIN = 2, FUN = which.max)


        ret <- c(res,
                 list(FM_change = FM_change,
                                        # FM_relative = FM_relative,
                      E_change = E_change,
                      Lc_change = Lc_change,
                      tc_change = tc_change,
                      Lt = Lt,
                      sel = sel,
                      mat_FM_Lc_com.C = mat_FM_Lc_com.C,
                      mat_FM_Lc_com.Y = mat_FM_Lc_com.Y,
                      mat_FM_Lc_com.V = mat_FM_Lc_com.V,
                      mat_FM_Lc_com.B = mat_FM_Lc_com.B,
                      mat_FM_Lc_com.SSB = mat_FM_Lc_com.SSB,
                      mat_FM_Lc_com.SPR = mat_FM_Lc_com.SPR,
                      df_Es = df_Es))

        if(!is.na(curr.E)){
          if(!is.na(curr.Lc)){
            curr.tc <- VBGF(L=curr.Lc, pars = list(Linf=Linf, K=K, t0=t0))
          }else curr.tc <- NA

          # current exploitation rate
          curr.F = (nM * curr.E)/(1-curr.E)

          if(is.na(curr.Lc)){
            sel <- FM / max(FM, na.rm = TRUE)
          }else if(!is.na(curr.Lc) | length(s_list) == 1){
            s_list <- list(selecType = "knife_edge", L50 = curr.Lc)
            sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
          }else if(!is.na(curr.Lc) | length(s_list) != 1){
            sel <- select_ogive(s_list, Lt = Lt, Lc = curr.Lc)
          }
          mati <- sel * curr.F
          parloop <- res
          parloop$par$FM <- mati
          parloop$par$Z <- mati + nM
          res2 <- stock_sim(parloop, age_unit,
                            stock_size_1, plus_group=plus_group)
          mati2 <- res2$totals

## SPR
        parloop <- res
        parloop$FM <- 0.0 * sel
        parloop$Z <- nM + parloop$FM
        res2 <- stock_sim(parloop, age_unit = age_unit,
                          stock_size_1 = stock_size_1, plus_group=plus_group)
        SSB0 <- res2$totals$meanSSB
        SPR <- mati2$meanSSB/SSB0

          df_currents <- data.frame(curr.Lc = curr.Lc,
                                    ## curr.tc = curr.tc,
                                    ## curr.E = curr.E,
                                    ## curr.F = curr.F,
                                    ## curr.C = mati2$totC,
                                    ## curr.Y = mati2$totY,
                                    ## curr.V = mati2$totV,
                                    ## curr.B = mati2$meanB)
     curr.tc = curr.tc,
                                              curr.E = curr.E,
                                              curr.F = curr.F,
                                              curr.C = mati2$totC,
                                              curr.Y = mati2$totY,
                                              curr.V = mati2$totV,
                                              curr.B = mati2$meanB,
                                              curr.SSB = mati2$meanSSB,
                                              curr.SPR = SPR)
          ret$currents <- df_currents
        }
      }
    }

    # return results and plot
    class(ret) <- "predict_mod"

    return(ret)

}

plotYPR <- function (x, type = "ypr", xaxis1 = "FM", yaxis1 = "Y_R.rel",
    yaxis2 = "B_R.rel", yaxis_iso = "L50", identify = FALSE, mark = FALSE,
    contour = TRUE, xlab = NA, ylab1 = NA, ylab2 = NA, ylab3 = NA,
    plot_refs = c("F01","Fmax","F05","F04"),
    cols_refs = c("goldenrod2","darkred","darkgreen","darkorange"),
    ...)
{
    pes <- x
    image.identifier <- function(xyz, markII = TRUE, digits = 2) {
        intiX <- (xyz$x[2] - xyz$x[1])/2
        intiY <- (xyz$y[2] - xyz$y[1])/2
        newX <- c(xyz$x - intiX, xyz$x[length(xyz$x)] + intiX)
        newY <- c(xyz$y - intiY, xyz$y[length(xyz$y)] + intiY)
        nx <- length(xyz$x)
        ny <- length(xyz$y)
        mesi <- data.frame()
        xy <- locator(1)
        while (!is.null(xy)) {
            xbin <- as.numeric(cut(xy$x, newX))
            ybin <- as.numeric(cut(xy$y, newY))
            cat("[", xbin, ",", ybin, "] = ", xyz$z[xbin, ybin],
                "\n", sep = "")
            lcc <- xy$y * pes$Linf
            mesi <- rbind(mesi, data.frame(i = xbin, j = ybin,
                x = xy$x, y = xy$y, z = xyz$z[xbin, ybin], Lc = lcc))
            rm(xy)
            xy <- locator(1)
        }
        if (markII) {
            points(mesi$x, mesi$y, pch = 19, cex = 0.5, col = "blue")
            text(mesi$x, mesi$y, format(mesi$z, digits = digits),
                adj = -0.2, col = "blue")
        }
        colnames(mesi) <- c("i", "j", p.FE, "Lc/Linf", p.yield,
            "Lc")
        mesi
    }
    if ("totY" %in% names(pes)) {
        df_Es <- pes$df_Es
        if (xaxis1 == "FM") {
            px <- pes$FM_change
            xlabel1 <- "Fishing mortality"
            if (pes$FM_relative) {
                xlabel1 <- "rel. Fishing mortality"
            }
            N01 <- df_Es$F01
            N04 <- df_Es$F04
            N05 <- df_Es$F05
            Nmax <- df_Es$Fmax
            ## legend.lab <- c("Fmax")
            ## if (length(N01) == 1) {
            ##     legend.lab <- c("F0.1",legend.lab)
            ## }
            ## if (length(N05) == 1) {
            ##     legend.lab <- c("F0.5",legend.lab)
            ## }
            refs <- c("F01","Fmax","F05","F04")
            ind <- which(refs %in% plot_refs & !is.na(df_Es[names(df_Es) %in% refs]))
            legend.lab <- refs[ind]

            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            px <- pes$E_change
            xlabel1 <- "Exploitation rate"
            if (pes$FM_relative) {
                xlabel1 <- "rel. Exploitation rate"
            }
            N05 <- df_Es$E05
            ## Nmax <- df_Es$Emax
            ## if (length(N05) == 1) {
            ##     legend.lab <- c("E0.5", "Emax")
            ## }
            ## else legend.lab <- c("Emax")
            Nmax <- df_Es$Emax
            N01 <- df_Es$E01
            N04 <- df_Es$E04

            refs <- c("E01","Emax","E05","E04")
            ind <- which(refs %in% plot_refs & !is.na(df_Es[names(df_Es) %in% refs]))
            legend.lab <- refs[ind]
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (is.na(ylab1[1])) {
            ylabel1 <- "Yield"
        }
        if (!is.na(ylab1[1])) {
            ylabel1 <- ylab1
        }
        if (is.na(ylab2[1])) {
            ylabel2 <- "Biomass"
        }
        if (!is.na(ylab2[1])) {
            ylabel2 <- ylab2
        }
        if (is.na(ylab3[1])) {
            ylabel3 <- "Value"
        }
        if (!is.na(ylab3[1])) {
            ylabel3 <- ylab3
        }
        max_val <- round(max(pes$totV, na.rm = TRUE), digits = 0)
        dim_val <- 10^(nchar(max_val) - 1)
        max_yiel <- round(max(pes$totY, na.rm = TRUE), digits = 0)
        dim_yiel <- 10^(nchar(max_yiel) - 1)
        max_bio <- round(max(pes$meanB, na.rm = TRUE), digits = 0)
        dim_bio <- 10^(nchar(max_bio) - 1)

        py <- pes$totY[1:length(px)]
        py2 <- pes$meanB[1:length(px)]
        px2 <- px

        ## NEW: TODO: this does not necessarily go through (0,0)
        ## ind <- which(pes$newdat$F >= min(px) & pes$newdat$F <= max(px))
        ## px2 <- pes$newdat$F[ind]
        ## py <- pes$newdat$YPR[ind]
        ## py2 <- pes$newdat$BPR[ind]

        ## NEW:
        yprFmax <- df_Es$YPR_Fmax
        yprF01 <- df_Es$YPR_F01
        yprF04 <- df_Es$YPR_F04
        bprF05 <- df_Es$BPR_F05
        cols <- cols_refs

        plot(px2, py, type = "l", ylab = ylabel1, xlab = xlabel1,
             col = "black", ylim = c(0, max(c(ceiling(py),ceiling(max_yiel/dim_yiel) * dim_yiel))),
             lwd = 1.6)

        ## segments(x0 = -3, x1 = Nmax, y0 = py[which(px == Nmax)],
        ##     y1 = py[which(px == Nmax)], col = "goldenrod1", lty = 2,
        ##     lwd = 1.6)
        ## segments(x0 = Nmax, x1 = Nmax, y0 = -3, y1 = py[which(px ==
        ##                                                       Nmax)], col = "goldenrod1", lty = 2, lwd = 1.6)
        ## segments(x0 = -3, x1 = N01, y0 = py[which(px == N01)],
        ##     y1 = py[which(px == N01)], col = "darkgreen", lty = 2,
        ##     lwd = 1.6)
        ## segments(x0 = N01, x1 = N01, y0 = -3, y1 = py[which(px ==
        ##     N01)], col = "darkgreen", lty = 2, lwd = 1.6)


      ## F or E max
      if(any(plot_refs == "F01")){
          points(N01,yprF01, pch = 21, col=cols[1], bg=cols[1])
          segments(x0 = N01, x1 = N01, y0 = -1000, y1 = yprF01,
                   col= cols[1],lty = 3, lwd=1.6)
          ## abline(h = yprF01,
          ##        col= 'goldenrod1',lty = 2, lwd=1.6)
      }
      if(any(plot_refs == "Fmax")){
          ## segments(x0 = -1, x1 = Nmax, y0 = yprFmax,
          ##          y1 = yprFmax,
          ##          col= 'goldenrod1',lty = 2, lwd=1.6)
          points(Nmax,yprFmax, pch = 22, col=cols[2], bg=cols[2])
          segments(x0 = Nmax, x1 = Nmax, y0 = -1000, y1 = yprFmax,
                   col= cols[2],lty = 3, lwd=1.6)
          ## abline(h = yprFmax,
          ##        col= 'goldenrod1',lty = 2, lwd=1.6)
      }
      if(any(plot_refs == "F04")){
          points(N04,yprF04, pch = 24, col=cols[4], bg=cols[4])
          segments(x0 = N04, x1 = N04, y0 = -1000, y1 = yprF04,
                   col= cols[4],lty = 3, lwd=1.6)
          ## abline(h = yprF04,
          ##        col= 'goldenrod1',lty = 2, lwd=1.6)
      }

        if (!is.null(pes$currents) & mark) {
            currents <- pes$currents
            if (!is.na(currents$curr.E) & yaxis1 == "Y_R" | yaxis1 ==
                "Y_R.rel") {
                px1 <- ifelse(xaxis1 == "FM", currents$curr.F,
                  currents$curr.E)
                py1 <- currents$curr.Y
                ## points(px1, py1, pch = 16, col = "grey30")
                ## abline(v = px1, col = "grey30", lty = 2)
                points(px1,py1, pch = 4, col="grey30")
                segments(x0=px1, x1=px1, y0 = -1000, y1 = py1,
                         col="grey30",lty=3, lwd=1.6)
            }
        }
        ## par(new = TRUE)
        ## plot(px, py2, type = "l", ylab = "", xlab = "", col = "blue",
        ##     lwd = 1.6, axes = FALSE)
        ## axis(4, at = pretty(c(0, max(pes$meanB))), col = "blue",
        ##     col.axis = "blue")
        ## mtext(ylabel2, side = 4, line = 2.5, col = "blue", cex = 1)
        ## segments(x0 = -1, x1 = N05, y0 = py2[which(px == N05)],
        ##     y1 = py2[which(px == N05)], col = "red", lty = 2,
        ##     lwd = 1.5)
        ## segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px ==
        ##     N05)], col = "red", lty = 2, lwd = 1.5)
        ## Biomass
        par(new=TRUE)
        plot(px2,py2,type ='l',ylab='',xlab='',
             col='darkblue',lwd=1.6,axes=FALSE)
        axis(4,at=pretty(c(0,max(pes$meanB))),
             col = "darkblue", col.axis="darkblue")
        mtext(ylabel2, side=4, line=2.5, col = "darkblue", cex=1)

        ## F or E 05
        if(any(plot_refs == "F05")){
            ## segments(x0 = 1000, x1 = N05, y0 = bprF05, y1 = bprF05,
            ##        col= 'red',lty = 3, lwd=1.5)
            points(N05,bprF05, pch = 25, col=cols[3], bg=cols[3])
            segments(x0 = N05, x1 = N05, y0 = -1000, y1 = bprF05,
                     col= cols[3],lty = 3, lwd=1.5)
        }
        if (!is.null(pes$currents) & mark) {
            currents <- pes$currents
            if (!is.na(currents$curr.E) & yaxis1 == "B_R" | yaxis1 ==
                "B_R.rel") {
                px1 <- ifelse(xaxis1 == "FM", currents$curr.F,
                  currents$curr.E)
                py1 <- currents$curr.B
                ## points(px1, py1, pch = 16, col = "grey30")
                ## abline(v = px1, col = "grey30", lty = 2)
                points(px1,py1, pch = 4, col="grey30")
                segments(x0=px1, x1=px1, y0 = -1000, y1 = py1,
                         col="grey30",lty=3, lwd=1.6)
            }
        }
        ## legend("top", legend = legend.lab, xpd = TRUE, horiz = TRUE,
        ##     inset = c(0, 0), bty = "n", lty = c(2), col = c("red","darkgreen",
        ##         "goldenrod1"), seg.len = 1, pt.cex = 2, x.intersp = c(0.7,0.7,
        ##         0.7), merge = TRUE, y.intersp = -2, box.lty = 0,
        ##     cex = 1, lwd = 2)
        ## Legend
        if(any(!is.na(plot_refs)) && plot_refs[1] != FALSE){

            legend("top", legend = legend.lab, xpd = TRUE, horiz = TRUE,
                   bty = "n", lty = 3,
                   col = cols[ind], pt.bg = cols[ind],
                   pch = c(21,22,25,24)[ind],
                   seg.len = 2.5, x.intersp = 0.3, merge=TRUE,
                   y.intersp = -2.5, box.lty=0, cex=0.9, lwd =2)
        }
        if (any(pes$totV != 0)) {
            py3 <- pes$totV[1:length(px)]
            par(new = TRUE)
            plot(px, py3, type = "l", axes = FALSE, ylab = "",
                xlab = "", col = "darkgreen", lwd = 1.6, ylim = c(0,
                  ceiling(max_val/dim_val) * dim_val))
            axis(4, at = pretty(c(0, pes$totV)), line = 3.6,
                col.axis = "darkgreen", col = "darkgreen")
            mtext(ylabel3, side = 4, line = 5.7, col = "darkgreen")
        }
        box(lwd=1.2)
    }
    if ("mat_FM_Lc_com.C" %in% names(pes)) {
        p.yield <- yaxis1
        p.FE <- xaxis1
        p.B <- yaxis2
        xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality",
            "Exploitation rate")
        if (pes$FM_relative) {
            xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality",
                "rel. Exploitation rate")
        }
        ylabel_iso <- ifelse(yaxis_iso == "L50", "L50", "Lc / Linf")
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (!is.na(ylab1[1])) {
            ylabel_iso <- ylab1
        }
        Lc_change <- pes$Lc_change
        FM_change <- pes$FM_change
        if (p.FE == "FM") {
            px <- FM_change
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            px <- FM_change/(FM_change + pes$M)
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (p.yield == "Y_R.rel" | p.yield == "Y_R") {
            pz <- pes$mat_FM_Lc_com.Y
        }
        else if (p.yield == "B_R.rel" | p.yield == "B_R") {
            pz <- pes$mat_FM_Lc_com.B
        }
        else if (p.yield == "value") {
            pz <- pes$mat_FM_Lc_com.V
        }
        else if (p.yield == "catch") {
            pz <- pes$mat_FM_Lc_com.C
        }
        pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5), rgb(1,
            1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1))))
        if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
            pal <- colorRampPalette(c(rgb(1, 0.5, 0.5), rgb(1,
                1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5, 1)))
        }
        m <- list(x = px, y = Lc_change, z = pz)
        if ("currents" %in% names(pes))
            curr_markY <- pes$currents$curr.Lc
        if (yaxis_iso != "L50" & "Linf" %in% names(pes)) {
            m$y <- Lc_change/pes$Linf
            if ("currents" %in% names(pes))
                curr_markY <- pes$currents$curr.Lc/pes$Linf
        }
        if (identify == TRUE) {
            dev.new(noRStudioGD = TRUE)
        }
        image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
        if (is.numeric(contour)) {
            contour(m, add = TRUE, nlevels = contour)
        }
        else if (contour == TRUE) {
            contour(m, add = TRUE)
        }
        if ("currents" %in% names(pes) & mark) {
            points(x = curr_markX, y = curr_markY, pch = 16,
                col = "grey30")
            abline(v = curr_markX, col = "grey30", lty = 2)
            abline(h = curr_markY, col = "grey30", lty = 2)
        }
        if (identify == TRUE)
            image.identifier(m)
    }
    if ("list_Lc_runs" %in% names(pes) | "list_tc_runs" %in%
        names(pes)) {
        FM <- pes$FM
        if ("tc" %in% names(pes))
            if (!is.null(pes$tc))
                tc_Lc <- pes$tc
        if ("Lc" %in% names(pes))
            if (!is.null(pes$Lc))
                tc_Lc <- pes$Lc
        if (is.null(pes$tc) & is.null(pes$Lc))
            tc_Lc <- names(pes$list_Lc_runs)
        if ("list_tc_runs" %in% names(pes))
            list_tc_Lc_runs <- pes$list_tc_runs
        if ("list_Lc_runs" %in% names(pes))
            list_tc_Lc_runs <- pes$list_Lc_runs
        p.yield <- yaxis1
        p.FE <- xaxis1
        p.B <- yaxis2
        xlabel1 <- ifelse(xaxis1 == "FM", "Fishing mortality",
            "Exploitation rate")
        if (pes$FM_relative) {
            xlabel1 <- ifelse(xaxis1 == "FM", "rel. Fishing mortality",
                "rel. Exploitation rate")
        }
        ylabel1 <- ifelse(yaxis1 == "Y_R", "Y/R", "rel. Y/R")
        ylabel2 <- ifelse(yaxis2 == "B_R", "B/R", "B/R [%]")
        ylabel_iso <- ifelse(yaxis_iso == "L50", "L50", "Lc / Linf")
        if (!is.na(xlab[1])) {
            xlabel1 <- xlab
        }
        if (!is.na(ylab1[1])) {
            ylabel1 <- ylab1
        }
        if (!is.na(ylab2[1])) {
            ylabel2 <- ylab2
        }
        if (!is.na(ylab1[1])) {
            ylabel_iso <- ylab1
        }
        df_Es <- pes$df_Es
        if (xaxis1 == "FM") {
            N01 <- df_Es$F01
            N05 <- df_Es$F05
            Nmax <- df_Es$Fmax
            if (length(N05) == 1) {
                legend.lab <- c("F0.1", "F0.5", "Fmax")
            }
            else legend.lab <- c("F0.1", "Fmax")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.F
        }
        else {
            N01 <- df_Es$E01
            N05 <- df_Es$E05
            Nmax <- df_Es$Emax
            if (length(N05) == 1) {
                legend.lab <- c("E0.1", "E0.5", "Emax")
            }
            else legend.lab <- c("E0.1", "Emax")
            if ("currents" %in% names(pes))
                curr_markX <- pes$currents$curr.E
        }
        if (type == "ypr") {
            label <- ifelse("L50" %in% names(pes), "L50", "t50")
            tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            py <- p.dat[, which(names(p.dat) == p.yield)]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            offset_text <- py[length(py)] * 0.05
            offset_x <- py[length(px)] * 0.1
            runs <- sapply(strsplit(names(list_tc_Lc_runs), split = "_"),
                "[[", 2)
            plot(px, py, type = "l", ylim = c(0, max(py, na.rm = TRUE) *
                1.2), ylab = ylabel1, xlab = xlabel1, lty = tc_Lc_start,
                ...)
            text(x = px[length(px)] - offset_x, y = py[length(py)] +
                offset_text, labels = bquote(.(label)[.(round(as.numeric(as.character(runs[tc_Lc_start])),
                2))]))
            seq_tc_Lc <- 1:length(list_tc_Lc_runs)
            seq_tc_Lc <- seq_tc_Lc[-tc_Lc_start]
            for (j in seq_tc_Lc) {
                p.dat <- list_tc_Lc_runs[[j]]
                py <- p.dat[, which(names(p.dat) == p.yield)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1,
                  lty = j)
                text(x = px[length(px)], y = (py[length(py)] +
                  offset_text), labels = bquote(.(label)[.(round(as.numeric(as.character(runs[j])),
                  2))]))
            }
            if (length(N01) == 1) {
                p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
                py <- p.dat[, which(names(p.dat) == p.yield)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                if (N01 == Nmax) {
                  add_shift <- 1.005
                }
                else add_shift <- 1
                segments(x0 = -1, x1 = N01, y0 = py[which(px ==
                  N01)], y1 = py[which(px == N01)], col = "darkgreen",
                  lty = 1, lwd = 1.5)
                segments(x0 = N01, x1 = N01, y0 = -1, y1 = py[which(px ==
                  N01)], col = "darkgreen", lty = 1, lwd = 1.5)
                segments(x0 = -1, x1 = Nmax, y0 = py[which(px ==
                  Nmax)] * add_shift, y1 = py[which(px == Nmax)] *
                  add_shift, col = "goldenrod1", lty = 2, lwd = 1.5)
                segments(x0 = Nmax * add_shift, x1 = Nmax * add_shift,
                  y0 = -1, y1 = py[which(px == Nmax)], col = "goldenrod1",
                  lty = 2, lwd = 1.5)
                if (length(N05) == 1) {
                  legend("top", legend = legend.lab, xpd = TRUE,
                    horiz = TRUE, inset = c(0, 0), bty = "n",
                    lty = c(1, 3, 2), col = c("darkgreen", "red",
                      "goldenrod1"), seg.len = 1, pt.cex = 2,
                    x.intersp = c(0.7, 0.7, 0.7), merge = TRUE,
                    y.intersp = -2, box.lty = 0, cex = 0.8, lwd = 2)
                }
                else {
                  legend("top", legend = legend.lab, xpd = TRUE,
                    horiz = TRUE, inset = c(0, 0), bty = "n",
                    lty = c(1, 2), col = c("darkgreen", "goldenrod1"),
                    seg.len = 1, pt.cex = 2, x.intersp = c(0.7,
                      0.7, 0.7), merge = TRUE, y.intersp = -2,
                    box.lty = 0, cex = 0.8, lwd = 2)
                }
                if (!is.null(pes$currents)) {
                  currents <- pes$currents
                  if (!is.na(currents$curr.E)) {
                    px <- ifelse(p.FE == "FM", currents$curr.F,
                      currents$curr.E)
                    py <- ifelse(p.yield == "Y_R", currents$curr.YPR,
                      currents$curr.YPR.rel)
                    points(px, py, pch = 16)
                    abline(v = px, col = "grey30", lty = 2)
                  }
                }
            }
            par(new = T)
            px <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) ==
                p.FE)]
            py <- list_tc_Lc_runs[[1]][, which(names(list_tc_Lc_runs[[1]]) ==
                p.B)]
            plot(px, py, type = "l", axes = F, ylab = "", xlab = "",
                lty = tc_Lc_start, col = "blue")
            axis(side = 4, at = pretty(range(py, na.rm = TRUE)),
                col = "blue", col.axis = "blue")
            mtext(side = 4, text = ylabel2, line = 3, col = "blue")
            for (j in seq_tc_Lc) {
                p.dat <- list_tc_Lc_runs[[j]]
                py <- p.dat[, which(names(p.dat) == p.B)]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                lines(px, py, type = "l", ylab = ylabel1, xlab = xlabel1,
                  col = "blue", lty = j)
            }
            if (length(N05) == 1) {
                p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
                px <- p.dat[, which(names(p.dat) == p.FE)]
                py2 <- p.dat[, which(names(p.dat) == p.B)]
                segments(x0 = -1, x1 = N05, y0 = py2[which(px ==
                  N05)], y1 = py2[which(px == N05)], col = "red",
                  lty = 3, lwd = 1.5)
                segments(x0 = N05, x1 = N05, y0 = -1, y1 = py2[which(px ==
                  N05)], col = "red", lty = 3, lwd = 1.5)
            }
        }
        if (type == "Isopleth") {
            tc_Lc_start <- which(tc_Lc == max(tc_Lc, na.rm = T))
            p.dat <- list_tc_Lc_runs[[tc_Lc_start]]
            px <- p.dat[, which(names(p.dat) == p.FE)]
            if (length(N01) > 1) {
                Lc_change <- pes$Lc
                list_Lc_runs <- pes$list_Lc_runs
                Y_R.vec <- vector("list", length(list_Lc_runs))
                for (fx in 1:length(list_Lc_runs)) {
                  yr <- list_Lc_runs[[fx]][, which(names(list_Lc_runs[[fx]]) ==
                    p.yield)]
                  Y_R.vec[[fx]] <- yr
                }
                mat_FM_Lc_com.Y <- as.matrix(do.call(cbind, Y_R.vec))
                rownames(mat_FM_Lc_com.Y) <- round(px, digits = 2)
                colnames(mat_FM_Lc_com.Y) <- round(Lc_change/pes$Linf,
                  digits = 2)
                m <- list(x = px, y = Lc_change, z = mat_FM_Lc_com.Y)
                if ("currents" %in% names(pes))
                  curr_markY <- pes$currents$curr.Lc
                if (yaxis_iso != "L50" & "Linf" %in% names(pes)) {
                  m$y <- Lc_change/pes$Linf
                  if ("currents" %in% names(pes))
                    curr_markY <- pes$currents$curr.Lc/pes$Linf
                }
                pal <- colorRampPalette(rev(c(rgb(1, 0.5, 0.5),
                  rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5, 0.5,
                    1))))
                if (yaxis1 == "B_R" | yaxis1 == "B_R.rel") {
                  pal <- colorRampPalette(c(rgb(1, 0.5, 0.5),
                    rgb(1, 1, 0.5), rgb(0.5, 1, 1), rgb(0.5,
                      0.5, 1)))
                }
                if (identify == TRUE) {
                  dev.new(noRStudioGD = TRUE)
                }
                image(m, col = pal(100), xlab = xlabel1, ylab = ylabel_iso)
                if (is.numeric(contour)) {
                  contour(m, add = TRUE, nlevels = contour)
                }
                else if (contour == TRUE) {
                  contour(m, add = TRUE)
                }
                if ("currents" %in% names(pes) & mark) {
                  points(x = curr_markX, y = curr_markY, pch = 16,
                    col = "grey30")
                  abline(v = curr_markX, col = "grey30", lty = 2)
                  abline(h = curr_markY, col = "grey30", lty = 2)
                }
                if (identify == TRUE)
                  image.identifier(m)
            }
        }
    }
}
