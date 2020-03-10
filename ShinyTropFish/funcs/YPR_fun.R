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
        
        pred_res_df <- do.call(rbind, pred_res_list)
        pred_res_df$FM_change <- FM_change
        pred_res_df$E_change <- E_change
        
        res2 <- pred_res_df
        res3 <- c(res,res2)
        
        
        ## reference points
        ## Fmax
        Nmax <- which.max(pred_res_df$totY)
        ## F01 (proxy for Fmsy in ICES)
        slopeOrg <- (pred_res_df$totY[2] - pred_res_df$totY[1]) / (FM_change[2] - FM_change[1])
        slope01 <- round(0.1*slopeOrg, 2)
        slopes <- rep(NA, length(FM_change))
        slopes[1] <- slopeOrg
        for(i in 3:length(FM_change)){
          slopes[i-1] <- round((pred_res_df$totY[i] - pred_res_df$totY[i-1]) /
                                 (FM_change[i] - FM_change[i-1]),2)
        }
        dif <- abs(slopes - slope01)
        dif[is.na(dif)] <- 1e+11
        difpot <- dif[1:Nmax]
        N01 <- which.min(difpot)
        ## F05
        Bper <- rep(NA,length(pred_res_df$meanB))
        Bper[1] <- 100
        for(ix in 2:length(Bper)){
          Bper[ix] <- pred_res_df$meanB[ix]/pred_res_df$meanB[1] * 100
        }
        N05 <- which.min(abs(Bper - 50))
        
        if(!is.null(Lc[1]) & !is.null(tc[1])){
          df_Es <- data.frame(Lc = Lc,
                              tc = tc,
                              F01 = FM_change[N01],                                    
                              Fmax = FM_change[Nmax],
                              F05 = FM_change[N05],
                              E01 = E_change[N01],                                    
                              Emax = E_change[Nmax],
                              E05 = E_change[N05])
        }else{
          df_Es <- data.frame(F01 = FM_change[N01],
                              Fmax = FM_change[Nmax],
                              F05 = FM_change[N05],
                              E01 = E_change[N01],                      
                              Emax = E_change[Nmax],
                              E05 = E_change[N05])
        }
        
        
        ret <- c(res3, list(df_Es = df_Es))
        
        
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
          
          df_currents <- data.frame(curr.Lc = curr.Lc,
                                    curr.tc = curr.tc,
                                    curr.E = curr.E,
                                    curr.F = curr.F,
                                    curr.C = mati2$totC,
                                    curr.Y = mati2$totY,
                                    curr.V = mati2$totV,
                                    curr.B = mati2$meanB)
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
        
        pred.FM_Lc_com_res_loopC_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopY_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopB_list <- vector("list",length(FM_Lc_com_mat.list))
        pred.FM_Lc_com_res_loopV_list <- vector("list",length(FM_Lc_com_mat.list))
        
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
          prev_mat <- do.call(rbind, pred.FM_Lc_com_res_loop1_list)
          prev_matC <- prev_mat[,'totC']
          prev_matY <- prev_mat[,'totY']
          prev_matB <- prev_mat[,'meanB']
          prev_matV <- prev_mat[,'totV']
          
          pred.FM_Lc_com_res_loopC_list[[x21]] <- prev_matC
          pred.FM_Lc_com_res_loopY_list[[x21]] <- prev_matY
          pred.FM_Lc_com_res_loopB_list[[x21]] <- prev_matB
          pred.FM_Lc_com_res_loopV_list[[x21]] <- prev_matV
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
        
        #for value
        mat_FM_Lc_com.V <- do.call(rbind, pred.FM_Lc_com_res_loopV_list)
        rownames(mat_FM_Lc_com.V) <- Lc
        colnames(mat_FM_Lc_com.V) <- FM_change
        
        # transvers matrices for plotting (the opposite arrangement from book)
        mat_FM_Lc_com.C <- t(mat_FM_Lc_com.C)
        mat_FM_Lc_com.Y <- t(mat_FM_Lc_com.Y)
        mat_FM_Lc_com.B <- t(mat_FM_Lc_com.B)
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
          
          df_currents <- data.frame(curr.Lc = curr.Lc,
                                    curr.tc = curr.tc,
                                    curr.E = curr.E,
                                    curr.F = curr.F,
                                    curr.C = mati2$totC,
                                    curr.Y = mati2$totY,
                                    curr.V = mati2$totV,
                                    curr.B = mati2$meanB)
          ret$currents <- df_currents
        }
      }
    }
    
    # return results and plot
    class(ret) <- "predict_mod"
    
    return(ret)
  
}
