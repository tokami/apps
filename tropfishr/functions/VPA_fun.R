VPA_shiny <- function(x,
                catch_columns = NA, catch_corFac = NA,
                terminalF = NA, plus_group = FALSE,
                LW_unit = "g", catch_unit = NA,
                analysis_type = "VPA", 
                algorithm = "new"
                ){
    res <- x
    
    catch_columns <- as.numeric(catch_columns)
    catch_corFac <- as.numeric(catch_corFac)
    terminalF <- as.numeric(terminalF)
    
    if(is.na(catch_columns[1])) catch <- res$catch
    if(!is.na(catch_columns[1])){
      catchmat <- res$catch[,(catch_columns)]
      if(length(catch_columns) > 1){
        catch <- rowSums(catchmat, na.rm = TRUE)
      }else catch <- catchmat
    }

      
      classes <- as.character(res$midLengths)
      
      
        Linf <- res$par$Linf
        K <- res$par$K
        t0 <- ifelse("t0" %in% names(res$par), res$par$t0, 0)
        C <- ifelse("C" %in% names(res$par), res$par$C, 0)
        ts <- ifelse("ts" %in% names(res$par), res$par$ts, 0)


      a <- res$a
      b <- res$b
      M <- res$M
      if(length(M) == length(classes)){
        M_vec <- M
      }else if(length(M) > 1){
        writeLines(noquote("The number of natural mortality values does not correspond to the number of length classes. \nOnly the first value will be used."))
        M_vec <- rep(M[1],length(classes))
      }else{
        M_vec <- rep(M, length(classes))
      }

      terminalE <- terminalF / (terminalF + M_vec[length(M_vec)])

      terminalZ <- terminalF + M_vec[length(M_vec)]
      
      # correct catch with raising factor
      if(!is.na(catch_corFac)) catch_cor <- catch * catch_corFac
      if(is.na(catch_corFac)) catch_cor <- catch
      
      # create column without plus group (sign) if present
      classes.num <- do.call(rbind, strsplit(classes, split="\\+"))
      classes.num <- as.numeric(classes.num[,1])
      
      #calculate size class interval
      interval <- classes.num[2] - classes.num[1]
      
      # lower and upper length vectors
      lowerLength <- classes.num - (interval / 2)
      upperLength <- classes.num + (interval / 2)
      if(plus_group) upperLength[length(upperLength)] <- Linf
      
      #Mean body weight
      # FAO manual:
      meanBodyWeight <- a * ((lowerLength + upperLength)/2)^b    # a * classes.num ^ b
      # same as what provided in FAO manual: a * ((lowerLength + upperLength)/2)^b
      #meanBodyWeight <- meanBodyWeight / 1000  # in kg
      #according to Beyer (1987) (FISAT II)
      # meanBodyWeight <- (1/(upperLength - lowerLength)) * (a / (b + 1)) * (upperLength^(b+1) - lowerLength^(b+1))
      
      # translate catch in tons into numbers
      if(catch_unit %in% c("tons", "t", "T", "Tons", "tonnes", "Tonnes")){
        catch_numbers <- (catch_cor * 1000) / meanBodyWeight
      }else if(catch_unit %in% c("kg", "Kg", "KG", "kilo", "KILO", "kilogramm", "Kilogramm")){
        catch_numbers <- catch_cor / meanBodyWeight
      }else if(catch_unit %in% c("'000","1000","1e3")){
        catch_numbers <- catch_cor * 1000
      }else if(catch_unit %in% c("'000000","1000000","1e6","'000.000")){
        catch_numbers <- catch_cor * 1000000
      }else if(!is.na(catch_unit)){
        stop(paste0(catch_unit, " not known. Please use either 'tons' or 'kg' for catch in weight or NA, '000, or '000000 for catch in numbers."))
      }else{
        catch_numbers <- catch_cor
      }
      
      # t of lower length classes
      t_L1 <- t0 - (1/K) * log(1 - (lowerLength / Linf))
      
      # t of lower upper classes
      t_L2 <- t0 - (1/K) * log(1 - (upperLength / Linf))
      if(upperLength[length(upperLength)] > Linf){
        writeLines(noquote("Upper limit of last length class is larger than Linf, \nconsider creating lower plus group or set the argument plus_group = TRUE."))
      }
      
      # delta t
      # dt <- t_L2 - t_L1
      dt <- (1/K) * log((Linf - lowerLength)/(Linf - upperLength))
      
      #Survivors
      survivors <- rep(NA, length(classes.num))
      
      # survivors last size class
      lastLengthClass <- max(which(!is.na(catch_numbers)),na.rm=TRUE)
      if(plus_group) survivors[length(survivors)] <- catch_numbers[length(survivors)] / terminalE
      if(!plus_group){
        survivors[length(survivors)] <- catch_numbers[length(survivors)] /
          (terminalE * (1 - exp(-terminalZ * dt[length(survivors)])))
      }

      
      ###  Jones' Length-based Cohort Analysis
      if(analysis_type == "CA"){
        # H (L1,L2)   #H(L1,L2)=((Linf-L1)/Linf-L2)^(M/2K)
        H <- ((Linf - lowerLength)/(Linf - upperLength))^(M_vec/(2*K))
        
        # other survivors
        for(x3 in (length(survivors)-1):1){
          survivors[x3] <- (survivors[x3+1] * H[x3] + catch_numbers[x3]) * H[x3]
        }
        
        # F/Z  #F(L1,L2)/Z(L1,L2)=C(L1,L2)/(N(L1)-N(L2))
        deads <- abs(diff(survivors))
        F_Z <- catch_numbers[-length(survivors)] / deads
        F_Z[length(survivors)] <- terminalE
        
        #F  # F = M * (F_Z / 1-F_Z)
        FM_calc <- M_vec * F_Z / (1 - F_Z)
      }
      
      ###  Length-based VPA
      if(analysis_type == "VPA"){
        #other survivors and fishing mortality
        FM_calc <- rep(NA,length(classes.num))
        FM_calc[lastLengthClass] <- terminalF
        
        for(num_class in (lastLengthClass-1):1){
          
          sur.C <- catch_numbers[num_class]
          sur.Ntplus1 <- survivors[(num_class+1)]
          sur.M <- M_vec[num_class]
          sur.dt <- dt[num_class]
          LHS <-  sur.C / sur.Ntplus1
          sur.F <- 0
          
          if(algorithm == "old"){
            LHS <-  sur.C / sur.Ntplus1
            sur.F <- 0
            seqi <- c(1e-1,1e-2,1e-3,1e-4,1e-5,1e-6,1e-7)
            #trail and error
            for(y in seqi){
              stepi <- y
              for(x in seq(sur.F,10,stepi)){
                sur.F <- x
                RHS <- (sur.F/(sur.F + sur.M)) * (exp(sur.F+sur.M) * sur.dt - 1)
                if(LHS-RHS < 0) break
              }
              sur.F = x-stepi
            }
          }
          
          if(algorithm == "new"){
            Fcalc <- function(sur.F=sur.M){
              ((sur.F/(sur.F+sur.M)) * (exp((sur.F+sur.M) * sur.dt) - 1) - (sur.C / sur.Ntplus1))^2
            }
            tmp <- optimise(f = Fcalc, interval=c(0,100))
            sur.F <- tmp$min
          }
          
          #fill F
          FM_calc[num_class] <- sur.F
          
          #fill survivors
          survivors[num_class] <- survivors[(num_class+1)] *
            exp((sur.F + sur.M) * sur.dt)
        }
      }
      
      # Z
      Z <- M_vec + FM_calc
      
      #Annual mean Nr
      deads <- abs(diff(survivors))
      annualMeanNr <- deads / Z[-length(survivors)]
      # annualMeanNr[length(survivors)] <- NA
      annualMeanNr[length(survivors)] <- survivors[length(survivors)] / Z[length(survivors)]
      
      #Mean biomass
      meanBiomass <- annualMeanNr * meanBodyWeight
      meanBiomassTon <- meanBiomass / 1000
      
      #Yield
      yield <- catch_numbers * meanBodyWeight
      yieldTon <- yield / 1000
      
      #FOR PLOT
      #Survivors rearranged
      survivors_rea <- rep(NA,length(classes.num))
      for(x8 in 1:(length(survivors_rea)-1)){
        survivors_rea[x8] <- survivors[x8+1]
      }
      survivors_rea[length(survivors_rea)] <- 0
      
      #Calculate natural losses
      natLoss <- survivors - survivors_rea - catch_numbers
      
      #put together in dataframe
      df.VPAnew <- data.frame(survivors = survivors_rea,
                              nat.losses = natLoss,
                              catch = catch_numbers,
                              FM_calc = FM_calc,
                              meanBodyWeight = meanBodyWeight,
                              meanBiomassTon = meanBiomassTon)
      
      #transpose matrix for barplot function
      df.VPAnew <- t(as.matrix(df.VPAnew))
      colnames(df.VPAnew) <- classes.num
      
      #save all in list
      ret <- c(res,list(
        classes.num = classes.num,
        FM_calc = FM_calc,
        Z = Z,
        meanBodyWeight = meanBodyWeight,
        survivors_L1 = survivors,
        survivors_L2 = survivors_rea,
        catch_numbers = catch_numbers,
        annualMeanNr = annualMeanNr,
        
        meanBiomassTon = meanBiomassTon,
        
        yieldTon = yieldTon,
        natLoss = natLoss,
        plot_mat = df.VPAnew))
      
      class(ret) <- "VPA"
      
      
      return(ret)
      
}
