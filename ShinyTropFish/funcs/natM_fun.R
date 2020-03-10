natM_shiny <- function(Linf = NULL, K_l = NULL, temp = NULL, tmax = NULL,
                       schooling=FALSE, method){
  
  temp <- as.numeric(temp)
    
    if(method == "Pauly_Linf"){
      mEst  <- round(10^(-0.0066 - 0.279 * log10(Linf) + 0.6543 * log10(K_l) + 0.4634 * log10(temp)), 3) 
      if(schooling == TRUE){
        mEst <- 0.8 * mEst
      }
    }
    if (method == "Then_growth") {
      mEst <- round(4.118 * (K_l^0.73) * (Linf^-0.33), 3)
    }
    if (method == "Then_tmax") {
        mEst <- round(4.899 * tmax^-0.916, 3)
    }    
    return(mEst)
}
