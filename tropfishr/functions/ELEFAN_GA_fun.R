ELEFAN_GA_shiny <- function(
  x,
  seasonalised = FALSE,
  low_par = NULL,
  up_par = NULL,
  popSize = 50,
  maxiter = 100,
  run = maxiter,
  parallel = FALSE,
  pmutation = 0.2,
  pcrossover = 0.8,
  elitism = base::max(1, round(popSize*0.05)),
  MA = 5,
  addl.sqrt = FALSE,
  agemax = NULL,
  flagging.out = TRUE,
  seed = NULL,
  ...
){

  lfq <- x
  classes <- lfq$midLengths
  n_classes <- length(classes)
  Linf_est <- classes[n_classes]


  # ELEFAN 0
  lfq <- lfqRestructure(lfq, MA = MA, addl.sqrt = addl.sqrt)

  # seasonalised fitness function
  sofun <- function(lfq, par, agemax, flagging.out){
    Lt <- lfqFitCurves(lfq,
                       par=list(Linf=par[1], K=par[2], ta=par[3], C=par[4], ts=par[5]),
                       agemax = agemax, flagging.out = flagging.out)
    return(Lt$fESP)
  }
  # non-seasonalised fitness function
  fun <- function(lfq, par, agemax, flagging.out){
    Lt <- lfqFitCurves(lfq,
                       par=list(Linf=par[1], K=par[2], ta=par[3], C = 0, ts = 0),
                       agemax = agemax, flagging.out = flagging.out)
    return(Lt$fESP)
  }


  # Genetic algorithm
  if(seasonalised){
    min = low_par
    max = up_par

    fit <- GA::ga(
      type = "real-valued",
      fitness = sofun, lfq=lfq,
      lower = min,
      upper = max,
      agemax = agemax,
      flagging.out = flagging.out,
      popSize = popSize, maxiter = maxiter, run = run, parallel = parallel,
      pmutation = pmutation, pcrossover = pcrossover, elitism = elitism,
      ...
    )
    pars <- as.list(fit@solution[1,])
    names(pars) <- c("Linf", "K", "ta", "C", "ts")
  }else{
    min = low_par
    max = up_par

    fit <- GA::ga(
      type = "real-valued",
      fitness = fun,
      lfq = lfq,
      lower = min,
      upper = max,
      agemax = agemax,
      flagging.out = flagging.out,
      popSize = popSize, maxiter = maxiter, run = run, parallel = parallel,
      pmutation = pmutation, pcrossover = pcrossover, elitism = elitism,
      seed = seed,
      ...
    )
    pars <- as.list(fit@solution[1,])
    names(pars) <- c("Linf", "K", "ta")
  }

  final_res <- lfqFitCurves(lfq = lfq, par=pars,
                            flagging.out = flagging.out,
                            agemax = agemax)

  # growth performance index
  phiL <- log10(pars$K) + 2 * log10(pars$Linf)
  pars$phiL <- phiL

  # Results
  lfq$ncohort <- final_res$ncohort
  lfq$agemax <- final_res$agemax
  lfq$pars <- pars
  lfq$Rn_max <- fit@fitnessValue

  # fit$lfq <- lfq
  return(fit)
}




shinyMonitor <- function(object, digits = getOption("digits")){
    shiny::incProgress(1/object@maxiter, detail = paste0("Iteration: ", object@iter,"/",object@maxiter))
}
