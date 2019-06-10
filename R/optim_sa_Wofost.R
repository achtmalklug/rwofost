#' WOFOST parameter optimisation
#' @param parameters named vector: the parameters with their values to set
#' @param parfiles character vector: which parfiles to manipulate
#' @description Optimisation of WOFOST crop and management parameters (for calibration). This wraps \code{runWofost} for using with \code{optimization::optim_sa}. Since runWofost needs some extra info that optim_sa does not include in the function call, some Global variables are set for this.
#' @param parfiles chracter vectore with the parameter files to be manipulated
#' @param wofostbin wofost executable (under unixoid systems use "wine wofost.exe")
#' @param wofostdir path in which the wofost executable resides (without the actual execuatable name)
#' @param qualityfun the function to be evaluated after wofost run
#' @param lower named numeric vector with the lower bounds (names must be the names of the parameters to manipulate)
#' @param upper named numeric vector with the upper bounds
#' @param start named numeric vector with the start values
#' @param control the control list passed to optim_sa
#' @param maximization maximization instead of minimization of the
#' @return The WOFOST output file
#' @export
optim_sa_Wofost<-function(parfiles,wofostbin=.GlobalEnv$wofostbin,wofostdir=.GlobalEnv$wofostdir,qualityfun=.GlobalEnv$qualityfun,start,lower,upper,control = list(nlimit=1,maxgood=1),maximization = T){
  .GlobalEnv$wofostbin<-wofostbin
  .GlobalEnv$parnames<-names(start)
  .GlobalEnv$wofostdir<-wofostdir
  .GlobalEnv$qualityfun<-qualityfun
  .GlobalEnv$parfiles<-parfiles
  .GlobalEnv$run<-0
  optim_sa(wrapWofostOptim,start = start,lower=lower,upper=upper,control=control,maximization = T)
}
