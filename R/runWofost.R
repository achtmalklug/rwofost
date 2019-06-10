#' Run WOFOST simulation
#' @param bindir
#' @param wofostbin wofost executable (under unixoid systems use "wine wofost.exe")
#' @param wofostdir path in which the wofost executable resides (without the actual execuatable name)
#' @export
runWofost<-function(wofostdir=.GlobalEnv$wofostdir,wofostbin=.GlobalEnv$wofostbin) {
  oldwd<-getwd()
  setwd(wofostdir)
  outfile<-file.path("OUTPUT",gsub("'","",readWofostParfile(file.path("RUNIO","RUNOPT.DAT"))$WOFOUT))
  if (system(wofostbin,ignore.stdout = T)) stop("Wofost run error")
  if(!file.exists(outfile)){outfile=file.path(dirname(outfile),tolower(basename(outfile)))}
  setwd(oldwd)
  readWofostOutput(file.path(wofostdir,outfile))
}

