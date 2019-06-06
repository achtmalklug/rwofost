#' wrap WOFOST run for optimisation functions
#' @param parameters named vector: the parameters with their values to set
#' @param parfiles character vector: which parfiles to manipulate
#' @return The WOFOST output file
#' @export
wrapWofostOptim<-function(parameters,
                          parnames=.GlobalEnv$parnames,
                          parfiles=.GlobalEnv$parfiles,
                          wofostdir=.GlobalEnv$wofostdir,
                          wofostbin=.GlobalEnv$wofostbin,
                          qualityfun=.GlobalEnv$qualityfun){
  cat ("\n run ",.GlobalEnv$run)
  names(parameters)<-parnames
  print(parameters)
  #return(1)
  oldwd<-getwd()
  setwd(wofostdir)
  files_pars<-lapply(parfiles, function(parfile)flatlist(readWofostParfile(parfile)))

  for (pname in names(parameters)){
    for (f in 1:length(files_pars)){
      if (any(grepl(pname,names(files_pars[[f]])))) {
        cat(sprintf("\nsetting %s in file %s to %s",pname,parfiles[[f]],parameters[pname]))
        cl<-class(files_pars[[f]][[pname]][[1]])
        files_pars[[f]][[pname]]<-parameters[[pname]]
        class(files_pars[[f]][[pname]])<-cl
        writeWofostParfile(config=flat2config(files_pars[[f]]),outfile=parfiles[[f]])
      }
    }
  }
  .GlobalEnv$run<-.GlobalEnv$run+1
  ret<-qualityfun(runWofost(wofostbin = wofostbin,wofostdir = wofostdir))
  cat("-->",ret)
  return(ret)
  # cat ("    parameters: ",paste(parameters))
  # for ( p in 1:length(parameters)){
  #   cl<-class(cfg[paridx][[1]])
  #   cfg[paridx[p]][[1]]<-parameters[p]
  #   class(cfg[paridx[p]][[1]])<-cl
  # }

  #par
  #flatlist()
  # paridx<-which(names(config_min_flat) %in% change_pars)
  # cfg<-config_flat
  # for ( p in 1:length(parameters)){
  #   cl<-class(cfg[paridx][[1]])
  #   cfg[paridx[p]][[1]]<-parameters[p]
  #   class(cfg[paridx[p]][[1]])<-cl
  # }
  # cfg<-flat2config(cfg)
  # writeWofostParfile(cfg,file.path(workfile))
  # res<-runWofost(wofostbin)
  # val<-qualityfun(res)
  # return (val)
}

# debug(wrapWofostOptim)
# for (sow in seq(20,200,by=30)){
#   print(wrapWofostOptim(parameters = c(IDSOW=sow),parfiles=parfiles[2],qualityfun=qualityfun))
# }


