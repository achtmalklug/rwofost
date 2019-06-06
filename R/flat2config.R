#' Convert flat list representation of WOFOST crop/program config file
#' @param config_flat the configuartion as flat R list
#' @return a list with the configuration
#' @export
flat2config<-function(config_flat){
  nm<-unique(gsub("([A-Z0-9])_\\d+","\\1",names(config_flat)))
  config<-lapply(nm,function(x){
    vals<-as.list(unlist(config_flat[names(config_flat)==x | grepl(paste0(x,"_"),names(config_flat))]))
    return (vals)
  })
  names(config)<-nm
  return(config)
}
