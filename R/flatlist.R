#' Flatten config list
#' @param mylist R List with WOFOST crop/program config file content
#' @return the flat (unnested) list
#' @export
flatlist <- function(mylist){
  config_flat<-lapply(rapply(mylist, enquote, how="unlist"), eval)
  names(config_flat)<-gsub("(\\d+)","_\\1",names(config_flat))
  names(config_flat)<-gsub("(TSUM)_(\\d)","\\1\\2",names(config_flat))
  names(config_flat)<-gsub("Q_10","Q10",names(config_flat))
  return(config_flat)
}
