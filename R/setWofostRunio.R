#' set WOFOST RUNIO file
#' @param file
#' @param key
#' @value
#' @export
setWofostRunio<-function(file,key,value){
  cfg<-readWofostParfile(file)
  cfg[[key]]<-value
  writeWofostParfile(cfg,file,spc = T)
}
