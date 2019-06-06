#' Read WOFOST crop/program configuration file
#' @param filename
#' @export
readWofostParfile<-function(filename){
  lines<-readLines(filename)
  lines<-gsub("(.*)!.*","\\1",lines)
  lines<-gsub(" +","",lines)
  lines<-lines[!grepl("\\*",lines) & lines!=""]
  lines<-gsub(" +","",lines)
  condensed<-paste(lines,collapse=";")
  condensed<-gsub(",;",",",condensed)
  expressions<-strsplit(condensed,";")

  config<-list()
  for (i in 1:length(expressions[[1]])){
    expr<-expressions[[1]][i]
    varname<-gsub("(.+?)\\=.*","\\1",expr,perl=T)
    RHS<-gsub("(.+?)\\=(.*)","\\2",expr,perl=T)
    if (substr(RHS,1,1) %in% c("\"","'")){
      config[[varname]]<-as.list(RHS)
      next
    }
    spl<-strsplit(RHS,",")[[1]]
    if (length(spl)==1){
      if (grepl("\\.",RHS)) config[[varname]]<-as.list(as.numeric(RHS))
      else config[[varname]]<-as.list(as.integer(RHS))
    } else {
      #config[[varname]]<-list(xvals=spl[seq(1,length(spl),by=2)],
      #                        yvals=spl[seq(2,length(spl),by=2)])
      config[[varname]]<-as.list(as.numeric(spl[seq(1,length(spl),by=1)]))
    }
  }
  return (config)
}
