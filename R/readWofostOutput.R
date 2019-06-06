#' Read WOFOST output  file
#' @param filename
#' @return a list with all the Wofost outputs (detailed and summaries)
#' @export
readWofostOutput<-function(filename){
  lines<-readLines(filename)
  lines<-lines[!grepl("no living leaves",lines)]
  starts<-which(grepl("===",lines))
  summarylines<-which(grepl("SUMMARY",lines))
  colnamelines<-which(grepl("YEAR  DAY",lines))
  colnames<-strsplit(lines[colnamelines]," +")
  colnames<-lapply(colnames,function(x)x[!x==""])
  headings<-lines[starts-1]
  nrows<-summarylines-starts-2
  filecontent<-paste(lines,collapse = "\n")
  outtables<-lapply(1:length(starts),function(i){
    return(suppressWarnings(
      fread(filecontent,skip = starts[i],nrows = nrows[i],col.names = colnames[[i]] )
    ))
  })
  outtables<-lapply(outtables,function(x){data.table(x,seasonstart=min(x$YEAR),seasonend=max(x$YEAR))})
  summaries<-lapply(1:length(summarylines),function(i){
    values<-as.numeric(unlist(strsplit(lines[summarylines[i]+2]," +",)))[-1]
    names(values)<-as.character(unlist(strsplit(lines[summarylines[i]+1]," +",)))[-1]
    return(values)
  })
  names(outtables)<-headings

  ##added later: rbind output of multiple years
  nm<-unique(names(outtables))
  details<-lapply(nm, function(n){
    rbindlist(outtables[names(outtables)==n])
  })
  names(details)<-nm

  summaries<-lapply(1:length(names(outtables)),function(n){
    cbind(YEAR=max(outtables[[n]]$YEAR),data.table(t(summaries[[n]])))
  })
  names(summaries)<-names(outtables)

  sum<-lapply(nm, function(n){
    rbindlist(summaries[names(summaries)==n])
  })
  names(sum)<-nm

  return(list(details=details,summaries=sum))
}
