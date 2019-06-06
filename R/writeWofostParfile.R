#' write WOFOST configuration file
#' @param config the configration (crop definition in the CROPD folder or Management/Global simulation config in the RUNIO dir)
#' @param outfile the output file to write to
#' @param spc (boolean) use spaces beween varname and assignment
#' @export
writeWofostParfile<-function(config,outfile,spc=F){
  write("** auto generated config file",file=outfile)
  if (spc) sep<-" = " else sep<-"="
  for (i in 1:length(config)){
    #cat("i=",i,"\n")
    if (length(config[[i]])==1){
      if (class(config[[i]][[1]]) %in% c("numeric")){
        s<-sprintf("%s%s%.3f",names(config)[i],sep,config[[i]][[1]])
        write(s,file=outfile,append = T)
      } else if (class(config[[i]][[1]]) %in% c("integer")){
        s<-sprintf("%s%s%d",names(config)[i],sep,config[[i]][[1]])
        write(s,file=outfile,append = T)
      } else if (class(config[[i]][[1]]) %in% c("character")){
        s<-sprintf("%s%s'%s'",names(config)[i],sep,gsub("'","",config[[i]][[1]]))
        write(s,file=outfile,append = T)
      }
    }
    else{
      s<-sprintf("%s=%s",names(config)[i],
                 paste(unlist(lapply(config[i],function(x)sprintf("%.4f",x))),collapse = ","))
      write(s,file=outfile,append = T)
      #write(
      #  paste(names(config)[i],"=",
      #        paste(
      #        unlist(
      #          lapply (1:length(config[[i]]$xvals),function(j){
      #              sprintf("%.3f,%.3f",as.numeric(config[[i]]$xvals[j]),as.numeric(config[[i]]$yvals[j]))
      #          })
      #          ),collapse = ",")),file=outfile,append = T)
    }
  }
}

