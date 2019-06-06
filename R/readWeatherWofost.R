#' Read WOFOST weather file
#' @param filename a single year WOFOST weather file
#' @return a list with elements "data" (the actual weather data) and "site" (lon, lat, alt, Ångström coefficients, label)
#' @export
readWeatherWofost<-function(files){
  # rbindlist(lapply(filenames, function(file){
  #   lines<-readLines(file)
  #   start<-which(grepl("YEARNR",lines))
  #   year<-as.numeric(gsub(".*(\\d{4}).*","\\1",lines[start]))
  #   weather<-fread(file,skip = start+1)
  #   names(weather)<-c("statno","year","doy","rad","tmin","tmax","vp","wind","pr")
  #   ret<-list(weather)
  #   names(ret)<-year
  #   return(ret)
  # })
  weathers<-lapply(files,function(file){

    shortlab<-gsub("([A-Za-z]+).*","\\1",basename(file))

    lines<-readLines(file)
    lines<-lines[!grepl("^\\*(?!\\*).*",lines,perl=T)]
    start<-which(grepl("YEARNR",lines))
    locline<-start+1

    descrline<-which(grepl("WCCDESCR",lines))
    sitelabel<-gsub(".*WCCDESCRIPTION=(.+)","\\1",lines[descrline])

    siteinfo=strsplit(lines[locline],"\\s+")[[1]]
    longitude<-as.numeric(siteinfo[[2]])
    latitude<-as.numeric(siteinfo[[3]])
    altitude<-as.numeric(siteinfo[[4]])
    angstr_a<--as.numeric(siteinfo[[5]])
    angstr_b<--as.numeric(siteinfo[[6]])
    year<-as.numeric(gsub(".*(\\d{4}).*","\\1",lines[start]))
    weather<-fread(paste(lines[(start+2):length(lines)],collapse="\n"))
    names(weather)<-c("statno","year","day","radn","mint","maxt","vp","wind","rain")
    weather[,lat:=latitude]
    weather[,lon:=longitude]
    #weather[,radn:=radn/1000]
    ret<-list(data=weather,site=list(longitude=longitude,latitude=latitude,altitude=altitude,angstr_a=angstr_a,angstr_b=angstr_b,name=sitelabel,shortlab=shortlab))
    #names(ret)<-year
    return(ret)
    #weather[,rad:=rad/1000]
    #return(weather)
  })
  return(list(data=rbindlist(lapply(weathers,function(x)x$data)),site=weathers[[1]]$site))

  }

