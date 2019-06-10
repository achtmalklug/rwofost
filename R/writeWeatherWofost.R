#' Write WOFOST weather file(s)
#' @param met
#' @param targetfolder
#' @param station_number
#' @export
# createWeatherWofost<-function(rawdata,lat,lon,angstr_a,angstr_b,altitude,shortname,longname,targetfolder,station_number=1){
#   #lat=41.5
#   #lon=0.5
#   #angstr_a=-.25
#   #angstr_b=-.50
#   #altitude=190
#   #station_number=1
#   #shortname="SPAIN"
#   header<-paste0("** WCCDESCRIPTION=",longname,"\n** WCCFORMAT=2\n** WCCYEARNR=")
#
#   yearly<-split(rawdata,by="year")
#   for ( i in 1:length(yearly)){
#     df<-yearly[[i]]
#     fn<-file.path(targetfolder,sprintf("%s%d.%s",shortname,station_number,substr(df$year[1],2,4)))
#     cat(fn,"\n")
#     content<-paste0(lapply(1:nrow(df),function(i){
#       val<-unlist(df[doy==i])
#       # paste0(station_number,
#       #        df[doy==i]$year,
#       #        df[doy==i]$doy,
#       #        df[doy==i]$radiation,
#       #        df[doy==i]$tmin,
#       #        df[doy==i]$tmax,
#       #        df[doy==i]$vp,
#       #        df[doy==i]$wind,
#       #        df[doy==i]$pr)
#       sprintf(" %2d %4d %4d %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f",
#               station_number,
#               #val["year"],
#               2001,
#               val["doy"],
#               val["radiation"],
#               val["tmin"],
#               val["tmax"],
#               val["vp"],
#               val["wind"],
#               val["pr"])
#       #sprintf(" %2d %4d %4d %7.1f %4.1f %4.1f %4.1f %4.1f %4.1f",
#       #        station_number, df[doy==i]$year,
#       #        df[doy==i]$doy,
#       #        df[doy==i]$radiation,
#       #        df[doy==i]$tmin,
#       #        df[doy==i]$tmax,
#       #        df[doy==i]$vp,
#       #        df[doy==i]$wind,
#       #        df[doy==i]$pr)
#     }),collapse = "\n")
#     #write(paste0(header,df$year[1]),fn)
#     write(paste0(header,2001),fn)
#     write(sprintf(" %.2f %.2f %.2f %.2f %.2f",lon,lat,altitude,angstr_a,angstr_b),fn,append=T)
#     write(content,fn,append = T)
#   }
# }

writeWeatherWofost<-function(met,targetfolder,station_number=1,prefix=met$site$shortlab){
  angstr_a=met$site$angstr_a
  angstr_b=met$site$angstr_b
  altitude=met$site$altitude
  longname=met$site$name
  longitude=met$site$longitude
  latitude=met$site$latitude

  #lat=met$lat
  #lon
  #lat=41.5
  #lon=0.5
  #angstr_a=-.25
  #angstr_b=-.50
  #altitude=190
  #station_number=1
  #shortname="SPAIN"
  header<-paste0("** WCCDESCRIPTION=",longname,"\n** WCCFORMAT=2\n** WCCYEARNR=")

  yearly<-split(met$data,by="year")
  for ( i in 1:length(yearly)){
    df<-yearly[[i]]
    fn<-file.path(targetfolder,sprintf("%s%d.%s",prefix,station_number,substr(df$year[1],2,4)))
    cat(fn,"\n")
    content<-paste0(lapply(1:nrow(df),function(i){
      val<-unlist(df[df$day==i,])
      sprintf("%4d%5d%4d%6.0f.%6.1f%6.1f%8.3f%6.1f%6.1f",
              station_number,
              #val["year"],
              val["year"],
              val["day"],
              val["radn"],
              val["mint"],
              val["maxt"],
              val["vp"],
              val["wind"],
              val["rain"])
    }),collapse = "\n")
    #write(paste0(header,df$year[1]),fn)
    write(paste0(header,df$year[1]),fn)
    cat(df$year[[1]])
    write(sprintf("%7.2f%7.2f%6.0f.%6.2f%6.2f",longitude,latitude,altitude,-angstr_a,-angstr_b),fn,append=T)
    write(content,fn,append = T)
  }
}
