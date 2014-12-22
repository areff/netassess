lam.string<-"+proj=lcc +lat_1=33n +lat_2=45n +lat_0=40 +lon_0=-97 +a=6.370e6 +b=6.370e6"


latlong2lam<-function(latlong){
require(rgdal)
lam.cdts<-as.data.frame(spTransform(SpatialPoints(latlong,proj4string=CRS('+proj=longlat')),CRS(lam.string)))
colnames(lam.cdts)<-c('Xcdt','Ycdt')
return(lam.cdts)
}


lam2latlong<-function(lam.cdts){
require(rgdal)
latlong<-as.data.frame(spTransform(SpatialPoints(lam.cdts,proj4string=CRS(lam.string)),CRS('+proj=longlat')))
colnames(latlong)<-c('lon','lat')
return(latlong)
}
