# setwd("~/shiny/netassess/")
library(plyr)
library(reshape)
library(ggplot2)
library(data.table)
library(ggmap)
library(rgdal)
source("latlong2lam.r")

load("data/daily.data.rdata")
load("data/all.fips.data.rdata")
all.fips.data<-data.frame(all.fips.data,lam2latlong(all.fips.data[c('Xcdt','Ycdt')]))
site.info<-unique(daily.data[,c("siteid","lon","lat","parm","parmdesc","poc","methcode","sample_coll_desc","sample_anal_desc","state_name","county_name","cbsa_short_name","epa_region_code","land_use_type"),with=F],by=NULL)

intersite.cors<-data.table(read.table("data/IntersiteCors.csv",sep=',',fill=T,header=T,stringsAsFactors=F))



