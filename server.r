shinyServer(function(input, output) {
source("global.r")

output$tssitesel1<-renderUI({
# input<-list(sitesel="010030010",parmsel='44201',yearsel=c(2009,2011))
curr.sites<-unique(subset(daily.data,parm==input$parmsel)$siteid)
selectInput("tssitesel1", label = h4("Choose a site ID"),choices = as.list(sort(unique(curr.sites))))
})

output$tspocsel1<-renderUI({
# input<-list(tssitesel1="010030010",parmsel='44201',yearsel=c(2009,2011))
curr.pocs<-unique(subset(daily.data,parm==input$parmsel & siteid==input$tssitesel1)$poc)
selectInput("tspocsel1", label = h4("Choose a POC"),choices = as.list(sort(unique(curr.pocs))))
})

output$tssitesel2<-renderUI({
# input<-list(sitesel="010030010",parmsel='44201',yearsel=c(2009,2011))
curr.sites<-unique(subset(daily.data,parm==input$parmsel & siteid!=input$tssitesel1)$siteid)
selectInput("tssitesel2", label = h4("Choose a 2nd site ID"),choices = as.list(sort(unique(curr.sites))))
})

output$tspocsel2<-renderUI({
# input<-list(tssitesel2="010030010",parmsel='44201',yearsel=c(2009,2011))
curr.pocs<-unique(subset(daily.data,parm==input$parmsel & siteid==input$tssitesel2)$poc)
selectInput("tspocsel2", label = h4("Choose a 2nd POC"),choices = as.list(sort(unique(curr.pocs))))
})

output$tsyearsel<-renderUI({
# input<-list(tssitesel1='370630015',tssitesel2='371190041',tspocsel1=1,tspocsel2=1,parmsel=44201,tsyearsel=2009:2013)
curr.years<-unique(subset(daily.data,(siteid==input$tssitesel1 & poc==input$tspocsel1) | (siteid==input$tssitesel2 & poc==input$tspocsel2) & parm==input$parmsel)$Year)
checkboxGroupInput("tsyearsel", label = h4("Choose Years"),choices = as.list(sort(unique(curr.years))),selected=sort(unique(curr.years)) )
})

output[["timeserplot"]]<-renderPlot({
# input<-list(tssitesel1='370630015',tssitesel2='371190041',tspocsel1=1,tspocsel2=1,parmsel=44201,tsyearsel=2009:2013)
curr.data<-daily.data[((siteid==input$tssitesel1 & poc==input$tspocsel1) | (siteid==input$tssitesel2 & poc==input$tspocsel2)) & parm==input$parmsel & Year %in% input$tsyearsel,]
curr.ts.plot<-ggplot(curr.data,aes(y=ifelse(conc<0,-1,conc),x=as.Date(Date,format='%Y-%m-%d')))+
facet_wrap(~siteid,ncol=1)+
geom_line(aes(group=1,colour=factor(Year)))+
geom_point()+
scale_x_date(name='')+
scale_y_continuous(name=paste(curr.data$parmdesc[1],ifelse(input$parmsel==88101," (ug/m3)"," (ppm)"),sep=''))+
scale_colour_discrete(name='Year')
curr.ts.plot
})

output[["scatplot"]]<-renderPlot({
# input<-list(tssitesel1='010730023',tssitesel2='010732006',tspocsel1=1,tspocsel2=1,parmsel=44201,tsyearsel=2009:2013)
curr.data<-daily.data[((siteid==input$tssitesel1 & poc==input$tspocsel1) | (siteid==input$tssitesel2 & poc==input$tspocsel2)) & parm==input$parmsel & Year %in% input$tsyearsel,]
curr.data2<-cast(curr.data,Date+Year~siteid,value='conc',fun.aggregate=function(x) mean(x,na.rm=T))
colnames(curr.data2)[3:4]<-gsub(input$tssitesel1,'Site1',gsub(input$tssitesel2,'Site2',colnames(curr.data2)[3:4],fixed=T)) 

curr.fit<-lm(Site2~Site1,data=curr.data2)
curr.coefs<-coef(curr.fit)
curr.r2<-round(summary(curr.fit)$r.squared,digits=2)

curr.scat.plot<-ggplot(curr.data2,aes(y=Site2,x=Site1))+
geom_point(aes(colour=factor(Year)))+
scale_colour_discrete(name='Year')+
scale_y_continuous(name=paste(input$tssitesel2,gsub('- Local Conditions','',curr.data$parmdesc[1],fixed=T),ifelse(input$parmsel==88101,"(ug/m3)","(ppm)"),sep=' '))+
scale_x_continuous(name=paste(input$tssitesel1,gsub('- Local Conditions','',curr.data$parmdesc[1],fixed=T),ifelse(input$parmsel==88101,"(ug/m3)","(ppm)"),sep=' '))+
geom_abline(slope=1,intercept=0)+
geom_abline(slope=curr.coefs[2],intercept=curr.coefs[1],linetype=2,colour='brown')+
ggtitle(paste('Regression (dashed line): y = ',round(curr.coefs[2],digits=2),'*x + ',round(curr.coefs[1],digits=2),'; R-squared: ',curr.r2,sep=''))
curr.scat.plot
})

output$corsitesel<-renderUI({
# input<-list(sitesel="010030010",parmsel='44201',yearsel=c(2009,2011))
curr.sites<-unique(subset(intersite.cors,parm==input$parmsel2)$Site1)
selectInput("corsitesel", label = h4("Choose a site ID"),choices = as.list(sort(unique(curr.sites))))
})

output$coryearsel<-renderUI({
# input<-list(sitesel="010030010",parmsel2='44201',yearsel=c(2009,2011))
curr.years<-unique(subset(intersite.cors,Site1==input$corsitesel & parm==input$parmsel2)$Year)
checkboxGroupInput("coryearsel", label = h4("Choose Years"),choices = as.list(sort(unique(curr.years))),selected=sort(unique(curr.years)) )
})

# plot the cors for the selected site on an interactive map
output[["cormaps"]]<-renderPlot({
# input<-list(corsitesel=10030010,parmsel2=44201,coryearsel=c(2009:2013),additdatasel='population')
curr.cors<-as.data.frame(intersite.cors[Site1==input$corsitesel & parm==input$parmsel2 & Year %in% input$coryearsel,])
curr.site.info<-as.data.frame(site.info[as.numeric(siteid) %in% c(curr.cors$Site1,curr.cors$Site2) & parm==input$parmsel2,])
curr.site.info$siteid<-as.numeric(curr.site.info$siteid)

curr.cdts<-data.frame(lon=range(curr.site.info$lon),lat=range(curr.site.info$lat))
curr.cdts<-data.frame(curr.cdts,latlong2lam(curr.cdts))
curr.bbox<-data.frame(Xcdt=c(curr.cdts$Xcdt[1]-1.05e5,curr.cdts$Xcdt[2]+1.05e5),Ycdt=c(curr.cdts$Ycdt[1]-1.05e5,curr.cdts$Ycdt[2]+1.05e5))
curr.bbox<-data.frame(lam2latlong(curr.bbox),curr.bbox)
curr.map<-get_map(location=c(curr.bbox$lon[1],curr.bbox$lat[1],curr.bbox$lon[2],curr.bbox$lat[2]),maptype='toner')

curr.cors<-merge(curr.site.info[c('siteid','lon','lat')],curr.cors[c('Site2','Rval','Year')],by.x='siteid',by.y='Site2',all.x=T,all.y=T)
curr.site.cors<-subset(curr.cors,is.na(Year))[1,]
curr.site.cors<-data.frame(curr.site.cors[1:3],Rval=1,Year=na.omit(unique(curr.cors$Year)))
curr.cors<-rbind(na.omit(curr.cors),curr.site.cors)
curr.cors$SiteType<-ifelse(curr.cors$siteid==input$corsitesel,'Center Site','Satellite')
curr.cdts.1<-subset(curr.site.info,siteid==as.numeric(input$corsitesel) & parm==input$parmsel2)

curr.shapes<-c(letters,toupper(letters))[1:length(unique(curr.cors$siteid))]
names(curr.shapes)<-c(input$corsitesel,unique(curr.cors$siteid)[-grep(input$corsitesel,unique(curr.cors$siteid))])

curr.map.plot<-ggmap(curr.map,extent = 'panel')+
#geom_point(data=curr.cors,aes(size=Rval,fill=factor(siteid),shape=SiteType))+
geom_point(data=curr.cors,aes(fill=Rval),shape=21,size=4.5)+
geom_point(data=curr.cors,aes(shape=factor(siteid)),colour='black',size=4)+
facet_wrap(~Year,ncol=1)+
scale_x_continuous(name='',breaks=NULL)+
scale_y_continuous(name='',breaks=NULL)+
#scale_fill_discrete(name='Site ID')+
#scale_size(range=c(2,7))+
#scale_shape_manual(name='',values=c('Center Site'=21,'Satellite'=23))+
scale_fill_gradientn(name='R value',colours=rainbow(100)[75:1],limits=c(0,1),breaks=round(seq(from=0,to=1,length.out=5),digits=2) )+
scale_shape_manual(name='Site ID',values=curr.shapes)+
ggtitle(paste(gsub('- Local Conditions','',curr.cdts.1$parmdesc,fixed=T),' Correlations within 100 km of Site ',input$corsitesel,':\n',curr.cdts.1$county_name,' County, ',curr.cdts.1$state_name,sep=''))+
theme(strip.text.x=element_text(size=15,face='bold',colour='black'),
legend.text=element_text(size=10,face='bold',colour='black',vjust=0),
legend.title=element_text(size=14,face='bold',colour='black') )+
#guides(fill = guide_legend(override.aes = list(shape=21,size=5)))
guides(shape = guide_legend(override.aes = list(size=5,colour='black')))

if (input$additdatasel!='None'){
curr.map.data<-subset(all.fips.data,variable==input$additdatasel & curr.bbox$lon[1]<=lon & lon<=curr.bbox$lon[2] & curr.bbox$lat[1]<=lat & lat<=curr.bbox$lat[2])
#curr.breaks<-round(seq(from=min(curr.map.data$value,na.rm=T),to=max(curr.map.data$value,na.rm=T),length.out=9),digits=0)
#print(curr.breaks)
curr.map.plot<-curr.map.plot+
geom_point(data=curr.map.data,aes(x=lon,y=lat,colour=cut(value,7,dig.lab=0)),fill=NA,shape=22)+
#scale_colour_gradientn(name=paste(input$additdatasel,'\n',sep=''),colours=rainbow(100)[75:1],limits=range(curr.breaks),breaks=curr.breaks,labels=formatC(curr.breaks,digits=0) )+
scale_colour_discrete(name=input$additdatasel)+
geom_point(data=curr.cors,aes(fill=Rval),shape=21,size=4.5)+
geom_point(data=curr.cors,aes(shape=factor(siteid)),colour='black',size=4)
}

curr.map.plot

})

})
