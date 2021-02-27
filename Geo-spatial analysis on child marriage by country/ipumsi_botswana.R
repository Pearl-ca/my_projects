
########################################################################

#BOTSWANA 2011#

########################################################################
botswana<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00013.csv")
botswana


##identify percentage of child marriage by district
BW<-botswana %>% 
  subset(AGE<18) %>% 
  rename(DISTRICT= GEO1_BW2011) %>% 
  group_by(DISTRICT) %>% 
  mutate(nchild = NROW(PERNUM),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2 & MARST<=4]),
         males_married=NROW(which((MARST>=2 & MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(DISTRICT, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100,
         av_percent_married=mean(percent_married,na.rm = T),
         av_percent_males_married=mean(percent_males_married,na.rm = T),
         av_percent_females_married=mean(percent_females_married,na.rm=T))%>% 
  distinct(DISTRICT, .keep_all=TRUE)

BW
## save datafile
write.csv(BW, file = 'Output/Botswana_child_2011_ipumsi.csv')

### find average child marriage  
mean(BW$percent_married,na.rm = T)##  0.708829%
mean(BW$percent_males_married,na.rm = T)##  0.5098831%
mean(BW$percent_females_married,na.rm=T)## 0.9070466%

## create plot
plot(density(BW$av_percent_males_married),
     xlim = c(0,2),
     col="blue",
     main = "Child Marriage in Botwsana 2011",
     xlab = "percentage",
     ylab = "Density of children married")
lines(density(BW$av_percent_married), col = "red")                     
lines(density(BW$av_percent_females_married), col = "green")

abline(v = mean(BW$av_percent_married), col = "red")
abline(v = mean(BW$av_percent_males_married), col = "blue")
abline(v = mean(BW$av_percent_females_married), col = "green")

legend("topright",                                  
       legend = c("Male", "Total", "Female"),
       col = c("blue", "red", "green"),
       lty = 1)

#read shapefile
shpBW = st_as_sf(readOGR(dsn = paste0(datadir, 'geo1_bw2011/geo1_bw2011.shp')))
shpBW

unique(shpBW$DIST2011)
unique(BW$DISTRICT)
class(shpBW$DIST2011)
class(BW$DISTRICT)

Bw<-BW %>% 
  mutate(DISTRICT=as.factor(DISTRICT))
BW
#shpBD<-shpBD %>% 
#mutate(GEOLEVEL2=substr(GEOLEVEL2,2,9))


shp_BW = merge(shpBW, BW, by.x = "DIST2011", by.y = "DISTRICT")
shp_BW




####plot map

##############################################################
for (j in 1:3){
  if (j == 1){
    shp_BW$v = shp_BW$percent_females_married
    savefile = 'percent_females_married_Botswana.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shp_BW$v = shp_BW$percent_males_married
    savefile = 'percent_males_married_Botswana.pdf'
    legendname = 'male'
  } else if (j == 3){
    shp_BW$v = shp_BW$percent_married
    savefile = 'percent_married_Botswana.pdf'
    legendname = 'Total'
  }
  
  
  ###plot the shapefile
  a = summary(shp_BW$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_BW$v,na.rm = T)
  maxnum = max(shp_BW$v,na.rm = T)
  minnum
  maxnum
  
  #assign proper labels
  labels = c()
  brks = c(minnum, pretty_breaks, maxnum)
  
  
  #round the extreme value label
  for (ix in 1:length(brks)){
    labels = c(labels, round(brks[ix+1],2))
  } #end for ix
  
  #last digit is NA, so drop it
  labels = labels[1:length(labels)-1]
  
  
  
  
  #define a new variable based on breaks
  shp_BW$v.qt = cut(shp_BW$v,
                    breaks = brks,
                    include.lowest = T,
                    labels = labels,na.rm=T)
  
  brks_scale = levels(shp_BW$v.qt)
  labels_scale = (brks_scale)
  
  shp_BW = st_sf(shp_BW)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shp_BW) +
    geom_sf(aes(fill = v.qt), lwd=0.0)+
    coord_sf() +
    theme_map() +
    theme(legend.position = 'bottom') +
    scale_fill_discrete_sequential(palette='Blues3', rev=T,
                                   drop = FALSE,
                                   breaks = (brks_scale),
                                   nmax = length(brks_scale) +1,
                                   order = 2:(length(brks_scale) +2),
                                   na.value = 'white',
                                   labels = labels_scale,
                                   name=paste( legendname, 'child marriage (%)'),
                                   guide = guide_legend(
                                     direction = 'horizontal',
                                     keyheight = unit(2, units='mm'),
                                     keywidth = unit(20, units = 'mm'),
                                     title.position = 'top',
                                     reverse = F,
                                     label.position = 'bottom',
                                     title.hjust = 0,
                                     label.hjust = 1,
                                     nrow = 1,
                                     byrow = T
                                   )) 
  print(pfull)
  dev.off()
  
  
}
