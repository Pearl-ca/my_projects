

########################################################################

#LIBERIA 2008#

########################################################################
liberia<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00021.csv")
liberia


##identify percentage of child marriage by district
LR<-liberia %>% 
  subset(AGE<18) %>% 
  rename(COUNTY=GEO1_LR2008) %>% 
  group_by(COUNTY) %>% 
  mutate(nchild = NROW(PERNUM),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2 & MARST<=4]),
         males_married=NROW(which((MARST>=2 & MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(COUNTY, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100,
         av_percent_married=mean(percent_married,na.rm = T),
         av_percent_males_married=mean(percent_males_married,na.rm = T),
         av_percent_females_married=mean(percent_females_married,na.rm=T))%>% 
  distinct(COUNTY, .keep_all=TRUE)

LR
## save datafile
write.csv(LR, file = 'Output/liberia_child_2008_ipumsi.csv')

### find average child marriage  
mean(LR$percent_married,na.rm = T)##  3.156933%
mean(LR$percent_males_married,na.rm = T)##  3.590022%
mean(LR$percent_females_married,na.rm=T)## 2.686727%

## create plot
plot(density(LR$av_percent_married),
     xlim = c(1,7),
     col="blue",
     main = "Child Marriage in Liberia 2008",
     xlab = "percentage",
     ylab = "Density of children married")
lines(density(LR$av_percent_males_married), col = "red")                     
lines(density(LR$av_percent_females_married), col = "green")

abline(v = mean(LR$av_percent_married), col = "blue")
abline(v = mean(LR$av_percent_males_married), col = "red")
abline(v = mean(LR$av_percent_females_married), col = "green")

legend("topright",                                  
       legend = c("Male", "Total", "Female"),
       col = c("red", "blue", "green"),
       lty = 1)


#read shapefile
shpLR = st_as_sf(readOGR(dsn = paste0(datadir, 'geo1_lr2008/geo1_lr2008.shp')))
shpLR

unique(shpLR$CNTY2008)
unique(LR$COUNTY)
class(shpLR$CNTY2008)
class(LR$COUNTY)

LR<-LR %>% 
  mutate(COUNTY=as.factor(COUNTY))
LR
#shpBD<-shpBD %>% 
#mutate(GEOLEVEL2=substr(GEOLEVEL2,2,9))


shp_LR = merge(shpLR, LR, by.x = "CNTY2008", by.y = "COUNTY")
shp_LR



####plot map

############################################################## 
for (j in 1:3){
  if (j == 1){
    shp_LR$v = shp_LR$percent_females_married
    savefile = 'percent_females_married_Liberia.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shp_LR$v = shp_LR$percent_males_married
    savefile = 'percent_males_married_Liberia.pdf'
    legendname = 'male'
    
  } else if (j == 3){
    shp_LR$v = shp_LR$percent_married
    savefile = 'percent_married_Liberia.pdf'
    legendname = 'Total'
  }
  
  
  ###plot the shapefile
  a = summary(shp_LR$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_LR$v,na.rm = T)
  maxnum = max(shp_LR$v,na.rm = T)
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
  shp_LR$v.qt = cut(shp_LR$v,
                    breaks = brks,
                    include.lowest = T,
                    labels = labels,na.rm=T)
  
  brks_scale = levels(shp_LR$v.qt)
  labels_scale = (brks_scale)
  
  shp_LR = st_sf(shp_LR)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shp_LR) +
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
