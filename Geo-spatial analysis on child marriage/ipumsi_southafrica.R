
########################################################################

# SOUTH AFRICA 2011#

########################################################################
south_africa<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00051.csv")
south_africa


##identify percentage of child marriage by district
SA<-south_africa %>% 
  subset(AGE<18) %>% 
  group_by(GEOLEV2) %>% 
  mutate(nchild = NROW(PERNUM),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2 & MARST<=4]),
         males_married=NROW(which((MARST>=2 & MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(GEOLEV2, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100,
         av_percent_married=mean(percent_married,na.rm = T),
         av_percent_males_married=mean(percent_males_married,na.rm = T),
         av_percent_females_married=mean(percent_females_married,na.rm=T))%>% 
  distinct(GEOLEV2, .keep_all=TRUE)

SA
## save datafile
write.csv(SA, file = 'Output/south_africa_child_2011_ipumsi.csv')

### find average child marriage  
mean(SA$percent_married,na.rm = T)##  1.139343%
mean(SA$percent_males_married,na.rm = T)##  1.041499%
mean(SA$percent_females_married,na.rm=T)## 1.238617%

## create plot
plot(density(SA$av_percent_females_married),
     xlim = c(0,3),
     col="green",
     main = "Child Marriage in South Africa 2011",
     xlab = "percentage",
     ylab = "Density of children married")
lines(density(SA$av_percent_married), col = "red")                     
lines(density(SA$av_percent_males_married), col = "blue")

abline(v = mean(SA$av_percent_married), col = "red")
abline(v = mean(SA$av_percent_males_married), col = "blue")
abline(v = mean(SA$av_percent_females_married), col = "green")

legend("topright",                                  
       legend = c("Male", "Total", "Female"),
       col = c("blue", "red", "green"),
       lty = 1)



#read global shapefile
shp = st_as_sf(readOGR(dsn = paste0(datadir, 'world_geolev2_2019/world_geolev2_2019.shp')))
shp


## read country shapefile
shpSA = shp[shp$CNTRY_NAME == "South Africa", ]
shpSA
unique(shpSA$GEOLEVEL2)
unique(SA$GEOLEV2)
class(shpSA$GEOLEVEL2)
class(SA$GEOLEV2)

SA<-SA %>% 
  mutate(GEOLEV2=str_pad(GEOLEV2, 9, pad = "0"))
SA
#shpBD<-shpBD %>% 
#mutate(GEOLEVEL2=substr(GEOLEVEL2,2,9))


shp_SA = merge(shpSA, SA, by.x = "GEOLEVEL2", by.y = "GEOLEV2")
shp_SA




####plot map

############################################################## 
for (j in 1:3){
  if (j == 1){
    shp_SA$v = shp_SA$percent_females_married
    savefile = 'percent_females_married_South_Africa.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shp_SA$v = shp_SA$percent_males_married
    savefile = 'percent_males_married_South_Africa.pdf'
    legendname = 'male'
    
  } else if (j == 3){
    shp_SA$v = shp_SA$percent_married
    savefile = 'percent_married_South_Africa.pdf'
    legendname = 'Total'
  }
  
  
  ###plot the shapefile
  a = summary(shp_SA$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_SA$v,na.rm = T)
  maxnum = max(shp_SA$v,na.rm = T)
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
  shp_SA$v.qt = cut(shp_SA$v,
                    breaks = brks,
                    include.lowest = T,
                    labels = labels,na.rm=T)
  
  brks_scale = levels(shp_SA$v.qt)
  labels_scale = (brks_scale)
  
  shp_SA = st_sf(shp_SA)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shp_SA) +
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
