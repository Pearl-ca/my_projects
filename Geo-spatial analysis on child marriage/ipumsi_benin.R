########################################################################

#BENIN 2013#

########################################################################
benin<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00036.csv")
benin


##identify percentage of child marriage by district
BN<-benin %>% 
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

BN

## save datafile
write.csv(BN, file = 'Output/Benin_child_2013_ipumsi.csv')

### find average child marriage in 2013 
mean(BN$percent_married,na.rm = T)##  1.3461%
mean(BN$percent_males_married,na.rm = T)##  0.486739%
mean(BN$percent_females_married,na.rm=T)## 2.250698%

## create plot
plot(density(BN$av_percent_males_married),
     xlim = c(0,3),
     col="blue",
     main = "Child Marriage in Benin 2013",
     xlab = "percentage",
     ylab = "Density of children married")
lines(density(BN$av_percent_married), col = "red")                     
lines(density(BN$av_percent_females_married), col = "green")

abline(v = mean(BN$av_percent_married), col = "red")
abline(v = mean(BN$av_percent_males_married), col = "blue")
abline(v = mean(BN$av_percent_females_married), col = "green")

legend("topright",                                  
       legend = c("Male", "Total", "Female"),
       col = c("blue", "red", "green"),
       lty = 1)

#read global shapefile
shp = st_as_sf(readOGR(dsn = paste0(datadir, 'world_geolev2_2019/world_geolev2_2019.shp')))
shp


## read country shapefile
shpBN = shp[shp$CNTRY_NAME == "Benin", ]
shpBN
unique(shpBN$GEOLEVEL2)
unique(BN$GEOLEV2)
class(shpBN$GEOLEVEL2)
class(BN$GEOLEV2)

BN<-BN %>% 
  mutate(GEOLEV2=str_pad(GEOLEV2, 9, pad = "0"))
BN
#shpBD<-shpBD %>% 
#mutate(GEOLEVEL2=substr(GEOLEVEL2,2,9))


shp_BN = merge(shpBN, BN, by.x = "GEOLEVEL2", by.y = "GEOLEV2")
shp_BN




####plot map

##############################################################
for (j in 1:3){
  if (j == 1){
    shp_BN$v = shp_BN$percent_females_married
    savefile = 'percent_females_married_Benin.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shp_BN$v = shp_BN$percent_males_married
    savefile = 'percent_males_married_Benin.pdf'
    legendname = 'male'
  } else if (j == 3){
    shp_BN$v = shp_BN$percent_married
    savefile = 'percent_married_Benin.pdf'
    legendname = 'Total'
  }
  
  
  ###plot the shapefile
  a = summary(shp_BN$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_BN$v,na.rm = T)
  maxnum = max(shp_BN$v,na.rm = T)
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
  shp_BN$v.qt = cut(shp_BN$v,
                    breaks = brks,
                    include.lowest = T,
                    labels = labels,na.rm=T)
  
  brks_scale = levels(shp_BN$v.qt)
  labels_scale = (brks_scale)
  
  shp_BN = st_sf(shp_BN)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shp_BN) +
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
