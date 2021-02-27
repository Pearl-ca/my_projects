
########################################################################

# TOGO 2010#

########################################################################
togo<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00053.csv")
togo


##identify percentage of child marriage by district
TG<-togo %>% 
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

TG
## save datafile
write.csv(TG, file = 'Output/togo_child_2010_ipumsi.csv')

### find average child marriage  
mean(TG$percent_married,na.rm = T)##  0.8242724%
mean(TG$percent_males_married,na.rm = T)##  0.2432711%
mean(TG$percent_females_married,na.rm=T)## 1.461069%

## create plot
plot(density(TG$av_percent_males_married),
     xlim = c(0,3),
     col="blue",
     main = "Child Marriage in Togo 2010",
     xlab = "percentage",
     ylab = "Density of children married")
lines(density(TG$av_percent_married), col = "red")                     
lines(density(TG$av_percent_females_married), col = "green")

abline(v = mean(TG$av_percent_married), col = "red")
abline(v = mean(TG$av_percent_males_married), col = "blue")
abline(v = mean(TG$av_percent_females_married), col = "green")

legend("topright",                                  
       legend = c("Male", "Total", "Female"),
       col = c("blue", "red", "green"),
       lty = 1)


#read global shapefile
shp = st_as_sf(readOGR(dsn = paste0(datadir, 'world_geolev2_2019/world_geolev2_2019.shp')))
shp


## read country shapefile
shpTG = shp[shp$CNTRY_NAME == "Togo", ]
shpTG
unique(shpTG$GEOLEVEL2)
unique(TG$GEOLEV2)
class(shpTG$GEOLEVEL2)
class(TG$GEOLEV2)

TG<-TG %>% 
  mutate(GEOLEV2=str_pad(GEOLEV2, 9, pad = "0"))
TG
#shpBD<-shpBD %>% 
#mutate(GEOLEVEL2=substr(GEOLEVEL2,2,9))


shp_TG = merge(shpTG, TG, by.x = "GEOLEVEL2", by.y = "GEOLEV2")
shp_TG




####plot map

############################################################## 
for (j in 1:3){
  if (j == 1){
    shp_TG$v = shp_TG$percent_females_married
    savefile = 'percent_females_married_Togo.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shp_TG$v = shp_TG$percent_males_married
    savefile = 'percent_males_married_Togo.pdf'
    legendname = 'male'
    
  } else if (j == 3){
    shp_TG$v = shp_TG$percent_married
    savefile = 'percent_married_Togo.pdf'
    legendname = 'Total'
  }
  
  
  ###plot the shapefile
  a = summary(shp_TG$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_TG$v,na.rm = T)
  maxnum = max(shp_TG$v,na.rm = T)
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
  shp_TG$v.qt = cut(shp_TG$v,
                    breaks = brks,
                    include.lowest = T,
                    labels = labels,na.rm=T)
  
  brks_scale = levels(shp_TG$v.qt)
  labels_scale = (brks_scale)
  
  shp_TG = st_sf(shp_TG)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shp_TG) +
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