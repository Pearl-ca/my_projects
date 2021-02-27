#=============================================================
#
#
#
#@Objective     This code plots the spatial distribution of change
#               change in the child marriage in India between 2001-2011
#
#
#@Author        Pearl Etie and Swapnil Singh


library(ggthemes)

#read 2001 shapefile
shp2011dif = st_as_sf(readOGR(dsn = paste0(datadir, 'india_shapefiles/DISTRICT 2001.shp')))
shp2011dif = shp2011dif %>% 
  select(DIS_ID, NAME, STATE_UT, CODE) %>% 
  mutate(codes = as.character(levels(CODE))[CODE]) %>% 
  select(DIS_ID, NAME, STATE_UT, codes) %>% 
  mutate(state = as.numeric(substr(codes,1,2)), district=as.numeric(substr(codes,3,4))) 



#read the change in child marriage data
load('Output/child_marriage_change_2001_2011.RData')
d = d2001_2011
rm(d2001_2011)
colnames(d)



### merge both datasets
shpdiff = merge(shp2011dif, d, by.x = c("state","district"), by.y=c("state","district"))


for (j in 1:3){
  if (j == 1){
    shpdiff$v = shpdiff$percentchange_females_married
    savefile = 'percentchange_females_married_2011.pdf'
    legendname = 'female'
    
  } else if (j == 2){
    shpdiff$v = shpdiff$percentchange_males_married
    savefile = 'percentchange_males_married_2011.pdf'
    legendname = 'male'
  } else if (j == 3){
    shpdiff$v = shpdiff$percentchange_married
    savefile = 'percentchange_married_2011.pdf'
    legendname = 'TOTAL'
  }
  
  
  ###plot the shapefile
 
  ###find the min/max value
  minnum = min(shpdiff$v,na.rm = T)
  maxnum = max(shpdiff$v,na.rm = T)
  minnum
  maxnum
  
  #assign proper labels
  labels = c()
  brks = c(minnum,-0.01, 0.01 , maxnum)
  
  
  #round the extreme value label
  for (ix in 1:length(brks)){
    labels = c(labels, round(brks[ix+1],2))
  } #end for ix
  
  #last digit is NA, so drop it
  labels = labels[1:length(labels)-1]
  
  
  
  
  #define a new variable based on breaks
  shpdiff$v.qt = cut(shpdiff$v,
                     breaks = brks,
                     include.lowest = T,
                     labels = labels,na.rm=T)
  
  brks_scale = levels(shpdiff$v.qt)
  labels_scale = (brks_scale)
  
  shpdiff = st_sf(shpdiff)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
  
  pfull =  ggplot(data = shpdiff) +
    geom_sf(aes(fill = v.qt), lwd=0.0)+
    coord_sf() +
    theme_map() +
    theme(legend.position = 'bottom') +
    scale_fill_discrete_sequential(palette= c("YlOrRd"), rev=T,
                                   drop = FALSE,
                                   breaks = (brks_scale),
                                   nmax = length(brks_scale) +1,
                                   order = 2:(length(brks_scale) +2),
                                   na.value = 'white',
                                   labels = labels_scale,
                                   name=paste('Rural area', legendname, 'percent change in child marriage (%)'),
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
  
  
}  #end for loop v 


