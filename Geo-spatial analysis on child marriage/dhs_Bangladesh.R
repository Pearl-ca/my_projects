## load the data

dhs_Bangladesh<-read.dta("C:/Users/Kasutaja/Downloads/BD_2014_DHS/BDIR72DT/BDIR72FL.dta")

View(dhs_Bangladesh)
View(dhs_Bangladesh$v501)
## determine incidence of child marriage at cluster level     
BD<-dhs_Bangladesh %>% 
  select(caseid,v001,v011,v012,v101,v509,v511,) %>% 
  rename(cluster=v001,
         dob_cmc=v011,
         age=v012,
         region=v101,
         age_first_marriage_cmc = v509) %>% 
  group_by(region,cluster) %>% 
  mutate(age_first_marriage=floor((age_first_marriage_cmc - dob_cmc)/12),
         nfemales=NROW(caseid),
         females_married=NROW(age_first_marriage[age_first_marriage<18]),
         percent_females_married = (females_married/nfemales) * 100) %>% 
  select(region,cluster,nfemales, females_married,percent_females_married) %>% 
  distinct(cluster, .keep_all=TRUE)

BD

write.dta(BD, file = 'Output/dhs_Bangladesh_women.dta')

##### plot the shapefile
BD=read.dta('Output/dhs_Bangladesh_women.dta')

#read shapefiles
shp2014 = st_as_sf(readOGR(dsn = paste0(datadir, 'BDGE71FL/BDGE71FL.shp')))

shp2014<-shp2014 %>% 
  select(DHSID,DHSREGNA,DHSCLUST) %>% 
  mutate(DHSREGNA=tolower(DHSREGNA))
View(shp2014)

class(shp2014$DHSREGNA)
class(BD$region)
class(shp2014$DHSCLUST)
class(BD$cluster)

shp_BD_2014 = merge(shp2014, BD, by.x = c('DHSREGNA', 'DHSCLUST'), by.y=c('region', 'cluster'))
shp_BD_2014

library(tidyverse)
shp_BD<- shp_BD_2014 %>% 
  mutate(geom = gsub('[()]', '', geometry),
         geom = gsub('[c]', '', geom)) %>% 
  separate(col = geom, into = c('longitude','latitude' ), sep = '\\,') %>% 
  mutate(longitude=as.numeric(as.character(longitude)),
         latitude=as.numeric(as.character(latitude)))

shp_BD
shp_BD <- shp_BD[-c(544),]


####plot map
library(ggthemes)

############################################################## CIRCLE
for (j in 1){
    shp_BD$v = shp_BD$percent_females_married
    savefile = 'percent_females_married_BD2014.pdf'
    legendname = 'female'
  }
  
  
  ###plot the shapefile
  a = summary(shp_BD$v)
  a
  pretty_breaks = seq(a[2],a[5], length.out = 4)
  pretty_breaks
  
  
  
  ###find the min/max value
  minnum = min(shp_BD$v,na.rm = T)
  maxnum = max(shp_BD$v,na.rm = T)
  minnum
  maxnum
  
  #assign proper labels
  labels = c()
  brks = c(minnum, pretty_breaks, maxnum)
  brks
  
  #round the extreme value label
  for (ix in 1:length(brks)){
    labels = c(labels, round(brks[ix+1],2))
  } #end for ix
  
  #last digit is NA, so drop it
  labels = labels[1:length(labels)-1]
  labels
  
  
  
  #define a new variable based on breaks
  shp_BD$v.qt = cut(shp_BD$v,
                     breaks = brks,
                     include.lowest = T,
                     labels = labels,na.rm=T)
  
  brks_scale = levels(shp_BD$v.qt)
  labels_scale = (brks_scale)
  labels_scale
  
  shp_BD= st_sf(shp_BD)
  pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
  
 
  pfull =  ggplot(data = shp_BD) +
    geom_sf(aes(color = v.qt), lwd=5)+
    coord_sf() +
    theme_map() +
    theme(legend.position = 'bottom') +
    scale_color_discrete_sequential(palette='Blues3', rev=T,
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
  
  
####################################################
  
  #polygon <- shp_BD %>%
    #st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    #summarise(geom = st_(geometry)) %>%
    #st_cast("POLYGON")
  #polygon  
#plot(polygon)  


############################################################### DASH
for (j in 1){
  shp_BD$v = shp_BD$percent_females_married
  savefile = 'percent_females_married_BD20142.pdf'
  legendname = 'female'
}


###plot the shapefile
a = summary(shp_BD$v)
a
pretty_breaks = seq(a[2],a[5], length.out = 4)
pretty_breaks



###find the min/max value
minnum = min(shp_BD$v,na.rm = T)
maxnum = max(shp_BD$v,na.rm = T)
minnum
maxnum

#assign proper labels
labels = c()
brks = c(minnum, pretty_breaks, maxnum)
brks

#round the extreme value label
for (ix in 1:length(brks)){
  labels = c(labels, round(brks[ix+1],2))
} #end for ix

#last digit is NA, so drop it
labels = labels[1:length(labels)-1]
labels



#define a new variable based on breaks
shp_BD$v.qt = cut(shp_BD$v,
                  breaks = brks,
                  include.lowest = T,
                  labels = labels,na.rm=T)

brks_scale = levels(shp_BD$v.qt)
labels_scale = (brks_scale)
labels_scale

shp_BD= st_sf(shp_BD)
pdf(file = paste0('Figures/', savefile), width = 7,height = 10)


pfull =  ggplot(data = shp_BD) +
  geom_sf(aes(color = v.qt),shape = "-", lwd=10)+
  coord_sf() +
  theme_map() +
  theme(legend.position = 'bottom') +
  scale_color_discrete_sequential(palette='Blues3', rev=T,
                                  drop = FALSE,
                                  breaks = (brks_scale),
                                  nmax = length(brks_scale) +1,
                                  order = 2:(length(brks_scale) +2),
                                  na.value = 'white',
                                  labels = labels_scale,
                                  name=paste( legendname, 'child marriage (%)'),
                                  guide = guide_legend(
                                    direction = 'horizontal',
                                    keyheight = unit(5, units='mm'),
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


###################################################################### SQUARE

for (j in 1){
  shp_BD$v = shp_BD$percent_females_married
  savefile = 'percent_females_married_BD20143.pdf'
  legendname = 'female'
}


###plot the shapefile
a = summary(shp_BD$v)
a
pretty_breaks = seq(a[2],a[5], length.out = 4)
pretty_breaks



###find the min/max value
minnum = min(shp_BD$v,na.rm = T)
maxnum = max(shp_BD$v,na.rm = T)
minnum
maxnum

#assign proper labels
labels = c()
brks = c(minnum, pretty_breaks, maxnum)
brks

#round the extreme value label
for (ix in 1:length(brks)){
  labels = c(labels, round(brks[ix+1],2))
} #end for ix

#last digit is NA, so drop it
labels = labels[1:length(labels)-1]
labels



#define a new variable based on breaks
shp_BD$v.qt = cut(shp_BD$v,
                  breaks = brks,
                  include.lowest = T,
                  labels = labels,na.rm=T)

brks_scale = levels(shp_BD$v.qt)
labels_scale = (brks_scale)
labels_scale

shp_BD= st_sf(shp_BD)
pdf(file = paste0('Figures/', savefile), width = 7,height = 10)


pfull =  ggplot(data = shp_BD) +
  geom_sf(aes(color = v.qt),shape = 15, lwd=7)+
  coord_sf() +
  theme_map() +
  theme(legend.position = 'bottom') +
  scale_color_discrete_sequential(palette='Blues3', rev=T,
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


###################################################### POLYGON

for (j in 1){
  shp_BD$v = shp_BD$percent_females_married
  savefile = 'percent_females_married_BD20144.pdf'
  legendname = 'female'
}


###plot the shapefile
a = summary(shp_BD$v)
a
pretty_breaks = seq(a[2],a[5], length.out = 4)
pretty_breaks



###find the min/max value
minnum = min(shp_BD$v,na.rm = T)
maxnum = max(shp_BD$v,na.rm = T)
minnum
maxnum

#assign proper labels
labels = c()
brks = c(minnum, pretty_breaks, maxnum)
brks

#round the extreme value label
for (ix in 1:length(brks)){
  labels = c(labels, round(brks[ix+1],2))
} #end for ix

#last digit is NA, so drop it
labels = labels[1:length(labels)-1]
labels



#define a new variable based on breaks
shp_BD$v.qt = cut(shp_BD$v,
                  breaks = brks,
                  include.lowest = T,
                  labels = labels,na.rm=T)

brks_scale = levels(shp_BD$v.qt)
labels_scale = (brks_scale)
labels_scale

shp_BD= st_sf(shp_BD)
pdf(file = paste0('Figures/', savefile), width = 7,height = 10)


pfull =  ggplot(data = shp_BD) +
  geom_sf(aes(color = v.qt),shape = 18, lwd=10)+
  coord_sf() +
  theme_map() +
  theme(legend.position = 'bottom') +
  scale_color_discrete_sequential(palette='Blues3', rev=T,
                                  drop = FALSE,
                                  breaks = (brks_scale),
                                  nmax = length(brks_scale) +1,
                                  order = 2:(length(brks_scale) +2),
                                  na.value = 'white',
                                  labels = labels_scale,
                                  name=paste( legendname, 'child marriage (%)'),
                                  guide = guide_legend(
                                    direction = 'horizontal',
                                    keyheight = unit(5, units='mm'),
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

