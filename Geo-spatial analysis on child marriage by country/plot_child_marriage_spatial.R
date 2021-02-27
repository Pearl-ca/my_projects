#==========================================================
#
#
#
#@Objective              This file plots the spatial maps 
#                        of child marriage in India
#
#@Author                 Pearl Etie and Swapnil Singh
#
#
#
#@Description            First plot the spatial map for 2011 and then for 2001 and 1991
#


#===============================================================================================#
#                                                                                               #
#                        2011                                                                   #
#                                                                                               #
#===============================================================================================#

     
     #read the data
     d2011 = read.dta(file = 'Output/india_marital_status_2011.dta')
     d2011 = d2011 %>% 
          mutate(state = as.integer(levels(`State Code`))[`State Code`] ) %>% 
          mutate(age = as.character(levels(`Age-group`))[`Age-group`])
     
     #create dataset for district level, rural areas
     d2011.rural.female = d2011 %>% 
          subset(countryside == 'Rural' & state != 0 & `District code` != '000'  & (age == "0-9" | age == "10-14" | age == "15-19") ) %>% 
          mutate(district = substr(`Area Name`, (str_length(`Area Name`)-2),(str_length(`Area Name`)-1)))  %>%
          mutate(district = as.numeric(district)) %>% 
          group_by(state, district) %>% 
          mutate(nmales  = sum(`Total Males`),
                 nfemales = sum(`Total Females`),
                 married_males = sum(`Married Males`),
                 married_females = sum(`Married Females`),
                 npersons = sum(`Total Persons`),
                 npersons_married = sum(`Married Persons`)) %>% 
          select(state, district, nmales, nfemales, married_males, married_females, npersons, npersons_married , age) %>% 
          subset(age == '0-9') %>% 
          select(-age) %>% 
          mutate(percent_males_married = (married_males/nmales) * 100,
                 percent_females_married = (married_females/nfemales) * 100,
                 percent_married = (npersons_married/npersons) * 100)# %>% 
          subset(!is.na(percent_males_married) & !is.na(percent_females_married) & !is.na(percent_married))

        
     
     #read shapefiles
     shp2011 = st_as_sf(readOGR(dsn = paste0(datadir, 'india_shapefiles/2011_Dist.shp')))
     shp2011 = merge(shp2011, d2011.rural.female, by.x = c('ST_CEN_CD', 'DT_CEN_CD'), by.y=c('state', 'district'))
     shp2011

     
     for (j in 1:3){
             if (j == 1){
                     shp2011$v = shp2011$percent_females_married
                     savefile = 'percent_females_married_2011.pdf'
                     legendname = 'female'
                     
             } else if (j == 2){
                     shp2011$v = shp2011$percent_males_married
                     savefile = 'percent_males_married_2011.pdf'
                     legendname = 'male'
             } else if (j == 3){
                     shp2011$v = shp2011$percent_married
                     savefile = 'percent_married_2011.pdf'
                     legendname = 'male + female'
             }
             
             
             #plot the shapefile
             a = summary(shp2011$v)
             pretty_breaks = seq(a[2],a[5], length.out = 4)
             
             
             
             #find the min/max value
             minnum = min(shp2011$v,na.rm = T)
             maxnum = max(shp2011$v,na.rm = T)
             
             
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
             shp2011$v.qt = cut(shp2011$v,
                                 breaks = brks,
                                 include.lowest = T,
                                 labels = labels)
             
             brks_scale = levels(shp2011$v.qt)
             labels_scale = (brks_scale)
             
             shp2011 = st_sf(shp2011)
             pdf(file = paste0('Figures/', savefile), width = 7,height = 10)
             
             
             pfull =  ggplot(data = shp2011) +
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
                                                 name=paste('Rural area', legendname, 'child marriage (%)'),
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
     
     