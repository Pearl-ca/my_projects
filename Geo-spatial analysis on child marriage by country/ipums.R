
##-----------------------------------------------------------------------------------##

##USA 1880

##-----------------------------------------------------------------------------------##

##read in the data

memory.limit()

memory.limit(size=200000)

pop<-read.csv("C:/Users/Kasutaja/Downloads/usa_00005.csv")
                                                                                                                           
View(pop)


##identify percentage of child marriage by county
pop2<-pop %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2

## save datafile
write.csv(pop2, file = 'Output/USA_child_1880.csv')

### find average child marriage in USA 1880
mean(pop2$percent_married)## 0.5569712%
mean(pop2$percent_males_married,na.rm = T)##  0.06182563%
mean(pop2$percent_females_married,na.rm=T)## 1.071507%

##-----------------------------------------------------------------------------------------##

##USA 1900

##-----------------------------------------------------------------------------------------##




pop_1900<-read.csv("C:/Users/Kasutaja/Downloads/usa_00006.csv")

pop_1900


##identify percentage of child marriage by county
pop2_1900<-pop_1900 %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2_1900

## save datafile
write.csv(pop2_1900, file = 'Output/USA_child_1900.csv')

### find average child marriage in USA 1900
mean(pop2_1900$percent_married)## 0.546791%
mean(pop2_1900$percent_males_married,na.rm = T)##  0.09043483%
mean(pop2_1900$percent_females_married,na.rm=T)## 1.015295%


##-----------------------------------------------------------------------------------------##

##USA 1910

##-----------------------------------------------------------------------------------------##




pop_1910<-read.csv("C:/Users/Kasutaja/Downloads/usa_00007.csv")

pop_1910


##identify percentage of child marriage by county
pop2_1910<-pop_1910 %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2_1910

## save datafile
write.csv(pop2_1910, file = 'Output/USA_child_1910.csv')

### find average child marriage in USA 1910
mean(pop2_1910$percent_married)## 0.5103282%
mean(pop2_1910$percent_males_married,na.rm = T)##  0.06506588%
mean(pop2_1910$percent_females_married,na.rm=T)## 0.9679115%

##-----------------------------------------------------------------------------------------##

##USA 1920

##-----------------------------------------------------------------------------------------##




pop_1920<-read.csv("C:/Users/Kasutaja/Downloads/usa_00008.csv")

pop_1920


##identify percentage of child marriage by county
pop2_1920<-pop_1920 %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2_1920

## save datafile
write.csv(pop2_1920, file = 'Output/USA_child_1920.csv')

### find average child marriage in USA 1920
mean(pop2_1920$percent_married)## 0.625764%
mean(pop2_1920$percent_males_married,na.rm = T)##  0.1854865%
mean(pop2_1920$percent_females_married,na.rm=T)## 1.076813%


##-----------------------------------------------------------------------------------------##

##USA 1930

##-----------------------------------------------------------------------------------------##




pop_1930<-read.csv("C:/Users/Kasutaja/Downloads/usa_00009.csv")

pop_1930


##identify percentage of child marriage by county
pop2_1930<-pop_1930 %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2_1930

## save datafile
write.csv(pop2_1930, file = 'Output/USA_child_1930.csv')

### find average child marriage in USA 1930
mean(pop2_1930$percent_married)## 0.610274%
mean(pop2_1930$percent_males_married,na.rm = T)##  0.08934181%
mean(pop2_1930$percent_females_married,na.rm=T)## 1.147776%



##-----------------------------------------------------------------------------------------##

##USA 1930

##-----------------------------------------------------------------------------------------##




pop_1940<-read.csv("C:/Users/Kasutaja/Downloads/usa_00012.csv")

pop_1940


##identify percentage of child marriage by county
pop2_1940<-pop_1940 %>% 
  subset(AGE<18) %>% 
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST<6]),
         males_married=NROW(which((MARST < 6 )& (SEX==1))),
         females_married=NROW(which((MARST < 6 )& (SEX==2)))) %>% 
  select(STATEICP, COUNTYICP, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(STATEICP,COUNTYICP, .keep_all=TRUE)

pop2_1940

## save datafile
write.csv(pop2_1940, file = 'Output/USA_child_1940.csv')

### find average child marriage in USA 1940
mean(pop2_1940$percent_married)## 0.6541609%
mean(pop2_1940$percent_males_married,na.rm = T)##  0.1507451%
mean(pop2_1940$percent_females_married,na.rm=T)## 1.171827%


