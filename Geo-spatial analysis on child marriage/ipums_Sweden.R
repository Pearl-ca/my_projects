##-----------------------------------------------------------------------------------##

##                                 SWEDEN 1880

##-----------------------------------------------------------------------------------##

##read in the data


swed<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00005.csv")

View(swed)
unique(swed$PARSE)
NROW(unique(swed$PARSE))

table(swed$GEO1_SE1880)
table(swed$PARSE)
sum(table(swed$PARSE)>=5000)##135

swd<-swed %>% 
  mutate(parish= sprintf("%09d", as.numeric(PARSE)),
         municipality = substr(parish,1,4))
  
swd

NROW(unique(swd$municipality))
sum(table(swd$municipality)>=5000)

##identify percentage of child marriage by county
sw<-swd %>% 
  subset(AGE<18) %>% 
  rename(COUNTY= GEO1_SE1880) %>% 
  group_by(COUNTY, municipality) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2& MARST<=4]),
         males_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(COUNTY, municipality, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(COUNTY, municipality, .keep_all=TRUE)

sw

## save datafile
write.csv(sw, file = 'Output/SWEDEN_child_1880.csv')

### find average child marriage in Sweden 1880
mean(sw$percent_married,na.rm = T)##  0.02111476%
mean(sw$percent_males_married,na.rm = T)##  0.001034226%
mean(sw$percent_females_married,na.rm=T)## 0.04153657%


##-----------------------------------------------------------------------------------##

##                                  SWEDEN 1890

##-----------------------------------------------------------------------------------##


##read in the data

swed1890<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00006.csv")
swed1890

swd1890<-swed1890 %>% 
  mutate(parish= sprintf("%09d", as.numeric(PARSE)),
         municipality = substr(parish,1,4))

##identify percentage of child marriage by county
sw1890<-swd1890 %>% 
  subset(AGE<18) %>% 
  rename(COUNTY= GEO1_SE1890) %>% 
  group_by(COUNTY, municipality) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2& MARST<=4]),
         males_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(COUNTY, municipality, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(COUNTY, municipality, .keep_all=TRUE)

sw1890

## save datafile
write.csv(sw1890, file = 'Output/SWEDEN_child_1890.csv')

### find average child marriage in Sweden 1890
mean(sw1890$percent_married,na.rm = T)##  0.03570858%
mean(sw1890$percent_males_married,na.rm = T)##  0.01886205%
mean(sw1890$percent_females_married,na.rm=T)## 0.05294093%


##-----------------------------------------------------------------------------------------##

##                                SWEDEN 1900

##-----------------------------------------------------------------------------------------##

##read in the data

swed1900<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00007.csv")
swed1900

swd1900<-swed1900 %>% 
  mutate(parish= sprintf("%09d", as.numeric(PARSE)),
         municipality = substr(parish,1,4))

##identify percentage of child marriage by county
sw1900<-swd1900 %>% 
  subset(AGE<18) %>% 
  rename(COUNTY= GEO1_SE1900) %>% 
  group_by(COUNTY, municipality) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2& MARST<=4]),
         males_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(COUNTY, municipality, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(COUNTY, municipality, .keep_all=TRUE)

sw1900

## save datafile
write.csv(sw1900, file = 'Output/SWEDEN_child_1900.csv')

### find average child marriage in Sweden 1900
mean(sw1900$percent_married,na.rm = T)##  0.0116691%
mean(sw1900$percent_males_married,na.rm = T)##  0.00125674%
mean(sw1900$percent_females_married,na.rm=T)## 0.02251412%






##-----------------------------------------------------------------------------------------##

##                                      SWEDEN 1910

##-----------------------------------------------------------------------------------------##

##read in the data

swed1910<-read.csv("C:/Users/Kasutaja/Downloads/ipumsi_00008.csv")
swed1910

swd1910<-swed1910 %>% 
  mutate(parish= sprintf("%09d", as.numeric(PARSE)),
         municipality = substr(parish,1,4))

##identify percentage of child marriage by county
sw1910<-swd1910 %>% 
  subset(AGE<18) %>% 
  rename(COUNTY= GEO1_SE1910) %>% 
  group_by(COUNTY, municipality) %>% 
  mutate(nchild = NROW(SEX),
         nmales  = NROW(SEX[SEX==1]),
         nfemales = NROW(SEX[SEX==2]),
         nchild_married = NROW(MARST[MARST>=2& MARST<=4]),
         males_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==1))),
         females_married=NROW(which((MARST>=2& MARST<=4 )& (SEX==2)))) %>% 
  select(COUNTY, municipality, nchild, nmales, nfemales, nchild_married,males_married,females_married) %>% 
  mutate(percent_married = (nchild_married/nchild) * 100,
         percent_males_married=(males_married/nmales)*100,
         percent_females_married=(females_married/nfemales)*100)%>% 
  distinct(COUNTY, municipality, .keep_all=TRUE)

sw1910

## save datafile
write.csv(sw1910, file = 'Output/SWEDEN_child_1910.csv')

### find average child marriage in Sweden 1910
mean(sw1910$percent_married,na.rm = T)##  0.01027807%
mean(sw1910$percent_males_married,na.rm = T)##  0.001252339%
mean(sw1910$percent_females_married,na.rm=T)## 0.01966083%


