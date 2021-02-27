
#read the data
d2001 = read.dta(file = 'Output/india_marital_status_2001.dta')
d2001
d2001 = d2001 %>% 
  mutate(state = as.integer(levels(`State Code`))[`State Code`] ) %>% 
  mutate(age = as.character(levels(`Age-group`))[`Age-group`]) %>%  
  arrange(`state`)

View(d2001)

#create dataset for district level, rural areas in 2001
d2001.rural.female = d2001 %>% 
  subset(countryside == 'Rural' & state != 0 & `District code` != '00'  & (age == "0-9" | age == "10-14" | age == "15-19") ) %>% 
  mutate(district = substr(`Area Name`, (str_length(`Area Name`)-1),(str_length(`Area Name`))))  %>%
  mutate(district = as.numeric(district)) %>% 
  group_by(state, district) %>% 
  mutate(nmales  = sum(`Total Males`),
         nfemales = sum(`Total Females`),
         married_males = sum(`Married Males`,`Widowed Males`,`Divorced Males`),
         married_females = sum(`Married Females`,`Widowed Females`,`Divorced Females`),
         npersons = sum(`Total Persons`),
         npersons_married = sum(`Married Persons`,`Widowed Persons`,`Divorced Persons`)) %>% 
  select(state, district, nmales, nfemales, married_males, married_females, npersons, npersons_married , age) %>% 
  subset(age == '0-9') %>% 
  select(-age) %>% 
  mutate(percent_males_married = (married_males/nmales) * 100,
         percent_females_married = (married_females/nfemales) * 100,
         percent_married = (npersons_married/npersons) * 100) #%>%
  #subset(!is.na(percent_males_married) & !is.na(percent_females_married) & !is.na(percent_married))
View(d2001.rural.female)

#read the data
d2011 = read.dta(file = 'Output/india_marital_status_2011.dta')
d2011 = d2011 %>% 
  mutate(state = as.integer(levels(`State Code`))[`State Code`] ) %>% 
  mutate(age = as.character(levels(`Age-group`))[`Age-group`])
View(d2011)

#create dataset for district level, rural areas
d2011.rural.female = d2011 %>% 
  subset(countryside == 'Rural' & state != 0 & `District code` != '000'  & (age == "0-9" | age == "10-14" | age == "15-19") ) %>% 
  mutate(district = substr(`Area Name`, (str_length(`Area Name`)-2),(str_length(`Area Name`)-1)))  %>%
  mutate(district = as.numeric(district)) %>% 
  group_by(state, district) %>% 
  mutate(nmales  = sum(`Total Males`),
         nfemales = sum(`Total Females`),
         married_males = sum(`Married Males`,`Widowed Males`,`Divorced Males`,`Seperated Males`),
         married_females = sum(`Married Females`,`Widowed Females`,`Divorced Females`,`Seperated Females`),
         npersons = sum(`Total Persons`),
         npersons_married = sum(`Married Persons`,`Widowed Persons`,`Divorced Persons`,`Seperated Persons`)) %>% 
  select(state, district, nmales, nfemales, married_males, married_females, npersons, npersons_married , age) %>% 
  subset(age == '0-9') %>% 
  select(-age) %>% 
  mutate(percent_males_married = (married_males/nmales) * 100,
         percent_females_married = (married_females/nfemales) * 100,
         percent_married = (npersons_married/npersons) * 100)# %>% 
#subset(!is.na(percent_males_married) & !is.na(percent_females_married) & !is.na(percent_married))



b<-read.csv("Output/india_district_list_2011.csv")
View(b)

d2011.rural.female$district<-b$districtcode2001

View(d2011.rural.female)



#split and merge the districts back to its original form
divide3 <- function(x, na.rm=FALSE) (x/3)
divide2 <- function(x, na.rm=FALSE) (x/2)
same<-function(x,na.rm=FALSE)(x)

library(tidyr)
d2011.rural<- d2011.rural.female %>%
  mutate(nmales = ifelse((grepl("&",district)), divide2(nmales), same(nmales)),
         nfemales = ifelse((grepl("&",district)), divide2(nfemales), same(nfemales)),
         married_males = ifelse((grepl("&",district)), divide2(married_males), same(married_males)),
         married_females = ifelse((grepl("&",district)), divide2(married_females), same(married_females)),
         npersons = ifelse((grepl("&",district)), divide2(npersons), same(npersons)),
         npersons_married = ifelse((grepl("&",district)), divide2(npersons_married), same(npersons_married))) %>% 
  mutate(nmales = ifelse((grepl(",",district)), divide3(nmales), same(nmales)),
         nfemales = ifelse((grepl(",",district)), divide3(nfemales), same(nfemales)),
         married_males = ifelse((grepl(",",district)), divide3(married_males), same(married_males)),
         married_females = ifelse((grepl(",",district)), divide3(married_females), same(married_females)),
         npersons = ifelse((grepl(",",district)), divide3(npersons), same(npersons)),
         npersons_married = ifelse((grepl(",",district)), divide3(npersons_married), same(npersons_married))) %>%
  separate_rows(district,sep = "([\\&\\,])") %>% 
  group_by(state,district) %>% 
  summarise(nmales  = sum(nmales),
            nfemales = sum(nfemales),
            married_males = sum(married_males),
            married_females = sum(married_females),
            npersons = sum(npersons),
            npersons_married = sum(npersons_married)) %>% 
  mutate(percent_males_married = (married_males/nmales) * 100,
         percent_females_married = (married_females/nfemales) * 100,
         percent_married = (npersons_married/npersons) * 100) 
  

### merge the two datasets
d2001_2011=merge(d2001.rural.female,d2011.rural, by = c('state', 'district'))

## calculate change in 2011 from 2001
d2001_2011<-d2001_2011 %>% 
  group_by(state,district) %>% 
  arrange(state,district) %>% 
  mutate(percentchange_males_married = (percent_males_married.y-percent_males_married.x),
         percentchange_females_married = (percent_females_married.y-percent_females_married.x),
         percentchange_married = (percent_married.y-percent_married.x))

View(d2001_2011)
#fix(d2001_2011)


save(file = 'Output/child_marriage_change_2001_2011.RData', d2001_2011)

