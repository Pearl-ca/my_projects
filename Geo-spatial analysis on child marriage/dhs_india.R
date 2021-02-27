## load dhs data
library(haven)
dhs_india<-read_dta("Output/dhs_women_india_p7.dta")
dhs_india


## determine the those with child marriage
dhs_india_child<-dhs_india %>% 
  rename(district_code= district) %>% 
  group_by(district_code) %>% 
  mutate(marriage_age_months=cmc_code_of_first_marriage - dob_cmc,
         marriage_age_years = floor(marriage_age_months/12),
         nfemales=NROW(caseid),
         females_married=NROW(marriage_age_years[marriage_age_years<18]),
         percent_females_married = (females_married/nfemales) * 100) %>% 
  select(district_code,district_name,nfemales, females_married,percent_females_married) %>% 
  distinct(district_code, .keep_all=TRUE)

View(dhs_india_child)

#read the census data
d2011 = read.dta(file = 'Output/india_marital_status_2011.dta')
d2011 = d2011 %>% 
  mutate(state = as.integer(levels(`State Code`))[`State Code`] ) %>% 
  mutate(age = as.character(levels(`Age-group`))[`Age-group`])
View(d2011)

#create dataset for all district level
d2011.female.married = d2011 %>% 
  subset(countryside == 'Total' & state != 0 & `District code` != '000'  & (age == "0-9" | age == "10-14" | age == "15-19") ) %>% 
  mutate(district_code= `District code`,
         district_code=gsub("(?<![0-9])0+", "", district_code, perl = T),
         district_code=as.numeric(as.character(district_code))) %>% 
  group_by(state, district_code) %>% 
  mutate(nmales  = sum(`Total Males`),
         nfemales = sum(`Total Females`),
         nmarried_males = sum(`Married Males`,`Widowed Males`,`Divorced Males`,`Seperated Males`),
         nmarried_females = sum(`Married Females`,`Widowed Females`,`Divorced Females`,`Seperated Females`),
         npersons = sum(`Total Persons`),
         npersons_married = sum(`Married Persons`,`Widowed Persons`,`Divorced Persons`,`Seperated Persons`),
         percent_females_married=(nmarried_females/nfemales) * 100) %>% 
  select(state, district_code, nmarried_females,percent_females_married,age) %>% 
  subset(age == '0-9') %>% 
  select(-age)

View(d2011.female.married)

class(d2011.female.married$district_code)
class(dhs_india_child$district_code)

## merge the two datasets
dhs<-merge(dhs_india_child, d2011.female.married, by = "district_code")

dhs=dhs %>% 
  select(state, district_code,district_name,females_married,nmarried_females,percent_females_married.x,percent_females_married.y)

View(dhs)

## write the resulting data
write.dta(dhs, file = 'Output/dhs_indian_women.dta')
write.csv(dhs, file = 'Output/dhs_indian_women.csv')


## plot the correlation
plot(dhs$percent_females_married.y,dhs$percent_females_married.x, col="red",
     main = "Female Child Marriage in Indian Districts",
     xlab = "female census child marriage",
     ylab = "female survey child marriage",
     abline(lm(dhs$percent_females_married.x~dhs$percent_females_married.y,data = dhs), col="black"))


