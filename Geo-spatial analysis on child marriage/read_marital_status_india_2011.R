#=======================================================
#
#
#
#@Objective              Read the marital status data for
#                        India census 2011
#
#
#@Author                 Pearl Etie and Swapnil Singh


read_marital_status_india_2011 <- function(){


#=========================
#Read the file in sequence
#=========================

#get the file list for states

a_2011 <- list.files(path = paste0(datadir, 'india_census_marital_status/2011/'), pattern = '.xls', full.names=T)

#read each file and append it properly
#a_2011 <- lapply(a_2011, function(x) x[!is.na(x)])  ## removes NA within the lists

data_2011<-lapply(a_2011, read.xlsx, sheetIndex = 1, startRow = 8,colIndex = 1:27, header=F) ## reads & creates a nested list
View(data_2011)

my_data_2011 <- do.call(rbind, data_2011) ##flattens it and appends the data into one file



colnames(my_data_2011) <- c("Table Name", "State Code", "District code","Area Name","countryside",
                            "Age-group","Total Persons","Total Males","Total Females",
                            "Single Persons","Single Males","Single Females",
                            "Married Persons","Married Males","Married Females",
                            "Widowed Persons","Widowed Males","Widowed Females",
                            "Seperated Persons","Seperated Males","Seperated Females",
                            "Divorced Persons","Divorced Males","Divorced Females",
                            "Unspecified Persons","Unspecified Males","Unspecified Females")

View(my_data_2011)
## write data
write.dta(my_data_2011, file = 'Output/india_marital_status_2011.dta')
write.csv(my_data_2011, file = 'Output/india_marital_status_2011.csv')




##rename differing states and districts
levels(my_data_2011$`Area Name`)
my_data_2011$`Area Name`<- fct_relevel(my_data_2011$`Area Name`, "State - NCT OF DELHI (07)",
                                       "State - ODISHA (21)",
                                       "State - UTTARAKHAND (05)",
                                       "State - PUDUCHERRY (34)",
                                       "District - Rajouri (06)",
                                       "District - Almora\n (09)",
                                       "District - Shahid Bhagat Singh Nagar  (05)",
                                       "District - Mahamaya Nagar (13)" ,
                                       "District - Bara Banki (45)",
                                       "District - North  District (01)",
                                       "District - West District (02)",
                                       "District - East District (04)" ,
                                       "District - South District (03)",
                                       "District - Ribhoi (05)",
                                       "District - Sivasagar (12)",
                                       "District - Dima Hasao (16)" ,
                                       "District - Paschim Medinipur (18)",
                                       "District - Pakur (08)",
                                       "District - Subarnapur (23)",
                                       "District - Kabeerdham (08)" ,
                                       "District - Uttar Bastar Kanker (14)",
                                       "District - Dakshin Bastar Dantewada (17)",
                                       "District - Khandwa (East Nimar) (49)",
                                       "District - Khargone (West Nimar) (23)",
                                       "District - Mumbai (23)",
                                       "District - Sri Potti Sriramulu Nellore (19)",
                                       "District - Y.S.R. (20)" ,
                                       "District - Puducherry (02)" ,
                                       "District - North  & Middle Andaman (02)" )

my_data_2011$`Area Name`<- fct_recode(my_data_2011$`Area Name`,
                                      "State - DELHI (07)"="State - NCT OF DELHI (07)",
                                      "State - ORISSA (21)"="State - ODISHA (21)",
                                      "State - UTTARANCHAL (05)"="State - UTTARAKHAND (05)",
                                      "State - PONDICHERRY (34)"="State - PUDUCHERRY (34)",
                                      "District - Rajauri (06)"="District - Rajouri (06)",
                                      "District - Almora (09)"="District - Almora\n (09)",
                                      "District - Nawanshahr (05)"="District - Shahid Bhagat Singh Nagar  (05)",
                                      "District - Hathras (13)"="District - Mahamaya Nagar (13)" ,
                                      "District - Barabanki (45)"="District - Bara Banki (45)",
                                      "District - North (01)"="District - North  District (01)",
                                      "District - West (02)"="District - West District (02)",
                                      "District - East (04)"="District - East District (04)" ,
                                      "District - South (03)"="District - South District (03)",
                                      "District - Ri Bhoi (05)"="District - Ribhoi (05)",
                                      "District - Sibsagar (12)"="District - Sivasagar (12)",
                                      "District - North Cachar Hills (16)"="District - Dima Hasao (16)" ,
                                      "District - Medinipur (18)"="District - Paschim Medinipur (18)",
                                      "District - Pakaur (08)"="District - Pakur (08)",
                                      "District - Sonapur (23)"="District - Subarnapur (23)",
                                      "District - Kawardha (08)"="District - Kabeerdham (08)" ,
                                      "District - Kanker (14)"="District - Uttar Bastar Kanker (14)",
                                      "District - Dantewada (17)"="District - Dakshin Bastar Dantewada (17)",
                                      "District - East Nimar (49)"="District - Khandwa (East Nimar) (49)",
                                      "District - West Nimar (23)"="District - Khargone (West Nimar) (23)",
                                      "District - Mumbai (Suburban) (23)"="District - Mumbai (23)",
                                      "District - Nellore (19)"="District - Sri Potti Sriramulu Nellore (19)",
                                      "District - Cuddapah (20)"="District - Y.S.R. (20)" ,
                                      "District - Pondicherry (02)"="District - Puducherry (02)" ,
                                      "District - Andamans (02)"="District - North  & Middle Andaman (02)" )
levels(my_data_2011$`Area Name`)

View(my_data_2011)

## write data
write.dta(my_data_2011, file = 'Output/india_marital_status_2011.dta')
write.csv(my_data_2011, file = 'Output/india_marital_status_2011.csv')


} #end function

