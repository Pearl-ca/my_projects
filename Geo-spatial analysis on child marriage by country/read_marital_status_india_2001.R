#=======================================================
#
#
#
#@Objective              Read the marital status data for
#                        India census 2001
#
#
#@Author                 Pearl Etie and Swapnil Singh



read_martial_status_india_2001 <- function(){
#=========================
#Read the file in sequence
#=========================

library(xlsx)
#get the file list for states

a_2001 <- list.files(path = paste0(datadir, 'india_census_marital_status/2001/'), pattern = '.xls', full.names=T)
View(a_2001)

#read each file and append it properly
#a_2001 <- lapply(a_2001, function(x) x[!is.na(x)]) ## removes NA within the lists

data_2001<-lapply(a_2001, read.xlsx, sheetIndex = 1, startRow = 8,colIndex = 1:25, header=F)##creates nested list

View(data_2001)

#fix(data_2001)

my_data_2001 <- do.call(rbind, data_2001)##flattens the nested list and appends into one file

##alternatively to read and append file
#f <- a_2001 %>%
#  set_names() %>% 
#  map_dfr(
#    ~ read.xlsx(.x, sheetIndex = 1, startRow = 8,colIndex = 1:22, header=F))


colnames(my_data_2001) <- c("Table Name", 
                            "State Code",
                            "District code", 
                            "Tehsil code", 
                            "Area Name", 
                            "countryside",
                                "Age-group","Total Persons","Total Males","Total Females",
                                "Single Persons","Single Males","Single Females",
                                "Married Persons","Married Males","Married Females",
                                "Widowed Persons","Widowed Males","Widowed Females",
                                "Divorced Persons","Divorced Males","Divorced Females",
                            "Unspecified Persons","Unspecified Males","Unspecified Females")

levels(my_data_2001$`Area Name`)
##write data
write.dta(my_data_2001, file = 'Output/india_marital_status_2001.dta')
write.csv(my_data_2001, file = 'Output/india_marital_status_2001.csv')



} #end function