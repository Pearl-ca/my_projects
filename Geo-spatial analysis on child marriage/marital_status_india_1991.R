#=======================================================
#
#
#
#@Objective              Read the marital status data for
#                        India census 2001
#
#
#@Author                 Pearl Etie and Swapnil Singh



marital_status_india_1991 <- function(){


#get the file list for states
a_1991 <- list.files(path = paste0(datadir, 'india_census_marital_status/1991/'), pattern ="csv",full.names = T)
a_1991  ## some files have duplicates

## select the copies
duplicate<-list.files(path = paste0(datadir, 'india_census_marital_status/1991/'), pattern ="\\(1)",full.names = T)

##keep original
a_1991<-a_1991[!a_1991%in%duplicate]
a_1991

# read in all the CSV files
my_data_1991 <- lapply(a_1991, function(X){
  data.frame(Area=gsub(".*For","", basename(X)),State="",Countryside="",read.csv(X,skip = 11,header = F))})

# append all files into a single dataframe
my_data_1991 <- do.call(rbind,my_data_1991 )
View(my_data_1991)

my_data_1991$Area<-gsub(".csv.*","",my_data_1991$Area)
my_data_1991$State<-gsub(".*District","",my_data_1991$Area)
View(my_data_1991)

##specify column names
colnames(my_data_1991) <- c( 
                            "Area name",
                            "State",
                            "countryside",
                            "Age-group","Total Persons","Total Males","Total Females",
                            "Single Males","Single Females",
                            "Married Males","Married Females",
                            "Widowed Males","Widowed Females",
                            "Divorced Males","Divorced Females",
                            "Unspecified Males","Unspecified Females")


View(my_data_1991)

##rearrange file in ascending order by State not Area name
my_data_1991<-my_data_1991 %>% 
  arrange(State)
View(my_data_1991)

sapply(my_data_1991,class)

## fill countryside

count<-0
Region<-NULL
for (i in  my_data_1991$`Total Persons`){
  
  if (i %in% c("TOTAL","RURAL","CITY/U.A.:","URBAN")){
    
  prev=i
  count=count+1
  rbind(Region,data.frame(x=i))->Region
  a<-my_data_1991$`Total Persons`[my_data_1991$`Total Persons`[i]]
  }else{
    
    
    i=prev
    count=count+1
    rbind(Region,data.frame(x=i))->Region
    

  }  
}  
    
print(str(my_data_1991))    

View(Region)


my_data_1991$countryside<-Region$x
my_data_1991$countryside         

View(my_data_1991)

##remove unwanted rows
table(my_data_1991$`Total Persons`%in%c("TOTAL","RURAL","CITY/U.A.:","URBAN"))
##c("TOTAL","RURAL","CITY/U.A.:","URBAN")shows 1147 times
## number of columns needed
1147*19##(for all age groups)

##number of rows needed 21793
my_data_1991<-na.omit(my_data_1991)
View(my_data_1991)

# write resulting data
write.dta(my_data_1991, file = 'Output/india_marital_status_1991.dta')
write.csv(my_data_1991, file = 'Output/india_marital_status_1991.csv')


}  #end function