#================================================================
#
#
#
#@Objective         This file reads India census primary abstract
#                   



#get the list of files
lf = list.files(path = 'F:/Datasets/India_Census/2011', pattern = '.xlsx', full.names = T)

d = NULL
i = 0

for (j in lf){
     
     i = i + 1
     print(i)
     d = rbind(d,read.xlsx(j, sheetIndex = 1))
     
}


