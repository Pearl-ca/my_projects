library(raster)
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
##r = raster("C:/Users/Kasutaja/Documents/microeconometrics/precip.mon.mean.0.5x0.5 (1).nc")
#brick(r)

#------------------------------------------#

##read the dataset of precipitation

#------------------------------------------#


#prep<-ncdf4::nc_open("https://psl.noaa.gov/thredds/dodsC/Datasets/precl/0.5deg/precip.mon.mean.0.5x0.5.nc")
prep<-nc_open("precip.mon.mean.0.5x0.5 (1).nc")

print(prep)

lon <- ncvar_get(prep, "lon")
nlon <- dim(lon)
nlon
head(lon)
tail(lon)
lat <- ncvar_get(prep, "lat", verbose = F)
nlat <- dim(lat)
head(lat)
tail(lat)
print(c(nlon, nlat))

time <- ncvar_get(prep, "time")
tunits <- ncatt_get(prep, "time", "units")
tunits
nt <- dim(time)

name<-"precip"
prep.array <- ncvar_get(prep, "precip")
dlname <- ncatt_get(prep, name, "long_name")
dunits <- ncatt_get(prep, name, "units")
fillvalue <- ncatt_get(prep, name, "_FillValue")
dim(prep.array)

title <- ncatt_get(prep, 0, "title")
institution <- ncatt_get(prep, 0, "institution")
datasource <- ncatt_get(prep, 0, "source")
references <- ncatt_get(prep, 0, "references")
history <- ncatt_get(prep, 0, "history")
Conventions <- ncatt_get(prep, 0, "Conventions")


nc_close(prep)


# split the time units string into fields

tunits$value
tustr <- strsplit(tunits$value, " ")
tustr
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(time, origin = c(tmonth, tday, tyear))


prep.array[prep.array == fillvalue$value] <- NA

length(na.omit(as.vector(prep.array[, , 1])))

m <- 1
prep.slice <- prep.array[, , m]


grid <- expand.grid(lon = lon, lat = lat)
cutpts <- c(-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4)
levelplot(prep.slice ~ lon * lat, data = grid, at = cutpts, cuts = 11, pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))



lonlat <- expand.grid(lon, lat)
prep.vec <- as.vector(prep.slice)
length(prep.vec)


prep.df01 <- data.frame(cbind(lonlat, prep.vec))
names(prep.df01) <- c("lon", "lat", paste(name, as.character(m), sep = "_"))
head(na.omit(prep.df01), 100)
View(prep.df01)

csvfile <- "precipitation.csv"
write.table(na.omit(prep.df01), csvfile, row.names = FALSE, sep = ",")


prep.vec.long <- as.vector(prep.array)
length(prep.vec.long)

prep.mat <- matrix(prep.vec.long, nrow = nlon * nlat, ncol = nt)
dim(prep.mat)
head(na.omit(prep.mat))

#------------------------------------------#

##read the dataset of temperature

#------------------------------------------#

temp = nc_open("air.mon.mean.nc")

print(temp)


tlon <- ncvar_get(temp, "lon")
tnlon <- dim(lon)
tnlon
head(tlon)
tail(tlon)
tlat <- ncvar_get(temp, "lat", verbose = F)
tnlat <- dim(tlat)
head(lat)
tail(lat)
print(c(nlon, nlat))

temptime <- ncvar_get(temp, "time")
temptunits <- ncatt_get(temp, "time", "units")
temptunits
tempnt <- dim(temptime)


 
tname<-"air"
temp.array <- ncvar_get(temp, tname)
#memory.limit()

## To increase the storage capacity
##memory.limit(size=56000)
tdlname <- ncatt_get(temp, tname, "long_name")
tdunits <- ncatt_get(temp, tname, "units")
fillvalue <- ncatt_get(temp, tname, "_FillValue")
dim(temp.array)

t_title <- ncatt_get(temp, 0, "title")
t_institution <- ncatt_get(temp, 0, "institution")
t_datasource <- ncatt_get(temp, 0, "source")
t_references <- ncatt_get(temp, 0, "references")
t_history <- ncatt_get(temp, 0, "history")
t_Conventions <- ncatt_get(temp, 0, "Conventions")


nc_close(temp)


# split the time units string into fields

temptunits$value
t_tustr <- strsplit(temptunits$value, " ")
t_tustr
t_tdstr <- strsplit(unlist(t_tustr)[3], "-")
t_tmonth = as.integer(unlist(t_tdstr)[2])
t_tday = as.integer(unlist(t_tdstr)[3])
t_tyear = as.integer(unlist(t_tdstr)[1])
chron(temptime, origin = c(t_tmonth, t_tday, t_tyear))
chron(temptime)

temp.array[temp.array == fillvalue$value] <- NA

length(na.omit(as.vector(temp.array[, , 1])))

n <- 1
temp.slice <- temp.array[, , n]


t_grid <- expand.grid(lon = tlon, lat = tlat)
t_cutpts <- c(220,230,240,250,260,270,280,290,300,310,320)
levelplot(temp.slice ~ lon * lat, data = t_grid, at = t_cutpts, cuts = 11, pretty = T, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))



t_lonlat <- expand.grid(tlon, tlat)
temp.vec <- as.vector(temp.slice)
length(temp.vec)


temp.df01 <- data.frame(cbind(t_lonlat, temp.vec))
names(temp.df01) <- c("lon", "lat", paste(name, as.character(n), sep = "_"))
head(na.omit(temp.df01), 100)
View(temp.df01)

t_csvfile <- "temperature.csv"
write.table(na.omit(temp.df01), t_csvfile, row.names = FALSE, sep = ",")


temp.vec.long <- as.vector(temp.array)
length(temp.vec.long)

temp.mat <- matrix(temp.vec.long, nrow = tnlon * tnlat, ncol = tempnt)
dim(prep.mat)
head(na.omit(temp.mat))

#library(idx2r)
#i<-read_idx("C:/Users/Kasutaja/Documents/microeconometrics/ghcn_cams_1948_cur_t62.grb.idx", endian = "big")







