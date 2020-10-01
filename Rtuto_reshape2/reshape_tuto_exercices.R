# R Tutorial - Aussois - September 26th, 2017
# Reshaping and Aggregating Data
# soulivanh.thao@gmail.com


# In this tutorial, we will talk about some useful functions that can help us prepare the data for one particular study or method. More specically, we will deal with the topic of reshaping (e.g. converting arrays to 2d tables) or aggregating (e.g. going from daily to monthly mean) data.

# We will have a particular focus on the package reshape2.

# Remarks: 
# It is also a good topic to mention high order functions: functions that takes other function as argument or that returns a function as a results). Most of the aggrating functions that we will see here are functionals (functions that takes other functions as argument).
# I only know the basics, so any new piece of information or corrections about this topic is welcome.


# sources: 
# for netcdf:
# http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html
# for reshape2 package:
# https://www.jstatsoft.org/article/view/v021i12
# http://seananderson.ca/2013/10/19/reshape.html
# http://www.milanor.net/blog/reshape-data-r-tidyr-vs-reshape2/

###################################################################
# Example

# loading packages
library(ncdf4)    # to read/write netcdf
library(reshape2) # to access reshaping and aggregating functions

library(fields)   # for the function image.plot
library(maps)     # provides functions that let us plot the maps
library(mapdata)  # contains the hi-resolution points that mark out the countries.

# getting the data for the world map in high resolution. 
worldmap <- map(database = "worldHires", plot = FALSE)

# importing netcdf data.
ncpath <- "tas_Amon_CNRM-CM5_abrupt4xCO2_r1i1p1_190001-194912.nc"
ncin <- nc_open(ncpath)
# get information about the netcdf
print(ncin)

# get longitude, latitude and time vectors
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")
time <- ncvar_get(ncin,"time") # according to the infomration in the netcdf, time is the number of day since the 1850-01-01

# get temperature at surface array
tas <- ncvar_get(ncin,"tas")
# Remark tas is a 3d array with dimension lon, lat and time

# rearrange the tas array so that longitudes are between - 180 and 180
lon <- ifelse(lon >  180, lon - 360, lon)
tas <- tas[order(lon),,]
lon <- sort(lon)

# give names to the dimmensions of the array
dimnames(tas) <- list("lon" = lon, "lat" = lat, "time" = time)

# get information about FillValue
fillvalue <- ncatt_get(ncin,"tas", "_FillValue")
# replace FillValues by NA
tas[tas == fillvalue$value] <- NA

# close the netcdf file.
nc_close(ncin)

# select 10W-40E 30N-50N box (because my computer is slow and has little memory)
ilon <- which(lon >= -10 & lon <= 40)
ilat <- which(lat >= 30 & lat <= 50)
tas_eur <- tas[ilon, ilat, ]
# Remark tas_eur is a 3d array with dimension lon, lat and time
dim(tas_eur)
str(tas_eur)

# reformat the array into a data.frame
# we used the function melt from the reshape2 packages
tas_eur_molten <- melt(tas_eur, value.name = "tas")
head(tas_eur_molten)
str(tas_eur_molten)

###################################################################
# Question:
# Is it better to work with an array or a data.frame?
# It probably depends on what we want to do.

###################################################################
# Task 1.
# Compute the climatology map and plot it.

# From the array
# Use a double loop to iterate over longitudes and latitudes and for each pair lat/lon compute the mean over the period.
# Using the fonctional function apply for the iteration: 
# it iterates over the first 2 dimensions of tas_eur (lon/lat) and make the mean of tas over the others dimensions (i.e. time).
tas_eur_clim_ar <- apply(tas_eur, MARGIN = c(1, 2), mean)

# From the molten data.frame
# acast splits the data.frame for each pair of lon and lat. For each part, it computes the mean of tas over the others dimensions(i.e. time) and then put everything back inyo a 2d array
tas_eur_clim_md <- acast(value.var = "tas", formula = lon ~ lat, data = tas_eur_molten, fun.aggregate = mean)
# give name to dimension of the array
names(dimnames(tas_eur_clim_md)) <- c("lon", "lat")

# check both results are the same
all.equal(tas_eur_clim_ar, tas_eur_clim_md)

# plot climatology map
image.plot(x = as.numeric(rownames(tas_eur_clim_md)),
           y = as.numeric(colnames(tas_eur_clim_md)),
           z = tas_eur_clim_md, 
           xlab = "lon", ylab = "lat", main = "climatology")
lines(worldmap)


################################################################## 
# Task 2.
# compute one climatology map per season
# adding columns about date and time

# with the array 
# compute seasons and other date variables from time
date <- as.Date(time, origin = "1850-01-01")
year <- as.numeric(format(date, "%Y"))
month <- as.numeric(format(date, "%m")) 
season <- as.character(cut(month, breaks = c(1,3, 6, 9, 12, 13), labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"), right = FALSE))

# iterates over the seasons and the each pair of lat/lon compute the mean of tas over the months belonging to the current season.
tas_eur_sclim_ar <- vapply(sort(unique(season)), 
                        FUN = function(s) apply(tas_eur[,, s == season], MARGIN = c(1, 2), FUN = mean), # anonymous function
                        FUN.VALUE = matrix(0, nrow = nrow(tas_eur), ncol = ncol(tas_eur))
)
names(dimnames(tas_eur_sclim_ar)) <- c("lon", "lat", "season")
str(tas_eur_sclim_ar)

# with the molten data.frame
# add in the data.frame information about dates and seasons obtained from the time variable.
tas_eur_molten$date <- as.Date(tas_eur_molten$time, origin = "1850-01-01")
tas_eur_molten$year <- as.numeric(format(tas_eur_molten$date, "%Y"))
tas_eur_molten$month <- as.numeric(format(tas_eur_molten$date, "%m")) 
tas_eur_molten$season <- as.character(cut(tas_eur_molten$month, breaks = c(1,3, 6, 9, 12, 13), labels = c("Winter", "Spring", "Summer", "Autumn", "Winter"), right = FALSE))
head(tas_eur_molten)

# use acast to compute the mean of tas for each combination of lon, lat and season. It returns everything back into a 3d array. 
tas_eur_sclim_md <- acast(value.var = "tas", formula = lon ~ lat ~ season, data = tas_eur_molten, fun.aggregate = mean)
# give name to dimension of the array.
names(dimnames(tas_eur_sclim_md)) <- c("lon", "lat", "season")
str(tas_eur_sclim_md)

# check both results are the same
all.equal(tas_eur_sclim_ar, tas_eur_sclim_md)

# plot climatology map for summer
image.plot(x = as.numeric(rownames(tas_eur_sclim_ar)),
           y = as.numeric(colnames(tas_eur_sclim_ar)),
           z = tas_eur_sclim_ar[,, "Summer"],
           xlab = "lon", ylab = "lat", main = "climatology of summer")
lines(worldmap)

##################################################################
# Some remarks:
# - working directly with an array could be a little messy if the number of dimensions is high and when the grouping factor is complex. (e.g. if we want to have the average value by season and by country using a contry mask)
# - using a function for aggregation such as dcast becomes easier to read and understand in such cases.
# - using a molten data.frame takes more memory since there is redundant information
# - depending on the implementation, aggregating functions on a molten data.frame can be quite slow.

##################################################################
# Exercices

# 1. Use acast to create yearly averaged time series for each lat lon.
tas_eur_yclim_md <- acast(value.var = "tas", formula = lon ~ lat ~ year, data = tas_eur_molten, fun.aggregate = mean)
names(dimnames(tas_eur_yclim_md)) <- c("lon", "lat", "year")

# 2. Use dcast to create a time series of yearly maxima over the whole region.
tas_eur_ymax_md <- dcast(value.var = "tas", formula = year ~ ., data = tas_eur_molten, fun.aggregate = max)
names(tas_eur_ymax_md) <- c("year", "tas")
with(tas_eur_ymax_md, plot(year, tas, type = "l"))

# 3. Use dcast to create from tas_eur a yearly time series averaged over the whole region but only for latitude over 40N (hint: the function subset can be used).

###################################################################
# Remarks:
# Other functions to aggregate data and available by default in R: 
# aggregate (library stats)
# by (library base)
# Other packages such as plyr, dplyr or data.table have functions for aggregation. So no need to use only the functions presented here.

# 4. Use the function aggregate to create the climatology map from tas_eur_molten and then plot it.

# 5. Use the function by to create the climatology map from tas_eur_molten and then plot it.

# 6. Use the function by to compute the map of linear trends of tas from tas_eur_molten and then plot it. 

# 7. Use the function apply to compute the map of linear trends of tas from tas_eur and then plot it.


