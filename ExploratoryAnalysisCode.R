#Put Code Here
# Kianoosh is here
# Harpreet here
# This is suhwan

# write your code and push the changes. then I will merge the files to gether. 

# needed packages ---------------------------------------------------------
# uncomment install lines if you do not have the package and comment them after you are done!
#install.packages("ncdf4")
library(ncdf4) # package for netcdf manipulation
#install.packages("raster")
library(raster) # package for raster manipulation
#install.packages("rgda1")
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


# reading the data
nc_SST_orig <- nc_open('SSTdata_011948_022018.nc')
nc_Prec_orig <- nc_open('Pdata_011948_022018-1.nc')


lon <- ncvar_get(nc_Prec_orig, "lon")
lat <- ncvar_get(nc_Prec_orig, "lat", verbose = F)
t <- ncvar_get(nc_Prec_orig, "time")

head(lon) # look at the first few entries in the longitude vector

# dimension reduction


# Clustering


# prediction

