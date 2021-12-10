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
nc_prec_orig <- nc_open('Pdata_011948_022018-1.nc')

##### SST variables 
zlev <- ncvar_get(nc_SST_orig, "zlev")
# East Longitude (deg)
X_SST <- ncvar_get(nc_SST_orig, "X")
print (dim(X_SST))
# time from Jan. 1948 to Feb. 2018 
# 12*(2018-1948)+1(Jan 2018)+1(Feb 2018) = 842
T_SST <- ncvar_get(nc_SST_orig, "T")
print (dim(T_SST))
# Latitude (deg)
Y_SST <- ncvar_get(nc_SST_orig, "Y")
print (dim(Y_SST))
SST_anom <- ncvar_get(nc_SST_orig, "anom")
print (dim(SST_anom))
image(x=X_SST, y=Y_SST, z=SST_anom[,,842], 
      col = hcl.colors(12, "YlOrRd", rev = TRUE),
      xlab="East Longitude (deg)",
      ylab="Latitude (deg)", main="SST Anomalies: Feb 2018")

##### precipitation variables
# East Longitude (deg)
X_prec <- ncvar_get(nc_prec_orig, "X")
print (dim(X_prec))
# time 
T_prec <- ncvar_get(nc_prec_orig, "T")
print (dim(T_prec))
# Latitude (deg)
Y_prec <- ncvar_get(nc_prec_orig, "Y")
print (dim(Y_prec))
# Precipitation
prec <- ncvar_get(nc_prec_orig, "rain")
# reproduce the image in file
image(x=X_prec, y=Y_prec, z=prec[,,842], 
      col = hcl.colors(12, "YlOrRd", rev = TRUE),
      xlab="West Longitude (deg)",
      ylab="Latitude (deg)", main="Precipitation: Feb 2018")

# dimension reduction


# Clustering


# prediction

