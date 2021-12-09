project3
================
12/08/2021

<!-- #Above is the header. there are options to change formatting if the default doesn't suit your style -->
<!-- #Below is an example of an r code chunk, Code->Insert Chunk (or Ctrl+Alt+I / Cmd+Option +I) the r means it is an r code chunk, the next word is the chunk title, and after the comma is chunk options--->

``` r
# needed packages ---------------------------------------------------------
# uncomment install lines if you do not have the package and comment them after you are done!
#install.packages("ncdf4")
library(ncdf4) # package for netcdf manipulation
#install.packages("raster")
library(raster) # package for raster manipulation
```

    ## Loading required package: sp

``` r
#install.packages("rgdal")
library(rgdal) # package for geospatial analysis
```

    ## Please note that rgdal will be retired by the end of 2023,
    ## plan transition to sf/stars/terra functions using GDAL and PROJ
    ## at your earliest convenience.
    ## 
    ## rgdal: version: 1.5-27, (SVN revision 1148)
    ## Geospatial Data Abstraction Library extensions to R successfully loaded
    ## Loaded GDAL runtime: GDAL 3.2.1, released 2020/12/29
    ## Path to GDAL shared files: C:/Users/70950/Documents/R/win-library/4.0/rgdal/gdal
    ## GDAL binary built with GEOS: TRUE 
    ## Loaded PROJ runtime: Rel. 7.2.1, January 1st, 2021, [PJ_VERSION: 721]
    ## Path to PROJ shared files: C:/Users/70950/Documents/R/win-library/4.0/rgdal/proj
    ## PROJ CDN enabled: FALSE
    ## Linking to sp version:1.4-6
    ## To mute warnings of possible GDAL/OSR exportToProj4() degradation,
    ## use options("rgdal_show_exportToProj4_warnings"="none") before loading sp or rgdal.
    ## Overwritten PROJ_LIB was C:/Users/70950/Documents/R/win-library/4.0/rgdal/proj

``` r
library(ggplot2) # package for plotting



# reading the data
nc_SST_orig <- nc_open('F:\\courses\\STAT\\8330 data analysis 3\\project3\\SSTdata_011948_022018.nc')
nc_prec_orig <- nc_open('F:\\courses\\STAT\\8330 data analysis 3\\project3\\Pdata_011948_022018-1.nc')

##### SST variables 
zlev <- ncvar_get(nc_SST_orig, "zlev")
# East Longitude (deg)
X_SST <- ncvar_get(nc_SST_orig, "X")
print (dim(X_SST))
```

    ## [1] 84

``` r
# time from Jan. 1948 to Feb. 2018 
# 12*(2018-1948)+1(Jan 2018)+1(Feb 2018) = 842
T_SST <- ncvar_get(nc_SST_orig, "T")
print (dim(T_SST))
```

    ## [1] 842

``` r
# Latitude (deg)
Y_SST <- ncvar_get(nc_SST_orig, "Y")
print (dim(Y_SST))
```

    ## [1] 33

``` r
SST_anom <- ncvar_get(nc_SST_orig, "anom")
print (dim(SST_anom))
```

    ## [1]  84  33 842

``` r
image(x=X_SST, y=Y_SST, z=SST_anom[,,842], 
      col = hcl.colors(12, "YlOrRd", rev = TRUE),
      xlab="East Longitude (deg)",
      ylab="Latitude (deg)", main="SST Anomalies: Feb 2018")
```

![](proj3_files/figure-gfm/no-format-1.png)<!-- -->

``` r
##### precipitation variables
# East Longitude (deg)
X_prec <- ncvar_get(nc_prec_orig, "X")
print (dim(X_prec))
```

    ## [1] 120

``` r
# time 
T_prec <- ncvar_get(nc_prec_orig, "T")
print (dim(T_prec))
```

    ## [1] 842

``` r
# Latitude (deg)
Y_prec <- ncvar_get(nc_prec_orig, "Y")
print (dim(Y_prec))
```

    ## [1] 50

``` r
# Precipitation
prec <- ncvar_get(nc_prec_orig, "rain")
# reproduce the image in file
image(x=X_prec, y=Y_prec, z=prec[,,842], 
      col = hcl.colors(12, "YlOrRd", rev = TRUE),
      xlab="West Longitude (deg)",
      ylab="Latitude (deg)", main="Precipitation: Feb 2018")
```

![](proj3_files/figure-gfm/no-format-2.png)<!-- -->

``` r
#prec
prec[is.na(prec)] <- 0
#prec
```
