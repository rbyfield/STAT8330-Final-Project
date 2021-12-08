# needed packages ---------------------------------------------------------
# uncomment install lines if you do not have the package and comment them after you are done!
#install.packages("ncdf4")
library(ncdf4) # package for netcdf manipulation
#install.packages("raster")
library(raster) # package for raster manipulation
#install.packages("rgdal")
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


# reading the data
nc_SST_orig <- nc_open('C:\\Users\\richa\\Downloads\\SSTdata_011948_022018.nc')
nc_prec_orig <- nc_open('C:\\Users\\richa\\Downloads\\Pdata_011948_022018-1.nc')

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
prec
prec[is.na(prec)] <- 0
prec
#quantile(prec)
#prec<-ifelse(prec>2.8619213 & prec<=31.719044,5,prec)
#prec<-ifelse(prec>1.4166624 & prec<=2.8619213,4,prec)
#prec<-ifelse(prec>0.3640884 & prec<=1.4166624,3,prec)
#prec<-ifelse(prec>0 & prec<=0.3640884,2,prec)
#prec<-ifelse(prec==0,1,prec)
#prec
#prec <- as.factor(prec)
#prec
#plot(prec)

# dimension reduction
prec[is.na(prec)] <- 0
prec_mean= rowMeans(prec)
library(phonTools)
mean_image = zeros(120,50)
p=841
for (i in 1:50)
   for (j in 1:50){
      mean_image[i,j]=prec_mean[p]
      p=p-1
   }
mean_image
image(mean_image)

library(matrixStats)
prec_sd= rowSds(prec)
Sd_image = zeros(120,50)
p=784
for (i in 1:28)
   for (j in 1:28){
      Sd_image[i,j]=shoes_sd[p]
      p=p-1
   }
image(Sd_image)




library(ggplot2)
library(phonTools)
scale.svd<-svd(scale(prec),nu=0)
var<-nrow(prec)-1   
vectors<-scale.svd$v  
sdev<-scale.svd$d/sqrt(var)  
values<-sdev*sdev
scores<-scale(prec)%*%vectors 
total.var<-sum(diag(cov(scale(prec))))
prop.var<-rep(NA,ncol(prec));cum.var<-rep(NA,ncol(prec)) 
for(i in 1:5){prop.var[i]<-var(scores[,i])/total.var} 
for(i in 1:5){cum.var[i]<-sum(prop.var[1:i])} 
prop.var[1]
prop.var[2]
prop.var[3]
prop.var[4]
prop.var[5]
cum.var[5]
prec_norm = matrix(0, ncol=ncol(prec), nrow=nrow(prec))
for(i in 1:nrow(prec)){
   prec_norm[i,] = prec[i,] - img_mean
}
pcob <- prcomp(prec_norm)
sX = svd(prec_norm)
image(rotate(matrix(sX$v[,1], nrow=sqrt(ncol(prec)))))
image(rotate(matrix(sX$v[,2], nrow=sqrt(ncol(prec)))))

shoes_random = matrix(0, ncol=ncol(shoes), nrow=nrow(shoes))
for (i in 1:ncol(shoes))
{
   shoes_random[,i] = sample(shoes[,i],nrow(shoes))
}

shoes_random_norm = matrix(0, ncol=ncol(shoes), nrow=nrow(shoes))
for(i in 1:nrow(shoes_random)){
   shoes_random_norm[i,] = shoes_random[i,] - img_mean
}
sX_random = svd(shoes_random_norm)

plot(sX$d^2/sum(sX$d^2), xlim = c(0, 10), type = "b", 
     pch = 15, xlab = "the number of PCs", ylab = "variance value", col="red")
points(sX_random$d^2/sum(sX_random$d^2), col="blue")
lines(sX_random$d^2/sum(sX_random$d^2), col="blue")
```
# Clustering

km.out <- kmeans('variable', 2, nstart = 20)
km.out$cluster
par(mfrow = c(1, 2))
plot('variable', col = (km.out$cluster + 1),
       main = "K-Means Clustering Results with K = 2",
       xlab = "", ylab = "", pch = 20, cex = 2)

# prediction
