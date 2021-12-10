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









prec_s1 <- which(is.na(prec[,,1]))
prec_s2 <- which(!is.na(prec[,,1]))
prec_sst <- matrix(0, nrow = dim(prec)[3], ncol = length(prec_s2))
for(i in 1:dim(prec)[3])
   prec_sst[i,] <- prec[,,i][-prec_s1]
prec_eof <- svd(prec_sst)$v
loc <- as.matrix(expand.grid(x = X_prec, Y_prec = Y_prec))[prec_s2,]
coltab <- colorRampPalette(brewer.pal(9,"BrBG"))(2048)
# plot the first EOF
par(mfrow=c(1,1))
quilt.plot(loc, prec_eof[,1], nx = length(X_prec), 
           ny = length(Y_prec), xlab = "longitude",
           ylab = "latitude", 
           main = "1st EOF", col = coltab,
           cex.lab = 3, cex.axis = 3, cex.main = 3,
           legend.cex = 20)
maps::map(database = "world", fill = TRUE, col = "gray", 
          ylim=c(-35, 35), xlim = c(123.9,290.1), add = T)

# plot the second EOF
par(mar = c(5,5,3,3), oma=c(1,1,1,1))
quilt.plot(loc, prec_eof[,2], nx = length(X_prec), 
           ny = length(Y_prec), xlab = "longitude",
           ylab = "latitude", 
           main = "2nd EOF", col = coltab,
           cex.lab = 3, cex.axis = 3, cex.main = 3,
           legend.cex = 20)
maps::map(database = "world", fill = TRUE, col = "gray", 
          ylim=c(-35, 35), xlim = c(123.9,290.1), add = T)

lon_prec <- ncvar_get(nc_prec_orig,"X")
lat_prec <- ncvar_get(nc_prec_orig,"Y")
time_prec <- ncvar_get(nc_prec_orig,"T")
time_prec = time_prec * 30.42
time_prec <- as.Date(time_prec, origin="1960-1-1 00:00", tz="UTC")
prec_mean <- apply(prec,3,mean,na.rm=TRUE)
tempseries <- data.frame(year=time_prec,prec=prec_mean)
tempseries %>% ggplot(aes(x=year,y=prec))+geom_line()+labs(title = "Monthly mean Precipitation from January 1948 to Feburary 2018", x="year",y="Precipitation" )
mov_avg <- tempseries %>% select(year, prec) %>% mutate(prec_1yr = rollmean(prec, k = 13, fill = NA, align = "right"), prec_5yr = rollmean(prec, k = 61, fill = NA, align = "right"))
mov_avg %>% gather(key="metrice",value = "value",prec:prec_5yr)%>% ggplot(aes(x=year,y=value,col=metrice))+
   geom_line()+scale_color_manual(values = c("bisque4","darkred","blue"),labels=c("Monthly mean","Annual mean","5 years moving Average"))+
   scale_x_date(limits =ymd(c("1948-01-01","2018-01-01")) ,breaks = seq(ymd("1948-01-01"),ymd("2018-01-01"),"10 years"),date_labels ="%Y")+ 
   scale_y_continuous(breaks = seq(23,33,0.5))+labs( x="year",y="SST [°C]" )+
   theme_clean(base_size = 12,)+
   theme(legend.title = element_blank(),legend.position = c("top"),legend.direction = "horizontal")
