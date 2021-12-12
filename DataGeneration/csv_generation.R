library(ncdf4) # package for netcdf manipulation

# reading the data
nc_SST_orig <- nc_open('C:\\jian\\data3\\project3\\SSTdata_011948_022018.nc')
sst_df = data.frame('lot'=character(0),'lat'=character(0),'time'=character(0),'sst'=character(0))
lon_sst <- ncvar_get(nc_SST_orig,"X")
lat_sst <- ncvar_get(nc_SST_orig,"Y")
time_sst <- ncvar_get(nc_SST_orig,"T")
sst_anom <- ncvar_get(nc_SST_orig,"anom")
row_count = 0
file_count = 1
for (i in 1:dim(sst_anom)[1])
{
   for (j in 1:dim(sst_anom)[2])
   {
      for (n in 1:dim(sst_anom)[3])
      {
         sst_df[nrow(sst_df)+1,] = list(lon_sst[i], lat_sst[j], time_sst[n], sst_anom[i,j,n])
         if (row_count %% 10000 == 0)
         {
            print(row_count)   
         }
         row_count = row_count + 1
         if (row_count %% 100000 == 0)
         {
            filename = sprintf('C:\\jian\\data3\\project3\\sst_%d.csv', file_count)
            write.csv(sst_df, filename, row.names=F)
            file_count = file_count + 1
            sst_df = data.frame('lot'=character(0),'lat'=character(0),'time'=character(0),'sst'=character(0))
         }
      }
   }
}
filename = sprintf('C:\\jian\\data3\\project3\\sst_%d.csv', file_count)
write.csv(sst_df, filename, row.names=F)


nc_prec_orig <- nc_open('C:\\jian\\data3\\project3\\Pdata_011948_022018-1.nc')
prec_df = data.frame('lot'=character(0),'lat'=character(0),'time'=character(0),'prec'=character(0))
lon_prec <- ncvar_get(nc_prec_orig,"X")
lat_prec <- ncvar_get(nc_prec_orig,"Y")
time_prec <- ncvar_get(nc_prec_orig,"T")
prec_anom <- ncvar_get(nc_prec_orig, "rain")
row_count = 0
file_count = 1
for (i in 1:dim(prec_anom)[1])
{
   for (j in 1:dim(prec_anom)[2])
   {
      for (n in 1:dim(prec_anom)[3])
      {
         prec_df[nrow(prec_df)+1,] = list(lon_prec[i], lat_prec[j], time_prec[n], prec_anom[i,j,n])
         if (row_count %% 10000 == 0)
         {
            print(row_count)   
         }
         row_count = row_count + 1
         if (row_count %% 100000 == 0)
         {
            filename = sprintf('C:\\jian\\data3\\project3\\prec_%d.csv', file_count)
            write.csv(prec_df, filename, row.names=F)
            file_count = file_count + 1
            prec_df = data.frame('lot'=character(0),'lat'=character(0),'time'=character(0),'prec'=character(0))
         }
      }
   }
}
filename = sprintf('C:\\jian\\data3\\project3\\prec_%d.csv', file_count)
write.csv(prec_df, filename, row.names=F)