sst = read.csv('F:\\courses\\STAT\\8330 data analysis 3\\project3\\github\\STAT8330-Final-Project\\DataGeneration\\sst_1.csv',sep = ',',header = TRUE)  
for (i in 2:24)
{
  filename = sprintf('F:\\courses\\STAT\\8330 data analysis 3\\project3\\github\\STAT8330-Final-Project\\DataGeneration\\sst_%d.csv', i)
  sst_i = read.csv(filename,sep = ',',header = TRUE)  
  sst = rbind(sst, sst_i)
}
sst

prec = read.csv('F:\\courses\\STAT\\8330 data analysis 3\\project3\\github\\STAT8330-Final-Project\\DataGeneration\\prec_1.csv',sep = ',',header = TRUE)  
for (i in 2:51)
{
  filename = sprintf('F:\\courses\\STAT\\8330 data analysis 3\\project3\\github\\STAT8330-Final-Project\\DataGeneration\\prec_%d.csv', i)
  prec_i = read.csv(filename,sep = ',',header = TRUE)  
  prec = rbind(prec, prec_i)
}
prec