  setwd("/Users/MOIN SHAIKH/Desktop/Data Analytics/Project 2/")
  library(xlsx)
  energy=read.xlsx("energy.xlsx",TRUE)
  
  sample_rows<-c(1:768)
  random_sample=list()
  
  energy$NA.<-NULL
  
  for (i in c(1:50)) 
  {
    r<-sample(sample_rows,30,FALSE)
    print(length(sample_rows))
    random_sample[[i]]<-data.frame(energy[r,])
  } 
  
  
  
  library(e1071)
  
  random_sample[[1]]

  
  
  shapiro.test(energy$X1)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X2)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X3)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X4)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X5)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X6)   #value way less than 0.05 , So do not follow normal distributionv
  shapiro.test(energy$X7)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$X8)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$Y1)   #value way less than 0.05 , So do not follow normal distribution
  shapiro.test(energy$Y2)   #value way less than 0.05 , So do not follow normal distribution
  
  
  hist(energy$X1)
  hist(energy$X2)
  hist(energy$X3)
  hist(energy$X4)  
  hist(energy$X5)
  hist(energy$X6)
  hist(energy$X7)
  hist(energy$X8)
  hist(energy$Y1)
  hist(energy$Y2)
  
  qqnorm(energy$X1);qqline(energy$X1)   # The plot should be along the straight line for normal distribution which is not
  
  #Let us check for a random sample of size 30
  qqnorm(random_sample[[2]]$X1);qqline(random_sample[[2]]$X1)
  
  # Which also does not foolow normal distribution
  
  # So I can finally conclude that none of the columns follow normal distribution
  
  # To calculate the population mean we take a random sample and calculate its mean ,as
  # is fairly random we can take the sample mean as population mean
  
  
  mean_list<-list()
  
  mean_list[["X1"]]=0
  mean_list[["X2"]]=0
  mean_list[["X3"]]=0
  mean_list[["X4"]]=0
  mean_list[["X5"]]=0
  mean_list[["X6"]]=0
  mean_list[["X7"]]=0
  mean_list[["X8"]]=0
  mean_list[["Y1"]]=0
  mean_list[["Y2"]]=0
  
  for (i in 1:50)
  {
   mean_list[["X1"]]=mean_list[["X1"]]+mean(random_sample[[i]]$X1)
   mean_list[["X2"]]=mean_list[["X2"]]+mean(random_sample[[i]]$X2)
   mean_list[["X3"]]=mean_list[["X3"]]+mean(random_sample[[i]]$X3)
   mean_list[["X4"]]=mean_list[["X4"]]+mean(random_sample[[i]]$X4)
   mean_list[["X5"]]=mean_list[["X5"]]+mean(random_sample[[i]]$X5)
   mean_list[["X6"]]=mean_list[["X6"]]+mean(random_sample[[i]]$X6)
   mean_list[["X7"]]=mean_list[["X7"]]+mean(random_sample[[i]]$X7)
   mean_list[["X8"]]=mean_list[["X8"]]+mean(random_sample[[i]]$X8)
   mean_list[["Y1"]]=mean_list[["Y1"]]+mean(random_sample[[i]]$Y1)
   mean_list[["Y2"]]=mean_list[["Y2"]]+mean(random_sample[[i]]$Y2)
  }
  
  
  
  mean_list[["X1"]]=mean_list[["X1"]]/50
  mean_list[["X2"]]=mean_list[["X2"]]/50
  mean_list[["X3"]]=mean_list[["X3"]]/50
  mean_list[["X4"]]=mean_list[["X4"]]/50
  mean_list[["X5"]]=mean_list[["X5"]]/50
  mean_list[["X6"]]=mean_list[["X6"]]/50
  mean_list[["X7"]]=mean_list[["X7"]]/50
  mean_list[["X8"]]=mean_list[["X8"]]/50
  mean_list[["Y1"]]=mean_list[["Y1"]]/50
  mean_list[["Y2"]]=mean_list[["Y2"]]/50

  final_mean_values <- data.frame(mean_list)
  
  final_mean_values
  # The mean of the sample means will be the mean of the population
  # As the data set is large and the sample is fairly randomly chosen this will be the
  # values for the population mean (stored column name wise in data frame final_mean_values)