rm(list=ls())

#Libraries
library(randomForest)
library(lubridate) #For date processing


#Load data for all cameras, 2008-2014
print("Downloading full data, 2008-2014")
footfall <- read.csv("full_data.csv")
nD = nrow(footfall)

#Basic data transformations
footfall$Month = as.factor(month(footfall$Date))
footfall$Hour = as.integer(footfall$Hour)
footfall$WeekDay = as.factor(footfall$WeekDay)
footfall$LocationName = as.factor(footfall$LocationName)
footfall$Count = as.integer(footfall$Count)
footfall$LogCount = log(footfall$Count+1)

#Sort out lagged counts
footfall$PreviousLogCount[1] = NA
idx = which(footfall$LocationName[2:nD]==footfall$LocationName[1:(nD-1)])
footfall$PreviousLogCount[idx+1] = footfall$LogCount[idx]
footfall$PreviousLogCount2[1] = NA
idx = which(footfall$LocationName[3:nD]==footfall$LocationName[1:(nD-2)])
footfall$PreviousLogCount2[idx+2] = footfall$LogCount[idx]
footfall$PreviousWeekLogCount[1] = NA
idx = which(footfall$LocationName[169:nD]==footfall$LocationName[1:(nD-168)])
footfall$PreviousWeekLogCount[idx+168] = footfall$LogCount[idx]

#Get rid of cases with missing data
footfall = footfall[complete.cases(footfall),]

#Variable names that we will test for inclusion
vnames_no_ar <- c("Hour", "WeekDay", "Month", "Year", "LocationName", "WeekNum")
nc = length(vnames_no_ar)

#Random Forest parameters
ssize = 10000
num_tree = 100

#Generate all combinations for inclusion/exclusion
A = dagR::allCombs(1:nc)

#Test all posssible models with no time lagged inputs
print("Testing models with no lagged variables")
V1= c(NA)
pb1 <- txtProgressBar(min=0, max=2^nc, char="=")
for (i in 2:2^nc){
  setTxtProgressBar(pb1, i)
  IN = A[i,]
  IN <- IN[!is.na(IN)]
  f <- as.formula(paste("LogCount ~", paste(vnames_no_ar[IN], collapse = " + ")))
  rf = randomForest(f, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = FALSE, na.action=na.roughfix)
  V1[i] = tail(rf$mse, n=1)
}

idx1 = which.min(V1)
IN = A[idx1,]
IN <- IN[!is.na(IN)]
f1 <- as.formula(paste("LogCount ~", paste(vnames_no_ar[IN], collapse = " + ")))
rf_no_ar = randomForest(f1, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = TRUE, na.action=na.roughfix)

varImpPlot(rf_no_ar, scale=FALSE)

#Test all possible models including last weeks count
print("Testing models with last week lagged variable")
V2= c(NA)
pb2 <- txtProgressBar(min=0, max=2^nc, char="=")
for (i in 2:2^nc){
  setTxtProgressBar(pb2, i)
  IN = A[i,]
  IN <- IN[!is.na(IN)]
  allnames = c("PreviousWeekLogCount", vnames_no_ar[IN])
  f <- as.formula(paste("LogCount ~", paste(allnames, collapse = " + ")))
  rf = randomForest(f, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = FALSE, na.action=na.roughfix)
  V2[i] = tail(rf$mse, n=1)
}

idx2 = which.min(V2)
IN = A[idx2,]
IN <- IN[!is.na(IN)]
allnames = c("PreviousWeekLogCount", vnames_no_ar[IN])

f2 <- as.formula(paste("LogCount ~", paste(allnames, collapse = " + ")))
rf_lw_ar = randomForest(f2, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = TRUE, na.action=na.roughfix)
varImpPlot(rf_lw_ar, scale=FALSE)

#Test all possible models with last weeks count and the last two hours
print("Testing models with all lagged variables")
V3= c(NA)
pb3 <- txtProgressBar(min=0, max=2^nc, char="=")
for (i in 2:2^nc){
  setTxtProgressBar(pb3, i)
  IN = A[i,]
  IN <- IN[!is.na(IN)]
  allnames = c("PreviousWeekLogCount", "PreviousLogCount", "PreviousLogCount2", vnames_no_ar[IN])
  f <- as.formula(paste("LogCount ~", paste(allnames, collapse = " + ")))
  rf = randomForest(f, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = FALSE, na.action=na.roughfix)
  V3[i] = tail(rf$mse, n=1)
}

idx3 = which.min(V3)
IN = A[idx3,]
IN <- IN[!is.na(IN)]
allnames = c("PreviousWeekLogCount", "PreviousLogCount", "PreviousLogCount2", vnames_no_ar[IN])

f3 <- as.formula(paste("LogCount ~", paste(allnames, collapse = " + ")))
rf_full_ar = randomForest(f3, data = footfall, sampsize = ssize, ntree = num_tree, keep.inbag=TRUE, importance = TRUE, na.action=na.roughfix)
varImpPlot(rf_full_ar, scale=FALSE)

print(paste("Best model with no lag variables: MSE = ", tail(rf_no_ar$mse, n=1)))
print(f1)
print(paste("Best model with last week lag variable: MSE = ", tail(rf_lw_ar$mse, n=1)))
print(f2)
print(paste("Best model with all lag variables: MSE = ", tail(rf_full_ar$mse, n=1)))
print(f3)