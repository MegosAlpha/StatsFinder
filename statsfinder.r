#Define functions
getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}
getlowerq <- function(ds) {
    quantile(ds)[2]
}
getupperq <- function(ds) {
    quantile(ds)[4]
}
getiqr <- function(ds) {
    lowerq <- quantile(ds)[2]
    upperq <- quantile(ds)[4]
    upperq - lowerq
}
#returns Mild Upper, Mild Lower, Extreme Upper, Extreme Lower.
getoutliers <- function(ds) {
    iqr <- IQR(ds)
    upperq <- getupperq(ds)
    lowerq <- getlowerq(ds)
    c((iqr * 1.5) + upperq, lowerq - (iqr * 1.5), (iqr * 3) + upperq, lowerq - (iqr * 3))
}

#Create Data Set
dataSet <- c(1,2,3,4,5,6,7,8,9,10)
outliers <- getoutliers(dataSet)

#Output Data :)
print(paste("Mean:", c(mean(dataSet))))
print(paste("Median:", c(median(dataSet))))
print(paste("Mode:", c(getmode(dataSet))))
print(paste("Range:", c(range(dataSet)[2]-range(dataSet)[1])))
print(paste("Mild Outlier (Upper Threshold):", c(outliers[1])))
print(paste("Mild Outlier (Lower Threshold):", c(outliers[2])))
print(paste("Extreme Outlier (Upper Threshold):", c(outliers[3])))
print(paste("Extreme Outlier (Lower Threshold):", c(outliers[4])))
