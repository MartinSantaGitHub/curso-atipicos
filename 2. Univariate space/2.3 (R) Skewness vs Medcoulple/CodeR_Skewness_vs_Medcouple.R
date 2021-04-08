library(e1071)
library(robustbase)

skewness(c(1,2,3,4,5)) #0
skewness(c(1,2,3,4,50)) #1.07
mc(c(1,2,3,4,5)) #0
mc(c(1,2,3,4,50)) #0
