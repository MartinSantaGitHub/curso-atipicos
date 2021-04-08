library(MASS)
library(depth)
library(tidyverse)

set.seed(3)

mu1 <- c(0, 0)
mu2 <- c(10, 0)
sigma <- matrix(c(1, 0, 0, 1), nc = 2)
X <-
  rbind(mvrnorm(80, mu1, sigma), mvrnorm(20, mu2, sigma)) #dimension 100x2
plot(X)

# Mean vector
m = c(mean(X[, 1]), mean(X[, 2]))
m

# Component-wise median
med1 = c(median(X[, 1]), median(X[, 2]))
med1

# Spatial median
med2 = med(X, method = "Spatial")
med2

as_tibble(X) %>%
  ggplot(aes(V1, V2)) +
  geom_point() +
  geom_point(aes(m[1], m[2], color = "red"), size = 3, shape = 8) +
  geom_point(aes(med1[1], med1[2], color = "green"), size = 3, shape = 8) +
  geom_point(aes(med2$median[1], med2$median[2], color = "blue"), size = 3, shape = 8) +
  #scale_color_manual(name = "Location Estimator", values=c("blue","green","red"), labels = c("Spatial median", "Median", "Mean")) + 
  scale_color_identity(name = "Location Estimator",
                       breaks = c("red", "green", "blue"),
                       labels = c("Mean", "Median", "Spatial median"),
                       guide = "legend")
