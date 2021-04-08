library(L1pack)
library(MASS)
library(tidyverse)
library(robustbase)

#Star data
data(starsCYG, package="robustbase")
View(starsCYG)
plot(starsCYG)

#Least squares
lm.stars <- lm(log.light ~ log.Te, data = starsCYG)
summary(lm.stars)

#Least trimmed squares LTS
lts.stars <- ltsReg(log.light ~ log.Te, data = starsCYG)
summary(lts.stars)

#LMS
lms.stars <- lqs(starsCYG$log.light, starsCYG$log.Te,method="lms")
summary(lms.stars)

#LAD
lad.stars <- l1fit(starsCYG$log.light, starsCYG$log.Te)
summary(lad.stars)

r.lm <- rlm(log.light ~ log.Te, data = starsCYG)
summary(r.lm)

#plot
plot(starsCYG)
abline(lm.stars, lty=2, lwd=2, col="green")
abline(lad.stars, lty=2, lwd=2, col="magenta")
abline(lms.stars, lty=4, lwd=2, col="red")
abline(lts.stars, lty=4, lwd=2, col="blue")
legend("bottomleft", c("LS","LAD","LMS","LTS"),inset=0.02, lty=c(2,2,4,4), lwd=2, col=c("green","magenta", "red","blue"))

as_tibble(starsCYG) %>% 
  ggplot(aes(log.Te, log.light)) + 
  geom_point() + 
  geom_abline(aes(intercept = lms.stars$coefficients[1], slope = lms.stars$coefficients[2], color = "blue"), linetype = "dashed") + 
  geom_abline(aes(intercept = lad.stars$coefficients[1], slope = lad.stars$coefficients[2], color = "green"), linetype = "dashed") + 
  geom_abline(aes(intercept = lm.stars$coefficients[1], slope = lm.stars$coefficients[2], color = "black"), linetype = "dashed") +
  geom_abline(aes(intercept = lts.stars$coefficients[1], slope = lts.stars$coefficients[2], color = "red")) +
  geom_abline(aes(intercept = r.lm$coefficients[1], slope = r.lm$coefficients[2], color = "violet"), linetype = "dashed") +
  scale_color_manual(name = "Linear Model",
                     values = c("black", "blue", "green", "red", "violet"),
                     labels = c("LS", "LMS", "LAD", "LTS","LRM"),
                     guide = guide_legend(override.aes = list(linetype = c(2,2,2,1,2))))
