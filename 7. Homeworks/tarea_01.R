library(L1pack)
library(MASS)
library(robustbase)
library(tidyverse)

# 1.

View(kootenay)

class(kootenay)
ktny_tbl <- as_tibble(kootenay)
class(ktny_tbl)

ktny_tbl %>% 
  ggplot(aes(Libby,Newgate)) + 
  geom_point(data = ktny_tbl[-4,]) + 
  geom_point(data = ktny_tbl[4,], colour = "#ff3300", size = 3)

# Se observa una tendencia lineal pero para el valor de Libby cercano a 80 podemos observar un 
# outlier que afecta la recta de regresión. Lo pinté de color rojo y lo hice un poco más grande 
# para marcarlo del resto de los datos.

# 2.

ls.reg <- lm(Newgate ~ Libby, data = ktny_tbl)
summary(ls.reg)

# Vemos un R cuadrado ajustado espantoso cercano a 0 (-0.08879). Esto es debido al outlier. 
# Miremos la recta en el gráfico

ktny_tbl %>% 
  ggplot(aes(Libby,Newgate)) + 
  geom_point(data = ktny_tbl[-4,]) + 
  geom_point(data = ktny_tbl[4,], colour = "#ff3300", size = 3) + 
  geom_abline(aes(intercept = ls.reg$coefficients[1], 
                  slope = ls.reg$coefficients[2]), color = "red")

# 3.

# LAD
lad.reg <- l1fit(ktny_tbl$Libby, ktny_tbl$Newgate)

# LMS
lms.reg <- lqs(Newgate ~ Libby, data = ktny_tbl, method="lms")

# LTS
lts.reg <- ltsReg(Newgate ~ Libby, data = ktny_tbl)

ktny_tbl %>% 
  ggplot(aes(Libby,Newgate)) + 
  geom_point(data = ktny_tbl[-4,]) + 
  geom_point(data = ktny_tbl[4,], colour = "#ff3300", size = 3) +
  geom_abline(aes(intercept = ls.reg$coefficients[1], slope = ls.reg$coefficients[2], color = "black"), linetype = "dashed") + 
  geom_abline(aes(intercept = lad.reg$coefficients[1], slope = lad.reg$coefficients[2], color = "green"), linetype = "dashed") + 
  geom_abline(aes(intercept = lms.reg$coefficients[1], slope = lms.reg$coefficients[2], color = "blue"), linetype = "dashed") +
  geom_abline(aes(intercept = lts.reg$coefficients[1], slope = lts.reg$coefficients[2], color = "red")) +
  scale_color_manual(name = "Linear Model",
                     values = c("black", "blue", "green", "red"),
                     labels = c("LS", "LMS", "LAD", "LTS"),
                     guide = guide_legend(override.aes = list(linetype = c(2,2,2,1))))

# En el plot se puede observar que la recta que mejor se ajusta es la de LTS pero la recta de LMS
# no se queda atrás, también nos brinda un ajuste muy bueno.

summary(lts.reg)
