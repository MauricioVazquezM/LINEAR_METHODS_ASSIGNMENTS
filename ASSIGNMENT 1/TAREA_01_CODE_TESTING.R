# CODE TESTING SCRIPT

## Librerias
library(ggplot2)
library(kableExtra)

### Ejercicio 01
data <- read.table(file = "datos_ej_1.txt", header = FALSE, sep = "|", strip.white = TRUE, skip = 1)
names(data) <- c("Tamaños", "Num_horas")
head(data)


### Ejercicio 02
data_02 <- read.table(file = "datos_ej_2.txt", header = FALSE, sep = "|", strip.white = TRUE)
names(data_02) <- c("Servicio", "Equipos")
head(data_02)

modelo <- lm(formula = Servicio ~ Equipos, data = data_02)
summary(modelo)

ggplot(data_02, aes(x = Equipos, y = Servicio)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relación entre Servicio y Equipos", x = "Equipos", y = "Tiempo de Servicio (minutos)") +
  theme_minimal()


