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

residuales <- residuals(modelo)
SSE <- sum(residuales^2)

residuales_str <- toString(residuales)
SSE_str <- toString(SSE)

n <- length(residuales)

sigmac_gorro <- sum(residuales^2) / (n - 2)
sigma_gorro <- sqrt(sigmac_gorro)

residuales <- residuals(modelo)
SSE <- sum(residuales^2)

x.datos <- data_02$Equipos
x.barra <- mean(data_02$Equipos)
abajo <- sum((x.datos - x.barra)^2)

beta.1.gorro <- sum((data_02$Equipos - x.barra)*data_02$Servicio) / abajo
alpha <- 1 - 0.90
p <- 1-(alpha/2)

vc <- qt(p = p, df = n - 2)
y.gorro.dt <- predict(object = modelo)

mse <- SSE/n

sigma.2.gorro.b1 <- mse / abajo

ls <- beta.1.gorro + vc*sqrt(mse/abajo)
li <- beta.1.gorro - vc*sqrt(mse/abajo)

summary_modelo <- summary(modelo)
valor_p <- summary_modelo$coefficients["Equipos", "Pr(>|t|)"]

