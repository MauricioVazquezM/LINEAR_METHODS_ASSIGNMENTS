# CODE TESTING SCRIPT

## Librerias
library(ggplot2)
library(kableExtra)

### Ejercicio 01
data <- read.table(file = "datos_ej_1.txt", header = FALSE, sep = "|", strip.white = TRUE, skip = 1)
names(data) <- c("Tamaños", "Num_horas")
head(data)

x.barra <- mean(data$Tamaños)
y.barra <- mean(data$Num_horas)
data$dif.x <- data$Tamaños - x.barra
data$dif.y <- data$Num_horas - y.barra
data$dif.x_x_dif.y <- data$dif.x*data$dif.y
b.1.est <- sum(data$dif.x_x_dif.y)/sum((data$dif.x)^2)
b.0.est <- y.barra - b.1.est * x.barra

y_hat <- b.0.est + b.1.est*65

y.gorro <- b.0.est + b.1.est*data$Tamaños
se.b1.est <- 
  sqrt(sum(((data$Num_horas - y.gorro)^2)/(nrow(data)-2))) / 
  sqrt((sum((data$Tamaños - mean(data$Tamaños))^2)))
v.c <- qt(p = 0.975, df = nrow(data)-2)
i.c.1 <- b.1.est - v.c*se.b1.est
i.c.2 <- b.1.est + v.c*se.b1.est
t.est <- b.1.est/se.b1.est
v.c.2 <- qf(p = 0.95, df1 = 1, df2 = nrow(data)-2)

t_statistic <- b.1.est / se.b1.est
p_value <- 2 * pt(-abs(t_statistic), df = nrow(data) - 2)
cat("Estadística t:", t_statistic, "\n")
cat("Valor p:", p_value, "\n")
if(p_value < 0.05) {
  cat("Rechazamos la hipótesis nula. Hay suficiente evidencia para concluir que \\( \\beta_1 \\) es diferente de 0, indicando una relación lineal significativa.\n")
} else {
  cat("No se rechaza la hipótesis nula. No hay suficiente evidencia para concluir que \\( \\beta_1 \\) es diferente de 0.\n")
}

t_statistic <- b.1.est / se.b1.est

p_value <- pt(t_statistic, df = nrow(data) - 2, lower.tail = FALSE) # para obtener el valor p de una cola hacia la derecha.

if(p_value < 0.05) {
  cat("Rechazamos la hipótesis nula. Hay suficiente evidencia para concluir que beta_1 es mayor que 0, indicando una tasa de cambio positiva en el número de horas por lote trabajado.\n")
} else {
  cat("No se rechaza la hipótesis nula. No hay suficiente evidencia para concluir que beta_1 es mayor que 0.\n")
}

MSE <- sum((data$Num_horas - y.gorro)^2)/(nrow(data)-2)
se.b0.est <- 
  sqrt(
    MSE * 
      (
        1 / nrow(data) + 
          x.barra^2 / ((sum((data$Tamaños - mean(data$Tamaños))^2)))
      )
  )
v.c <- qt(p = 0.95, df = nrow(data)-2)
i.c.1 <- b.0.est - v.c*se.b0.est
i.c.2 <- b.0.est + v.c*se.b0.est
t.est <- b.0.est / se.b0.est


X_h1 <- 65
X_h2 <- 100
Y_h1 <- b.0.est + b.1.est * X_h1
Y_h2 <- b.0.est + b.1.est * X_h2
n <- nrow(data)
SSE <- sum((data$Num_horas - (b.0.est + b.1.est * data$Tamaños))^2)
MSE <- SSE / (n - 2)
S_xx <- sum((data$Tamaños - x.barra)^2)
SE_Y_h1 <- sqrt(MSE * (1/n + (X_h1 - x.barra)^2 / S_xx))
SE_Y_h2 <- sqrt(MSE * (1/n + (X_h2 - x.barra)^2 / S_xx))
alpha <- 0.10
t_critical <- qt(1 - alpha/2, df = n - 2)
IC_Y_h1_lower <- Y_h1 - t_critical * SE_Y_h1
IC_Y_h1_upper <- Y_h1 + t_critical * SE_Y_h1
IC_Y_h2_lower <- Y_h2 - t_critical * SE_Y_h2
IC_Y_h2_upper <- Y_h2 + t_critical * SE_Y_h2
cat("Intervalo de confianza al 90% para X_h = 65: [", IC_Y_h1_lower, ", ", IC_Y_h1_upper, "]\n")
cat("Intervalo de confianza al 90% para X_h = 100: [", IC_Y_h2_lower, ", ", IC_Y_h2_upper, "]\n")

X_h1 <- 65
X_h2 <- 100
Y_h1 <- b.0.est + b.1.est * X_h1
Y_h2 <- b.0.est + b.1.est * X_h2
n <- nrow(data)
SSE <- sum((data$Num_horas - (b.0.est + b.1.est * data$Tamaños))^2)
MSE <- SSE / (n - 2)
S_xx <- sum((data$Tamaños - x.barra)^2)
SE_Y_h1_pred <- sqrt(MSE * (1 + 1/n + (X_h1 - x.barra)^2 / S_xx))
SE_Y_h2_pred <- sqrt(MSE * (1 + 1/n + (X_h2 - x.barra)^2 / S_xx))
alpha <- 0.10
t_critical <- qt(1 - alpha/2, df = n - 2)
IP_Y_h1_lower <- Y_h1 - t_critical * SE_Y_h1_pred
IP_Y_h1_upper <- Y_h1 + t_critical * SE_Y_h1_pred
IP_Y_h2_lower <- Y_h2 - t_critical * SE_Y_h2_pred
IP_Y_h2_upper <- Y_h2 + t_critical * SE_Y_h2_pred
cat("Intervalo de predicción al 90% para un nuevo lote de 65 piezas: [", IP_Y_h1_lower, ", ", IP_Y_h1_upper, "]\n")
cat("Intervalo de predicción al 90% para un nuevo lote de 100 piezas: [", IP_Y_h2_lower, ", ", IP_Y_h2_upper, "]\n")

modelo <- lm(Num_horas ~ Tamaños, data = data)
summary_modelo <- summary(modelo)

F_statistic <- summary_modelo$fstatistic[1]
p_value <- summary_modelo$coefficients["Tamaños", "Pr(>|t|)"]

cat("Estadística F:", F_statistic, "\n")
cat("Valor p:", p_value, "\n")
if(p_value < 0.05) {
  cat("Rechazamos la hipótesis nula. Hay suficiente evidencia para concluir que \\( \\beta_1 \\) es diferente de 0.\n")
} else {
  cat("No se rechaza la hipótesis nula. No hay suficiente evidencia para concluir que \\( \\beta_1 \\) es diferente de 0.\n")
}

R2 <- summary_modelo$r.squared

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

coeficiente_estimado <- coef(modelo)["Equipos"]
std_error <- summary(modelo)$coefficients["Equipos", "Std. Error"]
t_value <- (coeficiente_estimado - 14) / std_error

tiempo_estimado_6 <- coef(modelo)["(Intercept)"] + coef(modelo)["Equipos"] * 6
n <- nrow(data_02)
x_bar <- mean(data_02$Equipos)
sigma_gorro <- sqrt(sum(residuals(modelo)^2) / (n - 2))
SE <- sigma_gorro * sqrt(1/n + (6 - x_bar)^2 / sum((data_02$Equipos - x_bar)^2))
t_test <- qt(0.95, df = n - 2)
margen_error <- t_test * SE
IC_inferior <- tiempo_estimado_6 - margen_error
IC_superior <- tiempo_estimado_6 + margen_error

tiempo_estimado_6 <- coef(modelo)["(Intercept)"] + coef(modelo)["Equipos"] * 6
SE_prediccion <- sigma_gorro * sqrt(1 + 1/n + (6 - x_bar)^2 / sum((data_02$Equipos - x_bar)^2))
t_test <- qt(0.95, df = n - 2)
margen_error_prediccion <- t_test * SE_prediccion
IC_inferior_prediccion <- tiempo_estimado_6 - margen_error_prediccion
IC_superior_prediccion <- tiempo_estimado_6 + margen_error_prediccion

y_gorro <- predict(modelo, newdata = data.frame(Equipos = 6), interval = "confidence", level = 0.90)[, "fit"]
n <- nrow(data_02)
x_bar <- mean(data_02$Equipos)
S_xx <- sum((data_02$Equipos - x_bar)^2)
sigma_hat <- sqrt(sum(residuals(modelo)^2) / (n - 2))
SE_y_gorro <- sigma_hat * sqrt(1 + 1/n + (6 - x_bar)^2 / S_xx)
t_critico <- qt(0.95, df = n - 2)
margen_error_banda <- t_critico * SE_y_gorro
banda_inf <- y_gorro - margen_error_banda
banda_sup <- y_gorro + margen_error_banda

tabla_anova <- anova(modelo)
print(tabla_anova)

resumen_modelo <- summary(modelo)
valor_p_f <- resumen_modelo$fstatistic[3]
print(valor_p_f)
if (valor_p_f < 0.05) {
  print("Rechazamos la hipótesis nula: hay una relación lineal significativa.")
} else {
  print("No rechazamos la hipótesis nula: no hay suficiente evidencia de una relación lineal significativa.")
}

r <- sqrt(summary(modelo)$r.squared)
signo_r <- ifelse(coef(modelo)["Equipos"] >= 0, "positivo", "negativo")

data_02$residuales <- residuals(modelo)
data_02$valores_predichos <- fitted(modelo)
ggplot(data_02, aes(x = valores_predichos, y = residuales)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuales vs. Valores Predichos",
       x = "Valores Predichos (Servicio^)",
       y = "Residuales")
ggplot(data_02, aes(x = Equipos, y = residuales)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residuales vs. Equipos",
       x = "Variable Independiente (Equipos)",
       y = "Residuales")
