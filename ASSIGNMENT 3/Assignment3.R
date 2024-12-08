##### Problema 1 #####

# Lectura de datos
datos_1 <- read.table(file = 'datos_prob_1.txt', 
                      sep = '\t', 
                      header =FALSE,
                      col.names = c('Y','X_1','X_2','X_3'))

# Plos de caja y brazos
c1 <- ggplot(data = datos_1) +
  aes(x = X_1) +
  geom_boxplot(mapping = aes(y = 'X_1')) +
  labs(title = "Paquetes distribuidos", x="Valor", y="") +
  theme_minimal()

c2 <- ggplot(data = datos_1) +
  aes(x = X_2) +
  geom_boxplot(mapping = aes(y = 'X_2')) +
  labs(title = "Costos indirectoss", x="Valor", y="") +
  theme_minimal()

# Histogramas
h1 <-  ggplot(data = datos_1) +
  aes(x = X_1) +
  geom_histogram() +
  labs(title = "Paquetes distribuidos", x="Valor", y="Frecuencia") +
  theme_minimal()

h2 <- ggplot(data = datos_1) +
  aes(x = X_2) +
  geom_histogram() +
  labs(title = "Costos indirectoss", x="Valor", y="Frecuencia") +
  theme_minimal()

# Conteo por catergoria
c3 <- ggplot(data = datos_1) +
  aes(x = factor(X_3)) +
  geom_bar() +
  labs(title = "Semana con dias feriados", x ="Días feriados (1) o no (0)", y="Conteo") +
  theme_minimal()

# Mostrar ambos gráficos en un solo layout
grid.arrange(c1, c2, h1, h2, c3, ncol = 2)

# Graficos de dispersion
d1 <- ggplot(data = datos_1) +
  aes(x = 1:52, y = X_1) +
  geom_point() +
  labs(title = "Paquetes distribuidos por semana", x="Semana", y="Valor") +
  theme_classic()

d2 <- ggplot(data = datos_1) +
  aes(x = 1:52, y = X_2) +
  geom_point() +
  labs(title = "Costos indirectos por semana", x="Semana", y="Valor") +
  theme_classic()

d3 <- ggplot(data = datos_1) +
  aes(x = 1:52, y = X_3) +
  geom_point() +
  labs(title = "Semana con dias feriados", x="Semana", y="Valor") +
  theme_classic()

# Mostrar gráficos en un solo layout
grid.arrange(d1, d2, d3 , ncol = 2)

# Dataset reducido
datos_1$X_3 <- as.factor(datos_1$X_3)
numerical_vars <- datos_1[, c('Y','X_1','X_2')]

# Creando el pairs plot 
ggpairs(
  numerical_vars,
  lower = list(continuous = "points"),
  diag = list(continuous = "densityDiag"), 
  upper = list(continuous = "cor"),    
  aes(color = datos_1$X_3, alpha = 0.7) 
) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"), 
    axis.title = element_text(size = 12),               
    axis.text = element_text(size = 10), 
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text.y = element_text(angle = 0), 
    legend.position = "top"                             
  ) +
  labs(title = "Analisis de correlación (por semana)", color = "Semana con dia feriado o no")

# Lectura de datos
datos_1 <- read.table(file = 'datos_prob_1.txt', 
                      sep = '\t', 
                      header =FALSE,
                      col.names = c('Y','X_1','X_2','X_3'))

# Estimar el modelo de regresión lineal
modelo_1 <- lm(Y ~ X_1 + X_2 + X_3, data = datos_1)
coeficientes <- coef(modelo_1)

# Intercepto
intercepto <- coeficientes["(Intercept)"]

# Coeficiente de X_1
coef_X1 <- coeficientes["X_1"]

# Coeficiente de X_2
coef_X2 <- coeficientes["X_2"]

# Coeficiente de X_3
coef_X3 <- coeficientes["X_3"]

# Calcular los residuales
residuales <- residuals(modelo_1)

# Df auxiliar
residuales_df <- data.frame(Residuales = residuales)

# Diagrama de caja y brazos
ggplot(residuales_df, aes(y = Residuales)) +
  geom_boxplot(color = "black") +
  labs(
    title = "Boxplot de residuales",
    y = "Residuales"
  ) +
  theme_minimal()

# Residuales y valores ajustados
residuales <- residuals(modelo_1)
valores_ajustados <- fitted(modelo_1)

# variable X_1 * X_2
datos_1$X1_X2 <- datos_1$X_1 * datos_1$X_2

# DF auxiliar
residuals_df <- data.frame(
  Residuales = residuales,
  ValoresAjustados = valores_ajustados,
  X_1 = datos_1$X_1,
  X_2 = datos_1$X_2,
  X_3 = datos_1$X_3,
  X1_X2 = datos_1$X1_X2
)

# Plot residuales vs valores ajustados
plot1 <- ggplot(residuals_df, aes(x = Residuales, y = ValoresAjustados)) +
  geom_point(alpha = 0.7) +
  labs(title = "vs. Valores Ajustados", x = "Residuales", y = "Y_hat") +
  theme_minimal()

# Plot residuales vs X_1
plot2 <- ggplot(residuals_df, aes(x = Residuales, y = X_1)) +
  geom_point(alpha = 0.7) +
  labs(title = "vs. X_1", x = "Residuales", y = "X_1") +
  theme_minimal()

# Plot residuales vs X_2
plot3 <- ggplot(residuals_df, aes(x = Residuales, y = X_2)) +
  geom_point(alpha = 0.7) +
  labs(title = "vs. X_2", x = "Residuales", y = "X_2") +
  theme_minimal()

# Plot residuales vs X_3
plot4 <- ggplot(residuals_df, aes(x = Residuales, y = X_3)) +
  geom_point(alpha = 0.7) +
  labs(title = "vs. X_3", x = "Residuales", y = "X_3") +
  theme_minimal()

# Plot residuales vs X_1 * X_2
plot5 <- ggplot(residuals_df, aes(x = Residuales, y = X1_X2)) +
  geom_point(alpha = 0.7) +
  labs(title = "vs. X_1 * X_2", x = "Residuales", y = "X_1 * X_2") +
  theme_minimal()

# QQ Plot
plot6 <- ggplot(residuals_df, aes(sample = Residuales)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot Residuales", x = "Cuantiles Teóricos", y = "Cuantiles Residuales") +
  theme_minimal()

# Grid
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3)

# Calcular los residuales
residuales <- residuals(modelo_1)

# Df auxiliar
residuales_df <- data.frame(Residuales = residuales)

# Scatterplot
ggplot(residuales_df, aes(x = 1:52, y = Residuales)) +
  geom_point(color = "black") +
  labs(
    title = "Residuales en el tiempo",
    x= 'Semana',
    y = "Residuales"
  ) +
  theme_minimal()

# Residuales
datos_1$residual <- modelo_1$residuals

# Dividiendo grupo por la mediana
grupo.1 <- datos_1[which(modelo_1$fitted.values <= median(modelo_1$fitted.values)),]
grupo.2 <- datos_1[which(modelo_1$fitted.values > median(modelo_1$fitted.values)),]

# Calculando medianas de cada grupo
mediana.epsilon.gorro.1 <- median(grupo.1$residual)
mediana.epsilon.gorro.2 <- median(grupo.2$residual)

# Calculando los valores absolutos de las desviaciones respecto a la mediana
grupo.1$d <- abs(grupo.1$residual-mediana.epsilon.gorro.1)
grupo.2$d <- abs(grupo.2$residual-mediana.epsilon.gorro.2)

# Calculando las medias de las desviaciones absolutas en cada grupo
d.media.1 <- mean(grupo.1$d)
d.media.2 <- mean(grupo.2$d)

# Suma de cuadrados
SSD <-(sum((grupo.1$d - d.media.1)^2) + sum((grupo.2$d - d.media.2)^2)) / (nrow(datos_1) - 2)

# Calculando el estadisticod de prueba
t.star.bf <- abs(d.media.1 - d.media.2)/sqrt(SSD*(1/nrow(grupo.1) + 1/nrow(grupo.2)))

# Calculando el valor p
t.star.bf.p.value <- pt(q = t.star.bf, df = nrow(datos_1)-2, lower.tail = FALSE)


##### Problema 2 #####
