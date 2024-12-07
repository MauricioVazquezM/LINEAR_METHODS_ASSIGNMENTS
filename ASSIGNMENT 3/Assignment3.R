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
