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


