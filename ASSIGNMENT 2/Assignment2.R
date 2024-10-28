## Tarea 2: Regresion multiple


img <- readJPEG("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/ASSIGNMENT 2/img1.jpg")

ggplot() +
  annotation_custom(
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme_minimal()

img <- readJPEG("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/ASSIGNMENT 2/img2.jpg")

ggplot() +
  annotation_custom(
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")),
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
  ) +
  theme_minimal()

# $$
#   \begin{align*}
# \text{Casualties} = &\ \beta_0 - 0.22\cdot \text{ln(Pop)} + 36.462 \cdot \text{ln(QY1)} -58.876 \cdot \text{ln(QY2)} \\
# & + 26.470 \cdot \text{ln(QY3)} - 15.870 \cdot \text{ln(QY4)} + 12.863 \cdot \text{ln(QY5)} + 1.416 \cdot \text{mUnemp} \\
# & - 2.955 \cdot \text{highPR} + 2.582 \cdot \text{lowPR} + 0.0233 \cdot \text{areakmsq10K} + 7.094 \cdot \text{tropicAr} \\
# & - 20.749 \cdot \text{elev100m} - 20.749 \cdot \text{langfrac} + 24.907 \cdot \text{langfracsq} + \epsilon
# \end{align*}
# $$
  
# Por valores proporcionados por la tabla
coef_ln_Pop <- -0.022       
se_ln_Pop <- 0.176        
z_90 <- 1.645              

# Cálculo del margen de error
margin_error <- z_90 * se_ln_Pop

# Cálculo del intervalo de confianza
ci_lower <- coef_ln_Pop - margin_error
ci_upper <- coef_ln_Pop + margin_error

# Resultado
ci_ln_Pop <- c(ci_lower, ci_upper)

# Leyendo archivo
file_path <- "C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/ASSIGNMENT 2/datos_ejercicio_2_tarea_3.txt"

# Leyendo el archivo con el delimitador '|'
datos <- read.delim(file_path, sep = "|")
head(datos)

#$$
#  \text{BMI18} = \beta_0 + \beta_1 \cdot \text{Sexo} + \beta_2 \cdot \text{WT2} + \beta_3 \cdot \text{HT2} + \beta_4 \cdot \text{WT9} + \beta_5 \cdot \text{HT9} + \beta_6 \cdot \text{LG9} + \beta_7 \cdot \text{ST9} + \beta_8 \cdot \text{WT18} + \beta_9 \cdot \text{HT18} + \beta_{10} \cdot \text{LG18} + \beta_{11} \cdot \text{ST18} + \beta_{12} \cdot \text{Soma} + \epsilon
#$$

# Ajustar el modelo de regresión lineal
modelo <- lm(BMI18 ~ Sex + WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + WT18 + HT18 + LG18 + ST18 + Soma, data = datos)

# Mostrar los coeficientes estimados
summary(modelo)$coefficients

# Extraer el estimador y el error estándar para WT2 de coeficientes
coeficientes <- summary(modelo)$coefficients
coef_WT2 <- coeficientes["WT2", "Estimate"]
se_WT2 <- coeficientes["WT2", "Std. Error"]

# Calcular el intervalo de confianza al 99%
z_99 <- 2.576  
margin_error <- z_99 * se_WT2
ci_lower <- coef_WT2 - margin_error
ci_upper <- coef_WT2 + margin_error

# Obtener el resumen del modelo
summary_model <- summary(modelo)

# Extraer R2 y R2 ajustado
r_squared <- summary_model$r.squared
adj_r_squared <- summary_model$adj.r.squared

# Ver la significancia de los coeficientes
coef_significance <- summary_model$coefficients

# Valores para la prediccion
nuevo_sujeto <- data.frame(
  Sex = 0,    
  WT2 = 15,
  HT2 = 90,
  WT9 = 35,
  HT9 = 142,
  LG9 = 32,
  ST9 = 71,
  WT18 = 0,   
  HT18 = 0,   
  LG18 = 0,  
  ST18 = 0,   
  Soma = 0    
)

# Predicción puntual y intervalo de confianza al 99%
prediccion <- predict(modelo, newdata = nuevo_sujeto, interval = "confidence", level = 0.99)

# Sacar los valores
fit <- prediccion[1, "fit"]
lwr <- prediccion[1, "lwr"]
upr <- prediccion[1, "upr"]

# Valores de la nueva prediccion
nueva_observacion <- data.frame(
  WT2 = 15,
  HT2 = 90,
  WT9 = 35,
  HT9 = 142,
  LG9 = 32,
  ST9 = 71,
  WT18 = 0,   
  HT18 = 0,   
  LG18 = 0,   
  ST18 = 0,   
  Soma = 0    
)

# Realizando la predicción puntual y obtener el intervalo de predicción al 99%
prediccion_nueva <- predict(modelo, newdata = nueva_observacion, interval = "prediction", level = 0.99)

# Sacar los valores
fit2 <- prediccion_nueva[1, "fit"]
lwr2 <- prediccion_nueva[1, "lwr"]
upr2 <- prediccion_nueva[1, "upr"]

# Ajustar el modelo de regresión lineal
modelo2 <- lm(BMI18 ~ Sex + WT2 + HT2 + WT9 + HT9 + LG9 + ST9 + WT18 + HT18 + LG18 + ST18 + Soma, data = datos)

# Mostrar los coeficientes estimados
summary(modelo2)$coefficients

# Obtener el resumen del modelo
summary_model2 <- summary(modelo2)

# Extraer R2 y R2 ajustado
r_squared2 <- summary_model2$r.squared
adj_r_squared2 <- summary_model2$adj.r.squared

# Ver la significancia de los coeficientes
coef_significance2 <- summary_model2$coefficients
