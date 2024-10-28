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
