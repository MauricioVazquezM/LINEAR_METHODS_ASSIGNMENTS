## Pruebas proyecto 1

# Leer el archivo
data <- read.csv("C:/Users/mauva/OneDrive/Documents/ITAM/9no Semestre/METODOS LINEALES/REPOSITORIO/LINEAR_METHODS_ASSIGNMENTS_FALL2024/PROJECT/Student_Performance.csv")

# Ver las primeras filas del DataFrame
head(data)

# Nombres originales de las columnas
colnames(data) <- c("Hours.Studied", "Previous.Scores", "Extracurricular.Activities", 
                    "Sleep.Hours", "Sample.Question.Papers.Practiced", "Performance.Index")

# Renombrar las columnas (estandarizando los nombres de las columnas)
colnames(data) <- tolower(colnames(data))  
colnames(data) <- gsub("\\.", "_", colnames(data))  

# Acortar los nombres de las columnas
colnames(data) <- c("hrs_studied", "prev_scores", "xtr_activities", 
                    "sleep_hrs", "sample_questions", "performance_idx")

# Verificar los nuevos nombres
print(colnames(data))

# Funcion de analisis univariado 
univar_analisis <- function(data) {
  results <- list()
  
  for (feature in colnames(data)) {
    data_type <- class(data[[feature]])[1]
    
    total <- nrow(data)
    nan_count <- sum(is.na(data[[feature]]))
    no_missings <- total - nan_count
    pct_missings <- nan_count / total
    
    if (is.numeric(data[[feature]])) {
      promedio <- round(mean(data[[feature]], na.rm = TRUE),2)
      desv_estandar <- round(sd(data[[feature]], na.rm = TRUE),2)
      varianza <- round(var(data[[feature]], na.rm = TRUE),2)
      minimo <- min(data[[feature]], na.rm = TRUE)
      p10 <- quantile(data[[feature]], 0.10, na.rm = TRUE)
      q1 <- quantile(data[[feature]], 0.25, na.rm = TRUE)
      mediana <- quantile(data[[feature]], 0.50, na.rm = TRUE)
      q3 <- quantile(data[[feature]], 0.75, na.rm = TRUE)
      p90 <- quantile(data[[feature]], 0.90, na.rm = TRUE)
      p95 <- quantile(data[[feature]], 0.95, na.rm = TRUE)
      p99 <- quantile(data[[feature]], 0.99, na.rm = TRUE)
      maximo <- max(data[[feature]], na.rm = TRUE)
      
      inf_count <- sum(is.infinite(data[[feature]]) & data[[feature]] > 0)
      neg_inf_count <- sum(is.infinite(data[[feature]]) & data[[feature]] < 0)
    } else {
      promedio <- NA
      desv_estandar <- NA
      varianza <- NA
      minimo <- NA
      p1 <- NA
      p5 <- NA
      p10 <- NA
      q1 <- NA
      mediana <- NA
      q3 <- NA
      p90 <- NA
      p95 <- NA
      p99 <- NA
      maximo <- NA
      inf_count <- 0
      neg_inf_count <- 0
    }
    
    results[[length(results) + 1]] <- list(
      
      Variable = feature,
      Total = total,
      No_Missings = no_missings,
      Missings = nan_count,
      Pct_Missings = pct_missings,
      Promedio = promedio,
      Desv_std = desv_estandar,
      Varianza = varianza,
      Minimo = minimo,
      p10 = p10,
      q1 = q1,
      Mediana = mediana,
      q3 = q3,
      p90 = p90,
      p95 = p95,
      p99 = p99,
      Maximo = maximo
    )
  }
  
  result_df <- do.call(rbind, lapply(results, as.data.frame))
  
  rownames(result_df) <- NULL
  
  return(result_df)
  
}

# Ejecutar la función de análisis univariante
resultados <- univar_analisis(data)

# Violin plot y boxplot
data$hrs_studied <- as.character(data$hrs_studied) 
mean_values_A <- aggregate(performance_idx ~ xtr_activities, data = data, FUN = mean, na.rm = TRUE)
data_violines <- na.omit(data)

ggplot(data = data_violines, aes(x = xtr_activities, y = performance_idx)) + 
  geom_boxplot(alpha = 0.2) + 
  geom_hline(yintercept = 1, color = 'red', linetype = 2) +
  geom_point(data = mean_values_A, aes(x = xtr_activities, y = performance_idx), color = "blue", size = 3, shape = 18) +
  geom_text(data = mean_values_A, aes(x = xtr_activities, label = round(performance_idx, 2)), 
            color = "blue", y = 4, vjust = -1, size = 3.5) +
  ylim(0, 100) +
  labs(title = "Titulo",
       subtitle = 'Subtitulo',
       x = "X_titulo",
       y = "Y_titulo") +
  theme_bw()

