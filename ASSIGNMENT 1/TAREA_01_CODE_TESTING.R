# CODE TESTING SCRIPT

## Librerias
library(ggplot2)
library(kableExtra)

## Ejercicio 01

### Lectura de datos
data <- read.table(file = "datos_ej_1.txt", header = FALSE, sep = "|", strip.white = TRUE)
names(data) <- c("Tamaños", "Num_horas")
head(data)


