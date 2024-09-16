# CODE TESTING SCRIPT

## Librerias
library(ggplot2)
library(kableExtra)

## Ejercicio 01

### Lectura de datos
data <- read.table(file = "datos_ej_1.txt", header = FALSE, sep = "|", strip.white = TRUE)
names(data) <- c("Tamaños", "Num_horas")
head(data)

data_02 <- read.table(file = "datos_ej_2.txt", header = FALSE, sep = "|", strip.white = TRUE)
names(data_02) <- c("Servicio", "Equipos")
head(data_02)

