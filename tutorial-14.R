#Clase 14
# carga paquetes 
library(tidyverse) 
library(ggplot2) 
# Carga la base de datos 
setwd("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-14\\Regresiones-lineales-generalizadas-main")
    
# Primero checar la distribución de los datos
Weevil_damage <- read.csv("Weevil_damage.csv") 
head(Weevil_damage)
hist(Weevil_damage$population)

# Haz el bloque un factor
# Corre el modelo

weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage) 
summary(weevil.m)