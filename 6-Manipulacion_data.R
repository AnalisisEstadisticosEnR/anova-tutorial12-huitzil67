setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6")
library(dplyr)
animal_p1 <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6\\animal_p1.csv") 
animal_p2 <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6\\animal_p2.csv") 
animal_rp <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6\\animal_rp.csv") 
animal_meal <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6\\animal_meal.csv")
animal_p1 
animal_p2
(animal <- bind_rows(animal_p1, animal_p2))
setequal(animal_p1, animal_p2)
intersect(animal, animal_rp)
setdiff(animal, animal_rp)
setdiff(animal_rp, animal)
(animal_weight <- union(animal, animal_rp) %>% 
    arrange(id))
animal_meal
(animal_joined <- left_join(animal_weight, animal_meal, 
                            by = c("id" = "IDs"))) 
# hay que indicar a qué columnas en ambas talbas se refieren a `id` 
# O podemos usar el operador %>% 
(animal_joined <- animal_weight %>% 
    left_join(animal_meal, by = c("id" = "IDs")))
inner_join(animal_weight, animal_meal, by = c("id" = "IDs")) 
# ¿Qué hace este comando? 
right_join(animal_weight, animal_meal, by = c("id" = "IDs")) 
# Tenemos todos los id's para dieta, pero varios NAs para `animal` y `weight` 
#¿Por que? 
full_join(animal_weight, animal_meal, by = c("id" = "IDs"))
full_join(animal_p1, animal_p2, by = c("id", "animal", "weight"))
semi_join(animal_weight, animal_meal, by = c("id" = "IDs")) 

#EJERCICIO 1
#EJERCICIO 1

# ¿Qué sucede aquí? 
anti_join(animal_weight, animal_meal, by = c("id" = "IDs"))
library(tidyr) 
library(readr)
marine <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-6\\LPI_marine.csv")

marine2 <- marine %>%
  