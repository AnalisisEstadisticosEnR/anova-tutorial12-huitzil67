#Haz un resumen de las diferentes especies que se encuentran dentro del castillo de 
#Craigmillar, pero dividido en cuatro cuadrantes (NE, NW, SE, SW). (Puedes comenzar 
#usando el objeto trees.genus creado anteriormente).
setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-5")
str(trees)
trees <- read.csv(file = "trees.csv", header = TRUE) 
head(trees) #
str(trees)
trees$LatinName
trees$Site
trees$CommonName
trees$AgeGroup
trees$Easting
trees$Northing
nuevo.objeto <- trees$LatinName
trees.genus <- trees %>% 
  mutate(Genus = case_when(
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus", 
    grepl("Betula", LatinName) ~ "Betula", 
    grepl("Populus", LatinName) ~ "Populus", 
    grepl("Laburnum", LatinName) ~ "Laburnum", 
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus", 
    grepl("Prunus", LatinName) ~ "Prunus", 
    grepl("Pinus", LatinName) ~ "Pinus", 
    grepl("Sambucus", LatinName) ~ "Sambucus", 
    grepl("Crataegus", LatinName) ~ "Crataegus", 
    grepl("Ilex", LatinName) ~ "Ilex", 
    grepl("Quercus", LatinName) ~ "Quercus", 
    grepl("Larix", LatinName) ~ "Larix", 
    grepl("Salix", LatinName) ~ "Salix", 
    grepl("Alnus", LatinName) ~ "Alnus") 
  )
summ.all <- summarise_all(trees, mean)

trees_grouped <- group_by(trees, Easting)
trees.grouped <- group_by(trees, Northing) # Crear grupos
trees.summary <- summarise(trees.grouped, count = length(CommonName)) 

# Aquí contamos el numero de filas por cada grupo. Usamos "CommonName", 
#pero pudimos utilizar cualquiera para hacer el conteo
# O usamos una funcion que justmente cuenta las filas 
trees.summary <- tally(trees.grouped)
trees.summary <- trees %>% 
  group_by(CommonName) %>% 
  tally() 
trees.subset <- trees %>% 
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')
  ) %>% 
  group_by(CommonName, AgeGroup) %>% 
  tally()
summ.all <- summarise_all(trees, mean)
vector <- c(4, 13, 15, 6)
ifelse(vector < 10, "A", "B")

vector2 <- c("What am I?", "A", "B", "C", "D") 
case_when(vector2 == "What am I?" ~ "I am the walrus", 
          vector2 %in% c("A", "B") ~ "goo", 
          vector2 == "C" ~ "ga", 
          vector2 == "D" ~ "joob")
unique(trees$LatinName)

trees.genus <- trees %>% 
trees.genus
#Pista: Para crear los cuadrantes necesitas hacer un poco de matemáticas; encuentra 
#las coordenadas del centro que dividirán los datos (agregando la mitad del rango en 
#longitud y latitud al valor más pequeño)


#Calcula la riqueza de especies (el número de especies diferentes) en cada cuadrante.


#Calcula la abundancia del género Acer (% del número total de árboles) en cada cuadrante.