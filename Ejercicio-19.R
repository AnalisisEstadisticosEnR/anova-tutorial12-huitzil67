# Ejercicio 19
# Ordinación
# 5 de mayo de 2022
# Roberto Márquez Huitzil
# Datos espaciales poblacionales
setwd("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-19\\16_Tendencias-poblacionales-main") 
# Packages ----
library(readr)
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)
library(ggthemes)
library(mapdata)
library(maps)
library(rgbif)
library(ggrepel)
library(png)
library(gridExtra)
#Haz tu propio tema ggplot2
# Personaliza el tema, cambia los números y colores a tu gusto 
theme_marine <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.border = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.margin = unit(c(1, 1, 1, 1), units = , "cm"), 
          plot.title = element_text(size = 20),
          legend.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.key = element_blank(),
          legend.background = element_rect(color = "black",
fill = "transparent",
size = 2, linetype="blank"))
}
  # Cargar datos de ocurrencia de especies y tendencias de población
load("beluga.RData")
belugas<- as.data.frame(beluga)
# Carga los datos de cambios poblacionales para las especies marinas 
#de la base de datos Living Planet 
load("marine.RData")
#Formateo de datos
# Simplifiquemos los datos de ocurrencia
belugas <- belugas %>% dplyr::select(key, name, decimalLongitude, 
decimalLatitude, year, individualCount, country)
# Dar formato y manipular el conjunto de datos de cambio de población
# veamos los datos 
head(marine)
# Formatea los datos de cambios poblacionales
beluga.pop <- marine %>% filter(Genus == "Delphinapterus") %>% # Selecciona 
#  solo poblaciónes de beluga 
gather(key = "year", value = "abundance", 26:70) %>%  # cambiar a formato  
#  largo
filter(is.na(abundance) == FALSE) %>%  # quiatr filas vacías
  group_by(id) %>%   # Agrupa filas para que cada grupo sea una población 
  mutate(scalepop = 
(abundance-min(abundance))/(max(abundance)-min(abundance)))%>% # escala la abundancia de 0 a 1
filter(length(unique(year)) > 4) %>% # Solo incluye poblaciones 
  #monitoreadas al menos 5 veces 
ungroup()  # Elimina el agrupamiento
# #Explora la base de datos 
str(beluga.pop)
# #Elimina la X
beluga.pop$year <- parse_number(beluga.pop$year)
#Cuantificar el cambio de población
# Calcula los cambios poblacionales con un modelo lineal 
beluga.slopes <- beluga.pop %>%
  group_by(Location.of.population, Decimal.Latitude, Decimal.Longitude, id)%>% do(mod = lm(scalepop ~ year, data = .))  
# extrae los coeficientes usando tidy() del paquete broom 
test <- beluga.pop %>%
group_by(Location.of.population, Decimal.Latitude, Decimal.Longitude, id)%>% 
data.frame(., as.list(coef(lm(scalepop ~ year, data = .)))) %>% 
rename_at(30:31, ~c("estimate", "term"))
head(beluga.slopes)
# selecciona las columnas que necesitamos 
beluga.slopes <- test %>%
  dplyr::select(Location.of.population, Decimal.Latitude, 
                Decimal.Longitude, id, term, estimate)
# 2. Visualización de datos de presencia de especies
(beluga.map <- ggplot(beluga, aes(x = decimalLongitude, y = decimalLatitude)) 
+
borders("worldHires", ylim = c(40, 100), colour = "gray40", fill = "gray40"
        , size = 0.3) +
    # selecciona el mapa 
    theme_map() +
    geom_point(alpha = 0.5, size = 2, colour = "aquamarine3")) # alpha controla 
#la transparencia 1=totalemente transparente
#2. Visualización de datos de presencia de especies
(beluga.map <- ggplot(beluga, aes(x = decimalLongitude, y = decimalLatitude)) 
  +
    borders("worldHires", ylim = c(40, 100), colour = "gray40", fill = "gray40", 
            size = 0.3) +
    # selecciona el mapa 
    theme_map() +
    geom_point(alpha = 0.5, size = 2, colour = "aquamarine3")) # alpha contro 
#la transparencia 1=totalemente transparente

# ** Aquí puedes limpiar los datos de ocurrencia ----
# puedes checar las coordenadas para todas las ocurrencias
# carga un objeto con una línea de costa que tiene un buffer para ver si los 
#puntos están en tierra o mar
#Instalar paquete sp
load("buffland_1deg.rda")
#Por ejemplo, si quisieramos conserver records de avistamientos que fueron to 
#mados desde la costa
# Usa clean_coordinates() del paquete CoordinateCleaner 
#instalar clean coordinates
library(CoordinateCleaner)
beluga.coord.test <- clean_coordinates(beluga, lon = "decimalLongitude", 
lat = "decimalLatitude", species = "name", tests = c("outliers", "seas", "zeros"),
outliers_method = "distance", outliers_td = 5000,seas_ref = buffland)
#species = "" se refiere al nombre la la columna que tiene el nombre de las 
#especies
# Por default, los puntos extremos son las ocurrencias que están más lejos de 
#1000km que cualquier otra ocurrencia 
# Puedes cambiar eso usando outliers.td() con el valor que quieras 
#Probemos si las ocurrencias están en tierra o en mar y si hay ceros en lat o 
#long

head(beluga.coord.test)
# Algunas ocurrencias están en la tierra
# Extrae solo las ocurrencias clasificadas como TRUE usando value = "clean"
beluga.clean <- clean_coordinates(beluga, lon = "decimalLongitude", 
lat = "decimalLatitude",species = "name", tests = c("outliers", "seas", "zeros"),
outliers_method = "distance", outliers_td = 5000,seas_ref = buffland, value = "clean")

#Haz un mapa las ocurrencias limpias (si lograste bajar el paquete)
beluga<- as.data.frame(beluga)
greenland <- filter(beluga, country == "Greenland")
# Selecciona una coordenada única para el las ocurrencias del mismo lugar 
greenland <- dplyr::select(greenland, decimalLongitude, decimalLatitude) %>%
  distinct() 
head(greenland)
# El punto a excluir es uno que está muy al este 
# Longitude -46.00000  Latitude 65.00000
# encuentra qué filas se tienen ese valor usando which()
which(beluga$decimalLongitude == -46 & beluga$decimalLatitude == 65) 
beluga.base <- beluga[-c(1421, 1422, 1423, 1424, 1530),] 
# O puedes filtrar esos puntos de esta manera
beluga.pipe <- beluga %>% filter(decimalLongitude != -46 | decimalLatitude != 65)
# Haz un mapa nuevo que incluya las locaciones de las poblaciónes
(beluga.map.LPI <- ggplot(beluga.base, aes(x = decimalLongitude, 
y = decimalLatitude)) + borders("worldHires", 
ylim = c(40, 100), colour = "gray40", fill = "gray4 0", 
size = 0.3) +
theme_map() +
geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
geom_point(data = beluga.slopes, aes(x = Decimal.Longitude, 
y = Decimal.Latitude),  # Agrega los puntos de los datos de los cambios de poblaciones 
size = 4, colour = "tan1"))
# Aquí especificaste de dónde vienen los datos

# checa los nombres de los sitios
print(beluga.slopes$Location.of.population) 
# Haz los nombres consistentes
beluga.slopes$Location.of.population <- recode(beluga.slopes$Location.of.population,
                                               "Cook Inlet stock, Alaska" = "
Cook Inlet stock")
beluga.slopes$Location.of.population <- recode(beluga.slopes$Location.of.population,
                                               "Eastern Hudson Bay, Quí©bec"
                                               = "Eastern Hudson Bay")
beluga.slopes$Location.of.population <- recode(beluga.slopes$Location.of.population,
                                               "St. Lawrence estuary populati
on" = "St. Lawrence Estuary")
beluga.slopes$Location.of.population <- recode(beluga.slopes$Location.of.population,
                                               "St. Lawrence Estuary population" = "St. Lawrence Estuary")
beluga.slopes$Location.of.population <- recode(beluga.slopes$Location.of.population,
                                               "St. Lawrence estuary, Canada"
                                               = "St. Lawrence Estuary") 
# Checalos
print(beluga.slopes$Location.of.population)
# Carga los paquetes para agregar imágenes 
packs <- c("png","grid")
#instalar paquete grid
lapply(packs, require, character.only = TRUE) 
# Carga el ícono de la beluga
icon <- readPNG("beluga_icon.png")
icon <- rasterGrob(icon, interpolate=TRUE) 
# Hay que "raterizarlo" para poder graficarlo
# ¡Ahora viene lo que parece un trozo gigantesco de código! 
beluga.map.final <- ggplot(beluga.base, aes(x = decimalLongitude, y = 
 decimalLatitude)) +
borders("worldHires", ylim = c(40, 100), colour = "gray40", fill = 
"gray40", size = 0.3) +
theme_map() +
geom_point(alpha = 0.3, size = 2, colour = "aquamarine3") +
geom_label_repel(data = beluga.slopes[1:3,], aes(x = Decimal.Longitude, 
y = Decimal.Latitude,
label = Location.of.population),
box.padding = 1, size = 5, nudge_x = 1,
 nudge_y = ifelse(beluga.slopes[1:3,]$id == 13273, 
4, - 4),
min.segment.length = 0, inherit.aes = FALSE) +
geom_point(data = beluga.slopes, aes(x = Decimal.Longitude, y =
Decimal.Latitude + 0.6),
size = 4, colour = "tan1") +
geom_point(data = beluga.slopes, aes(x = Decimal.Longitude, y = 
Decimal.Latitude - 0.3),
size = 3, fill = "tan1", colour = "tan1", shape = 25) +
#Aquí modifiqué para cambiar la posición de la beluga como icono
#Modifiqué las coordenadas en Y, pero sin cambiar las proporciones
# Agrega puntos para los cambios de poblaciones
annotation_custom(icon, xmin = -210, xmax = -120, ymin = 10, ymax = 30) + 
# Agrega el icono
labs(title = "a. Ocurrencias de Beluga GBIF") + # titulo
theme(plot.title = element_text(size = 20)) # tamaño del titulo

# Numero de registros de ocurrencias a traves del tiempo
yearly.obs <- beluga %>% group_by(year) %>% tally() %>% ungroup() %>% filter( 
  is.na(year) == FALSE)
(occurrences <- ggplot(yearly.obs, aes(x = year, y = n)) + 
    geom_line(colour = "aquamarine3", size = 1) +
    geom_area(aes(y = n), fill = "aquamarine3") + 
    labs(x = NULL, y = "Número de occurrencias\n") +
    theme_marine())

#metí el comando para que me limpiara el área de ploteo
dev.off()
# Crea un objeto para la población de Hudson Bay 
beluga1 <- filter(beluga.pop, id == "13273")
# Elige la pendiente de los cambios de la población par ponerlo en la grafica 
#, si quieres
print(beluga.slopes$year[beluga.slopes$id == "13273"])
(hudson.bay <- ggplot(beluga1, aes(x = year, y = abundance)) +
    geom_point(shape = 21, fill = "aquamarine3", size = 4) +
    geom_smooth(method = "lm", colour = "aquamarine3", 
fill = "aquamarine3", alpha = 0.4) +
labs(x = "", y = "IndividuOS\n", title = "c. Eastern Hudson Bay\n") +
    theme_marine())
# Ahora, podemos modificar un poco el código para hacer un gráfico para la población de 
#stock de Cook Inlet.
beluga2 <- filter(beluga.pop, id == "2191")
(cook.inlet <- ggplot(beluga2, aes(x = year, y = abundance)) + 
    geom_point(shape = 21, fill = "aquamarine3", size = 4) +
    geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", 
                alpha = 0.4) +
    labs(x = "", y = "", title = "d. Cook Inlet stock\n") + 
    theme_marine())

# Crea un objeto que tenga las tres poblaciónes del estuario de St. Lawrence 
#estuary
# usando el operador "|" 
beluga3 <- filter(beluga.pop, id == "1950" | id == "4557" | id == "4558") 
(st.lawrence.est <- ggplot(beluga3, aes(x = year, y = abundance, 
shape = as.factor(id))) +
    geom_point(fill = "aquamarine3", size = 4) + 
    scale_shape_manual(values = c(21, 23, 24)) +
    geom_smooth(method = "lm", colour = "aquamarine3", fill = "aquamarine3", 
                alpha = 0.4) +
    labs(x = "", y = "", title = "e. Estuario St. Lawrence\n") + 
    theme_marine() +
    guides(shape = FALSE))
# Organiza todos los gráficos en un panel con el paquete gridExtra
# Crea el panel con todas las graficas
row1 <- grid.arrange(beluga.map.final, occurrences, ncol = 2, widths = c(1.96 
                                                                         , 1.04))
# Especifica como quieres que sean las proporciones
row2 <- grid.arrange(hudson.bay, cook.inlet, st.lawrence.est, ncol = 3, 
widths = c(1.1, 1, 1))
# Cambia los Espacios de las graficas
beluga.panel <- grid.arrange(row1, row2, nrow = 2, 
heights = c(0.9, 1.1))

