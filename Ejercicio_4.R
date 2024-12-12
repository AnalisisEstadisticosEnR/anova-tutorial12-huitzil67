# 4.Manipulacion_Datos
# Roberto Márquez Huitzil
#
#EJERCICIO
#En el repositorio, encontrarás el archivo dragons.csv, que 
#proporciona la longitud (en cm) de las columnas de fuego que 
#respiran los dragones de diferentes especies cuando se 
#alimentan con diferentes especias.
#Tienes que ordenar los datos (formato largo) y crear una 
#gráfica de caja para cada especie que muestre el efecto de las
#especias en el tamaño de la columna de fuego, de 
#modo que pueda responder las preguntas: ¿Qué especia 
#desencadena la reacción más intensa?
setwd("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-4")
dragons <- read.csv("dragons.csv", header = TRUE)
head(dragons) #Veo el nombre de las columnas y datos superiores 
str(dragons) #Veo el nombre de las columnas y datos inferiores
dragons$dragon.ID #veo el nombre de los ID
dragons$species #veo el nombre de las especies
dragons$tabasco #veo las longitudes de fuego en esta especia
dragons$jalapeno#veo las longitudes de fuego en esta especia
dragons$wasabi#veo las longitudes de fuego en esta especia
dragons$paprika#veo las longitudes de fuego en esta especia
length(unique(dragons$dragon.ID))#veo la longitud de los datos 
#bajo esta categoría
length(unique(dragons$species))#veo el número de especies
#bajo esta categoría
length(unique(dragons$tabasco))#veo el número de distintas  
#longitudes bajo esta categoría
length(unique(dragons$jalapeno))#veo el número de distintas  
#longitudes bajo esta categoría
length(unique(dragons$wasabi))#veo el número de distintas  
#longitudes bajo esta categoría
length(unique(dragons$paprika))#veo el número de distintas  
#longitudes bajo esta categoría
dragons[2,5] #veo el dato que está en este renglón y columna
library(tidyr) #enciendo la librería
length(unique(dragons$dragon.ID))#"el número de datos en esta
#categoría
dragons2<-dragons #"hago una nueva copia de la BD"
names(dragons2) #"nombre de las columnas en la BD"
str(dragons2) #resumen de las columnas y los renglones en la BD
View(dragons2)
#renombrar la columna paprika por curcuma
library(dplyr)
library(tidyr)
dragons2 <- rename(dragons2, curcuma = paprika)

fire_long <- gather(dragons2,spice,long_fuego, c(tabasco, jalapeno, wasabi, curcuma))
View(fire_long)
View(dragons2)
#para ver únicamente los registros de "hungarian_horntail" 
dragons2[dragons2$species == "hungarian_horntail" & dragons2$tabasco >= 0,]
#Para ver sólo los registros de hungarian_horntail, para todas las especias
dragons2_hungarianTabasco <- dragons2[dragons2$species == "hungarian_horntail" 
                                      & dragons2$tabasco >= 0,]
dragons2_hungarianTabasco$tabasco_menos <- dragons2_hungarianTabasco[ ,3] -30
View(dragons2_hungarianTabasco)
#Gráfica de caja para hungarian_horntail 
fire_long_hungarian <- gather(dragons2_hungarianTabasco,spice,
                              long_fuego, c(tabasco_menos,jalapeno, 
                                                 wasabi, 
                                               curcuma))
View(fire_long_hungarian)
#Gráfica de cajas para Hungarian horntail sin restar los 30 a
#tabasco
boxplot(long_fuego ~ spice, data = fire_long, xlab = "especia", 
        ylab = "longitud del fuego (cm)", main = "Longitud del 
        fuego para Hungarian horntail sin restar 30
        a tabasco")
#Para crear una columna exactamente igual a la anterior
fire_long_hungarian$tabasco.menos <- fire_long_hungarian$long_fuego
# Creo una nueva columna en dragons2_hungarianTabasco y le copio la informacion
fire_long_hungarian$tabasco.menos <- fire_long_hungarian$long_fuego
View(fire_long_hungarian)
#para restar 30 a cada dato de hungarian horntail con tabasco
fire_long_hungarian$tabasco.menos <- fire_long_hungarian[ ,4] -30
fire_long_hungarian$long_fuego_m <- fire_long_hungarian$tab_menos / 100
boxplot(fire_long_hungarian$long_fuego_m ~ spice, data = fire_long_hungarian, 
        xlab = "especia", ylab = "longitud del fuego (cm)", ylim = c(0,1.5), main = 
          "Longitud del fuego para Hungarian horntail")
#Modifico la columna sólo en los valores que me interesa
#creo una nueva BD, le digo que la mute, que tome los datos de la BD
#original, le pongo la columna que va a modificar, en este caso ya se sabe
#que es de la BD. 

View(fire_long_hungarian)

#Para el caso del Hungarian horntail, la especia con la que lanza más fuego es el 
#jalapeño, mientras que la cúrcuma es con la que lanza menos.
#Ahora corrijo en la BD original las medidas a cm
fire_long_m <- gather(dragons2,spice,long_fuego, c(tabasco, jalapeno, wasabi, curcuma))
View(fire_long_m)
#Creo una nueva columna con las unidades de metros para todas las especies,
#aunque en el caso de Hungarian horntail ya había creado un subconjunto para
#esa especie solamente. 
fire_long_m$long_fuego_m <- fire_long_m$long_fuego / 100
View(fire_long_m)
#Gráfico para cada especie restante haciendo subconjuntos
#Para ver sólo los registros de swedish shortsnout
fire_long_swedish <- fire_long_m[fire_long_m$species == "swedish_shortsnout", ]
View(fire_long_swedish)
boxplot(fire_long_swedish$long_fuego_m ~ spice, data = fire_long_swedish, xlab = "especia", 
        ylab = "longitud del fuego (cm)", main = "Longitud del 
        fuego para Swedish shortsnout")
#Para el caso de la especie long swedish, la especia jalapeño da mayor longitud del fuego. La
#curcuma es la que da la menor longitud de fuego

#Para ver sólo los registros de welsh green
fire_long_welsh <- fire_long_m[fire_long_m$species == "welsh_green", ]
View(fire_long_welsh)
boxplot(fire_long_welsh$long_fuego_m ~ spice, data = fire_long_welsh, xlab = "especia", 
        ylab = "longitud del fuego (cm)", main = "Longitud del 
        fuego para Welsh green")
#Para el caso de la especies Wels green, la especia jalapeño da mayor longitud del fuego. La
#curcuma es la que da la menor longitud de fuego


#Pau: Abajo intenté con la fórmula que me enviaste, pero no logré correrlo,
#quizás so substituí correctamente las variable
boxplot(fire_long_m$long_fuego_m ~ spice, data = fire_long_m[fire_long_m$species =="'hungarian_horntail",],
        xlab = 'especia', ylab = 'largo de columna de fuego (m)',
        main = 'Hungarian Horntail')

