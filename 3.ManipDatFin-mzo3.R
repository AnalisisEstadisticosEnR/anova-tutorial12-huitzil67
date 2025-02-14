edidiv <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-3\\edidiv.csv")
#esta es la ruta donde se guard� la base de datos en mi compu
head(edidiv) #"Muestra las primeras filas"
tail (edidiv)# Muestra las ultimas filas
str(edidiv) # Muestra la estructura de las variables (entero, continuo, categ�rico, caracter)
head(edidiv$taxonGroup)
class(edidiv$taxonGroup)
edidiv$taxonGroup<-as.factor((edidiv$taxonGroup))
class(edidiv$taxonGroup) 
#�qu� clase tiene ahora? Factor
edidiv$year
class(edidiv$year)
edidiv$year
class(edidiv$year)
edidiv$year <- as.factor(edidiv$year)
class(edidiv$year)
class(edidiv)
# Calcular la riqueza de especies
Beetle <- filter(edidiv, edidiv$taxonGroup == "Beetle")
#NOTA: hab�a un errorcito en el tutorial de la clase, porque dec�a taxonGroup solamente y era
# edidiv$taxonGroup. Para la actividad hab�a que hacer lo mismo para todos los grupos
Beetle <- as.factor(Beetle)
# En esta funci�n, el primer argumento es la base de datos, el segundo es la 
# condici�n que quieres usar para filtrar. Solo queremos, en este caso, a los 
# escarabajos, entonces pedimos que la variable elija los campos que concuerdan 
# EXACTAMENTE (==) con Beetle, todo lo dem�s lo quita. 
# OJO. R es altamente sensible a errores de ortografia, por lo que la palabra 
# beetle (sin mayusculas) no va a funcionar en este caso).
install.packages("dplyr")
library(dplyr)
Beetle <- filter(edidiv, edidiv$taxonGroup == "Beetle")
Bird <- filter(edidiv, edidiv$taxonGroup == "Bird") #Ahora realiza estos pasos para TODOS los taxones en los datos
# Ahora realiza estos pasos para TODOS los taxones en los datos (plantas con flor, 
# marchantiophytas, hongos, mam�feros, mariposas, hymenopteros, moluscos, lib�lulas, 
# liquen)

#estos son los grupos de la base de datos: "Beetle"           "Bird"             
# "Butterfly"  "Dragonfly"        "Flowering.Plants" "Fungus"          
# "Hymenopteran"     "Lichen"           "Liverwort"       
# "Mammal"           "Mollusc"   

Beetle <- filter(edidiv, edidiv$taxonGroup == "Beetle")
Bird <- filter(edidiv, edidiv$taxonGroup == "Bird")
Butterfly <- filter(edidiv, edidiv$taxonGroup == "Butterfly")
Dragonfly <- filter(edidiv, edidiv$taxonGroup == "Dragonfly")
Flowering.Plants <- filter(edidiv, edidiv$taxonGroup == "Flowering.Plants")
Fungus <- filter(edidiv, edidiv$taxonGroup == "Fungus")
Hymenopteran <- filter(edidiv, edidiv$taxonGroup == "Hymenopteran")
Lichen <- filter(edidiv, edidiv$taxonGroup == "Lichen")
Liverwort <- filter(edidiv, edidiv$taxonGroup == "Liverwort")
Mammal <- filter(edidiv, edidiv$taxonGroup == "Mammal")
Mollusc <- filter(edidiv, edidiv$taxonGroup == "Mollusc")

# Para ver todas las especies del grupo taxon�mico
#Primero tuve que ver los niveles de la columna
levels(edidiv$taxonNam)
levels(edidiv$taxonGroup)
# Veo el tipo de variable que tiene la columna
class(edidiv$taxonName)
# Lo convierto a factor
edidiv$taxonName <- as.factor(edidiv$taxonName)
# veo las clases que tiene
class(edidiv$taxonName)
#veo los niveles del factor
levels(edidiv$taxonName)
levels(edidiv$taxonGroup)

#estos son los grupos de la base de datos: "Beetle"           "Bird"             
# "Butterfly"  "Dragonfly"        "Flowering.Plants" "Fungus"          
# "Hymenopteran"     "Lichen"           "Liverwort"       
# "Mammal"           "Mollusc"   

a <- length(unique(Beetle$taxonName))
b <- length(unique(Bird$taxonName))
# Puedes usar cualquier nombre, yo eleg� a,b,c......
# eneste caso eleg� los n�meros siguientes para las siguientes categor�as
c <- length(unique(Butterfly$taxonName))
d <- length(unique(Dragonfly$taxonName)) 
e <- length(unique(Flowering.Plants$taxonName))
f <- length(unique(Fungus$taxonName))
g <- length(unique(Hymenopteran$taxonName))
h <- length(unique(Lichen$taxonName))
i <- length(unique(Liverwort$taxonName))
j <- length(unique(Mammal$taxonName))
k <- length(unique(Mollusc$taxonName))

#Si escribes a (o como hayas nombrado tus variables de conteo) en la consola, �qu� devuelve? 
# devuelve 37L para a y 86L para b, que es el n�mero de especies en cada categor�a
#�Qu� significa eso? el n�mero de especies en a para aves y en b para escarabajos
#Beetle o a = 37, Bird o b =86, Butterfly o c =25,Dragonfly o d=11,Flowering.Plants o e =521 , Fungus o f= 219,
# Hymenopteran o g =112 Lichen o h=94, 
# Liverwort o i=40, Mammal o j=33, Mollusc o k=97

# Nuevamente, calcula la riqueza de especies para los otros taxones en el conjunto de 
# datos.

# Crea un vector y una gr�fica
biodiv <- c(a,b,c,d,e,f,g,h,i,j,k) # Estamos encadenando los valores; el 
# orden en que los pongas, es el orden en que debes nombrarlos
names(biodiv) <- c("Beetle", 
                   "Bird", 
                   "Butterfly", 
                   "Dragonfly", 
                   "Flowering.Plants", 
                   "Fungus", 
                   "Hymenopteran", 
                   "Lichen", 
                   "Liverwort", 
                   "Mammal", 
                   "Mollusc")
barplot(biodiv)
png("barplot.png", width=1600, height=600)
barplot(biodiv, xlab="Taxa", ylab="Number of species", ylim=c(0,600), cex.names= 
1.5, cex.axis=1.5, cex.lab=1.5)
dev.off() # Esto cierra el dispositivo de gr�fica que creaste y la siguiente 
# l�nea se abrir� en un nuevo archivo
getwd() # "obtener directorio de trabajo

# # Crea un objeto llamado "taxa" que contiene todos los nombres de los taxones
taxa <- c("Beetle", 
          "Bird", 
          "Butterfly", 
          "Dragonfly", 
          "Flowering.Plants", 
          "Fungus", 
          "Hymenopteran",
          "Lichen", 
          "Liverwort", 
          "Mammal", 
          "Mollusc")
# Cambia este objeto a una variable categ�rica (factor) llamada taxa_f
taxa_f <- as.factor(taxa)
# Combina todos los valores del numero de especies (a,b,c...) en un objeto llamado richness
richness <- c("37",
              "86",
              "25",
              "11",
              "521",
              "219",
              "112",
              "94",
              "40",
              "33",
              "97")
# Crea la base de datos para los dos factores
biodata <- data.frame(taxa_f, richness)
# Guardalo en un archivo
write.csv(biodata, file="biodata.csv") # Se va a guardar en tu directorio de  trabajo
biodata
# El dato original no era num�rico. Tuve que convertirlo a num�rico
biodata$richness <- as.numeric(biodata$richness)
#luego graficar. Primero lo traduje al espa�ol
barplot(biodata$richness, names.arg = c("Escarabajo", 
                                      "Ave", 
                                      "Mariposa", 
                                      "Lib�lula", 
                                      "Plantas con flor", 
                                      "Hongos", 
                                      "Himen�pteros", 
                                      "L�quenes", 
                                      "Hep�ticas", 
                                      "Mam�feros", 
                                      "Moluscos"), 
        xlab="Tax�n", ylab="N�mero de especies", ylim=c(0,600))


#Ejercicio
# Estos son valores (ficticios) de la envergadura (en cm) medidos en cuatro especies 
# diferentes de aves: 
Aves_Enver <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-3\\Aves_Enver.csv")
head(Aves_Enver)
tail(Aves_Enver)
str(Aves_Enver)
levels(Aves_Enver$bird_sp)
class(Aves_Enver$bird_sp)
class(Aves_Enver$wingspan)
# * Produce un gr�fico de barras de la envergadura media de cada especie y gu�rdalo en tu computadora? 
Aves_Enver$bird_sp <- as.factor(Aves_Enver$bird_sp)
Eagle <- filter(Aves_Enver, Aves_Enver$bird_sp == "eagle")
Eagle
Hummingbird <- filter(Aves_Enver, Aves_Enver$bird_sp == "hummingbird")
Hummingbird
Kingfisher <- filter(Aves_Enver, Aves_Enver$bird_sp == "kingfisher")
Kingfisher
Sparrow <- filter(Aves_Enver, Aves_Enver$bird_sp == "sparrow")
Sparrow
# Para ver todas las especies del grupo taxon�mico
#Primero tuve que ver los niveles de la columna
levels(Aves_Enver$bird_sp)
# Veo el tipo de variable que tiene la columna
class(Aves_Enver$bird_sp)
# Lo convierto a factor
Aves_Enver$bird_sp <- as.factor(Aves_Enver$bird_sp)
Aves_Enver$wingspan <- as.numeric(Aves_Enver$wingspan)
# veo las clases que tiene
class(Aves_Enver$bird_sp)
#veo los niveles del factor
levels(Aves_Enver$bird_sp)
#estos son los grupos de la base de datos: "eagle","hummingbird","kingfisher","sparrow" 
class(Aves_Enver$wingspan)
str(Aves_Enver$wingspan)

#(�Cu�l podr�a ser la funci�n para calcular la media?)
# Para la media general
mean <- mean(Aves_Enver$wingspan)
#Para la media por especie
Eagle <- filter(Aves_Enver, Aves_Enver$bird_sp == "eagle")
Eagle
a <- mean(Eagle$wingspan)
a
Hummingbird <- filter(Aves_Enver, Aves_Enver$bird_sp == "hummingbird")
Hummingbird
b <- mean(Hummingbird$wingspan)
b
Kingfisher <- filter(Aves_Enver, Aves_Enver$bird_sp == "kingfisher")
Kingfisher
c <- mean(Kingfisher$wingspan)
c
Sparrow <- filter(Aves_Enver, Aves_Enver$bird_sp == "sparrow")
Sparrow
d <- mean(Sparrow$wingspan)
d

# Crea un vector y una gr�fica
EnvMedia <- c(a,b,c,d) # Estamos encadenando los valores; el 
# orden en que los pongas, es el orden en que debes nombrarlos
names(EnvMedia) <- c("Eagle", 
                   "Hummingbird", 
                   "kingfisher", 
                   "Sparrow")
barplot(EnvMedia)
png("barplot.png", width=1600, height=600)
barplot(EnvMedia, xlab="Especie", ylab="Media de envergadura", ylim=c(0,600), cex.names= 
          1.5, cex.axis=1.5, cex.lab=1.5)
EnvMedia
plot(EnvMedia)
dev.off() # Esto cierra el dispositivo de gr�fica que creaste y la siguiente 
# l�nea se abrir� en un nuevo archivo
getwd() # "obtener directorio de trabajo

# # Crea un objeto llamado "taxa" que contiene todos los nombres de los taxones
setwd("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-3")
getwd()
EnvXsp <- c("Eagle", 
          "Hummingbird", 
          "kingfisher", 
          "Sparrow")
# Cambia este objeto a una variable categ�rica (factor) llamada taxa_f
EnvXsp <- as.factor(EnvXsp)
# Combina todos los valores del numero de especies (a,b,c...) en un objeto llamado richness
EnvMedia <- c("193.666667",
              "8.666667",
              "24.666667",
              "22.333333")
# Crea la base de datos para los dos factores
Talla <- data.frame(EnvXsp, EnvMedia)
# Guardalo en un archivo
write.csv(Talla, file="tallaXsp.csv") # Se va a guardar en tu directorio de  trabajo
Talla
# El dato original no era num�rico. Tuve que convertirlo a num�rico
Talla$EnvMedia <- as.numeric(Talla$EnvMedia)
#luego graficar. Primero lo traduje al espa�ol
barplot(Talla$EnvMedia, names.arg = c("Eagle", 
                                        "Hummingbird", 
                                        "Kingfisher", 
                                        "Sparrow"), 
        xlab="Especie", ylab="Envergadura media", ylim=c(0,200))

#Otro m�todo que hab�a utilizado
tapply(Aves_Enver$wingspan,Aves_Enver$bird_sp , mean) #"Para obtener las medias por cada clase"
aggregate(Aves_Enver$wingspan , list(Aves_Enver$bird_sp), mean) #para obtener la media
#por cada clase, pero en forma de lista 
barplot(tapply(Aves_Enver$wingspan,Aves_Enver$bird_sp , mean))
#En este caso, llam� a la base de datos Aves_Enver y nombr� a las variables con los nombres 
#que aparecen en el las instrucciones