#Ejercicio 18
#Ordinación
# 3 de mayo de 2022
#Roberto Márquez Huitzil
# paquetes (instala si no los tienes)
library(vegan)
library(ape)
library(dplyr)

# La base de datos que vamos a usar
data(varespec) # Cobertura vegetal de 44 especies
head(varespec)
# vamos a hacer un NMDS y graficar
varespec %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")

#Veamos cómo hacer un PCA en R.
PCA <- rda(varespec, scale = FALSE)
# Usa scale = TRUE si tus variables están en distintas escalas (ej. variables
#abióticas).
# Aquí, todas las especies se miden en la misma escala

# haz una gráfica de barras de valores propios relativos. Esto va a dar el
#porcentaje de varianza explicada por cada eje
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))
#¿Cuánto de la varianza es explicada por el primer componente principal?
  # Calcula el porcentaje de varianza explicado por los primeros dos ejes
  sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%
#Ahora para los primeros tres ejes
# grafica
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")

# Se pueden extraer los valores de especies y sitios del nuevo PC para otros
#análisis:
sitePCA <- PCA$CA$u # Sitios
speciesPCA <- PCA$CA$v # Especies
# En un grafica doble de PCA, las especies se dibujan como flechas que
#apuntan hacia la dirección del valor que incrementa para esa variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10))
# gráfica doble de los ejes 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points"))
# gráfica doble de los ejes 1 vs 3

# Calcular la matriz de distancias
# Bray-Curtis
dist <- vegdist(varespec, method = "bray")
# PCoA no está incluido en vegan.
library(ape)
PCOA <- pcoa(dist)
# graficar
barplot(PCOA$values$Relative_eig[1:10])
# Algunas distancias pueden terminar en valores propios negativos. Se pueden
#corregir:
  PCOA <- pcoa(dist, correction = "cailliez")
# gráfica
biplot.pcoa(PCOA)

# Aquí no se grafican las especies pues la matriz de distancia lo hace sitio
#por sitio
#pero podemos hacer los siguiente:
biplot.pcoa(PCOA, varespec)
# Se pueden extraer los primeros dos ejes, si lo necesitas para análisis
#estadísticos
PCOAaxes <- PCOA$vectors[,c(1,2)]
# comparar los resultados con PCA
biplot.pcoa(PCOA)
plot(PCA)

# Calcular la matriz de distancias
dist <- vegdist(varespec, method = "bray")

# NMDS.scree() automáticamente hace un NMDS para 1-10 dimensiones y grafica
NMDS.scree <- function(x) { # x es el nombre de la variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress)
       , xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# de Dimensiones", ylab = "Tensi
on", main = "NMDS")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i +
                                                 1)$stress))
  }
}
# Usa la función que acabamos de crear para elegir el número óptimo de dimensiones
NMDS.scree(dist)

set.seed(2)
# Vamos a hacer los resultados de dos formas, usando la matriz de distancias
#y usando los datos "crudos":
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
NMDS1
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F)
# Si no le das una matriz de similitud, metaMDS automáticamente aplica
#Bray-Curtis.
NMDS2
#grafiqué para ver las diferencias entre el NMDS2 y el NMDS3
NMDS2 <- plot(NMDS2)
#Verifica el archivo de ayuda para metaNMDS() e intente adaptar 
#la función para NMDS2, de modo que se desactive la transformación automática.
#Había que incluir el valor autotransform=FALSE para que ya no lo corra
NMDS3 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE)
NMDS3 <- plot(NMDS3)

##ERROR en los siguientes 3 códigos

#corrección, poniendo antes
dev.off()

plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")


# Usando ordiplot y orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# Cargamos la otra base de datos (ambientales)
data(varechem)
# envfit va a agregar datos de variables ambientales como vectores de
#ordinación
ef <- envfit(NMDS3, varechem, permu = 999)
ef
# Las últimas dos columnas nos interesan: el coeficiente de correlación (cuad
#rada) y el valor-p asociado
# grafica los vectores con correlaciones significativas e interpreta la gráfi
#ca

##error corregido al poner el dev off, en el NMDS, como el anterior
plot(NMDS3, type = "t", display = "sites")
plot(ef, p.max = 0.05)

# Define la variable que agrupa (las primeras 12 muestras son de grupo 1, las
#ltimas 12, de grupo 2
group = c(rep("Group1", 12), rep("Group2", 12))
# Un vector de colores con la misma dimensión que los grupos
colors = c(rep("red", 12), rep("blue", 12))
# Polígonos de colores que corresponden a los grupos
ordiplot(NMDS3, type = "n") 

#error en este script, corregido con el dev off
for(i in unique(group)) {ordihull(NMDS3$point[grep(i, group),], 
draw="polygon",groups = group[group == i],col = colors[grep(i,group)],label=F)}

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
rep("blue", 12)), air = 0.01, cex = 1.25)


Ejercicio
#Realiza un análisis de ordinación con la base de datos de dunas
#(dune) proporcionado por 
#el paquete vegan. Interpreta tus resultados usando las variables
#ambientales de dune.env.
# La base de datos que vamos a usar
data("dune")
head(dune)
# vamos a hacer un NMDS y graficar
dune %>%
  metaMDS(trace = F) %>%
  ordiplot(type = "none") %>%
  text("sites")



#Veamos cómo hacer un PCA en R.
PCA <- rda(dune, scale = FALSE)
# Usa scale = TRUE si tus variables están en distintas escalas (ej. variables
#abióticas).
# Aquí, todas las especies se miden en la misma escala

# haz una gráfica de barras de valores propios relativos. Esto va a dar el
#porcentaje de varianza explicada por cada eje
barplot(as.vector(PCA$CA$eig)/sum(PCA$CA$eig))
#¿Cuánto de la varianza es explicada por el primer componente principal?
# Calcula el porcentaje de varianza explicado por los primeros dos ejes
sum((as.vector(PCA$CA$eig)/sum(PCA$CA$eig))[1:2]) # 79%
#Ahora para los primeros tres ejes
# grafica
plot(PCA)
plot(PCA, display = "sites", type = "points")
plot(PCA, display = "species", type = "text")

# Se pueden extraer los valores de especies y sitios del nuevo PC para otros
#análisis:
sitePCA <- PCA$CA$u # Sitios
speciesPCA <- PCA$CA$v # Especies
# En un grafica doble de PCA, las especies se dibujan como flechas que
#apuntan hacia la dirección del valor que incrementa para esa variable
biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10))
# gráfica doble de los ejes 1 vs 2
biplot(PCA, choices = c(1,3), type = c("text","points"))
# gráfica doble de los ejes 1 vs 3

# Calcular la matriz de distancias
# Bray-Curtis
dist <- vegdist(dune, method = "bray")
# PCoA no está incluido en vegan.
library(ape)
PCOA <- pcoa(dist)
# graficar
barplot(PCOA$values$Relative_eig[1:10])


# Algunas distancias pueden terminar en valores propios negativos. Se pueden
#corregir:
PCOA <- pcoa(dist, correction = "cailliez")
# gráfica
biplot.pcoa(PCOA)

# Aquí no se grafican las especies pues la matriz de distancia lo hace sitio
#por sitio
#pero podemos hacer los siguiente:
biplot.pcoa(PCOA, dune)
# Se pueden extraer los primeros dos ejes, si lo necesitas para análisis
#estadísticos
PCOAaxes <- PCOA$vectors[,c(1,2)]
# comparar los resultados con PCA
biplot.pcoa(PCOA)
plot(PCA)

# Calcular la matriz de distancias
dist <- vegdist(dune, method = "bray")

# NMDS.scree() automáticamente hace un NMDS para 1-10 dimensiones y grafica
NMDS.scree <- function(x) { # x es el nombre de la variable
  plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress)
       , xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# de Dimensiones", ylab = "Tensi
on", main = "NMDS")
  for (i in 1:10) {
    points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i +
                                                 1)$stress))
  }
}
# Usa la función que acabamos de crear para elegir el número óptimo de dimensiones
NMDS.scree(dist)

set.seed(2)
# Vamos a hacer los resultados de dos formas, usando la matriz de distancias
#y usando los datos "crudos":
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
NMDS1
NMDS2 <- metaMDS(dune, k = 2, trymax = 100, trace = F)
# Si no le das una matriz de similitud, metaMDS automáticamente aplica
#Bray-Curtis.
NMDS2
#grafiqué para ver las diferencias entre el NMDS2 y el NMDS3
NMDS2 <- plot(NMDS2)
#Verifica el archivo de ayuda para metaNMDS() e intente adaptar 
#la función para NMDS2, de modo que se desactive la transformación automática.
#Había que incluir el valor autotransform=FALSE para que ya no lo corra
NMDS3 <- metaMDS(dune, k = 2, trymax = 100, trace = F, autotransform = FALSE)
NMDS3 <- plot(NMDS3)

##ERROR en los siguientes 3 códigos
plot(NMDS3, display = "sites", type = "n")
points(NMDS3, display = "sites", col = "red", cex = 1.25)
text(NMDS3, display ="species")


# Usando ordiplot y orditorp
ordiplot(NMDS3, type = "n")
orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", cex = 1.1, air = 0.01)

# Cargamos la otra base de datos (ambientales)
data(varechem)
# envfit va a agregar datos de variables ambientales como vectores de
#ordinación
ef <- envfit(NMDS3, varechem, permu = 999)
ef
# Las últimas dos columnas nos interesan: el coeficiente de correlación (cuad
#rada) y el valor-p asociado
# grafica los vectores con correlaciones significativas e interpreta la gráfi
#ca

##error en el NMDS, como el anterior
plot(NMDS3, type = "t", display = "sites")

plot(ef, p.max = 0.05)
# Define la variable que agrupa (las primeras 12 muestras son de grupo 1, las
#ltimas 12, de grupo 2
group = c(rep("Group1", 12), rep("Group2", 12))
# Un vector de colores con la misma dimensión que los grupos
colors = c(rep("red", 12), rep("blue", 12))
# Polígonos de colores que corresponden a los grupos
ordiplot(NMDS3, type = "n") 

#error en este script
for(i in unique(group)) {ordihull(NMDS3$point[grep(i, group),], 
draw="polygon",groups = group[group == i],col = colors[grep(i,group)],label=F)}

orditorp(NMDS3, display = "species", col = "red", air = 0.01)
orditorp(NMDS3, display = "sites", col = c(rep("red",12),
rep("blue", 12)), air = 0.01, cex = 1.25)
