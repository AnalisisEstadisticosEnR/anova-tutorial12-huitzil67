#8. Visualizaci�n de datos de dos tierras m�gicas: Hogsmeade y Narnia 
#Curso R
#1� marzo 2022
#Roberto M�rquez Huitzil
#Empieza como siempre con un script nuevo e importa las bases de datos
setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-9_Distribucion-de-los-datos") 
estatura<- read.csv('ANSUR II FEMALE Public.csv', header = T,sep = ',') 
lobo<- read.csv('wolf_hormone_data_for_dryad.csv', header = T,sep = ',')
install.packages("measurements")
library(measurements)
library(dplyr)
str(estatura)
# Convierte la estatura de pulgadas (in) a cent�metros (cm)
estatura$heightcm <- estatura$Heightin*2.54
# Crea un histograma
estaturacm <- mutate(estatura, heightcm = estatura[,106]*2.54)
estaturacm$heightcm
hist <- ggplot(estaturacm, aes(x = heightcm)) + geom_histogram()
hist
# Valores con baja probabilidad: Observar una estatura mayor a 180cm es casi cero
length(which(estatura$cm>180))
# Saca la mediana y la moda de la estatura
median(estatura$heightcm, na.rm=TRUE)
sort (estaturacm$heightcm, decreasing = TRUE)
#Procedimiento para sacar la moda hecho por Josu�
frecuencias <- data.frame(table(estaturacm$heightcm))
moda <- frecuencias[which.max(frecuencias$Freq),1]
#Grafico con GGPLOT, pero tengo que cambiar el nombre de mis variales en data y 
#en el valor de x 
ggplot(data=estaturacm, 
       aes(x=heightcm)) + 
  geom_boxplot() + 
  labs(x='Estatura',y='Conteo') + 
  theme_bw() + 
  theme(axis.title = element_text(size=20), 
        axis.text = element_text(size=16))

# Calcula cada medida e identifica lo que significan en la grafica

#DESVIACI�N EST�NDAR
sd(estaturacm$heightcm,na.rm = FALSE)
# DE= 7.116923
#RANGO INTERCUANTIL
IQR(estaturacm$heightcm,na.rm = FALSE, type = 7)
# IQR = 7.62
#DESVIACI�N ABSOLUTA DE LA MEDIA
mad(estaturacm$heightcm,center = median(estaturacm$heightcm,na.rm = FALSE, low= FALSE, high=FALSE))
#la desviaci�n absoluta dela media es de 7.531608
# �Que significan estos n�meros?
# La desviaci�n estandar es la desviaci�n de los datos respecto a
#la media, si lo multiplicamos por 3 entonces y lo sumamos y lo restamos, es el
#rango en el que estar�an el 99 % de los valores
#el m�ximo y el m�nimo de los datos
range(estaturacm$heightcm)
#en este caso son 142.24 y 203.20

quantile(estaturacm$heightcm, probs = c(0.01,0.99))

# �Que significan estos n�meros?
# En este caso son  1% = 149.86 y 99% =  180.34

# Produce un histograma 
ggplot(data=estatura, 
       aes(x=buttockkneelength)) + 
  geom_histogram() + 
  labs(x='x', 
       y='Conteo') + 
  theme_bw() + 
  theme(axis.title = element_text(size=20), 
        axis.text = element_text(size=16))

# Saca la ???? y la ???? de la variable 'longitud del gl�teo
# a la rodilla' y graf�calas en tu histograma

# Distribuci�n binomial
ggplot(data=lobo, 
       aes(x=Sex)) + 
  geom_bar() + 
  labs(x='Sexo', 
       y='Conteo') + 
  theme_bw() + 
  theme(axis.title = element_text(size=20), 
        axis.text = element_text(size=16))
ggplot(data=estatura, 
       aes(x=buttockkneelength)) + 
  geom_histogram() + 
  labs(x='x', 
       y='Conteo') + 
  theme_bw() + 
  theme(axis.title = element_text(size=20), 
        axis.text = element_text(size=16))


#Elimina esta categor�a y vuelve a graficar


# Identifica la variable que tiene distribuci�n Poisson en la base de datos 
# de los lobos.
