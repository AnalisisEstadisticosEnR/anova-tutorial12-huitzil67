# carga paquetes 
library(tidyverse) 
library(ggplot2) 
# Carga la base de datos 
ranas_data <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-12\\frogs_messy_data.csv")
head(ranas_data)
### Formatea tu base de datos 
# Re�ne los tiempos de eclosi�n por cada temperatura 
frogs_tidy_data <- gather(ranas_data, Temperature, Hatching_time, c(2:4)) %>%    
 
# Hatching times (value) to be gathered by Temperature (key)
mutate(Temperature = parse_number(Temperature)) %>%                 

# To get rid of the non-numerical part
select("Hatching_time", "Temperature") %>%                          

# Keeping only the columns we need for the analysis
na.omit() 

# guardarlo a factor con as.factor
frogs_tidy_data$Temperature <- as.factor(frogs_tidy_data$Temperature)
# lo hab�a convertido a factor, por eso lo tengo que cambiar
# primero lo cambio a character y luego a num�rico
frogs_tidy_data$Hatching_time <- as.numeric(as.character(frogs_tidy_data$Hatching_time))
head(frogs_tidy_data)
#hacer un histograma con ggplot
ggplot(frogs_tidy_data, aes(x = Hatching_time)) +  
  geom_histogram(stat = "count") +
  geom_vline(aes(xintercept = mean(Hatching_time)),            
             colour = "red", linetype = "dashed", size = 1) +
  labs(x = "\n Tiempo eclosion (dias)", y = "Frecuencia \n") +   
  guides(fill = guide_legend(title = "Temperatura (�C)"))
str(frogs_tidy_data)
class(frogs_tidy_data$Temperature)
class(frogs_tidy_data$Hatching_time)
#hacer gr�fica de caja opci�n con r basic
boxplot(Hatching_time  ~ Temperature, data = frogs_tidy_data, 
        xlab = "Temperatura", ylab = "Tiempo eclosi�n", 
        main = "Temperatura de eclosi�n")

#opci�n con ggplot
ggplot(frogs_tidy_data, aes(x = Temperature, y = Hatching_time)) +
  geom_boxplot() + 
  labs(x = "\nTemperatura (�C)", y = "Tiempo eclosion (dias)")

# Correr la anova
ranas_anova <- aov(Hatching_time ~ Temperature, data = frogs_tidy_data)
ranas_anova
summary(ranas_anova)
# A. Histograma de residuos y gr�fico Q-Q normal:
par(mfrow = c(1,2))
plot(ranas_anova)
par(mfrow = c(2,2))
plot(ranas_anova)
par(mfrow = c(1,1))
plot(ranas_anova)
# esto pone ls dos graficas en la misma ventana 
hist(ranas_anova$residuals) 
# histograma de residuoss 
plot(ranas_anova, which = 2) # hace la grafica Q-Q
# le estoy diciendo que s�lo la segunda gr�fica)
plot(ranas_anova)
# checando homoscedasticidad (Homogeneidad de varianzas) 
plot(ranas_anova, which = 1) # residuos VS datos

resumen_stats <- frogs_tidy_data %>% 
  group_by(Temperature) %>% 
  summarise(n = n(),
            average_hatch = mean(Hatching_time), 
# calcular la media de tiempo de eclosi�n 
SD = sd(Hatching_time))%>% 
  # calcular la desviaci�n est�ndar 
  mutate(SE = SD / sqrt(n)) # Calcular el error est�ndar
resumen_stats


resumen_stats$average_hatch <- as.numeric(as.character(resumen_stats$average_hatch))
head(resumen_stats)

# # Haz un gr�fico de barras con la base de datos anterior
#opci�n con ggplot
ggplot(resumen_stats, aes(x = Temperature, y = average_hatch)) +
  barplot(resumen_stats$average_hatch) + 
  labs(x = "\nTemperatura (�C)", y = "Tiempo promedio de eclosi�n (dias)")


#hacer un histograma con ggplot
##ERROR, me salen todas las barras del mismo tama�o

ggplot(resumen_stats, aes(x = Temperature)) +  
  geom_histogram(stat = "count") +
  geom_vline(aes(xintercept = mean(average_hatch)),            
             colour = "red", linetype = "dashed", size = 1) +
  labs(x = "\n Temperatura (�C)", y = "Eclosi�n \n") +   
  guides(fill = guide_legend(title = "Promedio de eclosi�n por temperatura (n)"))
str(resumen_stats)
class(frogs_tidy_data$Temperature)
class(frogs_tidy_data$Hatching_time)

str(resumen_stats)
class(resumen_stats$Temperature)
class(resumen_stats$average_hatch)
