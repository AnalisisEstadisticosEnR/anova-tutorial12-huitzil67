#Ejercicio 17
#Series de tiempo
# 28 de abril de 2022
#Roberto Márquez Huitzil
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)
monthly_milk <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-17\\Series-de-tiempo-main\\monthly_milk.csv") 
head(monthly_milk)
str(monthly_milk)
# Producción de leche por vaca por mes
daily_milk <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-17\\Series-de-tiempo-main\\daily_milk.csv")
head(daily_milk)
#Producción de leche por vaca por ordeñada
#1. Formatear datos de series temporales
head(monthly_milk) 
class(monthly_milk)
class(monthly_milk$month)
monthly_milk$month_date <- as.Date(monthly_milk$month, format = "%Y-%m-%d") 
class(monthly_milk$month_date)
monthly_milk$month_date
#Prueba algunas combinaciones diferentes de códigos de fecha de la tabla 
#anterior, utilizando el siguiente código como ejemplo (ojo, no se 
#asignarán los resultados a un objeto, sino que simplemente los imprimirá 
#en la consola):
format(monthly_milk$month_date, format = "%Y-%B-%u") 
format(monthly_milk$month_date, format = "%Y-%m-%u") 
class(format(monthly_milk$month_date, format = "%Y-%B-%u"))
#Aquí corrijo formato para ponerlo en número y no en letra
format(monthly_milk$month_date, format = "%Y-%m-%u") 
class(format(monthly_milk$month_date, format = "%Y-%m-%u"))
#Fechas y horas
head(daily_milk)
class(daily_milk$date_time)
daily_milk$date_time_posix <- as.POSIXct(daily_milk$date_time, format = "%Y-%m-%d %H:%M:%S")
daily_milk$date_time
daily_milk$date_time_posix
class(daily_milk$date_time_posix)
# Corrección de datos de fecha mal formateados
monthly_milk$bad_date <- format(monthly_milk$month_date, format = "%d/%b/%Y-%u")
monthly_milk$bad_date <- format(monthly_milk$month_date, format = "%d/%m/%Y-%u")
head(monthly_milk$bad_date)  
class(monthly_milk$bad_date)
#2. Visualización de datos de series temporales
(time_plot <- ggplot(monthly_milk, aes(x = month_date, 
y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
    theme_classic())
#Juega con date_labels, reemplazando "%Y" con algunas otras marcas de fecha de la 
#tabla anterior (por ejemplo, %m-%Y). date_breaks también se puede personalizar para 
#cambiar la frecuencia de la etiqueta del eje. Otras opciones incluyen mes, semana y día (por 
#Juego con variables:
(time_plot <- ggplot(monthly_milk, aes(x = month_date, 
                                       y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_date(date_labels = "%b%y", date_breaks = "96 weeks") + 
    theme_classic())

#El trazado de datos de fecha y hora se realiza de manera similar utilizando 
#scale_x_datetime():
(time_plot_2 <- ggplot(daily_milk, aes(x = date_time_posix,y = milk_prod_per_cow_kg)) +
    geom_line() + scale_x_datetime(date_labels = "%p-%d", 
date_breaks = "36 hour") + theme_classic())

(time_plot_2 <- ggplot(daily_milk, aes(x = date_time_posix,y = milk_prod_per_cow_kg)) +
    geom_line() + scale_x_datetime(date_labels = "%p-%d", 
 date_breaks = "96 hour") + theme_classic())
#3. Análisis estadístico de datos de series temporales
#Descomposición
(decomp_1 <- ggplot(monthly_milk, aes(x = month_date, 
y = milk_prod_per_cow_kg)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())
# Una regresión de loess ajusta una curva suave entre dos variables:
(decomp_2 <- ggplot(monthly_milk, aes(x = month_date, 
y = milk_prod_per_cow_kg)) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, span = 0.6) +
    theme_classic())
# Extrae mes y año en columnas separadas 
monthly_milk$year <- format(monthly_milk$month_date, format = "%Y") 
monthly_milk$year
monthly_milk$month_num <- format(monthly_milk$month_date, format = "%m")
monthly_milk$month_num
# Crea una paleta de colores usando `colortools` 
year_pal <- sequential(color = "darkturquoise", percentage = 5, what = "value")
# Haz la grafica
(seasonal <- ggplot(monthly_milk, aes(x = month_num, y = milk_prod_per_cow_kg, 
group = year)) +
    geom_line(aes(colour = year)) + 
    theme_classic() +
    scale_color_manual(values = year_pal))
# Transforma a clase `ts` 
monthly_milk_ts <- ts(monthly_milk$milk_prod, start = 1962, end = 1975, freq 
                      = 12)

# Decomponer usando `stl()`
monthly_milk_stl <- stl(monthly_milk_ts, s.window = "period") 
plot(monthly_milk_stl)
monthplot(monthly_milk_ts)  # variación de producción de leche por cada mes 
seasonplot(monthly_milk_ts)
#Pronóstico
monthly_milk_model <- window(x = monthly_milk_ts, start = c(1962), end = c(1970))
monthly_milk_model
monthly_milk_test <- window(x = monthly_milk_ts, start = c(1970))
monthly_milk_test
# Crea objetos de modelos de cada tipo de modelo ets 
milk_ets_auto <- ets(monthly_milk_model)
milk_ets_auto
milk_ets_mmm <- ets(monthly_milk_model, model = "MMM")
milk_ets_mmm
milk_ets_zzz<- ets(monthly_milk_model, model = "ZZZ")
milk_ets_zzz
milk_ets_mmm_damped <- ets(monthly_milk_model, model = "MMM", damped = TRUE)
milk_ets_mmm_damped
# Crea objecto pronóstico de cada objeto de modelo
milk_ets_fc <- forecast(milk_ets_auto, h = 60) # `h = 60` el pronostico va a 
#durar 60 dias (un mes)
milk_ets_fc

milk_ets_mmm_fc <- forecast(milk_ets_mmm, h = 60)
milk_ets_mmm_fc
milk_ets_zzz_fc <- forecast(milk_ets_zzz, h = 60)
milk_ets_zzz_fc
milk_ets_mmm_damped_fc <- forecast(milk_ets_mmm_damped, h = 60)
milk_ets_mmm_damped_fc
# Convierte el pronóstico en base de datos
milk_ets_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_fc)), 
as.data.frame(milk_ets_fc))  
milk_ets_fc_df
names(milk_ets_fc_df) <- gsub(" ", "_", names(milk_ets_fc_df))  # Quita espacios en blanco de los nombres de las columnas 
names(milk_ets_fc_df)

milk_ets_fc_df$Month
#en el siguiente renglón me empieza a marcar en NA los nombres de los días
milk_ets_fc_df$Date <- as.Date(paste("01-", milk_ets_fc_df$Month, sep = ""), 
format = "%d-%b %Y") # antepón el día del mes 
milk_ets_fc_df$Date

milk_ets_fc_df$Model <- rep("ets") # Agrega columna del tipo de modelo
milk_ets_fc_df$Model
milk_ets_mmm_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_fc)), as.data.frame(milk_ets_mmm_fc))
milk_ets_mmm_fc_df
names(milk_ets_mmm_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_fc_df))
names(milk_ets_mmm_fc_df)

#Lo mismo pasa en el riguiente renglón, no hace la operación
milk_ets_mmm_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_fc_df$Month,sep= ""), format = "%d-%b %Y")
milk_ets_mmm_fc_df$Date
milk_ets_mmm_fc_df$Model <- rep("ets_mmm")
milk_ets_mmm_fc_df$Model
milk_ets_zzz_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_zzz_fc)), as.data.frame(milk_ets_zzz_fc))
milk_ets_zzz_fc_df
names(milk_ets_zzz_fc_df) <- gsub(" ", "_", names(milk_ets_zzz_fc_df))
milk_ets_zzz_fc_df$Date <- as.Date(paste("01-", milk_ets_zzz_fc_df$Month, sep 
                                         = ""), format = "%d-%b %Y")
milk_ets_zzz_fc_df$Date
milk_ets_zzz_fc_df$Model <- rep("ets_zzz")
milk_ets_zzz_fc_df$Model
milk_ets_mmm_damped_fc_df <- cbind("Month" = rownames(as.data.frame(milk_ets_mmm_damped_fc)), as.data.frame(milk_ets_mmm_damped_fc))
names(milk_ets_mmm_damped_fc_df) <- gsub(" ", "_", names(milk_ets_mmm_damped_fc_df))
milk_ets_mmm_damped_fc_df$Date <- as.Date(paste("01-", milk_ets_mmm_damped_fc_df$Month, 
                                                sep = ""), format = "%d-%b %Y")
milk_ets_mmm_damped_fc_df$Model <- rep("ets_mmm_damped") 
# Combina en una base de datos 
forecast_all <- rbind(milk_ets_fc_df, milk_ets_mmm_fc_df, milk_ets_zzz_fc_df, 
                      milk_ets_mmm_damped_fc_df)
head(forecast_all)
str(forecast_all)
#Ahora si, una grafica
(forecast_plot <- ggplot() + geom_line(data = monthly_milk, aes(x = month_date, 
y = milk_prod_per_cow_kg)) +  #la información original
    geom_line(data = forecast_all, aes(x = Date, y = Point_Forecast, colour = 
                                         Model)) + # el pronóstico
    theme_classic())
head(forecast_all)
class(forecast_all$Model)

#También se puede comparar numéricamente la precisión de diferentes modelos con los 
#datos que excluimos del modelo (monthly_milk_test) usando accuracy():
  accuracy(milk_ets_fc, monthly_milk_test)
accuracy(milk_ets_mmm_fc, monthly_milk_test)
accuracy(milk_ets_zzz_fc, monthly_milk_test)
accuracy(milk_ets_mmm_damped_fc, monthly_milk_test)
# Extraer valores de un pronóstico
milk_ets_fc_df %>%
  filter(Month == "Jan 1975") %>% 
  select(Month, Point_Forecast)
milk_ets_zzz_fc_df %>%
  filter(Month == "Jan 1975") %>%
  select(Month, Point_Forecast)


#4. Desafío de codificación
#Ahora usa lo que ha aprendido para hacer algunos modelos pronóstico y trazar algunas 
#gráficas para investigar patrones temporales para nuestros datos sobre concentraciones de 
#CO2 en Mauna Loa, Hawái. Ve si puedes predecir la concentración de CO2 para junio de 
#2050. Puedes encontrar los datos en co2_loa.csv en la carpeta del repositorio de GitHub 
#para esta clase.

