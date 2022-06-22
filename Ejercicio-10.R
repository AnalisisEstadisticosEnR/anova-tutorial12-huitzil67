# 10.Pruebas Comunes
# Curso R
# 8 marzo 2022
# Roberto Márquez Huitzil
Mis_datos <- ToothGrowth
Mis_datos
installed.packages(ggpubr)
installed.packages(datarium)
library(datarium)
library(ggpubr)
shapiro.test(Mis_datos$len)
ggqqplot(Mis_datos$len)
res.Ftest <- var.test(len~supp, data = Mis_datos)
res.Ftest
# Datos en dos vectores 
peso_mujer <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5) 
peso_hombre <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4) 
# Crear base de datos
mi_data <- data.frame( 
  grupo = rep(c("Mujer", "Hombre"), each = 9), 
  peso = c(peso_mujer, peso_hombre) 
  )
library(dplyr) 
group_by(mi_data, grupo) %>% 
  summarise( 
    count = n(), 
    mean = mean(peso, na.rm = TRUE), 
    sd = sd(peso, na.rm = TRUE) 
    ) 
library("ggpubr") 
ggboxplot(mi_data, x = "grupo", y = "peso", 
          color = "grupo", palette = c("#00AFBB", "#E7B800"), 
          ylab = "Peso", xlab = "Grupos")
# Shapiro-Wilk shapiro.test(mi_data$peso) 
res.ftest <- var.test(peso ~ grupo, data = mi_data) 
res.ftest
res <- t.test(peso ~ grupo, data = mi_data, var.equal = TRUE) 
res
t.test(peso ~ grupo, data = mi_data, paired = TRUE, 
       alternative = "less")
#Muestras dependientes 
#Formato ancho 
library(tidyr) 
data("mice2", package = "datarium") 
head(mice2, 3) 
#tansforma a formato largo 
mice2.long <- mice2 %>% 
  gather(key = "group", value = "weight", before, after) 
head(mice2.long, 3)
mice2.long %>% group_by(group) %>% get_summary_stats(weight, type = "mean_sd")
# Y ahora sacar la prueba de t 
res <- t.test(weight ~ group, data = mice2.long, paired = TRUE) 
res
# Y visualizar los datos 
bxp <- ggpaired(mice2.long, x = "group", y = "weight", 
                order = c("before", "after"), 
                ylab = "Weight", xlab = "Groups")
# Comparar dos medianas
# Muestras emparejadas, Prueba Wilcoxon
dat2 <- data.frame(
  Beginning = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14), 
  End = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18) 
  ) 
dat2 
dat2 <- data.frame(Time = c(rep("Before", 12), 
                            rep("After", 12)), 
  Grade = c(dat2$Beginning, dat2$End)) 
dat2
# Reordenamos el tiempo 
dat2$Time <- factor(dat2$Time, 
 levels = c("Before", "After")) 

ggplot(dat2) + 
  aes(x = Time, y = Grade) + 
  geom_boxplot(fill = "#0c4c8a") + 
  theme_minimal()
test <- wilcox.test(dat2$Grade ~ dat2$Time, 
                    paired = TRUE)
test
#esta prueba no es la correcta, hay otra más correcta
#hay que hacerla con la prueba de muestras pareadas
dat <- data.frame( 
  Sex = as.factor(c(rep("Girl", 12), rep("Boy", 12))), 
  Grade = c( 
    19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18, 16, 
    5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14)) 
dat 
#Visualizar 
ggplot(dat) + 
  aes(x = Sex, y = Grade) + 
  geom_boxplot(fill = "#0c4c8a") + 
  theme_minimal()
# ¿Cómo puedes ver si hay normalidad de los datos?
# Dado que no la hay, podemos continuar con 
# esta prueba no paramétrica
test <- wilcox.test(dat$Grade ~ dat$Sex, 
                    alternative = "less" )
test
binom.test(9, 24, 1/6)
binom.test(11, 30, 0.5, alternative="less")
binom.test(46, 50, 0.8, alternative="greater")
# Pearson 
my_data <- mtcars 
head(my_data, 6) 
library("ggpubr") 
ggscatter(my_data, x = "mpg", y = "wt", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
# Shapiro-Wilk para mpg 
shapiro.test(my_data$mpg) # => p = 0.1229 
shapiro.test(my_data$wt) # => p = 0.09 
#visual 
# mpg 
ggqqplot(my_data$mpg, ylab = "MPG") 
# wt 
ggqqplot(my_data$wt, ylab = "WT")
res <- cor.test(my_data$wt, my_data$mpg, 
                method = "pearson") 
res
# Spearman
res2 <-cor.test(my_data$wt, my_data$mpg, method = "spearman") 
res2
#Prueba de independencia, Chi-cuadrada
dat <- iris 
dat$size <- ifelse(dat$Sepal.Length < median(dat$Sepal.Length), 
                   "small", "big") 
#creamos una tabla de contingencia 
table(dat$Species, dat$size)
ggplot(dat) + 
  aes(x = Species, fill = size) + 
  geom_bar()
test <- chisq.test(table(dat$Species, dat$size)) 
test
