# carga paq
library(agridat) 

# Carga la base de datos 
setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-13")
apples <- agridat::archbold.apple 
head(apples) 
summary(apples)
theme.clean <- function(){ 
  theme_bw()+ 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 
1), 
axis.text.y = element_text(size = 12), 
axis.title.x = element_text(size = 14, face = "plain"), 
axis.title.y = element_text(size = 14, face = "plain"), 
panel.grid.major.x = element_blank(), 
panel.grid.minor.x = element_blank(), 
panel.grid.minor.y = element_blank(), 
panel.grid.major.y = element_blank(), 
plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"), 
plot.title = element_text(size = 20, vjust = 1, hjust = 0.5), 
legend.text = element_text(size = 12, face = "italic"), 
legend.position = "right") 
}
apples$spacing2 <- as.factor(apples$spacing)
library(ggplot2) 
(apples.p <- ggplot(apples, aes(spacing2, yield)) + 
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") + 
    theme.clean() + 
    theme(axis.text.x = element_text(size = 12, angle = 0)) + 
    labs(x = "Spacing (m)", y = "Yield (kg)"))
apples.m <- aov(yield ~ spacing2, data = apples) 
summary(apples.m)

sheep <- agridat::ilri.sheep 
library(dplyr)
sheep <- filter(sheep, ewegen == "R") 
# Vamos a considerer a corderos únicamente que vienen de madres "R". 
head(sheep) 
sheep.m1 <- lm(weanwt ~ weanage, data = sheep) 
# vamos a usar lm summary(sheep.m1)
summary(sheep.m1)


sheep.m2 <- lm(weanwt ~ weanage*sex, data = sheep) 
summary(sheep.m2)
(sheep.p <- ggplot(sheep, aes(x = weanage, y = weanwt)) + 
    geom_point(aes(colour = sex)) + 
    labs(x = "Edad al destete (días)", y = "Wean weight (kg)") + 
    stat_smooth(method = "lm", aes(fill = sex, colour = sex)) + 
    scale_colour_manual(values = c("#FFC125", "#36648B")) + 
    scale_fill_manual(values = c("#FFC125", "#36648B")) + 
    theme.clean() )

lm(apples.m)
# Voy a revisar los supuestos del modelo
# Revisar los residuos 
resid(apples.m)
apples.resid <- resid(apples.m) # Extrae los residuos del modelo 
apples.resid

shapiro.test(apples.resid) # Shapiro-Wilk 
# no podemos decir que la distribución se aleja de una normal
# Revisa las varianzas . Para ver si son homogeneas
bartlett.test(apples$yield, apples$spacing2) 
bartlett.test(yield ~ spacing2, data = apples) # mismos resultados
## //el resultado da que si son homogeneas
#también se puede ver si son homogeneas por "graficación" jajaja
plot(apples.m)

#ahora para corderitos con el modelo de sheep
sheep.m1
lm(sheep.m1)
# Voy a revisar los supuestos del modelo
# Revisar los residuos 
resid(sheep.m1)
sheep_m1.resid <- resid(sheep.m1) # Extrae los residuos del modelo 
sheep_m1.resid

shapiro.test(sheep_m1.resid) # Shapiro-Wilk 
# no podemos decir que la distribución se aleja de una normal
# Revisa las varianzas . Para ver si son homogeneas
bartlett.test(sheep$weanage, sheep$sex) #para sexo lo hace por ser categórica
#también se puede ver si son homogeneas por "graficación" jajaja
#para ovejas sólo sale la prueba por gráficas
plot(sheep.m1)

setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Tutorial-13\\clasemodeloslineales22marzo")
shag <- read.csv("shagLPI.csv", header = TRUE) 
# Transforma año 
# Haz un histograma
shag.m <- glm(pop ~ year, family = poisson, data = shag) 
summary(shag.m)
# los datos dicen que la población vva disminuyendo por año en dos
shag.m

(shag.p <- ggplot(shag, aes(x = year, y = pop)) + 
    geom_point(colour = "#483D8B") + 
    geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 
                  0.6) + 
scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) 
  + 
    theme.clean() + 
    labs(x = " ", y = "European Shag abundance"))

# Un modelo con distribución binomial
Weevil_damage <- read.csv("Weevil_damage.csv") 
# Haz el bloque un factor 
# Corre el modelo 
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage) 
summary(weevil.m)

# Otro ejemplo desmenuzado
library(tidyverse) 
library(lme4) 
# modelos jerárquicos 
library(sjPlot) 
# visualizar las salidas de los modelos
install.packages("ggeffects")
install.packages("glmmTMB")
install.packages("sjPlot")
library(glmmTMB) 
toolik_plants <- read.csv("toolik_plants.csv") 
head(toolik_plants) 
str(toolik_plants)

# Un truco nuevo: 
toolik_plants <- 
  toolik_plants %>% 
  mutate(across(c(Site, Block, Plot), as.factor)) 
str(toolik_plants)

unique(toolik_plants$Site) 
length(unique(toolik_plants$Site))

# Agrupemos por sitio 
toolik_plants %>% group_by(Site) %>% 
  summarise(block.n = length(unique(Block)))

toolik_plants %>% group_by(Block) %>% 
  summarise(plot.n = length(unique(Plot)))

unique(toolik_plants$Year)

length(unique(toolik_plants$Species))

unique(toolik_plants$Species)

toolik_plants <- toolik_plants %>% 
  filter(!Species %in% c("Woody cover", "Tube", 
                         "Hole", "Vole trail", 
                         "removed", "vole turds", 
                         "Mushrooms", "Water", 
                         "Caribou poop", "Rocks", 
                         "mushroom", "caribou poop", 
                         "animal litter", "vole poop", 
                         "Vole poop", "Unk?"))

length(unique(toolik_plants$Species))

# Calcular riqueza 
toolik_plants <- toolik_plants %>% 
  group_by(Year, Site, Block, Plot) %>% 
  mutate(Richness = length(unique(Species))) %>% 
  ungroup()

(hist <- ggplot(toolik_plants, aes(x = Richness)) + 
    geom_histogram() + 
    theme_classic())

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) 
  + geom_histogram() 
  + theme_classic())
# Modelo sin efectos aleatorios: 
plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants) 
summary(plant_m)

plot(plant_m)

# Modelos jerárquicos usando lme4
library(ggeffects)

plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants
) 
summary(plant_m_plot)
plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants) 
summary(plant_m_plot2)
# ¿Han cambiado las estimaciones de los tamaños del efecto? 
plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants) 
summary(plant_m_plot3)

plot(plant_m_plot3)

# Establezcamos un tema limpio 
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank(), 
                  panel.grid.minor.y = element_blank(), 
                  panel.grid.major.y = element_blank(), 
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"))) 
# Visualicemos los efectos aleatorios 
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))

# Año 
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))

plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year), 
                     data = toolik_plants) 
summary(plant_m_temp)

# 
(temp.fe.effects <- plot_model(plant_m_temp, show.values = TRUE))

(temp.re.effects <- plot_model(plant_m_temp, type = "re", show.values = TRUE))


## TARDA MUCHO Y AL FINAL ME MARCA UN ERROR. todo marca error
## después de esto
plant_m_rs <- lmer(Richness ~ Mean.Temp + 
(Mean.Temp|Site/Block/Plot) + (1|Year), 
data = toolik_plants) 
summary(plant_m_rs)

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site) + (1|Year), 
                   data = toolik_plants) 
summary(plant_m_rs)

(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))

ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot() 
ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), type = "re") %>% plot() + 
  theme(legend.position = "bottom")

predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp")) 

(pred_plot1 <- ggplot(predictions, aes(x, predicted)) + 
    geom_line() + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) + 
    scale_y_continuous(limits = c(0, 35)) + 
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

# Predicciones para cada grupo (cada parcela es un factor aleatorio) 
# re = efecto aleatorio 
predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Site"), 
type = "re") 

(pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, 
colour = group)) + 
    stat_smooth(method = "lm", se = FALSE) +
scale_y_continuous(limits = c(0, 35)) + 
    theme(legend.position = "bottom") + 
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))
(pred_plot3 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, 
colour = group)) + 
    stat_smooth(method = "lm", se = FALSE) + 
    theme(legend.position = "bottom") + 
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

