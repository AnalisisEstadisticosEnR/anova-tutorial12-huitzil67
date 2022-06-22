#Ejercicio 15
#Modelos Mixtos
# 5 de abril de 2022
#Roberto Márquez Huitzil
# carga paquetes 
library(tidyverse) 
library(ggplot2) 
# Carga la base de datos 
file.choose("D:\Doctorado\R-2022\Ejercicios\Clase-15\Modelos-lineales-mixtos-main\dragons.RData")
load("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-15\\Modelos-lineales-mixtos-main\\dragons.RData")
head(dragons)
hist(dragons$testScore)
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE) 
hist(dragons$bodyLength)
hist(dragons$bodyLength2)
#Ajusta los datos en un análisis
basic.lm <- lm(testScore ~ bodyLength2, data = dragons) 
summary(basic.lm)
library(tidyverse)(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) + 
                     geom_point() +
                     geom_smooth(method = "lm"))

#Checamos los supuestos
plot(basic.lm, which = 1) 
plot(basic.lm, which = 2)

boxplot(testScore ~ mountainRange, data = dragons) 
(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = 
mountainRange)) + geom_point(size = 2) + 
    theme_classic() +
    theme(legend.position = "none"))

#Ejecutamos múltiples análisis
(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
    geom_point() +
    facet_wrap(~ mountainRange) +   xlab("length") + 
    ylab("test score"))
# Modifiquemos el modelo actual
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons) 
summary(mountain.lm)

#Modelos de efectos mixtos
library(lme4)
#Ajustaremos el efecto aleatorio usando la sintaxis (1|variable):
mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)
# Checar los supuestos
plot(mixed.lmer) 
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer)) 
dragons <- within(dragons, sample <- factor(mountainRange:site))
dragons
mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), 
data = dragons) 
summary(mixed.WRONG)
mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                    data = dragons)
summary(mixed.lmer2)
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site) 
)+ facet_wrap(~mountainRange, nrow=2) + geom_point(alpha = 0.5) + 
    theme_classic() + geom_line(data = cbind(dragons, 
pred = predict(mixed.lmer2)), aes(y = pred), size = 1) + theme(legend.position = "none",
panel.spacing = unit(2, "lines")))
# Pendientes aleatorias
mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site),
data = dragons) 
summary(mixed.ranslope)
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site) 
)+
    facet_wrap(~mountainRange, nrow=2) + 
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y 
                                                                         = pred), size = 1) +        theme(legend.position = "none",
                                                                                                           panel.spacing = unit(2, "lines"))
)
# Trazado de predicciones del modelo
library(ggeffects)
# Extrae la predicción del modelo
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2")) 
(ggplot(pred.mm) +
    geom_line(aes(x = x, y = predicted)) +         # pendiente
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error)
geom_point(data = dragons,
           # datos (escalados)
           aes(x = bodyLength2, y = testScore, colour = mountainRange)) +
  labs(x = "Largo cuerpo (indexado)", y = "Prueba") + 
  theme_minimal())
ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "re" 
) %>%
  plot() +
  labs(x = "Largo cuerpo", y = "Prueba") +
#  ¿Qué sucede si quieres visualizar cómo varían las relaciones según los diferentes niveles de 
#efectos aleatorios? Puedes especificar type = "re" (para "efectos aleatorios") en la función 
#ggpredict() y agregar el nombre del efecto aleatorio al argumento de los términos.
theme_minimal()
library(sjPlot)
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE)) 
summary(mixed.ranslope)
#Tablas
library(stargazer)
stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001), 
          digit.separator = "")
#Valores -p
full.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), 
                  data = dragons, REML = FALSE)
reduced.lmer <- lmer(testScore ~ 1 + (1|mountainRange) + (1|sample), 
                     data = dragons, REML = FALSE)
anova(reduced.lmer, full.lmer)
        