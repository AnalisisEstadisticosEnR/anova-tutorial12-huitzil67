#Roberto Márquez Huitzil
#Curso R
#Ejercicio-visualizacion_Datos_7
setwd("D:\\Doctorado\\R-2022\\Ejercicios\\Tutorial-7")
#Vuelve al conjunto de datos LPI original y
# 1 - Elije DOS especies de los datos de LPI y muestra sus tendencias de población 
#a lo largo del tiempo, utilizando un diagrama de dispersión (pista: debe de ser 
#una gráfica de puntos) y un ajuste de modelo lineal.
library(tidyr) 
library(dplyr)
library(ggplot2)
library(readr) 
library(gridExtra)
library(gridExtra)

LPI <- read.csv("LPIdata_CC.csv")
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)
LPI2$year <- parse_number(LPI2$year) 
str(LPI2)
LPI2$abundance <- as.numeric(LPI2$abundance)
unique(LPI2$Common.Name)
# Con Rbasic:
polar_bear <- filter(LPI2, Common.Name == "Polar bear")
head(polar_bear)
polar_bear <- na.omit(polar_bear)
base_hist <- hist(polar_bear$abundance)

polar_bear_hist <- ggplot(polar_bear, aes(x = abundance)) + 
  geom_histogram()
polar_bear_hist
polar_bear_hist <- ggplot(polar_bear, aes(x = abundance)) + 
  geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") + 
  # cambiar el ancho de cada "caja" y colores
  geom_vline(aes(xintercept = mean(abundance)), 
             # agregar una línea para la abundancia media
             colour = "red", linetype = "dashed", size=1) + 
  # cambiar la apariencia de la línea      
  theme_bw() +
  # cambiar el tema (el fondo) 
  ylab("Conteo\n") + 
  # cambiar el texto del eje x. '\n' crea un espacio entre el texto y el eje 
  xlab("\nAbundancia de Oso polar") + 
  # cambiar el texto del eje y. '\n' crea un espacio entre el texto y el eje 
  theme(axis.text = element_text(size = 12), 
        # cambiar el tamaño de la fuente 
        axis.title.x = element_text(size = 14, face = "bold"), 
        # cambiar el tipo de letra del eje 
        panel.grid = element_blank(), 
        # quitar las líneas grises 
        plot.margin = unit(c(1,1,1,1), units = , "cm"))
# poner 1cm de margen alrededor de la grafica
polar_bear_hist
# Filtrar la base de datos con `filter()` 
polar_bearITCR <- filter(polar_bear, Country.list %in% c("Canada", "United States", "Russian Federation")) 
# usando Rbase 
plot(polar_bearITCR$year, polar_bearITCR$abundance, col = c("#1874CD", "#68228B")) 
# Usando ggplot2 
(polar_bear_scatter <- ggplot(polar_bearITCR, aes(x = year, y = abundance, colour = 
                                              Country.list)) + # si ponermos el color 
    #dentro de aes() asegura que los punto see colorean de acuerdo con los 
    #niveles de ese factor 
    geom_point())
library(gridExtra)

#ERROR
#Editemos la gráfica anterior: 
polar_bear_scatter <- ggplot(polar_bearITCR, aes (x = year, y = abundance, colour = 
                                              Country.list)) + 
  geom_point(size = 2) + 
  # cambiar el tamaño del punto 
  geom_smooth(method = "lm", aes(fill = Country.list)) + 
  # agregar un modelo lineal, colorear por país 
  theme_bw() + 
  scale_fill_manual(values = c("#EE7600", "#00868B")) + 
  # seleccionar colores para los "listones" 
  scale_colour_manual(values = c("#EE7600", "#00868B"), 
                      # agregar colores para las líneas y puntos 
                      labels = c("Canada", "Estados Unidos", "Federación Rusa")) + 
  # Agregar etiquetas en la leyenda 
  ylab("Abundancia Oso polar\n") + 
  xlab("\nAño") +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust 
                                   = 1), 
        # Año en un ángulo 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "plain"), 
        panel.grid = element_blank(), 
        # quitar las líneas del fondo 
        plot.margin = unit(c(1,1,1,1), units = , "cm"), 
        # margen 1cm 
        legend.text = element_text(size = 12, face = "italic"), 
        # la fuente de la leyenda 
        legend.title = element_blank(), 
        # leyenda sin titulo 
        legend.position = c(0.9, 0.9)) 
# posición de la leyenda - 0 es izquierda/abajo, 1 es arriba/derecha 
polar_bear_scatter

#ERROR
(polar_bear_boxplot <- ggplot(polar_bearITCR, aes(Country.list, abundance)) + 
  geom_boxplot(aes(fill = Country.list))+
    theme_bw() + 
    scale_fill_manual(values = c("#EE7600", "#00868B","red")) + 
    scale_colour_manual(values = c("#EE7600", "#00868B", "red")) + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nPais") + 
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.position = "none"))

# Calcular riqueza de especies (esto ya lo hemos hecho) 
richness <- LPI2 %>% filter (Country.list %in% c("Canada", "United States", "Russian Federation")) %>%
  group_by(Country.list) %>% 
  mutate(richness = (length(unique(Common.Name)))) 
# aquí creamos una nueva columna basada en cuantas especies únicas hay en cada país  
# Graficar 
(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", 
             fill = "#00868B") + 
    theme_bw() + 
    ylab("Riqueza de especies\n") + 
    xlab("Pais") + 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
(polar_bear_scatter_all <- ggplot(polar_bear, aes(x = year, y = abundance, colour 
                                             = Country.list)) + 
    geom_point(size = 2) + 
    geom_smooth(method = "lm", aes(fill = Country.list)) + 
    theme_bw() + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nAño") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
(polar_bear_facets <- ggplot(vulture, aes (x = year, y = abundance, colour = 
                                                  Country.list)) + 
    geom_point(size = 2) + 
    geom_smooth(method = "lm", aes(fill = Country.list)) + 
    facet_wrap(~ Country.list, scales = "free_y") + 
    # Esto crea los paneles 
    theme_bw() + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nAño") + 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
grid.arrange(polar_bear_hist, polar_bear_scatter, polar_bear_boxplot, ncol = 1) 

# Y se puede mejorar: 
(panel <- grid.arrange(
  polar_bear_hist + ggtitle("(a)") + ylab("Conteo") + xlab("Abundancia") + 
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
  polar_bear_boxplot + ggtitle("(b)") + ylab("Abundancia") + xlab("Pais") + 
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
  polar_bear_scatter + ggtitle("(c)") + ylab("Abundancia") + xlab("Año") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) + 
    theme(legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = c(0.85, 0.85)), 
  ncol = 1))

#La segundaa especie fue el Delfin nariz de botella
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)
LPI2$year <- parse_number(LPI2$year) 
str(LPI2)
LPI2$abundance <- as.numeric(LPI2$abundance)
unique(LPI2$Common.Name)
# Con Rbasic:
Bottlenose_dolphin <- filter(LPI2, Common.Name == "Bottlenose dolphin")
head(Bottlenose_dolphin)
str(Bottlenose_dolphin)
Bottlenose_dolphin$Country.list
Bottlenose_dolphin <- na.omit(Bottlenose_dolphin)
base_hist <- hist(Bottlenose_dolphin$abundance)

unique(LPI2$Common.Name)
# Con Rbasic:
bottlenose_dolphin <- filter(LPI2, Common.Name == "Bottlenose dolphin")
head(bottlenose_dolphin)
bottlenose_dolphin <- na.omit(bottlenose_dolphin)
base_hist <- hist(bottlenose_dolphin$abundance)
base_hist
bottlenose_dolphin_hist <- ggplot(polar_bear, aes(x = abundance)) + 
  geom_histogram()
bottlenose_dolphin_hist
bottlenose_dolphin_hist <- ggplot(bottlenose_dolphin, aes(x = abundance)) + 
  geom_histogram(binwidth = 250, colour = "#8B5A00", fill = "#CD8500") + 
  # cambiar el ancho de cada "caja" y colores
  geom_vline(aes(xintercept = mean(abundance)), 
             # agregar una línea para la abundancia media
             colour = "red", linetype = "dashed", size=1) + 
  # cambiar la apariencia de la línea      
  theme_bw() +
  # cambiar el tema (el fondo) 
  ylab("Conteo\n") + 
  # cambiar el texto del eje x. '\n' crea un espacio entre el texto y el eje 
  xlab("\nAbundancia de Oso polar") + 
  # cambiar el texto del eje y. '\n' crea un espacio entre el texto y el eje 
  theme(axis.text = element_text(size = 12), 
        # cambiar el tamaño de la fuente 
        axis.title.x = element_text(size = 14, face = "bold"), 
        # cambiar el tipo de letra del eje 
        panel.grid = element_blank(), 
        # quitar las líneas grises 
        plot.margin = unit(c(1,1,1,1), units = , "cm"))

library(gridExtra)
# poner 1cm de margen alrededor de la grafica
bottlenose_dolphin_hist
# Filtrar la base de datos con `filter()` 
bottlenose_dolphinITCR <- filter(bottlenose_dolphin, Country.list %in% c("Greece", "United Kingdom", "Bahamas", "Italy", "Brazil")) 
# usando Rbase 
plot(bottlenose_dolphinITCR$year, bottlenose_dolphinITCR$abundance, col = c("#1874CD", "#68228B")) 
# Usando ggplot2 
(bottlenose_dolphin_scatter <- ggplot(bottlenose_dolphinITCR, aes(x = year, y = abundance, colour = 
                                                    Country.list)) + # si ponermos el color 
    #dentro de aes() asegura que los punto see colorean de acuerdo con los 
    #niveles de ese factor 
    geom_point())

bottlenose_dolphin$Country.list
#ERROR
#Editemos la gráfica anterior: 
bottlenose_dolphin_scatter <- ggplot(bottlenose_dolphinITCR, aes (x = year, y = abundance, colour = 
                                                    Country.list)) + 
  geom_point(size = 2) + 
  # cambiar el tamaño del punto 
  geom_smooth(method = "lm", aes(fill = Country.list)) + 
  # agregar un modelo lineal, colorear por país 
  theme_bw() + 
  scale_fill_manual(values = c("#EE7600", "#00868B")) + 
  # seleccionar colores para los "listones" 
  scale_colour_manual(values = c("#EE7600", "#00868B"), 
                      # agregar colores para las líneas y puntos 
                      labels = c("Greece", "United Kingdom", "Bahamas", "Italy", "Brazil")) + 
  # Agregar etiquetas en la leyenda 
  ylab("Abundancia Delfin nariz de botella\n") + 
  xlab("\nAño") +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust 
                                   = 1), 
        # Año en un ángulo 
        axis.text.y = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "plain"), 
        panel.grid = element_blank(), 
        # quitar las líneas del fondo 
        plot.margin = unit(c(1,1,1,1), units = , "cm"), 
        # margen 1cm 
        legend.text = element_text(size = 12, face = "italic"), 
        # la fuente de la leyenda 
        legend.title = element_blank(), 
        # leyenda sin titulo 
        legend.position = c(0.9, 0.9)) 
# posición de la leyenda - 0 es izquierda/abajo, 1 es arriba/derecha 



#ERROR
bottlenose_dolphin_scatter

#ERROR
(polar_bear_boxplot <- ggplot(polar_bearITCR, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list))+
    theme_bw() + 
    scale_fill_manual(values = c("#EE7600", "#00868B")) + 
    scale_colour_manual(values = c("#EE7600", "#00868B")) + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nPais") + 
    theme(axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"),
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.position = "none"))

# Calcular riqueza de especies (esto ya lo hemos hecho) 
richness <- LPI2 %>% filter (Country.list %in% c("Canada", "United States", "Russian Federation")) %>%
  group_by(Country.list) %>% 
  mutate(richness = (length(unique(Common.Name)))) 
# aquí creamos una nueva columna basada en cuantas especies únicas hay en cada país  
# Graficar 
(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) + 
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", 
             fill = "#00868B") + 
    theme_bw() + 
    ylab("Riqueza de especies\n") + 
    xlab("Pais") + 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm")))
(polar_bear_scatter_all <- ggplot(polar_bear, aes (x = year, y = abundance, colour 
                                                   = Country.list)) + 
    geom_point(size = 2) + 
    geom_smooth(method = "lm", aes(fill = Country.list)) + 
    theme_bw() + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nAño") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
(polar_bear_facets <- ggplot(vulture, aes (x = year, y = abundance, colour = 
                                             Country.list)) + 
    geom_point(size = 2) + 
    geom_smooth(method = "lm", aes(fill = Country.list)) + 
    facet_wrap(~ Country.list, scales = "free_y") + 
    # Esto crea los paneles 
    theme_bw() + 
    ylab("Abundancia Oso polar\n") + 
    xlab("\nAño") + 
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12), 
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
grid.arrange(polar_bear_hist, polar_bear_scatter, polar_bear_boxplot, ncol = 1) 

# Y se puede mejorar: 
(panel <- grid.arrange(
  polar_bear_hist + ggtitle("(a)") + ylab("Conteo") + xlab("Abundancia") + 
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
  polar_bear_boxplot + ggtitle("(b)") + ylab("Abundancia") + xlab("Pais") + 
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")), 
  polar_bear_scatter + ggtitle("(c)") + ylab("Abundancia") + xlab("Año") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) + 
    theme(legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = c(0.85, 0.85)), 
  ncol = 1))

#Bottlenose dolphin


unique(LPI2$Common.Name)
Gray_whale <- filter(LPI2, Common.Name == "Gray whale")
View(Gray_whale)
str(Orca)
# Con Rbasic: 
American_crow <- filter(LPI2, Common.Name == "American crow")
head(American_crow)

# 2 - Usando las mismas dos especies, filtra los datos para incluir solo registros 
#de CINCO países de tu elección y haz un diagrama de cajas para comparar cómo varía 
#la abundancia de esas dos especies entre los cinco países.