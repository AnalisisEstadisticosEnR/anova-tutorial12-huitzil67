#8. Visualización de datos de dos tierras mágicas: Hogsmeade y Narnia 
#Curso R
#1° marzo 2022
#Roberto Márquez Huitzil
library(dplyr) # manipulación 
library(ggplot2) # visualización 
setwd("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\R-2022\\Ejercicios\\Titorial-8") # lee la base de datos 
magic_veg <- read.csv("magic_veg.csv")
# Explora la base de datos con el comando str()
str(magic_veg)
species_counts <- magic_veg %>% 
  group_by(land, plot) %>% 
  summarise(Species_number = length(unique(species)))
hist <- ggplot(species_counts, aes(x = plot)) + 
  geom_histogram() 
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number)) + 
  geom_histogram(stat = "identity")
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) +
  geom_histogram(stat = "identity") 
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") 
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
  scale_y_continuous(limits = c(0, 50)) 
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
  scale_y_continuous(limits = c(0, 50)) + 
  labs(title = "Species richness by plot", 
       x = "\n Plot number", y = "Number of species \n") + 
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12, face = "italic"), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))
hist
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
  scale_y_continuous(limits = c(0, 50)) + 
  labs(title = "Species richness by plot", 
       x = "\n Plot number", y = "Number of species \n") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, 
                                  hjust = 0.5, face = "bold"))
hist

hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
  scale_y_continuous(limits = c(0, 50)) + 
  scale_fill_manual(values = c("rosybrown1", "#deebf7"), 
name = "Tierra de Magia") + labs(title = "Riqueza de especies por parcela"
, 
x = "\n Número de parcela", y = "Número de especies \n") +
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
  legend.title = element_text(face = "bold"), 
  legend.position = "bottom", 
  legend.box.background = element_rect(color = "grey", size = 0.3))
hist
magic_veg
str(magic_veg)
magic_veg$land <- as.factor(magic_veg$land)
levels(magic_veg$land) #marca nulo para todos los fatores
hist <- ggplot(species_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_histogram(stat = "identity", position = "dodge") + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6)) + 
  scale_y_continuous(limits = c(0, 50)) + 
  scale_fill_manual(values = c("rosybrown1", "#deebf7"), 
                    labels = c("HOGSMEADE", "NARNIA"), 
                    name = "Tierra de Magia") + 
  labs(title = "Riqueza de especies por parcela", 
       x = "\n Número de parcela", y = "Número de especies \n") + 
  theme_bw() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
        legend.title = element_text(face = "bold"), 
        legend.position = "bottom", 
        legend.box.background = element_rect(color = "grey", size = 0.3))
hist
yearly_counts <- magic_veg %>% 
  group_by(land, plot, year) %>% 
  summarise(Species_number = length(unique(species))) %>% 
  ungroup() %>% 
  mutate(plot = as.factor(plot)) 

boxplot <- ggplot(yearly_counts, aes(plot, Species_number, fill = land)) + 
  geom_boxplot()
boxplot
boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = 1:6) + 
  scale_fill_manual(values = c("rosybrown1", "#deebf7"), 
                    breaks = c("Hogsmeade","Narnia"), 
                    name="Tierra de Magia", 
                    labels=c("Hogsmeade", "Narnia")) + 
  labs(title = "Riqueza de especies por parcela", 
       x = "\n Número de parcela", y = "Número de especies \n") + 
  theme_bw() + 
  theme() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
        legend.position = "bottom",)
boxplot
summary <- species_counts %>% group_by(land) %>% summarise(mean = mean(Species_number),
sd = sd(Species_number))
summary
magic_veg

#Haz una gráfica de puntos
(magic_veg_scatter_all <- ggplot(summary, aes (x = land, y = mean, colour 
                                             = land)) + 
    geom_point(size = 2) + 
    geom_smooth(method = "lm", aes(fill = land)) + 
    theme_bw() + 
    ylab("abundancia\n") + 
    xlab("\nParcela") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
#para que me ponga 
(magic_veg_scatter_all <- ggplot(summary, aes (x = land, y = mean, colour 
                                               = land)) + 
    geom_point(size = 2) +
    geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd)) +
    geom_smooth(method = "lm", aes(fill = land)) + 
    theme_bw() + 
    ylab("abundancia\n") + 
    xlab("\nParcela") +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"), 
          panel.grid = element_blank(), 
          plot.margin = unit(c(1,1,1,1), units = , "cm"), 
          legend.text = element_text(size = 12, face = "italic"), 
          legend.title = element_blank(), 
          legend.position = "right"))
yearly_counts$land <- factor(yearly_counts$land, 
                             levels = c("Narnia", "Hogsmeade"), 
                             labels = c("Narnia", "Hogsmeade"))
yearly_counts
boxplot <- ggplot(yearly_counts, aes(x = plot, y = Species_number, fill = land)) + 
  geom_boxplot() + 
  scale_x_discrete(breaks = 1:6) + 
  scale_fill_manual(values = c("#deebf7", "rosybrown1"), 
                    breaks = c("Narnia","Hogsmeade"),
                    name = "Tierra de Magia", 
                    labels = c("Narnia", "Hogsmeade")) + 
  labs(title = "Riqueza de especies por parcela", 
       x = "\n Número de parcela", y = "Número de especies \n") + 
  theme_bw() + 
  theme() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), 
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
        plot.margin = unit(c(0.5,0.5,0.5,0.5), units = , "cm"), 
        legend.position = "bottom", 
        legend.box.background = element_rect(color = "grey", size = 0.3))
boxplot

#Datos cualitativos
install.packages("tidyverse")
library(tidyverse) 
library(RColorBrewer) 
install.packages("tidytext")
library(tidytext) 
install.packages("R.utils")
library(R.utils) 
install.packages("wordcloud")
library(wordcloud) 
install.packages("viridis")
library(viridis)
sust_data <- read_csv("sust_behaviour.csv")
sust_lookup <- read_csv("sust_lookup.csv")
unique(sust_data$sustainability_daily_think)
sust_data$sustainability_daily_think <- factor(sust_data$sustainability_daily_think, 
levels = c("Never", "Rarely", "Sometimes", "Often", "All the time"), 
ordered = TRUE)
#ACTIVIDAD: Busca otras columnas que creas que contienen datos 
#de factores ordenados como éstos, luego cámbialos a factores
#ordenados de la misma manera que lo hicimos anteriormente.
sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action))
sust_data$sustainability_daily_think
sust_think_summ_wide <- sust_data %>% 
  group_by(gender, sustainability_daily_think) %>% 
  tally() %>% 
  mutate(perc = n / sum(n) * 100) %>% 
  dplyr::select(-n) %>% 
  group_by(gender) %>% 
  spread(sustainability_daily_think, perc)
sust_think_summ_wide
#Obtener respuestas de aveces a ambos lados de una linea
sust_think_summ_hi_lo <- sust_think_summ_wide %>% 
  mutate(midlow = Sometimes / 2, 
         midhigh = Sometimes / 2) %>% 
  dplyr::select(gender, Never, Rarely, midlow, midhigh, Often, `All the time`) %>% 
  gather(key = response, value = perc, 2:7) %>% 
  `colnames<-`(c("gender", "response", "perc"))
sust_think_summ_hi_lo
#Dos conjuntos de barras ala izquyierda y derecha del cero
sust_think_summ_hi <- sust_think_summ_hi_lo %>% 
  filter(response %in% c("All the time", "Often", "midhigh")) %>% 
  mutate(response = factor(response, levels = c("All the time", "Often", "midhigh"))) 
## Error in filter(., response %in% c("All the time", "Often", "midhigh")): 
#object 'sust_think_summ_hi_lo' not found
sust_think_summ_lo <- sust_think_summ_hi_lo %>% 
  filter(response %in% c("midlow", "Rarely", "Never")) %>% 
  mutate(response = factor(response, levels = c("Never", "Rarely", "midlow")))
# Uasamos RColorBrewer para guardar el código de colores como un vector 
legend_pal <- brewer.pal(name = "RdBu", n = 5) 
# Duplic el valor medio, recuerda que "Sometimes" son dos grupos,
#"midhigh" y"midlow" 
legend_pal <- insert(legend_pal, ats = 3, legend_pal[3]) 
# remplazamos colores 
legend_pal <- gsub("#F7F7F7", "#9C9C9C", legend_pal) 
# Asignamos nombresa cada color 
names(legend_pal) <- c("All the time", "Often", "midhigh", "midlow", "Rarely", 
                       "Never" )
ggplot() + 
  geom_bar(data = sust_think_summ_hi, aes(x = gender, y=perc, fill = response), 
           stat="identity") + geom_bar(data = sust_think_summ_lo, 
aes(x = gender, y=-perc, fill = response), 
stat="identity") + geom_hline(yintercept = 0, color =c("black")) + 
  scale_fill_manual(values = legend_pal, 
    breaks = c("All the time", "Often", "midhigh", "Rarely", "Never"), 
    labels = c("All the time", "Often", "Sometimes", "Rarely", "Never")) + 
  coord_flip() + 
  labs(x = "Género", y = "Porcentaje de encuestados (%)") + 
  ggtitle(sust_lookup$survey_question[sust_lookup$column_title == 
"sustainability_daily_think"]) + 
  theme_classic()
#Gráfico de barras apiladas básico
sust_data$energy_action_n <- nchar(as.character(sust_data$energy_action)) 
barchart <- ggplot(sust_data, aes(x =energy_action_n, fill = age)) + 
  geom_bar() + 
  scale_fill_viridis_d() + 
  scale_x_continuous(breaks = seq(1:8)) + 
  theme_classic()
barchart
#Gráfico de burbujas
sust_bubble <- sust_data %>% 
  group_by(age, sustainability_daily_think) %>% 
  tally() 
bubbleplot <- ggplot(sust_bubble, 
                     aes(x = age, y = sustainability_daily_think)) + 
  geom_point(aes(size = n)) + theme_classic()
bubbleplot
#Gráfico de burbujas de la edad frente a pensamientos sostenibles
head(sust_data$energy_action_comment, 20)

sust_comm_gather <- sust_data %>% 
  dplyr::select(id, gender, energy_action_comment, 
                food_action_comment, water_action_comment, 
                waste_action_comment, other_action_comment) %>% 
  gather(action, comment, -id, -gender) %>% 
  mutate(comment = as.character(comment)) 
sust_comm_tidy <- sust_comm_gather %>% 
  unnest_tokens(output = comment_word, 
                input = comment) %>% 
  filter(!(is.na(comment_word)), 
         is.na(as.numeric(comment_word)), 
         !(comment_word %in% stop_words$word)) %>% 
  group_by(gender, comment_word) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  filter(n > 10)
sust_comm_tidy
male_female_pal <- c("#0389F0", "#E30031") 
names(male_female_pal) <- c("Male", "Female")
occurrence <- ggplot(sust_comm_tidy, aes(x = comment_word, y = n, fill = gender)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  scale_fill_manual(values = male_female_pal) + 
  theme_classic()
occurrence
#Gráfico de barras de palabras más comunes
tidy_energy_often_comment <- sust_data %>% 
  mutate(energy_action_comment = as.character(energy_action_comment)) %>% 
  unnest_tokens(output = energy_action_comment_word, 
                input = energy_action_comment) %>% 
  filter(!(is.na(energy_action_comment_word)), 
         is.na(as.numeric(energy_action_comment_word)), 
         !(energy_action_comment_word %in% stop_words$word)) %>% 
  group_by(gender, energy_action_comment_word) %>% 
  summarise(n = n()) %>% 
  ungroup()
tidy_energy_often_comment

tidy_energy_often_comment_summ <- tidy_energy_often_comment %>% 
  filter(n > 10) %>% 
  mutate(energy_action_comment_word = reorder(energy_action_comment_word, n )
         ) 
(most_common_plot <- ggplot(tidy_energy_often_comment_summ, aes(x = energy_action_comment_word, 
y = n)) + geom_col() + xlab(NULL) + # this means we don't want an axis title 
    coord_flip() + 
    theme_classic())
tidy_energy_often_comment %>% with(wordcloud(words = energy_action_comment_word, 
freq = n, max.words = 100))
 