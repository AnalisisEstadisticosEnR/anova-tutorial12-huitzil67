diet <- read.csv('~/Documents/TODO/RESPALDAR/2020_HACER/Doctorado/UAEM/2020/NuevoTramite/Logistica/Clases/Clases-ANOVA-R/stcp-Rdataset-Diet.csv',row.names=1)
diet <- read.csv("~/Descargas/stcp-Rdataset-Diet",row.names=1)
file.choose()
diet <- read.csv("C:\\Users\\Roberto.LAPTOP-JH51I47J\\Documents\\TODO\\RESPALDAR\\2020_HACER\\Doctorado\\UAEM\\2020\\NuevoTramite\\Logistica\\Clases\\Clases-ANOVA-R\\stcp-Rdataset-Diet.csv",row.names=1)
head(diet)
class(diet$pre.weight)
diet$pre.weight<- as.numeric(diet$pre.weight)
class(diet$weight6weeks)
diet$weight.loss <- diet$pre.weight - diet$weight6weeks
library(dplyr)
2+2
# R version
