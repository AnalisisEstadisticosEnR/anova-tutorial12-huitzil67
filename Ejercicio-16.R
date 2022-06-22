#Ejercicio 16
#Meta Análisis con inferencia bayesiana
# 26 de abril de 2022
#Roberto Márquez Huitzil
library("MCMCglmm") 
library("dplyr")
migracion <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-16\\Meta-Analisis_Inferencia-Bayesiana-main\\migration_metadata.csv", header = T)
# Échale un ojo a la base de datos y a su estructura
head(migracion)
str(migracion)
class(migracion)
migracion %>%
filter(Predictor == "year") -> migraciontiempo

plot(migraciontiempo$Slope, I(1/migraciontiempo$SE))
# Podemos hacer un "zoom"
plot(migraciontiempo$Slope, I(1/migraciontiempo$SE), xlim = c(-2,2), ylim = c 
     (0, 60))

randomtest <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, data 
                       = migraciontiempo)
summary(randomtest)

#Revisando la significancia
hist(mcmc(randomtest$VCV)[,"Study"])
hist(mcmc(randomtest$VCV)[,"Location"])
hist(mcmc(randomtest$VCV)[,"Species"])

#Convergencia del modelo
plot(randomtest$Sol)

plot(randomtest$VCV)

#Priorizaciones
a <- 1000
prior1 <- list(R = list(V = diag(1), nu = 0.002),#nu es la desviación std del fijo y del aleatorio
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V #aloha.mu le pido 
                                  #que la media sea 0, no quiero que la H0 cambie el número de días
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a)))
randomprior <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, 
                        data = migraciontiempo, prior = prior1, nitt = 60000)
summary(randomprior)

plot(randomprior$VCV)

prior2 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), fix = 1)))
randomerror2 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study + idh(SE):units, 
                         data = migraciontiempo, prior = prior2, nitt = 60000)
plot(randomerror2$VCV)

xsim <- simulate(randomerror2) # corre 100 modelos nuevos, alrededor de la mi 
#sma estructura de varianza/covarianza pero con datos simulados
plot(migraciontiempo$Slope, I(1/migraciontiempo$SE)) 
points(xsim, I(1/migraciontiempo$SE), col = "red")

prior3 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a)))
randomerror3 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study + idh(SE):units, 
                         data = migraciontiempo, prior = prior3, nitt = 60000
)

xsim <- simulate(randomerror3)
plot(migraciontiempo$Slope, I(1/migraciontiempo$SE)) 
points(xsim, I(1/migraciontiempo$SE), col = "red")

#¡Ahora es tu turno!
#  Filtra los datos por filas que tienen la temperatura como predictor 
#Traza los datos usando un gráfico de embudo
#Ejecuta un modelo básico de efectos aleatorios. 
#Guarda el modo posterior.
#Traza VCV (aleatorio) y Sol (fijo) y verificar la auto correlación 
#Aumenta el número de iteraciones y guarda, verifica tus priors 
#Haz las comprobaciones de modelo
#¡Interpreta tu modelo!
migracion <- read.csv("D:\\Doctorado\\R-2022\\Ejercicios\\Clase-16\\Meta-Analisis_Inferencia-Bayesiana-main\\migration_metadata.csv", header = T)
# Échale un ojo a la base de datos y a su estructura
head(migracion)
str(migracion)
class(migracion)
migracion %>%
filter(Predictor == "temperature") -> migraciontemperatura

plot(migraciontemperatura$Slope, I(1/migraciontemperatura$SE))
# Podemos hacer un "zoom"
plot(migraciontemperatura$Slope, I(1/migraciontemperatura$SE), xlim = c(-2,2), ylim = c 
     (0, 60))

randomtest <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, data 
                       = migraciontemperatura)
summary(randomtest)

#Revisando la significancia
hist(mcmc(randomtest$VCV)[,"Study"])
hist(mcmc(randomtest$VCV)[,"Location"])
hist(mcmc(randomtest$VCV)[,"Species"])

#Convergencia del modelo
plot(randomtest$Sol)

plot(randomtest$VCV)

#Priorizaciones
a <- 1000
prior1 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a)))
randomprior <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study, 
                        data = migraciontemperatura, prior = prior1, nitt = 60000)
summary(randomprior)

plot(randomprior$VCV)

prior2 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), fix = 1)))
randomerror2 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study + idh(SE):units, 
                         data = migraciontemperatura, prior = prior2, nitt = 60000)
plot(randomerror2$VCV)

xsim <- simulate(randomerror2) # corre 100 modelos nuevos, alrededor de la mi 
#sma estructura de varianza/covarianza pero con datos simulados
plot(migraciontemperatura$Slope, I(1/migraciontemperatura$SE)) 
points(xsim, I(1/migraciontemperatura$SE), col = "red")

prior3 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a),
                        G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V 
                                  = diag(1)*a)))
randomerror3 <- MCMCglmm(Slope ~ 1, random = ~Species + Location + Study + idh(SE):units, 
                         data = migraciontemperatura, prior = prior3, nitt = 60000
)

xsim <- simulate(randomerror3)
plot(migraciontemperatura$Slope, I(1/migraciontemperatura$SE)) 
points(xsim, I(1/migraciontemperatura$SE), col = "red")
