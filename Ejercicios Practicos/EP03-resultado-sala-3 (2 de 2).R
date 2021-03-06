# Instalacion de paquetes
library(tidyr)
library (ggplot2)
library (ggpubr)
library(gtools)

poblaci�n <- read.csv(file.choose(), encoding = "UTF-8")
set.seed(111) 
n.repeticiones <- 30

ensayo <- function(x) 
  ifelse(sample(poblaci�n[["sexo"]], 1) == "Mujer", 1, 0)

treinta.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Se calcula en numero de exitos de los ensayos
exitos <- sum(treinta.repeticiones, 0)

#Se calcula la probabilidad de exito
probabilidad <- exitos/n.repeticiones

#Distribuci�n Binomial

#Se crea la funcion de la distribuci�n binomial
dbinomial<- function(x, n, p) {
  binomial<-(factorial(n)/(factorial(x)*factorial(n - x)))*(p^x)*(1-p)^(n - x)
  return(binomial)
  }

#Se aplica la funci�n
b <- dbinomial(1:n.repeticiones, n.repeticiones, probabilidad)

#Se grafica la distribuci�n binomial
plot(b, type = "h", lwd = 2,
     main = "Funci�n de probabilidad binomial",
     ylab = "", xlab = "N�mero de repeticiones")

plot(stepfun(1:(n.repeticiones - 1), dbinomial(1:(n.repeticiones), n.repeticiones, probabilidad)),xlab="k",ylab="F(k)",main="Funci�n de distribuci�n Binomial")
    
#Distribuci�n geometrica

#se crea la funci�n de distribuci�n geometrica
dgeometrica<- function(x, p){
  geometrica<- ((1-p)^(x-1))*p
  return(geometrica)
  }

#Se grafica la distribuci�n
g <- dgeometrica(1:n.repeticiones, probabilidad)
plot(g, type = "h", lwd = 2,
     main = "Distribuci�n geom�trica",
     ylab = "", xlab = "N�mero de repeticiones")

#Distribuci�n binomial negativa

#Se crea la funci�n de distribuci�n binomial negativa
dbinomialnegativa <- function(r, n, p) {
  binomialnegativa<- ((factorial(n-1)) / (factorial(r-1) * factorial((n-1) - (r-1))))*(p^r)*((1-p)^(n - r))
  return(binomialnegativa)
  }

#Se grafica
bn <- dbinomialnegativa(1:n.repeticiones, n.repeticiones, probabilidad)
plot(bn, type = "h", lwd = 2,
     main = "Funci�n de probabilidad binomial negativa",
     ylab = "", xlab = "N�mero de repeticiones")


