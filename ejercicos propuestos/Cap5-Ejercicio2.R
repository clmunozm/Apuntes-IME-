# Instalacion de paquetes
library(tidyr)
library (ggpubr)


tiempo <- c(140.6, 133.3,142.4, 86.4, 129.9, 110.8, 133.2, 129.1, 142.5, 
            150.2, 141.6, 111.0, 127.2, 137.9, 131.9, 121.9)
print(tiempo)

datos <- data.frame("tiempo" = tiempo, stringsAsFactors = TRUE)
 
 # Gráfico Q-Q para la variable tiempo
g <- ggqqplot(datos, x = "tiempo", color = "red")

print(g)
 
#media
x <- mean(tiempo)
#desviación estandar
s <- sd(tiempo)

#Nivel de significación
alfa <- 0.05


#tamaño de la muestra
n <- 16
#grados de libertad
grados_libertad <- (n-1)
#error
error <- (s / sqrt(n))
#mu
m <- 120

t <- ((x - m) / (s/sqrt(n)))
cat("t =", t, "\n")
#valor p
p <- 2*(pt(t, df = grados_libertad , lower.tail = FALSE))
cat("p =", p, "\n")

#se construye el intervalo de confianza
t_critico <- qt(alfa , df = grados_libertad , lower.tail = FALSE)
superior <- x + (t_critico * error)
inferior <- x - (t_critico * error)

cat("Intervalo de confianza = [", inferior, ", ", superior , "]\n", sep = "")

prueba <- t.test(tiempo,
                 alternative = "two.sided",
                 mu = m,
                 conf.level = 1 - alfa)
print(prueba)

      