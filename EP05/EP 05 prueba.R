libray(ggplot2)

# Construir la distribución Estandar

x <- seq(7, 13, length = 200)

set.seed(15)

distribucion_botellas <- (rnorm(100, mean=10, sd=1))

hist(distribucion_botellas , main = "Muestra de 100",xlab = "", prob = TRUE)

#Pregunta1

#Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra presente 
#una media menor a 9,7 litros o
# mayor a 10,3 litros, ¿cuál es la probabilidad de que cometa un error de tipo I?