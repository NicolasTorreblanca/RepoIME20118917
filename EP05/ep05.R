# EP05 IME
# Esteban Arenas
# Nicolas Torreblanca
# Rodrigo Escobar

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Se sabe que una máquina que envasa detergentes industriales 
# llena bidones con un volumen de producto que sigue una distribución 
# normal con desviación estándar de 1 litro. 
# Usando una muestra aleatoria de 100 botellas, el ingeniero a cargo 
# de la planta requiere determinar si la máquina está llenando los
# bidones con una media de 10 litros

# Generar una muestra de 100 datos que siguen una distribución normal
# con media 10 y desv estandar de 1
dist_bidones <- rnorm(100, mean = 10, sd = 1)

#hist(dist_bidones, main = "Muestra de 100",xlab = "", prob = TRUE)

#---- Preguntas ----

#1. Si el ingeniero piensa rechazar la hipótesis nula cuando la muestra 
#presente una media menor a 9,7 litros o mayor a 10,3 litros, 
#¿cuál es la probabilidad de que cometa un error de tipo I?

sigma <- 12
alfa <- 0.05
n <- 36
media_nula <- 0

# Calcular error estandar
SE <- sigma / sqrt(n)

#Graficar si la hipotesis nula fuera verdadera
x <- seq(-6 * SE , 4 * SE, 0.01)
y <- dnorm(x, mean = media_nula , sd = SE)
g <- ggplot(data = data.frame(x, y), aes(x))

g <- g + stat_function( fun = dnorm ,
                        args = list(mean = media_nula , sd = SE),
                        colour = "red", size = 1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Diferencia en tiempos de ejecución [ms]", 
                               breaks = seq(-6, 4, 2))
g <- g + theme_pubr()


z_critico <- qnorm(alfa/2, mean = media_nula, sd = SE, lower.tail = FALSE)
q_critico_inferior <- -2
q_critico_superior <- 2

g <- g + geom_area(data = subset(df , x < q_critico_inferior), aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)

g <- g + geom_area(data = subset(df , x > q_critico_superior), aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)

print(g)




#2. Si el verdadero volumen medio de los bidones fuera de 10,2 litros,
#¿cuál sería la probabilidad de que el ingeniero, que obviamente 
#no conoce este dato, cometa un error de tipo II?






#3. Como no se conoce el verdadero volumen medio, genere un gráfico 
#del poder estadístico con las condiciones anteriores, pero suponiendo
#que el verdadero volumen medio podría variar de 9,5 a 10,5 litros.

#4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían
#revisarse para conseguir un poder estadístico de 0,75 y un nivel de
#significación de 0,05?

#5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la 
#probabilidad de cometer un error de tipo I a un 1% solamente?
