# EP05 IME
# Esteban Arenas
# Nicolas Torreblanca
# Rodrigo Escobar

# Importar paquetes.
library(dplyr)
library(ggpubr)
library(ggplot2)

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

sigma <- 1
alfa <- 0.05
n <- 100
media_nula <- 10

#Graficar si la hipotesis nula fuera verdadera
x <- seq(6, 14, 0.01)
y <- dnorm(x, mean = media_nula , sd = sigma)
g <- ggplot(data = data.frame(x, y), aes(x))

g <- g + stat_function( fun = dnorm ,
                        args = list(mean = media_nula , sd = sigma),
                        colour = "red", size = 1)
g <- g + ylab("")
g <- g + scale_y_continuous(breaks = NULL)
g <- g + scale_x_continuous(name = "Diferencia en tiempos de ejecución [ms]", 
                               breaks = seq(6, 14, 1))
g <- g + theme_pubr()


z_critico <- qnorm(alfa/2, mean = media_nula, sd =sigma, lower.tail = FALSE)
q_critico_inferior <- 9.7
q_critico_superior <- 10.3

xddf <- data.frame(x=x,y=y)

area_menor <- subset(xddf ,x < q_critico_inferior)

area_mayor <- subset(xddf ,x > q_critico_superior)

g <- g + geom_area(data=subset(xddf ,x < q_critico_inferior),aes(y = y),
                     colour = "red", fill = "red", alpha = 0.5)

g <- g + geom_area(data=subset(xddf ,x > q_critico_superior) ,aes(y = y),
                   colour = "red", fill = "red", alpha = 0.5)


# media_efecto = -4
# g <- g + stat_function (fun = dnorm ,
#                           args = list (mean = media_efecto , sd = SE ) ,
#                           colour = " blue " , size = 1)

# x1 <- seq ( -6 * SE , 4 * SE , 0.01)
# 
# y1 <- dnorm (x , mean = media_efecto , sd = SE )
# 
# xddf2 <- data.frame(x1=x1,y1 = y1)
#   
# g <- g + geom_area (data=subset(xddf2 ,
#                     x < q_critico_inferior ) ,
#                     aes ( x = x1 , y = y1 ) ,
#                     colour = " blue " ,
#                     fill = " blue " ,
#                     alpha = 0.5)
# 
# g <- g + geom_area (data=subset(xddf2 , x > q_critico_superior) ,
#                     aes ( x = x1 , y = y1 ) ,
#                     colour = " blue " ,
#                     fill = " blue " ,
#                     alpha = 0.5)
# 
# poder_teorico <-pnorm(q_critico_inferior ,mean = media_efecto ,sd = SE,lower.tail = TRUE)+ 
#                 pnorm(q_critico_superior ,mean = media_efecto,sd = SE ,lower.tail =FALSE)
# 
print(g)
# 
# beta <- 1 - poder_teorico

#2. Si el verdadero volumen medio de los bidones fuera de 10,2 litros,
#¿cuál sería la probabilidad de que el ingeniero, que obviamente 
#no conoce este dato, cometa un error de tipo II?






#3. Como no se conoce el verdadero volumen medio, genere un gráfico 
#del poder estadístico con las condiciones anteriores, pero suponiendo
#que el verdadero volumen medio podría variar de 9,5 a 10,5 litros.

#4. Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían
#revisarse para conseguir un poder estadístico de 0,75 y un nivel de
#significación de 0,05?

#En esta pregunta se busca cual debe ser el tamanho de una muestra para que

# El poder estadistico sea de 0,75

# Fijando los parametros conocidos de Tamanho del efecto, 
# nivel de significacion y poder


#Calculando el delta mediante d de cohen
delta_4 = (10 - 10) / 1

#Para los demas

poder_4 = 0.75
nivel_significacion_4 = 0.05


resultado_4 <- power.t.test(n = NULL,
                          delta = 0.1,
                          sig.level = nivel_significacion_4,
                          power = poder_4,
                          type = "paired" ,
                          alternative = "two.sided" )

print (resultado_4)

cat ( " \n Se obtiene que el n es igual a \n " )

#5. ¿Y si el ingeniero fuese muy exigente y quisiera reducir la 
#probabilidad de cometer un error de tipo I a un 1% solamente?


# Se solicita encontrar el N para el cual se tiene un nivel de significacion
# de 0.01

# Se mantienen las mismas variables solo que el nivel de significacion
# Disminuye de 0.05 a 0.01

poder_5 = poder_4
nivel_significacion_5 = 0.01


resultado_4 <- power.t.test(n = NULL,
                            delta = 0.1,
                            sig.level = nivel_significacion_5,
                            power = poder_5,
                            type = "paired" ,
                            alternative = "two.sided" )

print (resultado_4)