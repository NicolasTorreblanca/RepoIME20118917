#EP04 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

# Importar paquetes.
library(dplyr)
library(ggpubr)

# Lectura de archivos
datos <- read.csv2("EP04 datos.csv")

# ---- Pregunta 1 ----

# El Comité Olímpico cree que el mejor tiempo medio de los atletas negros 
# después de ingresar al programa de entrenamiento es superior a 9,52 segundos. 
# ¿Soportan los datos esta afirmación?

#Hipótesis Nula: El mejor tiempo medio de los atletas negros después de
#ingresar al programa de entrenamiento es igual a 9.52 segundos.
# u = 9.52

#Hipótesis Alternativa: El mejor tiempo medio de los atletas negros después de
#ingresar al programa de entrenamiento es mayor a 9.52 segundos.
#u > 9.52

# Se seleccionan los tiempos de los atletas negros
atletlas_n <- datos %>% filter(Raza == "Negra")
tiempos_posterior <- atletlas_n[["Posterior"]]


# Establecer los datos conocidos. 
n <- length(tiempos_posterior)
grados_libertad <- n - 1
valor_nulo_1 <- 9.52

grafico_1 <- ggqqplot(data = data.frame(tiempos_posterior),
                    x = "tiempos_posterior",
                    color = "steelblue",
                    xlab = "Teorico",
                    ylab = "Muestra",
                    title = "Gráfico Q-Q muestra vs distr. normal")
print(grafico_1)

# Se fija un nivel de significación
alfa <- 0.025


# Calcular el estadístico de prueba
cat("\tPrueba t para una muestra\n\n")
media_1 <- mean(tiempos_posterior)
cat("Media =", media_1 , "M$\n")
desv_est_1 <- sd(tiempos_posterior)
error_1 <- desv_est_1 / sqrt(n)
t_1 <- (media_1 - valor_nulo_1) / error_1
cat("t =", t_1, "\n")

# Calcular el valor p.
p_1 <- pt(t_1, df = grados_libertad , lower.tail = TRUE)
cat("p =", p_1, "\n")

# Construir el intervalo de confianza
t_critico_1 <- qt(alfa , df = grados_libertad , lower.tail = FALSE)
superior_1 <- media_1 + t_critico_1 * error_1
cat("Intervalo de confianza = (-Inf , ", superior_1 , "]\n", sep = "")

# Aplicar la prueba t de Student con la función R.
prueba_t_1 <- t.test(tiempos_posterior,
                     alternative = "greater",
                     mu = valor_nulo_1,
                     conf.level = 1 - alfa)
print(prueba_t_1)

# ---- Respuesta 1 ----
#en base a los resultados obtenidos por la prueba t en una variable
#existen suficientes datos para negar la hipotesis nula h0,
#por ello se acepta la hipotesis alternativa h1. 
#ya que la media obtenida es de 9.784283 segundos.
#En tanto a la afirmación del Comité Olímpico se puede decir
#que es correcta por lo ya mencionado.


# ---- Pregunta 2 ----
# Sugieren los datos que la mejor marca de los atletas blancos 
# se reduce en 5,04 segundos tras el entrenamiento?

#Hipótesis Nula: Las medias de las diferencias en las marcas de los
#atletas blancos es igual a 5.04 segundos
#udiff = 5.04

#Hipótesis Alternativa: Las medias de las diferencias en la marcas
#de los atletas blancos es menor que 5.04 segundos
#udiff < 5.04

#Se seleccionan los tiempos de los atletas blancos
atletlas_b <- datos %>% filter(Raza == "Blanca")
tiempos_posterior_b <- atletlas_b[["Posterior"]]
tiempos_previo_b <- atletlas_b[["Previo"]]

diferencia <- tiempos_previo_b - tiempos_posterior_b

# Verificar si la distribución se acerca a la normal.
normalidad <- shapiro.test(diferencia)
print(normalidad)

valor_nulo_2 <- 5.04

# Fijar un nivel de significación. (se usa el mismo de antes 0.025)

# prueba_t_2 <- t.test(diferencia,
#                      alternative = "two.sided",
#                      mu = valor_nulo_2,
#                      conf.level = 1 - alfa)
# print(prueba_t_2)

prueba_t_2.1 <- t.test(x = tiempos_previo_b,
                       y = tiempos_posterior_b,
                       paired = TRUE,
                       alternative = "less",
                       mu = valor_nulo_2,
                       conf.level = 1 - alfa)
print(prueba_t_2.1)


#---- Respuesta 2 ----
#Tomando como base los datos obtenidos al aplicar la
# prueba t de Student podemos concluir que la hipótesis alternativa 
# coincide con los resultados entregados, ya que la diferencia
# de medias corresponde a 2.721894 el cual es un valor menor al 5.04
# que se menciona en la pregunta. Es por esto que la afirmación se
# rechaza ya que las marcas de los atletas blancos no se 
# redujeron en 5.04 segundos tras el entrenamiento


#---- Pregunta 3 ----
# ¿Es posible afirmar que, en promedio, los atletas blancos superan 
# a los orientales por menos de 1,16 segundos antes del entrenamiento?

#Hipótesis Nula: 

#Hipótesis Alternativa:































#Se seleccionan los atletas orientales y sus tiempos previos
atletlas_o <- datos %>% filter(Raza == "Oriental")
tiempos_previo_o <- atletlas_o[["Previo"]]

# Se verifica si las muestras se distribuyen de manera cercana 
# a la normal.
normalidad_b <- shapiro.test(atletlas_b)
print(normalidad_b)
normalidad_o <- shapiro.test(atletlas_o)
print(normalidad_o)

#El nivel de significacion sigue siendo el mismo que anets (0.025)

#Se aplica la prueba t para dos muestras independientes
prueba_t_3 <- t.test(x = tiempos_previo_b,
                     y = tiempos_previo_o,
                     paired = FALSE,
                     alternative = "greater",
                     mu = 0,
                     conf.level = 1 - alfa)
print(prueba_t_3)

#Calcular la diferencia entre las medias
media_b <- mean(tiempos_previo_b)
media_o <- mean(tiempos_previo_o)
diferencia_bo <- media_b - media_o
cat("Diferencia de las medias =", diferencia_bo , "[segundos]\n")



