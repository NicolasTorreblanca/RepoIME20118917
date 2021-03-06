# EP11 IME
# Nicolas Torreblanca
# Esteban Arenas
# Rodrigo Escobar

# ---- Librerias ----
library(dplyr)
library(ggpubr)
library(simpleboot)
library(boot)

# ---- Definici�n de Funciones ----
# Funci�n para obtener una permutaci�n.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutaci�n.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Funci�n para calcular la diferencia de un estad�stico de inter�s entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la funci�n que calcula el estad�stico de inter�s.
# Valor:
# - diferencia de un estad�stico para dos muestras.
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Funci�n para calcular el valor p.
# Argumentos:
# - distribucion: distribuci�n nula del estad�stico de inter�s.
# - valor_observado: valor del estad�stico de inter�s para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hip�tesis alternativa. "two.sided" para
#   hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales.
# Valor:
# - el valorp calculado.
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

# Funci�n para graficar una distribuci�n.
# Argumentos:
# - distribucion: distribuci�n nula del estad�stico de inter�s.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estad�stico de inter�s",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una �nica figura con todos los gr�ficos de dispersi�n.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Funci�n para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores num�ricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: funci�n del estad�stico E para el que se calcula la diferencia.
# - alternative: tipo de hip�tesis alternativa. "two.sided" para
#   hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales.
# - plot: si es TRUE, construye el gr�fico de la distribuci�n generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hip�tesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribuci�n.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribuci�n.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

# ---- Pregunta 1
# Propongan una pregunta de investigaci�n original, que 
# involucre la comparaci�n de las medias de dos grupos 
# independientes (m�s abajo se dan unos ejemplos).
# Fijando una semilla propia, seleccionen una muestra aleatoria 
# de hogares (250 < n < 500) y respondan la pregunta propuesta 
# utilizando una simulaci�n Monte Carlo.

# ---- Enunciado Creado 1 ----
# Se dice que el promedio de ingresos per c�pita de las mujeres de 
# la Regi�n Metropolitana es igual al de las mujeres de 
# la Regi�n del Biob�o

# ---- Definici�n de Hip�tesis ----
# Hip�tesis Nula: En promedio, los ingresos per c�pita de las mujeres 
# de la Regi�n Metropolitana de Santiago y de las mujeres de la 
# Regi�n del Biob�o son iguales.
# uM - uB = 0

# Hip�tesis Alternativa: En promedio, los ingresos per c�pita de las mujeres 
# de la Regi�n Metropolitana de Santiago y de las mujeres de la 
# Regi�n del Biob�o son diferentes.
# uM - uB != 0

# Se define un nivel de significaci�n de 0.05
# alfa <- 0.05

# Lectura de datos
datos <- read.csv2("EP11 Datos.csv")

# Se define una semilla
set.seed(666)

# Se define el tama�o de la muestra
tamanoMuestra1 <- 300

# Se obtiene una muestra de los datos
muestra <- datos[sample(nrow(datos), tamanoMuestra1),]

# Se calcula el ingreso per capita de cada vivienda y se guardan 
# los resultados en una nueva variable ingresoPerCapita
muestra <- muestra %>% mutate(ytotcorh, numper, 
                          ingresoPerCapita = ytotcorh/numper)

# Se obtienen los datos de mujeres de la Regi�n del Biob�o
biobio <- muestra %>% filter(region == "Regi�n del Biob�o", 
                             sexo == "Mujer")

# Se obtienen los datos de mujeres de la Regi�n Metropolitana de Santiago
metropolitana <- muestra %>% filter(region == "Regi�n Metropolitana de Santiago", 
                                    sexo == "Mujer")

# Se seleccionan solo las columnas de id e ingresoPerCapita de cada tabla
mujerBiobioIngreso <- biobio %>% select(id.vivienda, ingresoPerCapita)
mujerMetropolitanaIngreso <- metropolitana %>% select(id.vivienda, ingresoPerCapita)

# Se pasan los datos de las columnas a vectores para 
# poder operar sobre ellos
vecBiobio <- mujerBiobioIngreso$ingresoPerCapita
vecMetropolitana <- mujerMetropolitanaIngreso$ingresoPerCapita

# Se establece la cantidad de repeticiones.
R = 5999

# Se realiza la prueba de permutaciones para la media.
contrastar_hipotesis_permutaciones(vecBiobio, vecMetropolitana, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")

# ---- Respuesta 1 ----
# Luego de realizar la prueba de permutaciones para comparar una variable 
# continua en dos muestras independientes se obtuvo un valor p de 0.03416667 
# el cual corresponde a un valor menor que el nivel de significaci�n definido
# de 0.05, es por esto que se logra rechazar la hip�tesis nula y se acepta 
# la hip�tesis alternativa, esto quiere decir que, en promedio, los ingresos per c�pita de las mujeres 
# de la Regi�n Metropolitana de Santiago y de las mujeres de la 
# Regi�n del Biob�o son diferentes.


# ---- Pregunta 2 ----
# Propongan una pregunta de investigaci�n original, que 
# involucre la comparaci�n de las medias de m�s de dos grupos 
# independientes (m�s abajo se dan unos ejemplos). Fijando una 
# semilla distinta a la anterior, seleccionen una muestra 
# aleatoria de hogares (400 < n < 600) y respondan la pregunta
# propuesta utilizando bootstrapping. Solo por ejercicio acad�mico,
# aplique un an�lisis post-hoc con bootstrapping aunque este no 
# sea necesario.

# ---- Enunciado Creado 2 ----
# Se dice que el promedio de ingresos per c�pita entre las regiones de Atacama, Tarapac�
# y Antofagasta son iguales.

# ---- Definici�n de Hip�tesis ----
# Hip�tesis Nula: En promedio, el ingreso per c�pita en la Regi�n de Atacama, 
# de Tarapac� y de Antofagasta son iguales. Por lo que las diferencias de
# las medias son igual a 0.

# Hip�tesis Alternativa: En promedio, el ingreso per c�pita en la Regi�n de Atacama, 
# de Tarapac� y de Antofagasta en al menos una de ellas es distinto. Por lo 
# que las diferencias de las medias son distintas a 0.

# Se define una semilla para este caso
set.seed(11)

# Se define un tama�o de muestra para este caso
tamanoMuestra2 <- 520

# Se toma una muestra de los datos originales
muestra2 <- datos[sample(nrow(datos), tamanoMuestra2),]

# Se calcula el ingreso per capita de cada vivienda y se guardan 
# los resultados en una nueva variable ingresoPerCapita
muestra2 <- muestra2 %>% mutate(ytotcorh, numper, 
                              ingresoPerCapita = ytotcorh/numper)

# Se obtienen los datos por regi�n
atacama <- muestra %>% filter(region == "Regi�n de Atacama")
tarapaca <- muestra %>% filter(region == "Regi�n de Tarapac�")
antofagasta <- muestra %>% filter(region == "Regi�n de Antofagasta")

# Se pasan los datos de las columnas a vectores para 
# poder operar sobre ellos
vecAtacama <- atacama$ingresoPerCapita
vecTarapaca <- tarapaca$ingresoPerCapita
vecAntofagasta <- antofagasta$ingresoPerCapita

# Se obtiene la cantidad de datos de cada muestra
n_atacama <- length(vecAtacama)
n_tarapaca <- length(vecTarapaca)
n_antofagasta <- length(vecAntofagasta)

# Comprobar normalidad de las muestras.
print(shapiro.test(vecAtacama))
print(shapiro.test(vecTarapaca))
print(shapiro.test(vecAntofagasta))

# Se definen 3 combinaciones posibles de Bootstrapping de 2 muestras independientes
# [1] Atacama - Tarapac�
# [2] Atacama - Antofagasta
# [3] Antofagasta - Tarapac�

# Se genera un dataframe por combinaci�n
# [1]
region1 <- c(rep("Regi�n de Atacama", n_atacama), rep("Regi�n de Tarapac�", n_tarapaca))
ingreso1 <- c(vecAtacama, vecTarapaca)
datos1 <- data.frame(region1, ingreso1)
# [2]
region2 <- c(rep("Regi�n de Atacama", n_atacama), rep("Regi�n de Antofagasta", n_antofagasta))
ingreso2 <- c(vecAtacama, vecAntofagasta)
datos2 <- data.frame(region2, ingreso2)
# [3]
region3 <- c(rep("Regi�n de Antofagasta", n_antofagasta), rep("Regi�n de Tarapac�", n_tarapaca))
ingreso3 <- c(vecAntofagasta, vecTarapaca)
datos3 <- data.frame(region3, ingreso3)

# Se calculan las diferencias observada entre las medias muestrales
# para las 3 combinaciones
media_Atacama <- mean(vecAtacama)
media_Tarapaca <- mean(vecTarapaca)
media_Antofagasta <- mean(vecAntofagasta)

diferencia_1 <- media_Atacama - media_Tarapaca
diferencia_2 <- media_Atacama - media_Antofagasta
diferencia_3 <- media_Antofagasta - media_Tarapaca

# Se establece el nivel de significaci�n.
# alfa <- 0.05

# Se define un valor nulo
valor_nulo <- 0

# Se crea la distribuci�n bootstrap para las 3 combinaciones
B <- 9999

distribucion_bootstrap1 <- two.boot(vecAtacama, vecTarapaca, 
                                    FUN = mean, R = B)
distribucion_bootstrap2 <- two.boot(vecAtacama, vecAntofagasta, 
                                    FUN = mean, R = B)
distribucion_bootstrap3 <- two.boot(vecAntofagasta, vecTarapaca, 
                                    FUN = mean, R = B)

desplazamiento1 <- mean(distribucion_bootstrap1[["t"]]) - valor_nulo
distribucion_nula1 <- distribucion_bootstrap1[["t"]] - desplazamiento1

desplazamiento2 <- mean(distribucion_bootstrap2[["t"]]) - valor_nulo
distribucion_nula2 <- distribucion_bootstrap2[["t"]] - desplazamiento2

desplazamiento3 <- mean(distribucion_bootstrap3[["t"]]) - valor_nulo
distribucion_nula3 <- distribucion_bootstrap3[["t"]] - desplazamiento3

# Se determina el valor p para cada combinaci�n
p1 <- (sum(abs(distribucion_nula1) > abs(diferencia_1)) + 1) / (B + 1)
cat("Valor p1:", p1)

p2 <- (sum(abs(distribucion_nula2) > abs(diferencia_2)) + 1) / (B + 1)
cat("Valor p2:", p2)

p3 <- (sum(abs(distribucion_nula3) > abs(diferencia_3)) + 1) / (B + 1)
cat("Valor p3:", p3)

# ---- An�lisis Post Hoc ----
#{Aqui ir�a el Analisis Post Hoc}

# ---- Respuesta 2 ----
# Luego de realizar el proceso de comparaci�n mediante bootstraping
# para cada combinaci�n de muestras con un valor nulo de 0,
# se obtuvieron los siguientes valores p, para el caso [1] se 
# obtuvo un valor p de 0.903, en tanto al caso [2] se obtuvo
# un valor p de 0.1394 y finalmente para el caso [3] se obtuvo
# un p de 0.1783, dado que los tres valores p son mayores que el
# nivel de significaci�n de 0.05, no se logra rechazar la hip�tesis
# nula, por lo que los datos no evidencian una diferencia entre
# ellos, es por esto se deber�a proceder a realizar pruebas post hoc.
# La prueba post hoc a utilizar deber�a ser para muestras independientes.

