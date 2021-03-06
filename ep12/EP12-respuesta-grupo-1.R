# EP12 IME
# Nicolas Torreblanca
# Esteban Arenas
# Rodrigo Escobar

# ---- Librerias ----
library(dplyr)
library(WRS2)
library(ggpubr)
library(DescTools)

# ---- Pregunta 1 ----
# En el trabajo de t�tulo de un estudiante del DIINF se 
# reportan los siguientes tiempos de ejecuci�n (en 
# milisegundos) medidos para dos versiones de un algoritmo
# gen�tico para resolver instancias del problema del 
# vendedor viajero disponibles en repositorios p�blicos.
# �Es uno de los algoritmos m�s r�pido que el otro?

# ---- Definici�n de Hip�tesis ----
# Hip�tesis Nula: En promedio, los tiempos de ejecuci�n
# para ambas versiones del algortimo gen�tico aplicado a
# la resoluci�n del problema del vendedor viajero 
# son iguales 
# uA = uB

# Hip�tesis Alternativa: En promedio, los tiempos de ejecuci�n
# para ambas versiones del algortimo gen�tico aplicado a
# la resoluci�n del problema del vendedor viajero 
# son diferentes.
# uA != uB

# Se definen los datos de cada algoritmo
tiemposA <- c(1510394, 402929, 885722, 4428151, 48667,
              834565, 70599, 783108, 210041, 37449)
instanciaA <- c(129,109,28,178,74,16,87,108,149,78)

tiemposB <- c(1252837, 2196277, 120276, 4629726, 4629726,
              4629726, 6568968, 6568968, 6568968, 35974)
instanciaB <- c(134,193,10,88,142,86,36,190,163,33)

reunidosA <- data.frame(instanciaA,tiemposA)

reunidosB <- data.frame(instanciaB,tiemposB)

# Prueba de Normalidad 
normalidad_A <- ggqqplot(tiemposA, color = "purple",title = "Normalidad Tiempos A")
print(normalidad_A)

normalidad_B <- ggqqplot(tiemposB, color = "purple", title = "Normalidad Tiempos B")
print(normalidad_B)

# Dado que los datos son dispersos y no se asemejan a la normal
# se procede a realizar una transformaci�n del tipo Box Cox
# a los datos.

# Se define una funci�n para transformar los datos utilizando
# Box Cox
box_cox <- function(x, lambda) {
  if(lambda == 0) {
    return(log(x))
  }
  
  resultado <- (x ** lambda -1) / lambda
  return(resultado)
}

# Se busca la mejor transformaci�n Box-Cox usando funciones de R.
lambdaA <- BoxCoxLambda(reunidosA$tiemposA, lower = -1, upper = 1)
cat("Lambda optimo:", lambdaA)
transformacionA <- BoxCox(reunidosA$tiemposA, lambdaA)
juntadosA <- data.frame(reunidosA, transformacionA)

lambdaB <- BoxCoxLambda(reunidosA$tiemposA, lower = -1, upper = 1)
cat("Lambda optimo:", lambdaB)
transformacionB <- BoxCox(reunidosB$tiemposB, lambdaB)
juntadosB <- data.frame(reunidosB, transformacionB)


# Se grafican los datos transformados.
norm_transformacion_A <- ggqqplot(transformacionA, color = "purple")
print(norm_transformacion_A)

hist_transformacion_A <- gghistogram(juntadosA, bins = 10, x = "transformacionA", color = "purple",
                  fill = "purple", xlab = "Transformaci�n de Tiempos A",
                  ylab = "Frecuencia") + rotate_x_text(45)
print(hist_transformacion_A)

norm_transformacion_B <- ggqqplot(transformacionB, color = "purple")
print(norm_transformacion_B)

hist_transformacion_B <- gghistogram(juntadosB, bins = 10, x = "transformacionB", color = "purple",
                  fill = "purple", xlab = "Transformaci�n de Tiempos B",
                  ylab = "Frecuencia") + rotate_x_text(45)
print(hist_transformacion_B)


# Se crea un dataframe con los datos transformados
transformadas <- data.frame(transformacionA,transformacionB)
algoritmo <- c(rep("A", length(tiemposA)), rep("B", length(tiemposB)))
datos <- data.frame(transformadas, algoritmo)

# Se define un nivel de significaci�n
alfa <- 0.05

# Se define un valor gamma para truncar datos
gamma <- 0.2

# Se obtiene la cantidad de datos
n_A <- length(transformacionA)
n_B <- length(transformacionB)

poda_A <- n_A * gamma
poda_B <- n_B * gamma

# Se truncan los datos
A_truncada <- transformacionA[poda_A:(n_A - poda_A)]
B_truncada <- transformacionB[poda_B:(n_B - poda_B)]

# Se crea un dataframe con los datos truncados
transformacionTruncada <- c(A_truncada, B_truncada)
algoritmoTruncado <- c(rep("A", length(A_truncada)), rep("B", length(B_truncada)))
datos_truncados <- data.frame(transformacionTruncada, algoritmoTruncado)

# Se aplica la prueba de Yuen para muestras independientes
prueba_yuen <- yuen(transformacionTruncada ~ algoritmoTruncado, data = datos_truncados, tr = gamma)
print(prueba_yuen)

# ---- Respuesta 1 ----

# Luego de realizar la prueba de Yuen para datos independientes a los datos
# transformados con Box Cox se obtuvo un p valor de 0.01354 el cual es menor que 
# el nivel de significaci�n definido en 0.05, con esto tenemos la suficiente 
# informaci�n para rechazar la hip�tesis nula, al mismo tiempo que se acepta
# la hip�tesis alternativa, es decir, que en promedio, los tiempos de ejecuci�n
# para ambas versiones del algortimo gen�tico aplicado a la resoluci�n 
# del problema del vendedor viajero son diferentes.


# ---- Pregunta 2 ----
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

# ---- Discusi�n de Prueba ----
# Dado que las muestras de este enunciado corresponden a dos
# muestras independientes, la prueba param�trica apropiada para
# este caso ser�a la Prueba t para dos muestras independientes,
# por lo que la prueba no param�trica que utilizar�amos ser�a
# la prueba de Yuen para dos muestras independientes.

# Se cargan los datos del archivo .csv
datosPregunta2 <- read.csv2("EP11 Datos.csv")

# Se define una semilla
set.seed(666)

# Se define el tama�o de la muestra
tamanoMuestra1 <- 300

# Se obtiene una muestra de los datos
muestra <- datosPregunta2[sample(nrow(datosPregunta2), tamanoMuestra1),]

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

# Se crea un dataframe con los datos con los que se desea trabajar
ingreso <- c(vecBiobio, vecMetropolitana)
mujeres <- c(rep("Mujeres Regi�n del Biob�o", length(vecBiobio)), rep("Mujeres Regi�n Metropolitana", length(vecMetropolitana)))
datosMujeres <- data.frame(ingreso, mujeres)

# Se comprueba la normalidad.
normalidad_mujeres <- ggqqplot(datosMujeres, x = "ingreso", facet.by = "mujeres",
              palette = c("blue", "red"), color = "mujeres")

print(normalidad_mujeres)

# Se observa que los datos de cada muestra son dispersos y no
# se asemeja a la normal.

# Se define un nivel de significaci�n
alfa2 <- 0.05

# Se define un gamma para truncar los datos
gamma2 <- 0.2

# Se obtiene la cantidad de datos de cada muestra
n_Biobio <- length(vecBiobio)
n_Metropolitana <- length(vecMetropolitana)

poda_Biobio <- n_Biobio * gamma
poda_Metropolitana <- n_Metropolitana * gamma

# Se truncan los datos segun el % de poda
biobio_truncada <- vecBiobio[poda_Biobio:(n_Biobio - poda_Biobio)]
metropolitana_truncada <- vecMetropolitana[poda_Metropolitana:(n_Metropolitana - poda_Metropolitana)]

# Se crea un dataframe con los datos truncados
ingresoTruncado <- c(biobio_truncada, metropolitana_truncada)
mujeresTruncadas <- c(rep("Mujeres Regi�n del Biob�o", length(biobio_truncada)), rep("Mujeres Regi�n Metropolitana", length(metropolitana_truncada)))
datos2_truncados <- data.frame(ingresoTruncado, mujeresTruncadas)

# Se comprueba la normalidad de los datos truncados
qq_mujeres <- ggqqplot(datos2_truncados, x = "ingresoTruncado", facet.by = "mujeresTruncadas",
              palette = c("blue", "red"), color = "mujeresTruncadas")
print(qq_mujeres)

# Se aplica la prueba de Yuen para muestras independientes.
prueba_yuen2 <- yuen(ingresoTruncado ~ mujeresTruncadas, data = datos2_truncados, tr = gamma2)
print(prueba_yuen2)

# ---- Respuesta 2 ----
# Luego de realizar la prueba de Yuen para datos independientes a 
# las muestras, se obtuvo un valor p de 0.12505 este valor es menor
# que el nivel de significaci�n de 0.05 definido con anterioridad,
# es por esto que no se posee informaci�n suficiente para rechazar
# la hip�tesis nula, esto quiere decir que, en promedio, los ingresos per c�pita de las mujeres 
# de la Regi�n Metropolitana de Santiago y de las mujeres de la 
# Regi�n del Biob�o son iguales.

# ---- Pregunta 3 ----
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

# ---- Discusi�n de Prueba ----
# Dado que las muestras de este enunciado corresponden a m�s de
# dos muestras independientes, la prueba param�trica apropiada para
# este caso ser�a el procedimiento Anova para muestras independientes
# por lo que la prueba no param�trica que utilizar�amos ser�a
# la prueba de comparaciones de una v�a para m�ltiples grupos 
# independientes con el m�todo de las medias truncadas.

# Se define una semilla para este caso
set.seed(11)

# Se define un tama�o de muestra para este caso
tamanoMuestra2 <- 520

# Se toma una muestra de los datos originales
muestra2 <- datosPregunta2[sample(nrow(datosPregunta2), tamanoMuestra2),]

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

# Se crea un dataframe con los datos
ingreso3 <- c(vecAtacama, vecTarapaca, vecAntofagasta)
vivienda <- c(rep("Regi�n de Atacama", length(vecAtacama)), rep("Regi�n de Tarapac�", length(vecTarapaca)), rep("Regi�n de Antofagasta", length(vecAntofagasta)))
datos3 <- data.frame(ingreso3, vivienda)


# Se fija el nivel de significaci�n.
alfa3 <- 0.05

# Se define un gamma para truncar los datos
gamma3 <- 0.2

# Se realiza la comparaci�n de una v�a para m�ltiples grupos independientes
# con el m�todo de medias truncadas.

cat("Comparaci�n entre grupos usando medias truncadas\n\n")
medias_truncadas3 <- t1way(ingreso3 ~ vivienda, data = datos3, tr = gamma3,
                          alpha = alfa3)

print(medias_truncadas3)

# ---- Respuesta 3 ----

# Luego de utilizar la comparaci�n de una v�a para m�ltiples grupos independientes
# con el m�todo de medias truncadas se obtuvo un valor p de 0.4273 esto quiere
# que no se tiene las suficientes pruebas para rechazar la hip�tesis nula, 
# por lo que se concluye que en promedio, el ingreso per c�pita en la Regi�n de Atacama, 
# de Tarapac� y de Antofagasta son iguales.











