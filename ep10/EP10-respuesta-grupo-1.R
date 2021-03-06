# EP10 IME
# Esteban Arenas
# Nicol�s Torreblanca
# Rodrigo Escobar 

# Librerias
library(dplyr)

# ---- Pregunta 1 ----
# �Existe diferencia en la puntuaci�n obtenida por los envases
# dise�ados por PackPro seg�n las evaluaciones realizadas por 
# ni�os y j�venes?

# Definici�n de Hip�tesis
# Hip�tesis Nula: No hay diferencia en las puntuaciones obtenidas
# por los envases dise�ados por PackPro seg�n las evaluaciones 
# realizadas por ni�os y j�venes

# Hip�tesis Alternativa: Si hay diferencia en las puntuaciones obtenidas
# por los envases dise�ados por PackPro seg�n las evaluaciones 
# realizadas por ni�os y j�venes

# Lectura de datos
datos <- read.csv2("EP10 Datos.csv")

packPro <- datos%>% filter(Diseno == "PackPro")

ninos <- (packPro %>% filter(Edad == "Nino")) %>% select(Puntaje)
jovenes <- (packPro %>% filter(Edad == "Joven")) %>% select(Puntaje)

vecNinos <- ninos$Puntaje
vecJovenes <- jovenes$Puntaje

# Se establece nivel de significaci�n.
alfa <- 0.05

# ---- Condiciones Wilcoxon ----
# 1. Las observaciones de ambas muestras son independientes porque 
# el estudio se�ala que los participantes fueron seleccionados de 
# forma aleatoria.

# 2. La escala utilizada es ordinal, ya que las variables medidas 
# pueden ser ordenadas presentando una cierta jerarqu�a, que en este 
# caso corresponde a una escala de notas.

# Dado que se cumplen las condiciones para el uso de la pueba de suma de
# rangos de Wilcoxon, se procede a utilizarla con los datos que se desean 
# analizar.

# Hacer la prueba de Mann-Whitney.
pruebaWilcoxon <- wilcox.test(vecNinos, vecJovenes, 
                      alternative = "two.sided", 
                      conf.level = 1 - alfa)
print(pruebaWilcoxon)

# ---- Respuesta 1 ----
# Luego de realizar la prueba de Mann-Whitney se obtuvo un valor p de
# 2.2e-16 el cual es menor que el nivel de significaci�n definido, por 
# lo que se tienen las suficientes pruebas para rechazar la hip�tesis nula,
# por lo que se acepta la hip�tesis alternativa, esto quiere decir que 
# Si hay diferencia en las puntuaciones obtenidas por los envases 
# dise�ados por PackPro seg�n las evaluaciones realizadas por 
# ni�os y j�venes.


# ---- Pregunta 2 ----
# �Existen diferencias entre las puntuaciones obtenidas para 
# los diferentes envases de galletas? De ser as�, �cu�l(es) envase(s) 
# se diferencia(n) de los dem�s?

# Definici�n de Hip�tesis
# Hip�tesis Nula: Las puntuaciones obtenidas por los diferentes envases
# de galletas son similares.

# Hip�tesis Alternativa: Al menos una de las puntuaciones de los diferentes
# envases de galletas es diferente.

# Se filtran los datos
envases <- datos %>% select(Id, Diseno, Puntaje)

# ---- Condiciones Friedman ----
# 1. La variable independiente en este caso es Dise�o la cual es categ�rica 
# y posee cuatro niveles, los cuales son packPro, koolDesign, laKajita y 
# disenoColor.

# 2. La escala de la variable dependiente que en este caso es el puntaje es 
# ordinal, ya que los puntajes tienen una jerarqu�a.

# 3. Los sujetos corresponden a una muestra aleatoria e independiente, ya 
# que el estudio recolect� los voluntarios de forma aleatoria.

# Dado que se cumplen las condiciones para usar la prueba de Friedman, 
# se procede a utilizarla con los datos que se desean analizar.


# Se establece nivel de significaci�n.
alfa <- 0.05

# Hacer la prueba de Friedman.
pruebaFriedman <- friedman.test(Puntaje ~ Diseno | Id, data = envases)
print(pruebaFriedman)

# Dado que se obtiene un valor p menor al nivel de significaci�n se 
# realiza una prueba post hoc, la cual ser� la prueba de Correci�n de Holm

# Se realiza la prueba de Correcci�n de Holm
post_hoc <- pairwise.wilcox.test(envases$Puntaje,
                                 envases$Diseno,
                                 p.adjust.method = "holm",
                                 paired = TRUE)
print(post_hoc)

# ---- Respuesta 2 ----
# A pesar de que al utilizar la prueba de Friedman se haya obtenido un valor p
# de 0.04924 el cual es menor al nivel de significaci�n, �ste por si solo no
# entrega suficiente informaci�n para concluir que al menos una de 
# las puntuaciones de los diferentes envases de galletas es diferente, por lo
# se procede a utilizar una prueba post hoc, la cual ser� la de Holm.

# Luego de realizar la prueba post hoc de Holm se observa que el menor 
# valor p obtenido en la matriz corresponde a 0.063 entre 
# LaKajita y DisenoColor, el cual al mismto tiempo es mayor que el nivel
# de significaci�n de 0.05, es por esto que se falla en rechazar 
# la hip�tesis nula, es decir que las puntuaciones obtenidas 
# por los diferentes envases de galletas son similares.




