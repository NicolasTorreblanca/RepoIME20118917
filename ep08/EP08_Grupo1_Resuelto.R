#EP NUMERO 8, Grupo 1
#Inferencia y Modelos Estocasticos

#Esteban Arenas
#Rodrigo Escobar
#Nicolas Torreblanca

#Fecha entrega: 27-04-2022

#----Librerias-----

library(car)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(ez)
library(tidyverse) 
library(DescTools)


#----Ejercicio 1-----

#En este momento, los investigadores buscan determinar 
#si existen diferencias en el tiempo que tardan los usuarios
#en formular una consulta para un problema de dificultad f�cil 
#en las �reas de computaci�n, literatura y qu�mica.

#----Desarollo---

#Primero que nada,se leen los datos

datos <- read.csv2("EP08 Datos.csv")

datos_difbaja <- datos %>% filter(dificultad == "Baja")

ramos_difbaja <- datos_difbaja %>% filter(area == "Computaci�n" |
                                            area == "Literatura" |
                                            area == "Qu�mica")

estudio <- ramos_difbaja %>% select(area,tiempo)

estudio[["area"]] <- factor(estudio [["area"]])
estudio[["instancia"]] <- factor (1: nrow(estudio))

#Despues de revisar los datos 
#se pueden plantear las hipotesis nula y alternativa

# H0 : El tiempo de formulaci�n de una consulta
#      en son iguales para las �reas de computaci�n,literatura y qu�mica 

# H1 : El tiempo de formulaci�n de una consulta
#      en es diferente para al menos una de las �reas de computaci�n,literatura y qu�mica 

#Se realiza el analisis si se cumplen las condiciones para realizar
#Un procedimiento Anova.

#Se realiza un grafico qqplot que permite visualizar si se cumple con el supuesto de
#Normalidad de las 3 muestras

graf_normalidades <- ggqqplot(estudio ,
              x = "tiempo",
              y = "area",
              color = "area")

graf_normalidades <- graf_normalidades + facet_wrap(~ area)
graf_normalidades <- graf_normalidades + rremove("x.ticks") + rremove("x.text")
graf_normalidades <- graf_normalidades + rremove("y.ticks") + rremove("y.text")
graf_normalidades <- graf_normalidades + rremove("axis.title")
print(graf_normalidades)

#Como se puede observar en el grafico de normalidades, si se puede suponer razonablemente
# que las muestras pertenecen cada una a una distribucion normal  

# La variable de tiempo se mide en el mismo intervalo de tiempo, ya que
# se puede observar en el grafico que los tiempos para cada area son similares.

# Las 3 muestras son obtenidas de manera aleatoria e independiente de la poblacion de
# origen, ya que son un conjunto de voluntarios que son un porcentaje minusculo de
# la totalidad de seres humanos.

# Solo falta probar la homocedasticidad, el cual sera probado por la 
# prueba omnibus anova a utilizar EzAnova

#Aplicando un procedimiento anova omnibus

#Fijamos un alpha

alfa <- 0.05

# Procedimiento ANOVA con ezANOVA().
cat("Procedimiento Anova Omnibus con Ezanova\n\n")
anova_estudio <- ezANOVA(
  data = estudio,
  dv = tiempo,
  between = area,
  wid = instancia,
  return_aov = TRUE)

print(anova_estudio)

#Luego, se grafican los resultados obtenidos.

grafico_promedios <- ezPlot(
  data = estudio,
  dv = tiempo,
  wid = instancia,
  between = area,
  y_lab = "Tiempo promedio de formulacion [s]",
  x = area
)

print(grafico_promedios)


#Se puede ver que se obtiene de la prueba omnibus de anova que el p obtenido es
# Mucho menor al alfa de 0.05 (8.237864e-07 < 0.05)

#Con ello en mente, aplicamos la prueba de Post-Hoc para verificar si
#los tiempos de formulacion de Preguntas difieren significativamente.

#Podemos aplicar Bonferroni y Holm para la correcion.

# Procedimiento post-hoc de Bonferroni.
cat("Procedimiento post-hoc de Bonferroni\n\n")

bonferroni <- pairwise.t.test(estudio[["tiempo"]],
                              estudio[["area"]],
                              p.adj = "bonferroni",
                              pool.sd = TRUE,
                              paired = FALSE,
                              conf.level = 1 - alfa)

print(bonferroni)

# Procedimiento post-hoc de Holm.
cat("\n\nProcedimiento post-hoc de Holm\n\n")

holm <- pairwise.t.test(estudio[["tiempo"]],
                        estudio[["area"]],
                        p.adj = "holm",
                        pool.sd = TRUE,
                        paired = FALSE,
                        conf.level = 1 - alfa)

print(holm)

#Agrupando los resultados obtenidos por los procedimientos obtenemos:

# Dif entre  :      Lit-Quim           Comp-Quim         Lit-Comp
# Bonferroni :      0.000001            0.00042          0.56208  
# Holm  :           0.000001            0.00028          0.18736

#Podemos observar que los Tanto las areas de computaci�n y de literatura
# no poseen diferencias significatias entre ellas(son mayores al alfa 0.05)
# en las pruebas de Bonferroni y de Holm.

# Por otro lado, si existen pruebas significativas entre estas �reas con el
# area de literatura, ya que para ambas pruebas se obtienen valores menores
# al alfa utilizado.

#----Conclusion----

# A partir de la prueba anova realizada, y de los procedimientos postHoc de
# Bonferroni y Holm podemos concluir que existe suficiente informacion para negar
# La hipotesis Nula, y se acepta la hip�tesis alternativa, la cual propone que 
# al menos una de las areas tiene un tiempo de formulacion diferente, el que para
# Este estudio podemos decir que es el area de Qu�mica el que tiene un tiempo de 
# Formulaci�n de consultas diferente.