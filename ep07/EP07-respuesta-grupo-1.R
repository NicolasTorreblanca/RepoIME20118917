#EP07 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

library(dplyr)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

#---- Pregunta 1 ----
# Una popular discoteca quiere analizar el inter�s que generan 
# los concursos de baile entre sus visitantes m�s asiduos. 
# Para ello ha invitado a sus mejores clientes, 8 hombres y 10 mujeres,
# a un evento privado con la posibilidad de participar en un concurso.
# 8 mujeres y 1 hombre decidieron participar. 
# �Influye el g�nero en la participaci�n en concursos de baile?

#Definici�n de Hip�tesis
#H0: Las variables son independientes
#HA: Las variables están relacionadas

#Construir la tabla de contingencia
Mujeres <- c(8,2)
Hombres <- c(1,7)

tabla <- as.table(rbind(Mujeres,Hombres))

dimnames (tabla)<-list(sexo = c("Mujeres" , "Hombres"),
                       especialidad = c("Pariticipa","No Participa"))
print(tabla)

# Se aplica la prueba exacta de Fisher.
alfa <- 0.05
prueba_fisher <- fisher.test(tabla, 1-alfa)
print(prueba_fisher)

#---- Respuesta 1 ----
# Dado que al realizar la prueba de Fisher se obtuvo un valor p de 
# 0.01522 menor que el alfa de 0.05, esto quiere decir que se 
# rechaza la hip�tesis nula en favor de la hip�tesis alternativa, esto
# quiere decir que las variables est�n relacionadas, en este caso, 
# los g�neros influyen en la participaci�n del concurso.


#---- Pregunta 2 ----
# En un estudio acerca del efecto de la deficiencia de vitamina B1 
# durante la infancia temprana Katz, Haltus & Friedmann (2022)
# (Journal of Neurolinguistics, 62(5), 101042), se ha concluido 
# que tal carencia se traduce en severos problemas de lenguaje. 
# Un nuevo estudio busca verificar este hallazgo, para lo cual 
# ha evaluado eldesarrollo del lenguaje a los 5 a�os de vida de 
# 35 parejas de gemelos id�nticos donde, por razones m�dicas, 
# beb�s son alimentados con diferentes f�rmulas (una carente de 
# vitamina B1 y la otra, no). Los datos registrados muestran que: 
# * En 10 parejas de gemelos, ninguno presenta trastornos del 
# lenguaje. 
# * En 5 parejas de gemelos, ambos presentan trastornos del lenguaje.
# * En 9 parejas de gemelos, solo aquel que fue alimentado con la
# f�rmula que contiene vitamina B1 desarrolla trastornos del lenguaje.
# * En las 11 parejas de gemelos restantes, solo el gemelo con 
# carencia de vitamina B1 presenta trastornos del lenguaje.
# �Soportan los nuevos datos la conclusi�n del estudio original?

#Definici�n de Hip�tesis
#H0: La deficiencia de B1 produce trastornos del lenguaje
#HA: La deficiencia de B1 no produce trastornos del lenguaje



#Construir la tabla de contingencia
Gemelo_B1 <- c(10,9)
Gemelo_Sin_B1 <- c(11,5)

tabla_2 <- as.table(rbind(Gemelo_B1,Gemelo_Sin_B1))

dimnames (tabla_2)<-list(gemelo_Sin_B1 = c("Sin Trastorno" , "Con Trastorno"),
                       gemelo_B1 = c("Sin Trastorno","Con Trastorno"))
print(tabla_2)


# Se aplica la prueba de McNemar
prueba_mcnemar <- mcnemar.test(tabla_2)
print(prueba_mcnemar)

#---- Respuesta 2 ----
# En base a lo resultados de la prueba de McNemar en donde se obtuvo
# un valor p de 0.8231 el cual es mayor al alfa de 0.05, se puede
# conluir que no hay evidencia suficiente para rechazar la hip�tesis
# nula, por lo tanto se rechaza la hip�tesis alternativa, es decir, 
# que la deficiencia de la vitamina B1 tiene gran incidencia en el
# desarrollo de trastornos del lenguaje en ni�os.

#---- Pregunta 3 ----
# En su af�n de comprender mejor a sus eternos enemigos,  
# los vampiros, Van Helsing ha decidido estudiar si vampiresas y 
# vampiros tienen preferencias similares en cuanto al tipo 
# sangu�neo de sus v�ctimas. 
# �Qu� puede concluir a partir las preferencias alimentarias 
# de estos seres de acuerdo a sus registros?

#Definici�n de Hip�tesis
#H0: Las variables g�nero de vampiro y tipo de sangre son independientes.
#H1: Las variables g�nero de vampiro y tipo de sangre est�n relacionadas
 
#Construir la tabla
Vampiro <- c(15, 12, 8, 6)
Vampiresa <- c(9, 14, 5, 7)

tabla_3 <- as.table(rbind(Vampiro,Vampiresa))

dimnames (tabla_3)<-list(genero = c("Vampiro" , "Vampiresa"),
                         tipo_sangre = c("A","B", "AB","O"))
print(tabla_3)

# Se hace prueba de independencia
prueba_independencia <- chisq.test(tabla_3)
cat("\nLa prueba internamente calcula los valores esperados :\n")
esperados <- round(prueba_independencia [["expected"]], 3)
print(esperados)
cat("\nResultado de la prueba :\n")
print(prueba_independencia) 

#---- Respuesta 3 ----
# En base a los resultados de la prueba de independencia, en la cual
# se obtuvo un valor p de 0.5804 se puede decir que se falla en rechazar
# la hip�tesis nula, por lo tanto, se rechaza la hip�tesis alternativa,
# la que propon�a una relaci�n entre el g�nero del vampiro y gusto en 
# el tipo de sangre de sus v�ctimas. Por ende, se puede afirmar con 95%
# de confianza que las los gustos de sangre de los vampiros son
# independientes de su g�nero.


#---- Pregunta 4 ----
# La Facultad de Ingenier�a desea saber si existe diferencia 
# significativa en el desempe�o de los estudiantes en asignaturas 
# cr�ticas de primer semestre. Para ello, le ha entregado un 
# archivo de datos que, para 3 asignaturas, indica si una muestra 
# de 50 estudiantes aprobados o reprobados. �Qu� puede concluir la 
# Facultad? Indicaci�n: obtenga la muestra a partir del archivo
# EP07 Datos.csv, usando la semilla 102. Considere un nivel de 
# significaci�n alfa = 0,05.

#Definici�n de Hip�tesis
#H0 : La proporci�n de aprobados es la misma para todos los grupos
#H1 : La proporci�n de aprobados es diferente para al menos un grupo

# Se extraen los datos del archivo csv
datos <- read.csv2("EP07 Datos.csv")

# Se define una semilla 102
set.seed(102)

# Se define un nivel de significaci�n de 0.05
alfa = 0.05

# Se obtiene una muestra de 50 estudiantes
muestra_50 <- sample(datos[["Id"]], 50, replace = FALSE, prob = NULL)
print(muestra_50)

# Se seleccionan los datos para trabajar con ellos con mayor facilidad
calculo_datos <- t(datos %>% select("Calculo"))
algebra_datos <- t(datos %>% select("Algebra"))
fisica_datos <- t(datos %>% select("Fisica"))

# Se guardan los valores de la muestra en vectores para cada asignatura
i = 1
calculo <- c()
algebra <- c()
fisica <- c()

while(i <= 50){
  pos <- muestra_50[i]
  calculo[i] <- calculo_datos[pos]
  algebra[i] <- algebra_datos[pos]
  fisica[i] <- fisica_datos[pos]
  i=i+1
}

# Se guardan todos los valores de la muestra en un dataframe
new_datos <- data.frame(muestra_50 , calculo , algebra , fisica)

# Se transforma la matriz a formato largo
new_datos_largos <- new_datos %>% pivot_longer(c("calculo", "algebra","fisica"),
                                 names_to = "Asignatura",
                                 values_to = "Estado")

# Se renombra la columna para mejor comprensi�n
names(new_datos_largos)[names(new_datos_largos) == "muestra_50"] <- "Id"

new_datos_largos[["Id"]] <- factor(new_datos_largos [["Id"]])
new_datos_largos[["Asignatura"]] <- factor(new_datos_largos[["Asignatura"]])

# Se realiza la prueba Q de Cochran.
prueba_cochran <- cochran.qtest(Estado ~ Asignatura | Id ,
                        data = new_datos_largos , alpha = alfa)
print(prueba_cochran)

#---- Respuesta 4 ----
# Dado que al realizar la prueba Q de Cochran se obtuvo
# valor p de 0.01083, esto quiere que se rechaza la hip�tesis nula en favor 
# a la hip�tesis alternativa, esto quiere decir que se concluye 
# con 95% de confianza que al menos una de las asignaturas los 
#estudiantes tienen desempe�o diferente a las dem�s.


  
  
  
  
  

