#EP07 IME
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

#---- Pregunta 1 ----
# Una popular discoteca quiere analizar el interés que generan 
# los concursos de baile entre sus visitantes más asiduos. 
# Para ello ha invitado a sus mejores clientes, 8 hombres y 10 mujeres,
# a un evento privado con la posibilidad de participar en un concurso.
# 8 mujeres y 1 hombre decidieron participar. 
# ¿Influye el género en la participación en concursos de baile?

#Definición de Hipótesis
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
# rechaza la hipótesis nula en favor de la hipótesis alternativa, esto
# quiere decir que las variables están relacionadas, en este caso, 
# los géneros influyen en la participación del concurso.


#---- Pregunta 2 ----
# En un estudio acerca del efecto de la deficiencia de vitamina B1 
# durante la infancia temprana Katz, Haltus & Friedmann (2022)
# (Journal of Neurolinguistics, 62(5), 101042), se ha concluido 
# que tal carencia se traduce en severos problemas de lenguaje. 
# Un nuevo estudio busca verificar este hallazgo, para lo cual 
# ha evaluado eldesarrollo del lenguaje a los 5 años de vida de 
# 35 parejas de gemelos idénticos donde, por razones médicas, 
# bebés son alimentados con diferentes fórmulas (una carente de 
# vitamina B1 y la otra, no). Los datos registrados muestran que: 
# ▪ En 10 parejas de gemelos, ninguno presenta trastornos del 
# lenguaje. 
# ▪ En 5 parejas de gemelos, ambos presentan trastornos del lenguaje.
# ▪ En 9 parejas de gemelos, solo aquel que fue alimentado con la
# fórmula que sí contiene vitamina B1 desarrolla trastornos del lenguaje.
# ▪ En las 11 parejas de gemelos restantes, solo el gemelo con 
# carencia de vitamina B1 presenta trastornos del lenguaje.
# ¿Soportan los nuevos datos la conclusión del estudio original?

#Definición de Hipótesis
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
# conluir que no hay evidencia suficiente para rechazar la hipótesis
# nula, por lo tanto se rechaza la hipótesis alternativa, es decir, 
# que la deficiencia de la vitamina B1 tiene gran incidencia en el
# desarrollo de trastornos del lenguaje en niños.

#---- Pregunta 3 ----
# En su afán de comprender mejor a sus eternos enemigos,  
# los vampiros, Van Helsing ha decidido estudiar si vampiresas y 
# vampiros tienen preferencias similares en cuanto al tipo 
# sanguíneo de sus víctimas. 
# ¿Qué puede concluir a partir las preferencias alimentarias 
# de estos seres de acuerdo a sus registros?

#H0 : 
#H1 : 
 
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



#---- Pregunta 4 ----

# La Facultad de Ingeniería desea saber si existe diferencia 
# significativa en el desempeño de los estudiantes en asignaturas 
# críticas de primer semestre. Para ello, le ha entregado un 
# archivo de datos que, para 3 asignaturas, indica si una muestra 
# de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir la 
# Facultad? Indicación: obtenga la muestra a partir del archivo
# EP07 Datos.csv, usando la semilla 102. Considere un nivel de 
# significación alfa=0,05.



#---- Respuesta 4 ----

  
  
  
  
  

