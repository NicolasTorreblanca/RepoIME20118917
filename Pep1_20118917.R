# PEP 1 INFERENCIA Y METODOS ESTOCASTICOS

#NICOLAS TORREBLANCA 20118917-9
#18 04 2022


#Librerias


library(dplyr)
library(tidyverse)
library(pwr)

#Usted ha sido designado asesor del mediador enviado por latonia,
#quien le ha solicitado responder las siguientes preguntas usando para ello 
#Una muestra de 400 trabajadores.


#En primer lugar debemos de construir la muestra
#Para ello

#Leemos los datos, asignamos una semilla y generamos la muestra.


datos <- read.csv2("Pep 1 Datos.csv")
set.seed(478)
muestra_400 <- sample(datos[["Id"]], 400, replace = FALSE, prob = NULL)

pais_datos <- t(datos %>% select("Pais"))
edad_datos <- t(datos %>% select("Edad"))
fav_datos <- t(datos %>% select("Favorito"))
estado_datos <- t(datos %>% select("Estado"))

i = 1
pais <- c()
edad <- c()
favorito <- c()
estado <- c()

while(i <= 400){
  pos <- muestra_400[i]
  pais[i] <- pais_datos[pos]
  edad[i] <- edad_datos[pos]
  favorito[i] <- fav_datos[pos]
  estado[i] <- estado_datos[pos]
  i=i+1
}

# Se guardan todos los valores de la muestra en un dataframe
new_datos <- data.frame(muestra_400,pais,edad,favorito,estado)

#seleccionamos de la muestra a los habitantes de cada pais 



#-----Pregunta 1------
#¿Es igual la proporción de bajas(abollados y abiertos) en salcacia y conservia.


#Se nos solicita comparar si la@ proporción de bajas entre los habitantes de 
#salsacia y conservia poseen una proporcion de bajas igual.

#Para poder responder esta pregunta necesitaremos obtener cual es la proporcion
# de bajas para cada uno de los paises.

#Luego de eso, utilizaremos el metodo de wald para 2 proporciones para verificar
#Nuestras hipotesis nula y alternativa


#------Desarrollo 1----

#Antes de comenzar el desarrollo proponemos nuestras hipotesis

#H0 : La proporcion de bajas es igual para ambos paises

#H1 : La proporcion de bajas es distinta para ambos paises

#Que en terminos estadisticos significaria:

# H0 : P1 - P2 = 0
# H1 : P1 - P2 != 0

# Para calcular la proporcion de utilizara la proporcion entre las bajas
# dividido por el total de la poblacion


#Obtenemos de la muestra los habitantes de cada pais en primer lugar
#Luego, obtenemos lo que refiere a su estado en cada pais
#Buscamos si esta abierto o abollado 

hab_salsacia <-new_datos %>% filter(pais == "Salsacia")
hab_conservia <-new_datos %>% filter(pais == "Conservia")

salsacios_abollados <-hab_salsacia %>% filter(estado == "Abollado")
salsacios_abiertos  <-hab_salsacia %>% filter(estado == "Abierto")

conservios_abollados <-hab_conservia %>% filter(estado == "Abollado")
conservios_abiertos  <-hab_conservia %>% filter(estado == "Abierto")

#Conociendo los habitantes que se encuentran abollados o abiertos
#De cada pais, realizamos el calculo de las proporciones

bajas_conservia = nrow(conservios_abollados) + nrow(conservios_abiertos)
bajas_salsacia = nrow(salsacios_abollados) + nrow(salsacios_abiertos)

habitantes_conservia = nrow(hab_conservia)
habitantes_salsacia = nrow(hab_salsacia)

proporcion_conservia =  bajas_conservia/habitantes_conservia
proporcion_salsacia = bajas_salsacia/habitantes_salsacia

diferencia = abs(proporcion_salsacia - proporcion_conservia) 
valor_nulo = 0
alfa = 0.05

# Sabemos que cada una de las muestras son independientes, al igual que
# provienen de distribuciones de la manera normal.

# Con ello demostrado, podemos aplicar el metodo de wald para 2 proporciones

prop_agrupada = (bajas_conservia + bajas_salsacia) / (habitantes_conservia+habitantes_salsacia)

error_conservia = ( prop_agrupada * (1-prop_agrupada ) ) / habitantes_conservia

error_salsacia = ( prop_agrupada * (1-prop_agrupada ) ) / habitantes_salsacia

error_est_hip <- sqrt(error_conservia + error_salsacia)

Z <- (diferencia-valor_nulo) / error_est_hip

p <- pnorm (Z,lower.tail = FALSE )

cat ( " Hipotesis alternativa bilateral \ n " )
cat ( " Z = " , Z , " \ n " )
cat ( " p = " , p )

#-------Respuesta 1------

#Dado que el p obtenido en el ejercicio corresponde a p = 0,036, el cual
# es menor que 0.05, obtenemos la suficiente informacion para negar 
# la hipotesis nula y aceptar la hipotesis altenativa.

# Por lo tanto, en la guerra entre conservios y salsacios, las
# proporciones entre bajas y poblacion general son diferentes en cada pais.


#-----Pregunta 2------

#Que poder estadistico tiene la prueba realizada.

sigma = 1

d <- (0 - diferencia)/sigma

#------Desarrollo 2---

# Se procede a calcular el poder estadístico con los datos definidos anteriormente
poder_1 <- power.prop.test(400, proporcion_conservia, proporcion_salsacia, alfa, power = NULL, alternative)

# Se muestran los resultado de la prueba de poder
print(poder_1)

# Se extrae solo el resultado de poder
valor_poder <- poder_1[["power"]]

#-------Respuesta 2------

#SE OBTIENE QUE DEBERA SER 0.419 DE PODER


#

poder_2 <- power.t.test(n = NULL,
                        delta = d,
                        sd = sigma,
                        sig.level = alfa,
                        power = 0.8,
                        type = 'one.sample',
                        alternative = 'two.sided')

# Se muestran los resultado de la prueba de poder
print(poder_2)

# Se extrae solo el resultado de poder
valor_poder <- poder_1[["power"]]


#-------Respuesta 3------

#Se deberian de encuestar almenos 1012 latas para obtener un 80
# porciendo de poder y 95% de confianza









