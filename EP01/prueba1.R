#Se importa el paquete dplyr
library(dplyr)

#Se importan los valores de un archivo separado por punto y comas
casos <- read.csv2("EP01 Datos Covid.csv")

#Se seleccionan las columnas que pertenecena a la region del Maule
casos_maule <- casos %>% filter(Region == "Maule")

#Se seleccionan las columnas entre el 1 de Octubre de 2020 y el 31 de Mayo de 2021
resultado <- casos_maule %>% select(X01.10.2020:X31.05.2021)

#se calcula el maximo de las fechas
max(resultado[1,])