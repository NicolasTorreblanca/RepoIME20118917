#EP03
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

#Paquete
library(ggpubr)

#Lectura de archivos
población <- read.csv2("EP03 Datos Casen 2017.csv")

#---- 1 ----
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(11)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)

#plot(ingreso.normal)

#---- 2 ----
#Calculo Distribucion Z
distribucion_z<-(ingreso.normal-media.ingreso)/sd.ingreso
#Se genera histograma
grafico_z<-gghistogram(distribucion_z,
                   bins=30,
                   title="            Distribucion normal ingreso",
                   xlab="      Z",
                   ylab="Frecuencia",
                   fill="red")
print(grafico_z)

#---- 3 ----
#Calculo X^2

