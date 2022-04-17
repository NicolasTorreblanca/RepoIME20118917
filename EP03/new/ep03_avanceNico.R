#EP03
#Esteban Arenas
#Nicolas Torreblanca
#Rodrigo Escobar

#Paquete
library(ggpubr)

#Lectura de archivos
población <- read.csv2("EP03 Datos Casen 2017.csv")

#-------- Parte 1 -------- 

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
distribucion_z <- (ingreso.normal-media.ingreso)/sd.ingreso
#Se genera histograma
grafico_z <- gghistogram(distribucion_z,
                   bins=30,
                   title="            Distribucion normal ingreso",
                   xlab="      Z",
                   ylab="Frecuencia",
                   fill="red")
print(grafico_z)

#---- 3 ----
#Calculo X^2
grados_libertad1 <- 8
grados_libertad2 <- 14

i = 1
distribucion_x21 <- c()
distribucion_x22 <- c()
while(i<=length(distribucion_z)){
  chi1 <-sample(distribucion_z,grados_libertad1,replace=FALSE)
  chi2 <-sample(distribucion_z,grados_libertad2,replace=FALSE)
  distribucion_x21[i]<-sum(chi1^2)
  distribucion_x22[i]<-sum(chi2^2)
  i=i+1
}

grafico_x21 <- gghistogram(distribucion_x21,
                         bins=30,
                         title="            Distribucion Jicuadrado Ingreso con 8 grados de libertad",
                         xlab="      Z",
                         ylab="Frecuencia",
                         fill="red")
print(grafico_x21)


grafico_x22 <- gghistogram(distribucion_x22,
                           bins=30,
                           title="          Distribucion Jicuadrado Ingreso con 14 grados de libertad",
                           xlab="      Z",
                           ylab="Frecuencia",
                           fill="blue")
print(grafico_x22)

#---- 4 ----
j = 1
distf <- c()
while(j<=length(distribucion_x21)){
  div1 <- distribucion_x21[j]/grados_libertad1 
  div2 <- distribucion_x22[j]/grados_libertad2
  distf[j] <- div1/div2
  
  j = j + 1
}


grafico_f <- gghistogram(distf,
                           bins=30,
                           title="          Distribucion F Ingreso",
                           xlab="      Z",
                           ylab="Frecuencia",
                           fill="green")
print(grafico_f)

#-------- Parte 2  --------
#Se define una semilla
set.seed(25)

#Se define un numero de repeticionesº
n.repeticiones <- 80

#77 repeticiones de un ensayo de Bernoulli con éxito si se elige la encuesta de una mujer 
#de entre los datos seleccionados desde la encuesta Casen 2017.
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)


muestras <- sapply(1:n.repeticiones, ensayo)

#
#Calcular exitos
i = 1
cantidad_exitos <- 0
while(i<=length(muestras)){
  if(muestras[i] == 1){
    cantidad_exitos = cantidad_exitos + 1
  }
  i = i + 1
}

#Calcular probabilidad de exito

probabilidad_exito = cantidad_exitos / n.repeticiones

# Distribucion Binomial

j = 1

dist_binom <- c()

while(j<= n.repeticiones){
  
  binom_a <-(probabilidad_exito)^j
  binom_b <-(1-probabilidad_exito)^(n.repeticiones-j)
  binom_c <-choose(n.repeticiones,j)
  
  
  dist_binom[j] <- binom_a * binom_b * binom_c
  
  j = j+1
}

plot(dist_binom,main="Distribucion Binomial",xlab= "Num_intentos",ylab = "Probabilidad" ,type='l',col = 'black')


# Distribucion Binomial Negativa

j = 1

dist_binom_neg <- c()

while(j<= n.repeticiones){
  
  binom_neg_a <-(probabilidad_exito)^j
  binom_neg_b <-(1-probabilidad_exito)^(n.repeticiones-j)
  binom_neg_c <-choose(n.repeticiones-1,j-1)
  
  
  dist_binom_neg[j] <- binom_neg_a * binom_neg_b * binom_neg_c
  
  j = j+1
}

plot(dist_binom_neg,main="Distribucion Binomial Negativa",xlab= "Num_intentos",ylab = "Probabilidad" ,type='l',col = 'green')


#Distribucion Geometrica


j = 1

dist_geom <- c()

while(j<= n.repeticiones){
  
  geom_a  <-(1-probabilidad_exito)^(j-1)
  
  dist_geom[j] <- geom_a * probabilidad_exito
  
  j = j+1
}

plot(dist_geom,main="Distribucion Geometrica",xlab= "Num_intentos",ylab = "Probabilidad" ,type='h',col = 'blue')








