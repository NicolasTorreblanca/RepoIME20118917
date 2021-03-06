#EP06 IME
#Esteban Arenas
#Nicolas Torreblanca 

# 1. Estudios previos hab�an determinado que la 
# proporci�n de autoras en la especialidad de 
# dermatolog�a era de 61%. 
# �Respaldan estos datos tal estimaci�n?

#H0: La proporci�n de autoras en la especialidad 
# dermatolog�a es de 61%
# p = 0.61

#HA: La proporci�n de autoras en la especialidad 
# dermatolog�a es menor a 61%
# p < 0.61

# Dado que la fuente de los datos es un articulo
# cient�fico se asume que es confiable y que los 
# datos entregados son independientes y aleatorios.

# Fijar valores conocidos
p_exito <- 0.461
n_derma <- 76
alfa <- 0.05
valor_nulo <- 0.61

#Construccion del intervalo de confianza
error_estandar <- sqrt(p_exito*(1-p_exito)/n_derma)
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
sup <- p_exito + Z_critico * error_estandar
inf <- p_exito - Z_critico * error_estandar
print(inf)
print(sup)

#Prueba de Hip�tesis
error_est_hip <- sqrt(( valor_nulo * (1 - valor_nulo)) / n_derma)
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hip�tesis alternativa unilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)

# Dado que se obtuvo un valor p de 0.9961 el cual es mayor al alfa
# de 0.05 la evidencia no es suficiente para rechazar la 
# hip�tesis nula por lo que se concluye con 95% de confianza que 
# no es cierto que la proporci�n de autoras en la especialidad 
# dermatolog�a es menor a 61%

#2. Seg�n estos datos, �es igual la proporci�n
# de autoras en las �reas de oncolog�a y medicina 
#interna?

#H0: No hay diferencia entre la proporcion de autoras en las
#�reas de oncolog�a y medicina interna
# p1 - p2 = 0

#HA: Hay diferencia entre la proporcion de autoras en las
#�reas de oncolog�a y medicina interna
# p1 - p2 != 0

#Fijar valores conocidos
n_onco <- 106
n_medi <- 110

#Exitos 
exitos_onco <- 44
exitos_medi <- 45

# Probabilidades de exito
p_exitos_onco <- exitos_onco/n_onco
p_exitos_medi <- exitos_medi/n_medi

valor_nulo_2 <- 0

# Estimar diferencia
diferencia <- p_exitos_onco - p_exitos_medi

# Construcci�n de intervalo de confianza
error_onco <- (p_exitos_onco * (1 - p_exitos_onco)) / n_onco
error_medi <- (p_exitos_medi * (1 - p_exitos_medi)) / n_medi

error_est_2 <- sqrt(error_onco + error_medi)

z_critico_2 <- qnorm(alfa / 2, lower.tail = FALSE)

inf_2 <- diferencia - z_critico_2 * error_est_2
sup_2 <- diferencia + z_critico_2 * error_est_2


#Prueba de Hip�tesis
p_agrupada <- (exitos_medi+exitos_onco)/(n_medi+n_onco)

error_onco_agrup <- (p_agrupada * (1-p_agrupada)) / n_onco
error_medi_agrup <- (p_agrupada * (1-p_agrupada)) / n_medi

error_est_hip_2 <- sqrt(error_onco_agrup + error_medi_agrup)


Z_2 <- (diferencia - valor_nulo_2) / error_est_hip_2

p_2 <- 2 * pnorm(Z_2, lower.tail = FALSE)
  
cat("Hip�tesis alternativa bilateral\n")
cat("Z =", Z_2, "\n")
cat("p =", p_2)

#3. Suponiendo que la diferencia en la proporci�n 
#de autoras en la especialidad de obstetricia y 
#la de neurolog�a es de 0,19. �A cu�ntos autores 
#deber�amos monitorear para obtener un intervalo de 
#confianza del 97,5% y poder estad�stico de 75%, 
#si se intenta mantener aproximadamente la misma 
#proporci�n de gente estudiada en cada caso?

