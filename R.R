# Tamaño de muestra para estimar proporcion
nc <- 97 #ingresar porcentaje


# Tamaño de muestra para diferentes niveles de confianza
shiny::runGitHub(repo="semilleroApps", user="fhernanb", sub="samplesize")
#shiny::runGitHub(repo="semilleroApps", user="fhernanb", sub="goodFit_espanish")

# Qqnorm o qqplot para ver si datios son de distribucion normal
datos <- rnorm(100)  #ingresar datos
qqplot(qqnorm(datos)$x, datos, main = "Gráfico Q-Q")
# o con la linea
qqnorm(datos, main = "Gráfico Q-Q Personalizado", xlab = "Cuantiles teóricos", ylab = "Cuantiles observados")
abline(a = 0, b = 1, col = "red", lty = 2) 
# Prueba anderson darling
library(nortest)
contenido <- c(510, 492, 494, 498, 492, 496, 502, 491, 507, 496) #ingresar
ad.test(contenido) # si p es mayor a 0.05 bien, si es menor mal





# Intervalos de confianza
# IC para media con varianza conocida (poblacion con distribucion normal, muestra de 30 o mas valores)
porcentaje_IC <- 95 #ingresar
datos <- c(0.975, 0.95, 0.931, 1.103, 1.038, 0.920, 0.935, 0.907, 0.809) #ingresar
varianza <- 0.1 #ingresar
media_muestral <- sum(datos)/length(datos)
n <- length(datos)
nc <- (1-(porcentaje_IC/100))/2
zalfa <- qnorm(nc, lower.tail = FALSE)
izq <- media_muestral - zalfa * ((sqrt(varianza))/sqrt(n))
der <- media_muestral + zalfa * ((sqrt(varianza))/sqrt(n))
print(paste("Intervalo de la media = [", izq, ",", der, "]"))



# IC para la media con varianza desconocida (poblacion con dist Normal)
porcentaje_IC <- 90 #ingresar
datos <- c(175, 177, 180, 165, 170, 170, 181, 169, 165, 190, 170, 171) #ingresar
sd <- sd(datos)
media_muestral <- sum(datos)/length(datos)
n <- length(datos)
nc <- (1-(porcentaje_IC/100))/2
talfa <- qt(nc, n-1, lower.tail = FALSE)
izq <- media_muestral - talfa * (sd/sqrt(n))
der <- media_muestral + talfa * (sd/sqrt(n))
print(paste("Intervalo de la media = [", izq, ",", der, "]"))



# IC para la varianza poblacional (poblacion con dist Normal)
porcentaje_IC <- 95 #ingresar
datos <- c(6, 6.4, 7, 5.8, 6, 5.8, 5.9, 6.7, 6.1, 6.5, 6.3, 5.8) #ingresar
sd2 <- sd(datos)**2
n <- length(datos)
nc <- (1-(porcentaje_IC/100))/2
tchizq <- qchisq(nc, n-1, lower.tail = FALSE)
tchider <- qchisq(1-nc, n-1, lower.tail = FALSE)
izq <- ((n-1)*sd2)/tchizq
der <- ((n-1)*sd2)/tchider
print(paste("Intervalo de la varianza = [", izq, ",", der, "]"))



# IC para una proporcion P (variables cualitativas, np y n(1-p) >= 10, intvlo de wald)
porcentaje_IC <- 97 #ingresar
nTotal <- 700 #ingresar
nEvento_ocurrir <- 450 #ingresar
p <- nEvento_ocurrir/nTotal
if (nTotal * p  >= 10 && nTotal*(1-p) >= 10){"se puede"}
nc <- 1-(porcentaje_IC/100)
zalfa <- qnorm(nc/2, lower.tail = FALSE)
izq <- p - zalfa*sqrt((p*(1-p))/nTotal)
der <- p + zalfa*sqrt((p*(1-p))/nTotal)
print(paste("Intervalo de P = [", izq, ",", der, "]"))



# IC de wilson score para proporcion (se puede usar siempre)
porcentaje_IC <- 90 #ingresar
n <- 500 #ingresar
nEvento <- 275 #ingresar
p <- nEvento/n
nc <- 1-(porcentaje_IC/100)
z <- qnorm(nc/2, lower.tail = FALSE)
temp <- (p+((z**2)/(2*n)))/(1+((z**2)/n))
izq <- temp - (z/(1+z**2/n))*sqrt(((p*(1-p))/n)+z**2/(4*n**2))
der <- temp + (z/(1+z**2/n))*sqrt(((p*(1-p))/n)+z**2/(4*n**2))
print(paste("Intervalo de P = [", izq, ",", der, "]"))



#IC para cociente de varianzas (si las varianzas de dos conjuntos se parecen o no)
porcentaje_IC <- 90 #ingresar
c1 <- c(34, 36, 39, 31, 33, 26, 45, 34, 39, 38, 37) #ingresar
c2 <- c(33, 41, 39, 32, 29, 28, 33, 34, 25, 28, 36, 33, 35) #ingresar
nC1 <- length(c1)
s2C1 <- var(c1)
nC2 <- length(c2)
s2C2 <- var(c2)
nc <- 1-(porcentaje_IC/100)
falfa1 <- qf(nc/2, nC1-1, nC2-1, lower.tail = FALSE)
falfa2 <- qf(nc/2, nC2-1, nC1-1, lower.tail = FALSE)
izq <- (s2C1/s2C2) * (1/falfa1)
der <- (s2C1/s2C2) * falfa2
print(paste("Intervalo cociente de varianzas = [", izq, ",", der, "]")) #Si el 1 queda en el intervalo las varianzas no son tan diferentes, si nom una varianza es mas grande que la otra
