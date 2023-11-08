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
print(paste("Intervalo de la media = [", izq, ",", der, "]"))