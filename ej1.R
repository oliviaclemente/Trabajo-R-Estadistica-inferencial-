library(tidyverse)

#Selección de dos m.a.s.

datos <- read_csv("flights.csv")  #Read_csv lee el archivo elegido

columnas <- datos %>% 
  select(distance , price)  #Seleccionamos dos columnas

#Estimación puntual de las medias y cuasivarianzas muestrales. Análisis de los resultados.

mediap <- mean(columnas$price)  #media de los precios
cuasivarianzap <- var(columnas$price)   #cuasivarianza de precios

mediad <- mean(columnas$distance)  #media de la distancia
cuasivarianzad <- var(columnas$distance)   #cuasivarianza de distancia

cat("Estimación puntual de la media de los precios:", mediap, "\n")
cat("Estimación puntual de la cuasivarianza de los precios:", cuasivarianzap, "\n")

cat("Estimación puntual de la media de la distancia:", mediad, "\n")
cat("Estimación puntual de la cuasivarianza de la distancia:", cuasivarianzad, "\n")

#Podemos observar que la media de precios es mayor a la media de distancia y por lo tanto la cuasivarianza de precios también es la mayor

#Estimación por intervalos de confianza al 95% de las medias poblacionales, varianzaspoblacionales, diferencia de medias poblacionales y cociente de varianzaspoblacionales. Análisis de los resultados

ICmediap <- t.test(columnas$price)$conf.int   #$conf.int usado para acceder al intervalo de confianza resultante
ICmediad <- t.test(columnas$distance)$conf.int

ICvarp <- var.test(columnas$price)$conf.int
ICvard <- var.test(columnas$distance)$conf.int

ICmedias <- t.test(columnas$price, columnas$distance)$conf.int
ICvars <- var.test(columnas$price, columnas$distance, ratio=1)$conf.int

cat("Intervalo de confianza al 95% de la media poblacional del percio es", ICmediap, "\n")
cat("Intervalo de confianza al 95% de la cuasivarianza del precio es", ICvarp, "\n")
