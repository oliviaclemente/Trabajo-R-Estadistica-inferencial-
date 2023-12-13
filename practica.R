datos <- read_csv("flights.csv")

columnad <- 'distance'
columnap <- 'price'

cold <- length(data[[columnad]])
colp <- length(data[[columnap]])

cat(paste("El tamaño de la columna '", columnad, "' es: ", cold, "\n"))
cat(paste("El tamaño de la columna '", columnad, "' es: ", colp, "\n"))

columnad <- 'distance'
columnap <- 'price'

cold <- length(data[[columnad]])
colp <- length(data[[columnap]])

cat(paste("El tamaño de la columna '", columnad, "' es: ", cold, "\n"))
cat(paste("El tamaño de la columna '", columnad, "' es: ", colp, "\n"))



ICmediap <- t.test(columnas$price)$conf.int   #$conf.int usado para acceder al intervalo de confianza resultante
ICmediad <- t.test(columnas$distance)$conf.int

ICvarp <- var.test(columnas$price)$conf.int
ICvard <- var.test(columnas$distance)$conf.int

ICmedias <- t.test(columnas$price, columnas$distance)$conf.int
ICvars <- var.test(columnas$price, columnas$distance, ratio=1)$conf.int

cat("Intervalo de confianza al 95% de la media poblacional del percio es", ICmediap, "\n")
cat("Intervalo de confianza al 95% de la cuasivarianza del precio es", ICvarp, "\n")
