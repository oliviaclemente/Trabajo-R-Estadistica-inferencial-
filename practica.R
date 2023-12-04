datos <- read_csv("flights.csv")

columnad <- 'distance'
columnap <- 'price'

cold <- length(data[[columnad]])
colp <- length(data[[columnap]])

cat(paste("El tamaño de la columna '", columnad, "' es: ", cold, "\n"))
cat(paste("El tamaño de la columna '", columnad, "' es: ", colp, "\n"))
