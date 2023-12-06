library(tidyverse)

# Función para cargar datos
load_data <- function(file_path) {
  read_csv(file_path)
}

# Función para realizar estimación puntual
estimacion_puntual <- function(column) {
  media <- mean(column)
  cuasivarianza <- var(column)
  return(list(media = media, cuasivarianza = cuasivarianza))
}

# Función para realizar intervalos de confianza
intervalo_confianza <- function(column, level = 0.95) {
  n <- length(column)
  t_critical <- qt(1 - (1 - level) / 2, df = n - 1, lower.tail = FALSE)
  
  media <- mean(column)
  cuasivarianza <- var(column)
  
  ci_mean <- c(media - t_critical * sqrt(cuasivarianza / n), media + t_critical * sqrt(cuasivarianza / n))
  ci_var <- c((n - 1) * cuasivarianza / qchisq(1 - (1 - level) / 2, df = n - 1), (n - 1) * cuasivarianza / qchisq((1 - level) / 2, df = n - 1))
  
  return(list(ci_mean = ci_mean, ci_var = ci_var))
}

# Función para realizar pruebas no paramétricas
test_no_parametrico <- function(column, mu = 0) {
  return(wilcox.test(column, mu = mu))
}

# Función para realizar pruebas paramétricas
test_parametrico <- function(column, mu = 0, paired = FALSE) {
  if (paired) {
    return(t.test(column, mu = mu, paired = TRUE))
  } else {
    return(t.test(column, mu = mu))
  }
}

# Función para crear histograma
histograma <- function(column, color, bins = 30) {
  ggplot(column, aes(x = column)) + geom_histogram(fill = color, bins = bins) + ggtitle(paste("Histograma de", colnames(column)))
}

# Función para crear gráfico de barras
grafico_barras <- function(datos, variable, medida, titulo) {
  ggplot(data = datos, aes(x = variable, y = medida, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
    labs(title = titulo, x = "Variable", y = medida) +
    theme_minimal()
}

# Función para imprimir gráficos
imprimir_grafico <- function(grafico) {
  print(grafico)
}

# Ejemplo de uso
datos <- load_data("flights.csv")
columnas <- datos %>% select(distance, price)

# Estimación puntual de 'price'
resultado_estimacion_precio <- estimacion_puntual(columnas$price)
cat("Estimación puntual de la media de los precios:", resultado_estimacion_precio$media, "\n")
cat("Estimación puntual de la cuasivarianza de los precios:", resultado_estimacion_precio$cuasivarianza, "\n")

# Intervalo de confianza para 'price'
resultado_intervalo_precio <- intervalo_confianza(columnas$price)
cat("Intervalo de confianza al 95% para la media de los precios:", resultado_intervalo_precio$ci_mean, "\n")
cat("Intervalo de confianza al 95% para la varianza de los precios:", resultado_intervalo_precio$ci_var, "\n")

