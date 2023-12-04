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
# Intervalos de confianza al 95% para medias y varianzas

n_price <- length(columnas$price)
t_critical_price <- qt(1 - 0.025, df = n_price - 1, lower.tail = FALSE)

ci_mean_price <- c(mediap - t_critical_price * sqrt(cuasivarianzap / n_price), mediap + t_critical_price * sqrt(cuasivarianzap / n_price))
ci_var_price <- c((n_price - 1) * cuasivarianzap / qchisq(1 - 0.025, df = n_price - 1), (n_price - 1) * cuasivarianzap / qchisq(0.025, df = n_price - 1))

n_distance <- length(columnas$distance)
t_critical_distance <- qt(1 - 0.025, df = n_distance - 1, lower.tail = FALSE)

ci_mean_distance <- c(mediad - t_critical_distance * sqrt(cuasivarianzad / n_distance), mediad + t_critical_distance * sqrt(cuasivarianzad / n_distance))
ci_var_distance <- c((n_distance - 1) * cuasivarianzad / qchisq(1 - 0.025, df = n_distance - 1), (n_distance - 1) * cuasivarianzad / qchisq(0.025, df = n_distance - 1))

# Diferencia de medias y cociente de varianzas
mean_difference <- mediap - mediad
se_difference <- sqrt(cuasivarianzap / n_price + cuasivarianzad / n_distance)
t_critical_difference <- qt(1 - 0.025, df = n_price + n_distance - 2, lower.tail = FALSE)

ci_mean_difference <- c(mean_difference - t_critical_difference * se_difference, mean_difference + t_critical_difference * se_difference)

var_ratio <- cuasivarianzap / cuasivarianzad
ci_var_ratio <- c(var_ratio / qf(1 - 0.025, df1 = n_price - 1, df2 = n_distance - 1), var_ratio / qf(0.025, df1 = n_price - 1, df2 = n_distance - 1))

# Print the results
cat("Intervalo de confianza al 95% para la media de los precios:", ci_mean_price, "\n")
cat("Intervalo de confianza al 95% para la varianza de los precios:", ci_var_price, "\n")

cat("Intervalo de confianza al 95% para la media de la distancia:", ci_mean_distance, "\n")
cat("Intervalo de confianza al 95% para la varianza de la distancia:", ci_var_distance, "\n")

cat("Intervalo de confianza al 95% para la diferencia de medias:", ci_mean_difference, "\n")
cat("Intervalo de confianza al 95% para el cociente de varianzas:", ci_var_ratio, "\n")

#Tests No Paramétricos:
#a) Para una sola muestra:
# Hipótesis nula: La mediana de 'price' es igual a un valor específico (por ejemplo, 0)
wilcox.test(columnas$price, mu = 0)

# Hipótesis nula: La mediana de 'distance' es igual a un valor específico (por ejemplo, 0)
wilcox.test(columnas$distance, mu = 0)

#Prueba de Wilcoxon para 'price': La hipótesis nula sería que la mediana de 'price' es igual a 0.
#Prueba de Wilcoxon para 'distance': La hipótesis nula sería que la mediana de 'distance' es igual a 0.

#b) Para dos muestras:
# Hipótesis nula: Las medianas de 'price' y 'distance' son iguales
wilcox.test(columnas$price, columnas$distance)

#Prueba de Wilcoxon para 'price' y 'distance': La hipótesis nula sería que las medianas de 'price' y 'distance' son iguales.

#Tests Paramétricos:
#a) Para una sola muestra:
# Hipótesis nula: La media de 'price' es igual a un valor específico (por ejemplo, 0)
t.test(columnas$price, mu = 0)

# Hipótesis nula: La media de 'distance' es igual a un valor específico (por ejemplo, 0)
t.test(columnas$distance, mu = 0)

#Prueba t para 'price': La hipótesis nula sería que la media de 'price' es igual a 0.
#Prueba t para 'distance': La hipótesis nula sería que la media de 'distance' es igual a 0.

#b) Para dos muestras:
# Hipótesis nula: Las medias de 'price' y 'distance' son iguales
t.test(columnas$price, columnas$distance)
# Hipótesis nula: La diferencia media entre 'price' y 'distance' es igual a 0
t.test(columnas$price, columnas$distance, paired = TRUE)

#Prueba t para 'price' y 'distance': La hipótesis nula sería que las medias de 'price' y 'distance' son iguales.
#Prueba t para la diferencia entre 'price' y 'distance': La hipótesis nula sería que la diferencia media entre 'price' y 'distance' es igual a 0 (prueba pareada).

# Interpretación de los resultados de tests no paramétricos
cat("Test No Paramétrico para 'price': p-value =", wilcox.test(columnas$price, mu = 0)$p.value, "\n")
cat("Test No Paramétrico para 'distance': p-value =", wilcox.test(columnas$distance, mu = 0)$p.value, "\n")
cat("Test No Paramétrico para 'price' y 'distance': p-value =", wilcox.test(columnas$price, columnas$distance)$p.value, "\n")

# Interpretación de los resultados de tests paramétricos
cat("Test Paramétrico para 'price': p-value =", t.test(columnas$price, mu = 0)$p.value, "\n")
cat("Test Paramétrico para 'distance': p-value =", t.test(columnas$distance, mu = 0)$p.value, "\n")
cat("Test Paramétrico para 'price' y 'distance': p-value =", t.test(columnas$price, columnas$distance)$p.value, "\n")
cat("Test Paramétrico para la diferencia entre 'price' y 'distance': p-value =", t.test(columnas$price, columnas$distance, paired = TRUE)$p.value, "\n")


# Gráficos
# Histogramas
hist_price <- ggplot(columnas, aes(x = price)) + geom_histogram(fill = "blue", bins = 30) + ggtitle("Histograma de Precios")
hist_distance <- ggplot(columnas, aes(x = distance)) + geom_histogram(fill = "green", bins = 30) + ggtitle("Histograma de Distancias")

# Imprimir gráficos
print(hist_price)
print(hist_distance)


# Gráficos
# Barras para medias
bar_medias <- ggplot(data = data.frame(variable = c("Precio", "Distancia"), media = c(mediap, mediad)), aes(x = variable, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  labs(title = "Estimación puntual de medias",
       x = "Variable",
       y = "Media") +
  theme_minimal()

# Barras para cuasivarianzas
bar_cuasivarianzas <- ggplot(data = data.frame(variable = c("Precio", "Distancia"), cuasivarianza = c(cuasivarianzap, cuasivarianzad)), aes(x = variable, y = cuasivarianza, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  labs(title = "Estimación puntual de cuasivarianzas",
       x = "Variable",
       y = "Cuasivarianza") +
  theme_minimal()

# Imprimir gráficos
print(bar_medias)
print(bar_cuasivarianzas)


# Gráficos
# Intervalo de confianza para medias
ggplot(df_ci_mean_price, aes(x = variable, y = (ci_upper + ci_lower) / 2, ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6)) +
  labs(title = "Intervalo de Confianza al 95% para Medias",
       x = "Variable",
       y = "Media") +
  theme_minimal()

# Intervalo de confianza para varianzas
ggplot(df_ci_var_price, aes(x = variable, y = (ci_upper + ci_lower) / 2, ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6)) +
  labs(title = "Intervalo de Confianza al 95% para Varianzas",
       x = "Variable",
       y = "Varianza") +
  theme_minimal()

# Intervalo de confianza para diferencia de medias
ggplot(df_ci_mean_difference, aes(x = variable, y = (ci_upper + ci_lower) / 2, ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6)) +
  labs(title = "Intervalo de Confianza al 95% para Diferencia de Medias",
       x = "Variable",
       y = "Diferencia de Medias") +
  theme_minimal()

# Intervalo de confianza para cociente de varianzas
ggplot(df_ci_var_ratio, aes(x = variable, y = (ci_upper + ci_lower) / 2, ymin = ci_lower, ymax = ci_upper)) +
  geom_errorbar(width = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(position = position_dodge(width = 0.6)) +
  labs(title = "Intervalo de Confianza al 95% para Cociente de Var






