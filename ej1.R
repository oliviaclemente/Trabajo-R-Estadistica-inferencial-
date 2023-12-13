library(tidyverse)

#Selección de dos m.a.s.

datos <- read_csv("flights.csv")  #Read_csv lee el archivo elegido

columnas <- datos %>% 
  select(distance , price)  #Seleccionamos dos columnas

# Gráficos
# Histogramas
hist_price <- ggplot(columnas, aes(x = price)) + geom_histogram(fill = "pink", bins = 30) + ggtitle("Histograma de Precios")
hist_distance <- ggplot(columnas, aes(x = distance)) + geom_histogram(fill = "purple", bins = 30) + ggtitle("Histograma de Distancias")

# Imprimir gráficos
print(hist_price)
print(hist_distance)


#Estimación puntual de las medias y cuasivarianzas muestrales. Análisis de los resultados.

mediap <- mean(columnas$price)  #media de los precios
cuasivarianzap <- var(columnas$price)   #cuasivarianza de precios

mediad <- mean(columnas$distance)  #media de la distancia
cuasivarianzad <- var(columnas$distance)   #cuasivarianza de distancia

cat("Estimación puntual de la media de los precios:", mediap, "\n")
cat("Estimación puntual de la cuasivarianza de los precios:", cuasivarianzap, "\n")

cat("Estimación puntual de la media de la distancia:", mediad, "\n")
cat("Estimación puntual de la cuasivarianza de la distancia:", cuasivarianzad, "\n")

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

#Análisis de los resultados
#Hemos realizado una estimación puntual de las medias y cuasivarianzas muestrales para los precios y distancias de vuelos. 
#Los resultados revelan que la media de los precios es de aproximadamente 957.38, indicando el valor promedio en la muestra, mientras que la cuasivarianza es alta (131269.9), sugiriendo una variabilidad significativa en los precios. 
#En cuanto a las distancias, la media es de alrededor de 546.96, señalando el promedio en la muestra, y la cuasivarianza (43618.86) indica una variabilidad menor que en los precios


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

cat("Intervalo de confianza al 95% para la media de los precios:", ci_mean_price, "\n")
cat("Intervalo de confianza al 95% para la varianza de los precios:", ci_var_price, "\n")

cat("Intervalo de confianza al 95% para la media de la distancia:", ci_mean_distance, "\n")
cat("Intervalo de confianza al 95% para la varianza de la distancia:", ci_var_distance, "\n")

cat("Intervalo de confianza al 95% para la diferencia de medias:", ci_mean_difference, "\n")
cat("Intervalo de confianza al 95% para el cociente de varianzas:", ci_var_ratio, "\n")

##Análisis de los resultados
#Los intervalos de confianza al 95% proporcionan información esencial sobre diversos parámetros estadísticos en base a la muestra de datos.
#Para la distancia, el intervalo [43387.92, 43851.66] indica, con un 95% de confianza, la posible variabilidad en las distancias en la población.
#La diferencia de medias entre los precios y las distancias se encuentra en el intervalo [408.8476, 411.9914], permitiendo evaluar si hay una diferencia significativa entre ambas variables.
#El cociente de varianzas, con un intervalo de [2.986936, 3.032185], sugiere una posible disparidad significativa entre las varianzas de los precios y las distancias.
#En cuanto a los precios, el intervalo para la media es [958.7369, 956.0132], brindando una estimación precisa de la media poblacional. La varianza de los precios, en el rango [130574.9, 131970.5], indica la variabilidad de los precios en la población.
#Finalmente, el intervalo para la media de la distancia, [547.7406, 546.1705], ofrece una estimación precisa de la media poblacional de las distancias.


# Gráficos
# Gráfico para intervalos de confianza de medias
bar_ci_mean <- ggplot(data = data.frame(variable = c("Precio", "Distancia"), media = c(mediap, mediad)), aes(x = variable, y = media, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  labs(title = "Intervalo de Confianza al 95% para Medias",
       x = "Variable",
       y = "Media") +
  geom_errorbar(aes(ymin = c(ci_mean_price[1], ci_mean_distance[1]), ymax = c(ci_mean_price[2], ci_mean_distance[2])), position = position_dodge(0.5), width = 0.25) +
  theme_minimal()

# Gráfico para intervalos de confianza de varianzas
bar_ci_var <- ggplot(data = data.frame(variable = c("Precio", "Distancia"), cuasivarianza = c(cuasivarianzap, cuasivarianzad)), aes(x = variable, y = cuasivarianza, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  labs(title = "Intervalo de Confianza al 95% para Cuasivarianzas",
       x = "Variable",
       y = "Cuasivarianza") +
  geom_errorbar(aes(ymin = c(ci_var_price[1], ci_var_distance[1]), ymax = c(ci_var_price[2], ci_var_distance[2])), position = position_dodge(0.5), width = 0.25) +
  theme_minimal()

# Gráfico para intervalos de confianza de diferencia de medias y cociente de varianzas
bar_ci_difference_ratio <- ggplot(data = data.frame(variable = c("Diferencia de Medias", "Cociente de Varianzas"), valor = c(mean_difference, var_ratio)), aes(x = variable, y = valor)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.5) +
  labs(title = "Intervalo de Confianza al 95% para Diferencia de Medias y Cociente de Varianzas",
       x = "Variable",
       y = "Valor") +
  geom_errorbar(aes(ymin = c(ci_mean_difference[1], ci_var_ratio[1]), ymax = c(ci_mean_difference[2], ci_var_ratio[2])), position = position_dodge(0.5), width = 0.25) +
  theme_minimal()

# Imprimir gráficos
print(bar_ci_mean)
print(bar_ci_var)
print(bar_ci_difference_ratio)


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


#Interpretación de los resultados procedentes de los contrastes de hipótesis ycomparación de los resultados obtenidos en la es�mación por intervalos de confianza.
#Los resultados de las pruebas de hipótesis, con un nivel de significación del 5% (α = 0.05), revelan de manera contundente que las medianas y medias de las variables 'price' y 'distance' son estadísticamente diferentes de cero. Los valores p asociados, todos iguales a cero, proporcionan evidencia sólida para rechazar las hipótesis nulas en todas las pruebas, indicando disparidades significativas en las distribuciones de precios y distancias.
#Tanto las pruebas no paramétricas (Wilcoxon) como las paramétricas (t-test) respaldan la conclusión de que las muestras provienen de poblaciones con características distintas. Estos resultados ofrecen una perspectiva sobre las diferencias estadísticas entre precios y distancias en la muestra analizada.

#Gráficos
# Gráfico para tests no paramétricos de una variable
bar_plot_no_parametric <- ggplot(data = data.frame(Test = c("Price", "Distance", "Price vs Distance"), p_value = c(wilcox.test(columnas$price, mu = 0)$p.value, wilcox.test(columnas$distance, mu = 0)$p.value, wilcox.test(columnas$price, columnas$distance)$p.value)), aes(x = Test, y = -log10(p_value), fill = Test)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tests No Paramétricos",
       x = "Variable o Comparación",
       y = "-log10(p-value)") +
  theme_minimal()

# Gráfico para tests paramétricos de una variable
bar_plot_parametric_one_var <- ggplot(data = data.frame(Test = c("Price", "Distance"), p_value = c(t.test(columnas$price, mu = 0)$p.value, t.test(columnas$distance, mu = 0)$p.value)), aes(x = Test, y = -log10(p_value), fill = Test)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tests Paramétricos (Una Variable)",
       x = "Variable",
       y = "-log10(p-value)") +
  theme_minimal()

# Gráfico para tests paramétricos de dos variables
bar_plot_parametric_two_var <- ggplot(data = data.frame(Test = c("Price vs Distance", "Difference Price-Distance"), p_value = c(t.test(columnas$price, columnas$distance)$p.value, t.test(columnas$price, columnas$distance, paired = TRUE)$p.value)), aes(x = Test, y = -log10(p_value), fill = Test)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tests Paramétricos (Dos Variables)",
       x = "Comparación",
       y = "-log10(p-value)") +
  theme_minimal()

# Imprimir gráficos
print(bar_plot_no_parametric)
print(bar_plot_parametric_one_var)
print(bar_plot_parametric_two_var)


