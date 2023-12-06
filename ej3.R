# Cargar bibliotecas
library(tidyverse)
library(car)
library(rstatix)

# Leer los datos
datos <- read_csv("flights.csv")

# Seleccionar las columnas price, time, distance
datos_seleccionados <- datos %>%
  select(price, time, distance)

# Reorganizar datos para los contrastes
datos_melted <- gather(datos_seleccionados, key = "variable", value = "value")

# Contrastar homogeneidad de varianzas (Levene's test)
levene_test_result <- leveneTest(value ~ variable, data = datos_melted)

# Contrastar igualdad de medianas (Kruskal-Wallis)
kruskal_test_result <- kruskal.test(value ~ variable, data = datos_melted)

# Contrastar igualdad de medias (ANOVA)
anova_test_result <- aov(value ~ variable, data = datos_melted)

# Imprimir resultados
print("Levene's Test:")
print(levene_test_result)

print("Kruskal-Wallis Test:")
print(kruskal_test_result)

print("ANOVA:")
print(summary(anova_test_result))

#Analisis de resultados:
#Contraste de Levene:
#La prueba de Levene para la homogeneidad de varianza muestra un valor p muy pequeño (p < 2.2e-16), indicando que hay evidencia significativa para rechazar la hipótesis nula de igualdad de varianzas entre los grupos. Esto sugiere que las varianzas no son iguales entre los grupos.

#Contraste de Kruskal-Wallis:
#La prueba de Kruskal-Wallis arroja un valor p extremadamente pequeño (p < 2.2e-16), lo que sugiere que hay diferencias significativas en las medianas entre los grupos. Este resultado indica que al menos un grupo tiene una mediana diferente de los demás.

#Contraste de ANOVA:
#La prueba de ANOVA también muestra un valor p muy pequeño (p < 2e-16), indicando que hay diferencias significativas en las medias entre los grupos. Esto concuerda con los resultados de Kruskal-Wallis y refuerza la idea de que al menos un grupo tiene una media diferente.