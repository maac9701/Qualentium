# -----------------------------------------------
# Análisis de la serie temporal: AirPassengers
# -----------------------------------------------

# 1. Cargar dataset y explorar su estructura
data("AirPassengers")  # Dataset incluido en R

# Verificar clase y resumen
print(class(AirPassengers))        # Debe ser "ts"
print(summary(AirPassengers))      # Estadísticas básicas
print(start(AirPassengers))        # Año de inicio
print(end(AirPassengers))          # Año de fin
print(frequency(AirPassengers))    # Frecuencia mensual = 12

# 2. Exploración inicial
plot(AirPassengers,
     main = "Serie temporal: Número de pasajeros aéreos (1949–1960)",
     ylab = "Pasajeros (en miles)", col = "steelblue", lwd = 2)

# Estadísticas descriptivas
media <- mean(AirPassengers)
desviacion <- sd(AirPassengers)
cat("Media:", media, "\n")
cat("Desviación estándar:", desviacion, "\n")

# 3. Análisis de tendencia y estacionalidad
descomposicion <- decompose(AirPassengers)
plot(descomposicion, col = "darkred")

# 4. Análisis de estacionariedad
# Instalar y cargar el paquete tseries si es necesario
if(!require(tseries)) install.packages("tseries")
library(tseries)

# Gráficos ACF y PACF
par(mfrow = c(1, 2))
acf(AirPassengers, main = "ACF - AirPassengers")
pacf(AirPassengers, main = "PACF - AirPassengers")

# Prueba de Dickey-Fuller aumentada
adf_test <- adf.test(AirPassengers)
print(adf_test)

# Diferenciación si no es estacionaria
diff_series <- diff(log(AirPassengers))  # Log para estabilizar varianza
par(mfrow = c(1, 1))
plot(diff_series,
     main = "Serie diferenciada (log)", ylab = "Diferencia log",
     col = "forestgreen", lwd = 2)

# Verificar estacionariedad después de la transformación
adf_test_diff <- adf.test(diff_series)
print(adf_test_diff)

# ACF y PACF después de diferenciar
par(mfrow = c(1, 2))
acf(diff_series, main = "ACF - Serie diferenciada")
pacf(diff_series, main = "PACF - Serie diferenciada")

# 5. Detección de valores atípicos
# Boxplot general
boxplot(AirPassengers,
        main = "Boxplot de la serie AirPassengers",
        ylab = "Pasajeros (en miles)",
        col = "skyblue")

# Destacar valores atípicos visualmente
outliers <- boxplot.stats(AirPassengers)$out
print("Valores atípicos:")
print(outliers)

# 6. Interpretación de resultados
cat("\nResumen interpretativo:\n")
cat("- Tendencia creciente clara hasta 1960.\n")
cat("- Fuerte estacionalidad anual (picos cada 12 meses).\n")
cat("- No estacionaria: se requiere transformación (log + diferencia).\n")
cat("- Aumento de la varianza a lo largo del tiempo.\n")
cat("- Algunos valores atípicos coinciden con picos estivales.\n")

