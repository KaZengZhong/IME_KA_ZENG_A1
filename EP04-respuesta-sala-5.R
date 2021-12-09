# Un estudio, encargado por bomberos, investigó si los sistemas aspersores de prevención, que deben ser
# instalados en los edificios de más de 4 pisos construidos después de 2001, cumplen con la norma que les
# obliga a que el tiempo promedio de activación no sobrepase los 25 s. Con una serie de pruebas obtuvieron
# la siguiente muestra:
# 27 41 22 27 23 35 30 33 24 27 28 23 24
# El estudio concluyó que la norma no se estaba cumpliendo. ¿Sugieren los datos esta conclusión?

# Como la cantidad de muestras es pequeña se utilizará la prueba t de student
# Se puede asumir que las muestras son independentes entre sí

# Hipótesis nula:  El tiempo promedio de activación es igual a 25
# Hipótesis alternativa: El tiempo promedio de activación es mayor de 25

# Se importan librerias
library(ggpubr)

# Cargar conjunto de datos en la variable tiempo
texto <- "27 41 22 27 23 35 30 33 24 27 28 23 24"
file <- textConnection(texto)
tiempo <- scan(file) 

# Establecer los datos conocidos
n <- length(tiempo)
grados_libertad <- n - 1
valor_nulo <- 25

# Observar si los observaciones proviene de una distribución cercana a a la normal
g <- ggqqplot(data = data.frame(tiempo),
              x = "tiempo",
              color = "steelblue",
              xlab = "Teórico",
              ylab = "Muestra",
              title = "Gráfico Q-Q muestra v/s distr. normal")

# Mostrar el gráfico
print(g)
# Segun el gráfico, se puede afirmar que es una distribución cercana a la normal

# Par tener mayor certeza en los resultados se realizará el procedimiento paso y paso y con la función
# t.test()

# Determinar un nivel de significación
alfa <- 0.05

# Calcular estadístico de prueba
cat("Prueba t para una muestra\n\n")
media <- mean(tiempo)
cat("Media =", media, "\n")
desv_est <- sd(tiempo)
error <- desv_est / sqrt(n)
t <- (media - valor_nulo) / error
cat("t =", t, "\n")

# Calcular p
p <- pt(t, df = grados_libertad, lower.tail = FALSE)
cat("p =", p, "\n")

# Calcular intervalo de confianza
t_critico <- qt(alfa, df = grados_libertad, lower.tail = FALSE)
inferior <- media - t_critico*error
cat("Intervalo de confianza = ([", inferior, ", Inf )\n")

# Calcular lo anterior mediante la prueba t de Student 
prueba <- t.test(tiempo,
                 alternative = "greater",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

# Mostrar los resultados
print(prueba)

# Como el valor p < α la evidencia a favor de la hipótesis alternativa es mayor, por lo que se rechaza la 
# hipotesis nula en favor de la hipotesis alternativa. Ademas, segun el intervalo de confianza, tenemos
# un 95% de confianza de que todas las muestras sean mayor a 25.26259. Con esto, es evidencia suficiente
# para asegurar que el tiempo promedio de activación sí sobrepasa los 25s y, en consecuencia, el estudio
# estaba en lo correcto al concluír que la norma no se estaba cumpliendo.