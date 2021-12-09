# Se importa la libreria pwr
library(ggpubr)
library(pwr)
#---------------------------------Enunciado-------------------------------------

# Los siguientes datos vienen de un estudio hecho por Rothman & Keller (1972) 
# (Journal of chronic diseases, 25(12), 711-716) sobre la incidencia de la 
# cantidad de alcohol y de tabaco que se consume en el riesgo de padecer cáncer
# oral. Las tablas muestran el número de personas que consumiendo una cierta 
# cantidad de alcohol o de tabaco, mostrada en cada fila, desarrollaron o no 
# desarrollaron (controles) la enfermedad durante su vida.

#---------------------------------Pregunta 1------------------------------------

# Estudios previos habían determinado que la incidencia de cáncer oral en la 
# población general que bebe regularmente entre 10 y 44 ml de alcohol era de 
# 60%. 
# ¿Respaldan estos datos tal estimación?

# Hipótesis nula: la incidencia de cancer oral en la población que bebe alcohol 
# entre  10 y 44 ml es 60%
# Hip. alternativa: la incidencia de cancer oral en la población que bebe 
# alcohol entre  10 y 44 ml no es 60%

# Se crea un dataframe con los datos entregados por el problema
datos_alcohol <- data.frame ("ml al dia" = c("0","1-9","10-44","45+"),
                             "Casos Cancer Oral" = c(43,89,109,242),
                             "Controles" = c(108,141,91,107))
print (datos_alcohol)

# Se identifican el total de casos de cancer para las distintas cantidades
# de alcohol (ml por día)
total_casos_cancer <- datos_alcohol$Casos.Cancer.Oral[1] + 
                      datos_alcohol$Casos.Cancer.Oral[2] +
                      datos_alcohol$Casos.Cancer.Oral[3] +
                      datos_alcohol$Casos.Cancer.Oral[4]


# Se define el alfa
alfa <- 0.05
# Valor nulo correspondiente al 60%
Valor_nulo <- 0.6

# Se define el éxito como la cantidad de casos con cancer oral para la población
# general que bebe regularmente entre 10 y 44 ml de alcohol

exito1044 <- datos_alcohol$Casos.Cancer.Oral[3]
total1044 <- datos_alcohol$Casos.Cancer.Oral[3] + datos_alcohol$Controles[3]

# Se obtiene la probabilidad de éxito con respecto a dicha población
pexito1044 <- exito1044 / total1044


# Luego, se utilizan los datos anteriores para utilizar el método de Wilson con 
# la función prop.test

prueba <- prop.test(exito1044,
                    n = total1044,
                    p = Valor_nulo,
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba)

# Tenemos 95% de confianza de que la proporcion de poblacion que bebe alcohol 
# entre 10 y 44 ml se encuentra entre 47,33% y 61,49%

# Como p < alfa, se rechaza la hipótesis nula en favor de la hipótesis 
# alternativa, y se concluye con 95% de confianza que la incidencia de cancer 
# oral en la población que bebe alcohol entre  10 y 44 ml no es 60%


#---------------------------------Pregunta 2------------------------------------

# Según estos datos, ¿da lo mismo beber de 10 a 44 ml de alcohol diariamente que 
# hacerlo con 45 o más ml?

# Se cumplen las condiciones para operar bajo el modelo normal


# Para tener el n total, se suman los casos de cancer oral y los controles
# Las personas que beben entre 10 y 44 ml son 200
# Las personas que beben 45 o mas ml son 349

# Hipótesis nula: La proporción de personas que bebe entre 10 a 44 ml que 
# desarrollan cancer oral es la misma que los que beben 45 o mas ml
# Hipótesis alternativa: La proporción de personas que bebe entre 10 a 44 ml que
# desarrollan cancer oral no es la misma que los que beben 45 o mas ml


exito45 <- datos_alcohol$Casos.Cancer.Oral[4] 
total45 <- datos_alcohol$Casos.Cancer.Oral[4] + datos_alcohol$Controles[4]

n <- c(c(total1044, total45))

# Fijar valores conocidos
exitos <- c(exito1044, exito45)
alfa <- 0.05
Valor_nulo <- 0


prueba <- prop.test(exitos,
                    n = n,
                    alternative = "two.sided",
                    conf.level = 1 - alfa)

print(prueba)

# Como en el rango del intervalo de confianza no entra el valor 0 no da lo mismo
# beber de 10 a 44 ml de alcohol diariamente que hacerlo con 45 o más ml

# El porcentaje del grupo de personas que beben entre 10 y 44 ml que desarrollan
# cancer oral es 54.5%
# El porcentaje del grupo de personas que beben entre 45 o mas ml que 
# desarrollan cancer oral es 69.3%

#---------------------------------Pregunta 3------------------------------------

# Suponiendo que la diferencia en la proporción de personas que desarrollan la 
# enfermedad entre quienes beben de 10 a 44 ml de alcohol por día y aquellos que
# beben 45 o más ml al día es de 0.20. ¿Cuánta gente deberíamos monitorear para
# obtener un intervalo de confianza del 99% y poder estadístico de 85%? si se 
# intente mantener aproximadamente la misma proporción de gente estudiada en 
# cada caso.

# Dado el enunciado, se define el poder y el nivel de significación establecidos

alfa <- 0.01
poder <- 0.85

# Hipótesis nula: La cantidad de gente que se debe monitorear es la misma que la
# del experimento realizado
# Hipótesis alternativa: La cantidad de gente que se debe monitorear es distinta
# que la del experimento realizado

# Para resolver esta pregunta, primero se debe obtener la diferencia entre las
# proporciones para los datos que tenemos actualmente

# Como ya se tiene la probabilidad de "exito" para cuando una persona bebe de 
# entre 10 a 44 ml de alcohol al día, Se obtiene sólo la probabilidad de que una
# persona desarrolle la enfermedad bebiendo 45 o más ml de alcohol al día

exito45 <- datos_alcohol$Casos.Cancer.Oral[4] 
total45 <- datos_alcohol$Casos.Cancer.Oral[4] + datos_alcohol$Controles[4]

pexito45 <- exito45 / total45

# Como se quiere que el poder sea 85 con un nivel de significación 0.01, y 
# manteniendo la proporción entre los n, se realizó la prueba pwr para pruebas 
# con 2 proporciones y muestras de distinto tamaño, en la cual se fue aumentando
# de manera proporcional el tamaño de las muestras hasta conseguir el poder 
# esperado (85%).

prueba1 <- pwr.2p2n.test(h = 0.2,
                       n1 = total1044,
                       n2 = total45,
                       sig.level = alfa,
                       power = NULL,
                       alternative = "two.sided")

prueba2 <- pwr.2p2n.test(h = 0.2,
                         n1 = (total1044 * 2),
                         n2 = (total45 * 2),
                         sig.level = alfa,
                         power = NULL,
                         alternative = "two.sided")

prueba3 <- pwr.2p2n.test(h = 0.2,
                         n1 = (total1044 * 3),
                         n2 = (total45 * 3),
                         sig.level = alfa,
                         power = NULL,
                         alternative = "two.sided")


print(prueba1)
print(prueba2)
print(prueba3)

# Finalmente, observando los resultados de las 3 pruebas, nos damos cuenta que
# para obtener los valores deseados (alfa = 0.01, poder = 85%), se deberían 
# monitorear 600 personas con cáncer por beber de entre 10 a 44 ml de alcohol 
# por día, y 747 personas con cáncer por beber 45 o más ml de alcohol al día.
