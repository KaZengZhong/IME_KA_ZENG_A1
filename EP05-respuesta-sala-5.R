# Se sabe que el proceso de fabricaci�n de barras de acero para concreto reforzado producen barras con
# medidas de dureza que siguen una distribuci�n normal con desviaci�n est�ndar de 10 kilogramos de
# fuerza por mil�metro cuadrado. Usando una muestra aleatoria de tama�o 50, un ingeniero quiere averiguar
# si una l�nea de producci�n est� generando barras con dureza media de 170 [kgf mm-2] 

#--------------------------------------------Pregunta 1-----------------------------------------------
# Si el ingeniero est� seguro que la verdadera dureza media no puede ser menor a los 170 [kgf mm-2] y
# piensa rechazar la hip�tesis nula cuando la muestra presente una media mayor a 174 [kgf mm-2], �cu�l es
# la probabilidad de que cometa un error de tipo 1?

# Nota: Error tipo I: rechazar H0 en favor de HA cuando H0 es en realidad verdadera.
#       Error tipo II: no rechazar H0 en favor de HA cuando HA es en realidad verdadera.

#HIPOTESIS
# Hipotesis nula: ?? = ??0, implica ?? = 170 [kgf mm-2]
# Hipotesis alternativa: ?? ??? ??0, implica ?? ??? 170 [kgf mm-2]

library(ggpubr)
library(pwr)
# Fijar  valores  conocidos.
s  <- 10
n <- 50
# Calcular  el error  est�ndar.
SE <- s / sqrt(n)

#media nula
media_nula <- (174 + 170)/2 #172

# Entonces para obtener cual es la probabilidad de que se cometa un error de tipo 1, es necesario obtener
# el valor de alfa, en este caso los parametros que podrian afectar para que se cometa un error de tipo 1
# son cuando la media de la dureza de la barra esta bajo de 170 y cuando esta sobre los 174, para ello se utilizara
# la funcion pnorm() intercambiando el valor de lower.tail por false o true segun corresponda.

alfa <- pnorm (170,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (174,mean = media_nula ,sd = SE,lower.tail = FALSE )
print(alfa)

# para verificar que el alfa obtenido es el correcto se verifican los qcriticos teoricos mediante la formula
# qcritico = mediaNula - o + zcritico, entonces calculamos el zcritico con qnorm()

# media nula para el calculo del z_critico para la region de rechazo
media_nula0 <- 0

Z_critico  <- qnorm(alfa/2, mean = media_nula0 , sd = SE , lower.tail = FALSE)
print(Z_critico)

# Entonces ahora se procede a calcular los qcriticos teoricos para comprobar si son iguales a los experimentales
q_critico_inferior<- media_nula - Z_critico
print(q_critico_inferior)
q_critico_superior<- media_nula + Z_critico
print(q_critico_superior)
# ya comprobado el resultado de los qcriticos se puede concluir que la probabilidad de que ocurra un error 
# de tipo 1 es de 0.1572992 o 15,72992%.

# -----------------------------------Pregunta 2---------------------------------------------------------

# Si la verdadera dureza media de la l�nea de producci�n fuera 173 [kgf mm-2], �cu�l ser�a la probabilidad de
# que el ingeniero, que obviamente no conoce este dato, cometa un error de tipo 2? 
# Calcular  el poder de  acuerdo  al an�lisis  te�rico.
media_efecto <- 173
# Fijar  valores  conocidos.
s  <- 10
n <- 50
# Calcular  el error  est�ndar.
SE <- s / sqrt(n)
poder  <- pnorm(170 ,
                mean = media_efecto ,
                sd = SE ,
                lower.tail = TRUE)
+ pnorm(174 ,
        mean = media_efecto ,
        sd = SE,
        lower.tail =   FALSE)
cat("Poder = ", poder , "\n")
# Calcular  la  probabilidad  de  cometer  un error  tipo II.
beta  <- 1 - poder
cat("Beta = ", beta , "\n")
# La probabilidad de obtener un error de tipo 2 es beta = 0.9830526 o beta= 98,30526%, entonces podemos decir
# que en un 98% aprox. el ingeniero cometera un error de tipo 2 debido a que la verdadera dureza media de la 
# linea de produccion es 173 sin que el lo sepa.

#---------------------------------------Pregunta 3--------------------------------------------------------

# Como no se conoce la verdadera dureza media, genere un gr�fico del poder estad�stico con las
# condiciones anteriores, pero suponiendo que las verdaderas durezas medias podr�an variar de 170 a 178
# [kgf mm-2].

library(ggpubr)

# Fijar  valores  conocidos.
n <- 50
s <- 10
SE <- s / sqrt(n)

media_nula<- (178+170)/2
efecto <- seq(170, 178, 0.01)

alfa <- pnorm (170,mean = media_nula,sd = SE,lower.tail = TRUE ) + 
  pnorm (178,mean = media_nula ,sd = SE,lower.tail = FALSE )
# Calcular  el poder  usando  la funci�n power.t.test().
cat("C�lculo  del  poder  con  power.t.test()\n")
resultado  <- power.t.test(n = n,
                           delta = media_nula - efecto ,
                           sd = SE ,
                           sig.level = alfa ,
                           power = NULL ,
                           type = "one.sample",
                           alternative = "two.sided")
poder<-resultado[["power"]]

# Crear un data frame
datos <- data.frame(efecto, poder)

# Graficar la curva de poder
g <- ggplot(datos, aes(efecto, poder))
g <- g + geom_line(colour = "blue")
g <- g + ylab("Poder estadistico")
g <- g + xlab("Dureza media")
g <- g + theme_pubr()
g <- g + ggtitle("Grafico del poder estadistico")

print(g)

# Grafico generado relacionando el poder estadistico con las durezas medias tomando como media nula 174
# A medida que se aleja de este valor va creciendo de manera bilateral en forma de curva hasta llegar a 1

#---------------------------------------Pregunta 4----------------------------------------------------

# �Cu�ntas barras deber�an revisarse para conseguir un poder estad�stico de 0,8 y un nivel de significaci�n
# de 0,05? 
library(pwr)
poder <- 0.8
alfa <- 0.05
diferencia <- 3 # usando de dureza media de la linea de produccion igual a 170 y a 173 por el caso de pregunta 2
desv_est<-10
resultado  <- power.t.test(n = NULL ,
                           delta = diferencia,
                           sd = desv_est ,
                           sig.level = alfa ,
                           power = poder ,
                           type = "one.sample",
                           alternative = "two.sided")
n <- ceiling(resultado [["n"]])
cat("n = ", n, "\n")

# Entonces con los valores dados se necesitarian 90 barras para conseguir un poder estadistico de 0,8 y un alfa =0,05.

#---------------------------------------Pregunta 5---------------------------------------------------

#�Y si quisiera ser bien exigente y bajar la probabilidad de cometer un error de tipo 1 a un 1% solamente? 

# Para que se cumpla la condicion anterior alfa tiene que ser igual a 0,01
library(pwr)
poder <- 0.8
alfa <- 0.01
diferencia <- 3 # usando de dureza media de la linea de produccion igual a 170 y a 173 por el caso de pregunta 2
desv_est<-10
resultado  <- power.t.test(n = NULL ,
                           delta = diferencia,
                           sd = desv_est ,
                           sig.level = alfa ,
                           power = poder ,
                           type = "one.sample",
                           alternative = "two.sided")
n <- ceiling(resultado [["n"]])
cat("n = ", n, "\n")

# Entonces al disminuir el valor del alfa o dicho de otra forma al bajar la probabilidad de cometer un error
# de tipo 1 implica que la muestra aumenta su numero, por lo que se podria concluir que a mayor tama�o de muestra
# menor sera el porcentaje de error de tipo 1.