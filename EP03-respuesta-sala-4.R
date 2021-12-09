#Integrantes:
#Felipe Carrasco
#Diego Armijo
#Vanessa Vidal
#Ka Zeng

#Se importan librerias 
library(ggpubr)
library(gtools)

#se lee el archivo Casen 2017
basename <- "Casen 2017 (4).csv"
file <- file.path("C:\\Users\\kahao\\Desktop\\USACH\\2-2021\\IME", basename)
población <- read.csv(file = file, fileEncoding = "UTF-8")
tamaño <- nrow(población)

#Definimos nuestra semilla y número de repeticiones
set.seed(200)
n.repeticiones <- 100

#Función de éxito para del sexo mujer
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)

#Se realizan treinta repeticiones
treinta.repeticiones <- sapply(1:n.repeticiones, ensayo)



#Distribución binomial

#Combinaciones totales
print(choose(n=100,k=5))

#Suma de exitos en el total de intentos
cuenta.exitos <- function(i) sum(sample(treinta.repeticiones, 5))
binomial <- sapply(1:100, cuenta.exitos)

#Gráfico distribución binomial
hist(binomial)



#Distribución geometrica

#Probabilidad de exito del sexo mujer
p <- cuenta.exitos()/n.repeticiones

#Formula
g <- function(j) ((1-p)^j-1)*p
geometrica <- sapply(1:100, g)

#Gráfico distribución geometrica
hist(geometrica)



#Distribución binomial negativa

#Probabilidad de obtener 15 exitos en 30 intentos
r <- 15

#Formula
z <- function(k) (factorial(k-1)/(factorial(r-1)*factorial(k-1-(r-1)))) * ((1-p)^k-r)*(p^r)
bnegativa <- sapply(1:100, z)

#Gráfico distribución binomial negativa
hist(bnegativa)



