#-----------------------------------Pregunta 1---------------------------------
# Se realizó un ensayo clínico para estudiar la toxina botulínica, una toxina 
# muy potente que puede ser usado como medicamento en dosis diminutas, como 
# posible tratamiento para el dolor de espalda crónico. Un total de 31 pacientes
# participaron del estudio, de los cuales 15 fueron asignados aleatoriamente al
# grupo de tratamiento y los otros 16 al grupo de control (placebo). Luego de 
# ocho semanas, 9 personas voluntarias del grupo de tratamiento reportaron 
# alivio del dolor, mientras que 5 personas lo hicieron en el grupo de control.
# ¿Qué se podría decir del tratamiento?

library(ggpubr)
#H0:Las variables son independientes.
#Ha:Las variables estan relacionadas.

#se crea la tabla de datos
tratamiento  <- c(9,6)
control <- c(5,11)
tabla  <- as.table(rbind(tratamiento , control))
dimnames(tabla) <- list(tipo = c("Tratamiento", "Control"),
                        ayuda = c("Alivio", "Sin alivio"))
print(tabla)
#Se aplica la prueba exacta de Fisher.
alfa  <- 0.05
prueba  <- fisher.test(tabla , 1-alfa)
print(prueba)

#Entonces, considerando un alfa=0.05 y como p es 0.1556, con alfa<p se 
#falla al rechazar la hipotesis nula. Entonces, se puede decir con un
#95% de confianza  que no hay una asociaci?n estad?sticamente significativa
#entre la cantidad de personas con alivio del dolor de espalda y el tratamiento.

#---------------------------------Pregunta 2------------------------------------

# La escuela de psicología quiere evaluar el impacto de una intervención que han
# diseñado para ayudar a dejar de fumar. Con este fin, reclutaron 25 personas 
# fumadoras que tenían la intención de dejar de hacerlo y 25 personas fumadoras 
# que no consideraban esta opción. Dos semanas después de finalizada la
# intervención (que consistía en mirar videos emotivos que mostraban el impacto
# que las muertes por cáncer asociado al cigarrillo tenía en las familias, 
# seguidas de rondas de conversación), se les preguntó a las y los participantes
# si tenían intención de intentar dejar de fumar. 2 personas que tenían la 
# intención de hacerlo antes de la intervención, cambiaron de opinión y ya no 
# quieren intentarlo, mientras que 11 participantes que no pensaban en dejar de
# fumar, ahora lo estaban considerando. ¿Qué se puede decir del impacto de
# la intervención?

#H0:No hay cambios significativos en las respuestas.
#Ha:Si hay cambios significativos en las respuestas.

# Construir la tabla de contingencia
persona <- seq(1:50)
antes_intervencion <- c(rep("Dejar de fumar", 25), rep("Seguir fumando", 25))
despues_intervencion <- c(rep("Seguir fumando", 2), rep("Dejar de fumar", 34), rep("Seguir fumando", 14))
datos <- data.frame(persona , despues_intervencion, antes_intervencion)
tabla_fumadores <- table(despues_intervencion, antes_intervencion)
print(tabla_fumadores)

# Aplicar prueba de Mcnemar
prueba_fumadores <- mcnemar.test(tabla_fumadores)
print(prueba_fumadores)

# Se obtiene que el valor p es igual a 0.0265, por lo que se rechaza la hipotesis
# nula a favor de la hipotesis alternativa (para un nivel de significacion α=0.05)
# Es por esto que hay una diferencia en el desempeño de ambos clasificadores y,
# en consecuencia, existe un impacto antes y despues de la intervencion con las 
# personas fumadoras

#---------------------------------Pregunta 3------------------------------------

# Un grupo de activistas ha denunciado racismo en la conformación de los jurados
# de un pequeño condado en Texas, EE.UU. Su denuncia se basa que, según ellos, 
# las proporciones raciales de las personas seleccionadas para ser jurado el año
# pasado (208 blancos, 28 negros, 20 latinos y 19 de otras razas) no se 
# corresponde con las proporciones reportadas en el último censo (72% adultos 
# blancos, 7% adultos negros, 12% adultos latinos y 9% adultos que se declaran 
# de otras raza). ¿Tienen razón los denunciantes?

# Hipotesis nula: las proporciones raciales de las personas seleccionadas para 
# ser jurado son las mismas
# Hip. alternativa: las proporciones raciales de las personas seleccionadas para
# ser jurado son diferentes

# Se crea un dataframe con los datos entregados por el problema
nomina_razas <- data.frame ("raza" = c("Blancos","Negros","Latinos","Otros"),
                            "nomina" = c(208, 28, 20, 19),
                            "proporciones_esperadas" = c(0.72, 0.07, 0.12, 0.09))
print(nomina_razas)

# Se comprueba si las proporciones esperadas coinciden 
total_razas <- nomina_razas$nomina[1] +
               nomina_razas$nomina[2] +
               nomina_razas$nomina[3] +
               nomina_razas$nomina[4] 
proporciones <- round(nomina_razas$nomina / total_razas, 3)
print(proporciones)

# Hacer prueba chi-cuadrado
prueba_bondad_ajuste <- chisq.test(x = nomina_razas$nomina,
                                   p = nomina_razas$proporciones_esperadas,
                                   correct = TRUE)
print(prueba_bondad_ajuste)

# Tras realizar la prueba de bondad de ajuste de chi cuadrado, el valor p
# resultante es p=0.01, por lo que se rechaza la hipotesis nula a favor de la
# hipotesis alternativa con un nivel de significacion α=0.05. En consecuencia,
# podemos concluir con 95% de confianza de que las proporciones raciales de las
# personas seleccionadas para ser jurado son diferentes

# Ademas, podemos darnos cuenta de que los valores observados, no son muy 
# similares a los valores esperados. Por lo tanto, se puede decir que los 
# denunciantes tienen razón, ya que las proporciones reportadas en el último
# censo no corresponden con las proporciones de las personas seleccionadas para
# ser jurados

#---------------------------------Pregunta 4------------------------------------

# Enuncie un ejemplo novedoso (no discutido en clase ni en las lecturas) 
# relacionado con las expectativas de los chilenos de ir al mundial de Catar, a 
# la luz de los resultados de la selección nacional de fútbol en la jornada 
# clasificatoria triple de octubre 2021, que requiera utilizar una prueba Q de 
# Cochran. Identifique las variables involucradas y las hipótesis a contrastar.