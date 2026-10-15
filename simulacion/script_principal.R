#### Ejercicio Simulación 1: Demanda con preferencias Cobb-Douglas ####

#### Setup ####

#Antes que nada, definimos una semilla. Ponemos 26/07/1842, la fecha de
#nacimiento del gran economista Alfred Marshall

set.seed(26071842)

#Importamos funciones.R
source("C:/Users/sebas/Desktop/LABDatos/LABDatos_TPR/simulacion/funciones.R")

#Definimos todas las variables que vamos a usar
n_hogares = 10000
p_1 = 10
p_2 = 5
a_1 = 0.6
a_2 = 0.4
k = 1000

#### Inciso 1: Generación de Ingresos ####
#Hecho en funciones.R

#¿Cómo elegimos k?
#¿Cómo cambia la dispersión de ingresos al variar k?

### La chi-cuadrado tiene una media de k y una varianza de 2k 
### Por lo tanto, tiene sentido elegir el k que represente 
### el ingreso promedio deseado. A medida que k aumente, la varianza
### de los ingresos aumentará linealmente con k también
 ###############   #Mejorar esta explicación. Varianza relativa vs varianza absoluta
Y <- simular_ingreso(n_hogares, k)

#### Inciso 2: Demanda óptima Cobb-Douglas ####
#Hecho en funciones.R

#### Inciso 3: Simulación Base ####

demandas <- demanda_cd(Y, p_1, p_2, a_1, a_2)

# Para más prolijidad, ponemos los datos directamente en el histograma

# Como tenemos que hacer 3 histogramas partiendo de los datos y reportar sus
# medias y cuartiles para todas, definimos directamente una función para crear
# el gráfico con los datos y la usamos para x1, x2 y la utilidad indirecta.

#Hecho en funciones.R

#Graficamos los 3 histogramas
par(mfrow = c(1, 3))
hist_resumen(demandas[[1]], "Histograma de la demanda del bien 1", "Demanda", "lightblue")
hist_resumen(demandas[[2]], "Histograma de la demanda del bien 2", "Demanda", "lightgreen")
hist_resumen(demandas[[3]], "Histograma de la utilidad indirecta", "Utilidad", "purple")
par(mfrow = c(1, 1))

#### Inciso 4: Probabilidad de bajo consumo en un bien ####
#Hecho en funciones.R
i <- 1
c <- mean(demandas[[i]])

prob_bajo_consumo(c, i, demandas)

#### Inciso 5 y 6: Shock de Precios y Visualización Comparada ####
# Aumenta p1 en 20%, p2 e Y permanecen constantes. Veamos los cambios y comparamos
# distribuciones de x1 pre y post shock

# Volvemos a correr la simulación pero ahora con p1 = 1.2p1

demandas_shock <- demanda_cd(Y, 1.2*p_1, p_2, a_1, a_2)

#En este caso la diferencia entre medias es -10 para el bien 1 y 0 para el 2.
#Es decir, dado un aumento del 20% del precio, cada agente consume (en promedio)
# 10 unidades menos del bien 1 (la caída individual es proporcional)
#y 0 unidades menos del bien 2. Tiene sentido que el consumo del bien 2 no se 
#vea afectado ya que la demanda óptima del bien 2 no depende de p1

diff_1<- mean(demandas_shock[[1]]) - mean(demandas[[1]])
diff_2<- mean(demandas_shock[[2]]) - mean(demandas[[2]])

#Ploteamos histograma de x1 pre y post shock, uno arriba del otro:
# (Para estética, ponemos colores transparentes y dejamos espacio para los datos. 
# También especificamos las medias en el gráfico para ver el efecto del shock
# con más claridad)
x_pre  <- demandas[[1]]
x_post <- demandas_shock[[1]]

# Como tenemos un solo gráfico en vez de dos, tenemos que definir los ejes en común
# No usaremos hist() directamente sino que ploteamos los histogramas que definimos antes
# (Porque queremos poner ambos en el mismo gráfico)
rango  <- range(c(x_pre, x_post))
cortes <- seq(rango[1], rango[2], length.out = 41)

h_pre  <- hist(x_pre,  breaks = cortes, plot = FALSE)
h_post <- hist(x_post, breaks = cortes, plot = FALSE)

# Primer histograma (azul transparente), con espacio arriba para los datos
plot(h_pre, col = rgb(0, 0, 1, 0.5), border = "white",
     ylim = c(0, max(h_pre$counts, h_post$counts) * 1.15),
     main = "Demanda del bien 1: pre y post shock",
     xlab = "Demanda", ylab = "Frecuencia", las = 1)

# Segundo histograma encima (rojo transparente)
plot(h_post, col = rgb(1, 0, 0, 0.5), border = "white", add = TRUE)

# Líneas de las medias
abline(v = mean(x_pre),  col = "blue", lwd = 2, lty = 2)
abline(v = mean(x_post), col = "red",  lwd = 2, lty = 2)

legend("topright",
       legend = c("Pre shock", "Post shock"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)),
       border = NA, bty = "n")


#Discutir qué sucede con la masa de hogares que consumen muy poco del
#bien 1 y cómo cambia la utilidad indirecta promedio U ∗

#En promedio, la utilidad de cada agente cae en 7 unidades
diff_U <- mean(demandas_shock[[3]]) - mean(demandas[[3]])

#Sobre los hogares que consumen poco: Como la demanda óptima de x1 es 
#(a_1*Y[i])/p_1 por Cobb-Douglas, el shock de p1 = 1.2p1 multiplica la 
#demanda por 1/1,2, desplazando toda la distribución hacia la izquierda

#### Inciso 7: Heterogeneidad en Preferencias ####

#Definimos los parámetros alpha y beta, las nuevas elasticidades y
#volvemos a definir las demandas con el mismo ingreso y precios, pero con los
#nuevos a1 y a2
a <- 3
b <- 2

new_a1 <- rbeta(n_hogares, a, b)

demandas_beta <- demanda_cd(Y, p_1, p_2, new_a1, 1-new_a1 )

#Contrastemos las demandas y utilidades de cada caso, todo en un solo gráfico:

par(mfrow = c(2, 3))
hist_resumen(demandas[[1]], "Demanda del bien 1 con a1 = 0.6", "Demanda", "lightblue")
hist_resumen(demandas[[2]], "Demanda del bien 2 con a1 = 0.6", "Demanda", "lightgreen")
hist_resumen(demandas[[3]], "Utilidad indirecta con a1 = 0.6", "Utilidad", "purple")
hist_resumen(demandas_beta[[1]], "Demanda del bien 1 con a1 ~ Beta(a,b)", "Utilidad", "lightblue")
hist_resumen(demandas_beta[[2]], "Demanda del bien 2 con a1 ~ Beta(a,b)", "Utilidad", "green")
hist_resumen(demandas_beta[[3]], "Utilidad indirecta con a1 ~ Beta(a,b)", "Utilidad", "purple")

par(mfrow = c(1, 1))


#Comparar desvíos estándares y sensibilidad al shock de precios, 
# preferencias heterogéneas