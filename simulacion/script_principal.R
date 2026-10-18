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

#La chi-cuadrado tiene media k y varianza 2k, así que elegimos k
#como el ingreso promedio deseado.
#Al aumentar k, la varianza ABSOLUTA (2k) crece linealmente: los
#ingresos se dispersan más nominalmente.
#Pero la dispersión RELATIVA (desvío estándar = sqrt(2/k))
#decrece: respecto a su media k, los ingresos son más homogéneos.
#En pocas palabras, k alto implica ingresos mayores y relativamente más parejos.

Y <- simular_ingreso(n_hogares, k)

#### Inciso 2: Demanda óptima Cobb-Douglas ####
#Hecho en funciones.R

#### Inciso 3: Simulación Base ####

demandas <- demanda_cd(Y, p_1, p_2, a_1, a_2)

#Para más prolijidad, ponemos los datos directamente en el histograma

#Como tenemos que hacer 3 histogramas partiendo de los datos y reportar sus
#medias y cuartiles para todas, definimos directamente una función para crear
#el gráfico con los datos y la usamos para x1, x2 y la utilidad indirecta.

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
c <- mean(demandas[[i]]) - sd(demandas[[i]])

prob_bajo_consumo(c, i, demandas)

#### Inciso 5 y 6: Shock de Precios y Visualización Comparada ####
#Aumenta p1 en 20%, p2 e Y permanecen constantes. Veamos los cambios y comparamos
#distribuciones de x1 pre y post shock

#Volvemos a correr la simulación pero ahora con p1 = 1.2p1

demandas_shock <- demanda_cd(Y, 1.2*p_1, p_2, a_1, a_2)

#En este caso la diferencia entre medias es -10 para el bien 1 y 0 para el 2.
#Es decir, dado un aumento del 20% del precio, cada agente consume (en promedio)
#10 unidades menos del bien 1 (la caída individual es proporcional)
#y 0 unidades menos del bien 2. Tiene sentido que el consumo del bien 2 no se 
#vea afectado ya que la demanda óptima del bien 2 no depende de p1

diff_1<- mean(demandas_shock[[1]]) - mean(demandas[[1]])
diff_2<- mean(demandas_shock[[2]]) - mean(demandas[[2]])

#Cómo cambió el umbral de consumo?
prob_bajo_consumo(c, i, demandas_shock)
#Es casi 1, la distribución se desplazó tanto que casi nadie consume tanto

#Ploteamos histograma de x1 pre y post shock, uno arriba del otro:
#(Para estética, ponemos colores transparentes y dejamos espacio para los datos. 
#También especificamos las medias en el gráfico para ver el efecto del shock
#con más claridad)
x_pre  <- demandas[[1]]
x_post <- demandas_shock[[1]]

#Como tenemos un solo gráfico en vez de dos, tenemos que definir los ejes en común
#No usaremos hist() directamente sino que ploteamos los histogramas que definimos antes
#(Porque queremos poner ambos en el mismo gráfico)
rango  <- range(c(x_pre, x_post))
cortes <- seq(rango[1], rango[2], length.out = 41)

h_pre  <- hist(x_pre,  breaks = cortes, plot = FALSE)
h_post <- hist(x_post, breaks = cortes, plot = FALSE)

#Primer histograma (azul transparente), con espacio arriba para los datos
plot(h_pre, col = rgb(0, 0, 1, 0.5), border = "white",
     ylim = c(0, max(h_pre$counts, h_post$counts) * 1.15),
     main = "Demanda del bien 1: pre y post shock",
     xlab = "Demanda", ylab = "Frecuencia", las = 1)

#Segundo histograma encima (rojo transparente)
plot(h_post, col = rgb(1, 0, 0, 0.5), border = "white", add = TRUE)

#Líneas de las medias
abline(v = mean(x_pre),  col = "blue", lwd = 2, lty = 2)
abline(v = mean(x_post), col = "red",  lwd = 2, lty = 2)

legend("topright",
       legend = c("Pre shock", "Post shock"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)),
       border = NA, bty = "n")

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
hist_resumen(demandas_beta[[1]], "Demanda del bien 1 con a1 ~ Beta(a,b)", "Demanda", "lightblue")
hist_resumen(demandas_beta[[2]], "Demanda del bien 2 con a1 ~ Beta(a,b)", "Demanda", "green")
hist_resumen(demandas_beta[[3]], "Utilidad indirecta con a1 ~ Beta(a,b)", "Utilidad", "purple")

par(mfrow = c(1, 1))


#Shock con preferencias heterogéneas (mismos Y y mismos new_a1)
demandas_beta_shock <- demanda_cd(Y, 1.2 * p_1, p_2, new_a1, 1 - new_a1)

#Desvíos estándar de x1 y U, caso homogéneo vs Beta
sd(demandas[[1]])
sd(demandas_beta[[1]])
sd(demandas[[3]])
sd(demandas_beta[[3]])

#Sensibilidad al shock: cambio individual en x1
sd(demandas_shock[[1]] - demandas[[1]])
sd(demandas_beta_shock[[1]] - demandas_beta[[1]])

#Claro, cuando todos los hogares tienen la misma preferencia (a1 = 0.6),
#la única fuente de variabilidad en la demanda es el ingreso (Y), 
#que con k = 1000 es casi constante, por eso el desvío de x1 es chico (2.68).
#Al permitir que a1 varíe como una Beta(3,2), la demandas mantienen
#aproximadamente la media pero las preferencias de los agentes 
#sobre el bien se vuelve heterogénea: la dispersión de x1 se multiplica
#por unas 7.6 veces, como vemos en la comparación entre desvíos estándares (20.27)
#y la de la utilidad indirecta por unas 4 (de 3.01 a 11.91)

#Sobre el cambio en la utilidad indirecta, antes con preferencias homogéneas la 
#U. Indirecta se distribuía casi simétricamente alrededor de la media, porque casi toda 
#la variación en este caso venía del ingreso, como dicho antes. Al variar las preferencias,
#la utilidad promedio sube y la dispersión aumenta: la intuición detrás de esto es que los 
#agentes que prefieren más el bien 2 (que cuesta la mitad de 1 pre shock) aprovechan de manera
#mucho mejor su ingreso.

#Frente al shock del 20% en p1, todos los hogares reducen su demanda del bien 1 
#en la misma proporción, independientemente de sus preferencias (por la forma de 
#la cpo de las cobb douglas)
#Lo que sí cambia es cuántas unidades pierde cada uno: como la caída individual es -x1/6,
#el desvío del cambio es el desvío de x1 dividido 6, que da 0.447 en el caso homogéneo y
#3.379 en el heterogéneo. En el caso homogéneo todos pierden más o menos lo mismo,
#alrededor de 10 unidades, mientras que con preferencias heterogéneas los hogares
#con a1 alto pierden mucho más y los de a1 bajo casi nada
#(pierden mucho más los que más querían el bien 1 y casi nada los que lo valoraban
#poco), aunque la pérdida media es parecida porque la media de a1 es la misma

#Ahora repetimos los incisos 4, 5 y 6 con preferencias heterogéneas

#Inciso 4: probabilidad de bajo consumo del bien 1.
#Usamos el MISMO umbral c que en el caso homogéneo (media - desvío de x1 con
#a1 = 0.6), así las cuatro probabilidades son comparables entre sí

prob_bajo_consumo(c, i, demandas)             #homogéneo, pre shock. 0.1585
prob_bajo_consumo(c, i, demandas_shock)       #homogéneo, post shock. 0.9992
prob_bajo_consumo(c, i, demandas_beta)        #Beta, pre shock. 0.4346
prob_bajo_consumo(c, i, demandas_beta_shock)  #Beta, post shock. 0.6394

#Con preferencias homogéneas pasa de 0.16 a 0.999 con el shock: como todos consumen
#parecido, correr toda la distribución un 20% hace que casi nadie supere el umbral 
#(Como especificamos en el inciso 4)

#Pero con Beta ya había mucha más gente con bajo consumo antes del shock (0.43) y sube a
#0.64: el aumento es mucho menor porque, al estar las demandas muy dispersas, el
#mismo corrimiento (1/1.2) cruza el umbral para una fracción más chica de hogares

#Inciso 5: histograma de x1 pre y post shock con preferencias heterogéneas
#Usamos el mismo procedimiento que antes, cortes en común y ploteamos con add = TRUE

x_pre_b  <- demandas_beta[[1]]
x_post_b <- demandas_beta_shock[[1]]

rango_b  <- range(c(x_pre_b, x_post_b))
cortes_b <- seq(rango_b[1], rango_b[2], length.out = 41)

h_pre_b  <- hist(x_pre_b,  breaks = cortes_b, plot = FALSE)
h_post_b <- hist(x_post_b, breaks = cortes_b, plot = FALSE)

plot(h_pre_b, col = rgb(0, 0, 1, 0.5), border = "white",
     ylim = c(0, max(h_pre_b$counts, h_post_b$counts) * 1.15),
     main = "Demanda del bien 1 (a1 ~ Beta): pre y post shock",
     xlab = "Demanda", ylab = "Frecuencia", las = 1)

plot(h_post_b, col = rgb(1, 0, 0, 0.5), border = "white", add = TRUE)

abline(v = mean(x_pre_b),  col = "blue", lwd = 2, lty = 2)
abline(v = mean(x_post_b), col = "red",  lwd = 2, lty = 2)

legend("topright",
       legend = c("Pre shock", "Post shock"),
       fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5)),
       border = NA, bty = "n")

#Observamos que la variación en medias es mucho menor al caso original


#Inciso 6: cambio en las medias y en la utilidad indirecta con Beta
diff_1_beta <- mean(demandas_beta_shock[[1]]) - mean(demandas_beta[[1]])
diff_2_beta <- mean(demandas_beta_shock[[2]]) - mean(demandas_beta[[2]])
diff_U_beta <- mean(demandas_beta_shock[[3]]) - mean(demandas_beta[[3]])
c(diff_1, diff_1_beta)   #cambio medio en x1: homogéneo vs Beta
c(diff_2, diff_2_beta)   #cambio medio en x2 (ambos son 0)
c(diff_U, diff_U_beta)   #cambio medio en U: homogéneo vs Beta

#El bien 1 cae casi lo mismo en ambos casos (−9,998 contra −9,954): como la media de α1 es la
#misma (0,6), la demanda media del bien 1 es parecida (alrededor de 60) y la caída es aproximadamente
#un sexto de eso (alrededor de 10). El bien 2 no cambia en ninguno de los dos casos, porque su
#demanda no depende de p1. La utilidad media cae un poco más con Beta en unidades (−6,97 contra−7,41),
#pero eso es porque parte de un nivel más alto (74,4 contra 67,3): en porcentaje la caída
#es similar, algo menor con Beta (−10,0 % contra −10,4 %). Fundamentalmente, lo que cambia con 
#la heterogeneidad de preferencias no es la caída media, sino quién pierde más: los hogares con α1 alto




