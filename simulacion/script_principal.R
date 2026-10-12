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
k = 1000000

#### Inciso 1: Generación de Ingresos ####
#Hecho en funciones.R

#¿Cómo elegimos k?
#¿Cómo cambia la dispersión de ingresos al variar k?

### La chi-cuadrado tiene una media de k y una varianza de 2k 
### Por lo tanto, tiene sentido elegir el k que represente 
### el ingreso promedio deseado. A medida que k aumente, la varianza
### de los ingresos aumentará linealmente con k también


#### Inciso 2: Demanda óptima Cobb-Douglas ####
#Hecho en funciones.R

#### Inciso 3: Simulación Base ####

demandas <- demanda_cd(simular_ingreso(n_hogares, k), p_1, p_2, a_1, a_2)

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


