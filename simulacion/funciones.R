#### Inciso 1: Generación de Ingresos ####
#Caso genérico para después poder simular los 10 mil hogares
simular_ingreso <- function(n, k){
  Y <- rchisq(n, k)
  return(Y)
}


#### Inciso 2: Demanda óptima Cobb-Douglas ####

#Creamos los vectores e iteramos un for loop para devolver una lista
#con todos los valores

demanda_cd <- function(Y, p_1, p_2, a_1, a_2){
  xopt_1 <- numeric(length(Y))
  xopt_2 <- numeric(length(Y))
  U_opt <-  numeric(length(Y))
  
  for (i in 1:length(Y)){
  xopt_1[i] <- (a_1*Y[i])/p_1
  xopt_2[i] <- (a_2*Y[i])/p_2
  U_opt[i]  <- (xopt_1[i]**a_1)* (xopt_2[i]**a_2)
  }
  
  return(list(xopt_1 = xopt_1, xopt_2 = xopt_2, U_opt = U_opt))
}

#### Inciso 3: Simulación Base ####

hist_resumen <- function(x, titulo, nombrex, color){
  #Definimos los cuantiles y los histogramas (lo necesitamos para después darle
  #espacio al texto con los datos)
  q <- quantile(x, probs = c(0.25, 0.5, 0.75))
  h <- hist(x, breaks = 30, plot = FALSE)   
  
  hist(x, breaks = 30, border = "white", col = color,
       main = titulo, xlab = nombrex, ylab = "Frecuencia",
       ylim = c(0, max(h$counts) * 1.35))    # espacio libre arriba (el máx es 1.35 * el máx del histograma)
  
  legend("topright",
         legend = c(paste("Media:", round(mean(x), 2)),
                    paste("Q1:", round(q[1], 2)),
                    paste("Mediana:", round(q[2], 2)),
                    paste("Q3:", round(q[3], 2))),
         bty = "n")
}

#### Inciso 5: Probabilidad de bajo consumo en un bien ####
# La probabilidad de que el consumo del bien i (x_i) sea menor a un umbral
# c no es otra cosa que la suma de todas los booleanos en donde cada demanda
# individual fue menor al umbral dividido por el total de casos. Entonces, 
# definimos:


prob_bajo_consumo <- function(c, j, data){
  
  prob <- sum(data[[j]] < c) / length(data[[j]])
  return(prob)

}



