#############################################################
###### FUNCIONES - Análisis econométrico con gapminder ######
#############################################################

library(tidyverse)

# Raíz del error cuadrático medio
rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2))
}

# Variación porcentual respecto del período anterior: (x_t - x_{t-1}) / x_{t-1}
variacion_pct <- function(x) {
  (x - lag(x)) / lag(x)
}

# Genera n_part particiones aleatorias entrenamiento/testeo.
# Devuelve una lista; cada elemento son los índices de las filas de entrenamiento.
generar_particiones <- function(n, n_part = 200, prop_train = 0.7) {
  replicate(n_part, sample(1:n, size = round(prop_train * n)), simplify = FALSE)
}

# Parte 1: ajusta los tres modelos de income_per_person sobre el tiempo
ajustar_modelos_ingreso <- function(datos_train) {
  list(
    lineal  = lm(income_per_person ~ tiempo, data = datos_train),
    grado2  = lm(income_per_person ~ poly(tiempo, 2), data = datos_train),
    grado10 = lm(income_per_person ~ poly(tiempo, 10), data = datos_train)
  )
}

# Parte 1: R2 y RECM en entrenamiento y RECM en testeo de una lista de modelos
evaluar_modelos_ingreso <- function(modelos, nombres, datos_train, datos_test) {
  R2_train <- sapply(modelos, function(m) summary(m)$r.squared)
  RECM_train <- sapply(modelos, function(m) {
    rmse(datos_train$income_per_person, fitted(m))
  })
  RECM_test <- sapply(modelos, function(m) {
    rmse(datos_test$income_per_person, predict(m, newdata = datos_test))
  })
  tibble(Modelo = nombres,
         R2_train = round(R2_train, 3),
         RECM_train = round(RECM_train, 0),
         RECM_test = round(RECM_test, 0))
}

# Parte 2: compara modelos por R2, R2 ajustado (muestra completa) y RECM de
# testeo promedio sobre las mismas particiones para todos los modelos.
# 'formulas' es una lista de fórmulas y 'nombres' un vector con sus etiquetas.
comparar_modelos <- function(formulas, nombres, datos, splits,
                             variable_y = "life_expectancy") {
  n_mod <- length(formulas)
  R2 <- rep(0, n_mod)
  R2_adj <- rep(0, n_mod)
  RECM_test <- rep(0, n_mod)
  
  for (i in 1:n_mod) {
    resumen <- summary(lm(formulas[[i]], data = datos))
    R2[i] <- resumen$r.squared
    R2_adj[i] <- resumen$adj.r.squared
    
    errores <- rep(0, length(splits))
    for (j in 1:length(splits)) {
      train <- datos[splits[[j]], ]
      test <- datos[-splits[[j]], ]
      mod_j <- lm(formulas[[i]], data = train)
      errores[j] <- rmse(test[[variable_y]], predict(mod_j, newdata = test))
    }
    RECM_test[i] <- mean(errores)
  }
  
  # Valores SIN redondear (se redondea solo al mostrar); id = posición en la lista
  tibble(id = 1:n_mod, Modelo = nombres, R2 = R2, R2_adj = R2_adj, RECM_test = RECM_test)
}
