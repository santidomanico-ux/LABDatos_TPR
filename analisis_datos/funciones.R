# Funciones del ejercicio de Analisis de datos
# Cada funcion recibe la columna a analizar como argumento (prod_gas o prod_pet),
# asi el mismo codigo sirve para el gas y para el petroleo.

# Produccion total y market share de cada empresa en todo el periodo,
# ordenado de mayor a menor.
share_total <- function(datos, variable) {datos %>% group_by(empresa) %>% summarise(produccion = sum({{ variable }})) %>%
mutate(market_share = produccion / sum(produccion)) %>% arrange(desc(produccion))}

# Produccion y market share de cada empresa, año por año.
share_anual <- function(datos, variable) {datos %>% group_by(anio, empresa) %>%
summarise(produccion = sum({{ variable }}), .groups = "drop_last") %>%mutate(market_share = produccion / sum(produccion)) %>% ungroup()
}

# Produccion anual como indice: (produccion en t / produccion en el año base) * 100.
indice_base <- function(datos, variable, anio_base = 2014) {datos %>% group_by(anio) %>% summarise(produccion = sum({{ variable }})) %>%
mutate(crecimiento = produccion / produccion[anio == anio_base] * 100)}

# Evolucion del market share de las empresas del top, una linea por empresa.
# `shares` es la salida de share_anual().
grafico_share_lineas <- function(shares, top, titulo) {
  shares %>% filter(empresa %in% top) %>% ggplot(aes(x = anio, y = market_share, color = empresa)) +
    geom_line() + labs(x = "Año", y = "Market share", color = "Empresa", title = titulo)
}

# Composicion del mercado como stacked area: las empresas del top por separado
# y el resto agrupado en "Otros". Cada año suma 1.
# complete() pone share 0 a las empresas sin produccion en un año; sin eso,
# geom_area deja huecos y el stacked area no llega a 1 en esos años.
grafico_share_area <- function(shares, top, titulo) {
  shares %>% mutate(empresa = if_else(empresa %in% top, empresa, "Otros")) %>%
    group_by(anio, empresa) %>% summarise(market_share = sum(market_share), .groups = "drop") %>%
    complete(anio, empresa, fill = list(market_share = 0)) %>%
    ggplot(aes(x = anio, y = market_share, fill = empresa)) +
    geom_area() + labs(x = "Año", y = "Market share", fill = "Empresa", title = titulo)
}
