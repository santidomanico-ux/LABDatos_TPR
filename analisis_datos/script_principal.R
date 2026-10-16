# Ejercicio de Analisis de datos: Oil & Gas no convencional en Argentina

# 1) ¿Cuales son los grandes players del Oil & Gas en Argentina y como se reparten
#    el mercado? Definimos "gran player" a aquellas empresas que esten en el top cinco
#    de mayor market share en terminos de cantidades a lo largo de todo el dataset.
# 2) ¿Petroleo o gas, como crecio cada uno?
# 3) ¿Los recursos se extraen de piedra Shale o Tight, como evoluciono?

# ¿Que son Shale y Tight? Basicamente, el tipo de piedra donde esta el recurso
# natural. Extraer recursos de piedra Tight es mas barato y tiene menor complejidad
# tecnologica que de piedra Shale. Sin embargo, cuando se extraen recursos de piedra
# Shale (que es capital intensivo y de mayor complejidad) se obtiene mayor cantidad.
# Es razonable hipotetizar que el tipo de roca en que se trabaja refleja la madurez
# del proyecto, la inversion, la calidad de los players y las expectativas de retorno.

# Aclaraciones metodologicas: tomamos 2014 como inicio de la muestra porque antes
# de ese año no habia suficientes observaciones como para obtener conclusiones
# significativas (entre 1.700 y 5.200 registros por año en 2006-2013, contra 8.777
# en 2014 y en aumento). 2026 lo excluimos porque, al ser el año en curso, tiene
# menos registros que el resto. El universo de analisis va de 2014 a 2025 inclusive.
# Todo el dataset (y por ende las conclusiones) es sobre produccion de gas y
# petroleo NO convencional.

# Como correrlo: abrir el proyecto en RStudio (directorio de trabajo = raiz del
# repositorio) y ejecutar este script completo.

# Setup

library(tidyverse)
source("analisis_datos/funciones.R")

datos <- read_csv("analisis_datos/data/produccin-de-pozos-de-gas-y-petrleo-no-convencional.csv.gz")

clean_data <- datos %>% filter(anio >= 2014, anio <= 2025) %>%
  select(anio, idpozo, prod_pet, prod_gas, empresa, sub_tipo_recurso)
# Exploracion: calidad de los datos

# Antes de responder, verificamos que no haya datos inesperados.
clean_data %>% summarise(na_pet = sum(is.na(prod_pet)), na_gas = sum(is.na(prod_gas)),
                         negativos_pet = sum(prod_pet < 0), negativos_gas = sum(prod_gas < 0),
                         na_tipo_recurso = sum(is.na(sub_tipo_recurso)))

# No hay NAs en la produccion. Hay 386 registros sin tipo de roca, que no afectan
# los incisos 1 y 2.

clean_data %>% count(anio)

# Limpieza: unificacion de nombres de empresas
# Algunas empresas aparecen con mas de un nombre porque lo cambiaron durante el
# periodo del dataset. Si no los unificamos, una misma empresa se cuenta como varias
# y su market share queda subestimado. Unificamos al nombre mas reciente.
nombres_unificados <- c(
  "Vista Oil & Gas Argentina SA"                 = "VISTA ENERGY ARGENTINA SAU",   # 2018-2019
  "VISTA OIL & GAS ARGENTINA SAU"                = "VISTA ENERGY ARGENTINA SAU",   # 2019-2022
  "PAN AMERICAN ENERGY (SUCURSAL ARGENTINA) LLC" = "PAN AMERICAN ENERGY SL",       # 2014-2019
  "WINTERSHALL ENERGIA S.A."                     = "WINTERSHALL DEA ARGENTINA S.A",# 2014-2019
  "MADALENA AUSTRAL S.A."                        = "MADALENA ENERGY ARGENTINA SRL" # 2016-2017
)

# No unificamos adquisiciones (otra empresa comprada por un grupo), porque eso
# cambiaria la pregunta de "quien produce" a "que grupo controla":
# Petrobras Argentina (comprada por Pampa Energia), Apache e YSUR (por YPF),
# CGC Energia (ex Sinopec, del grupo CGC) y Pluspetrol Cuenca Neuquina (ex ExxonMobil).

clean_data <- clean_data %>% mutate(empresa = coalesce(unname(nombres_unificados[empresa]), empresa))

# 1) Grandes players

players_gas <- share_total(clean_data, prod_gas)
players_gas

# Primero gas: a lo largo de los años, YPF, TECPETROL, TOTAL AUSTRAL, PAMPA ENERGIA
# y PAN AMERICAN ENERGY se reparten el 81% del market share.

players_pet <- share_total(clean_data, prod_pet)
players_pet

# YPF es mas dominante en el mercado del petroleo que en el del gas y es top uno
# en market share en ambos. En el mercado del petroleo aparecen jugadores distintos
# a los del gas, como Shell, Vista Energy y Pluspetrol, mientras que TECPETROL cae
# al sexto puesto. Los top cinco players se reparten el 87% del mercado: el mercado
# del petroleo en Argentina esta mas concentrado que el del gas.
# Es interesante a su vez ver la evolucion de estos players a lo largo de los años.

top_5_gas <- head(players_gas$empresa, 5)
top_5_pet <- head(players_pet$empresa, 5)

share_anual_gas <- share_anual(clean_data, prod_gas)
share_anual_pet <- share_anual(clean_data, prod_pet)

grafico_players_gas <- grafico_share_lineas(share_anual_gas, top_5_gas, "Market share del gas por año (top 5)")
grafico_players_gas

grafico_players_pet <- grafico_share_lineas(share_anual_pet, top_5_pet, "Market share del petroleo por año (top 5)")
grafico_players_pet

# Dado el feedback obtenido en las clases, tambien hacemos el grafico stacked area,
# que muestra de forma mas ilustrativa la concentracion de mercado y es mas facil
# de interpretar.

grafico_area_gas <- grafico_share_area(share_anual_gas, top_5_gas, "Composicion del mercado de gas")
grafico_area_gas

grafico_area_pet <- grafico_share_area(share_anual_pet, top_5_pet, "Composicion del mercado de petroleo")
grafico_area_pet

# En ambas industrias podemos ver un claro dominio de YPF que se ha moderado
# llegando hacia la actualidad. Jugadores como Vista, Pan American y Shell ayudaron
# a la diversificacion del mercado (en el caso del petroleo), fenomeno que se
# repite en el gas con Pampa Energia y Pan American.

# Otro detalle interesante es que Vista (2018), Shell (2019) y Pampa Energia (2018,
# tras comprar Petrobras Argentina) lograron posicionarse en el top 5 de market
# share siendo jugadores relativamente nuevos en el Oil & Gas no convencional
# argentino.

# 2) Crecimiento del petroleo y del gas

# Como estan en unidades distintas, comparamos indices de crecimiento con año base:
# Crecimiento base 2014 = (produccion en t / produccion en 2014) * 100

produccion_total <- inner_join(indice_base(clean_data, prod_gas), indice_base(clean_data, prod_pet),
                               by = "anio", suffix = c("_gas", "_pet"))

grafico_crecimiento <- ggplot(produccion_total, aes(x = anio)) +
  geom_line(aes(y = crecimiento_gas, color = "Gas")) +
  geom_line(aes(y = crecimiento_pet, color = "Petroleo")) +
  labs(x = "Año", y = "Crecimiento (base 100 = año 2014)", color = "Recurso",
       title = "Crecimiento del petroleo y el gas")
grafico_crecimiento

# Las curvas se movieron juntas al principio y luego divergen. Nos interesa saber
# cuando fue la ultima vez que se cruzaron, para ver cuando empezo la divergencia.
# Por Bolzano, si (curva petroleo - curva gas) cambia de signo, las curvas se cruzaron.

produccion_total <- produccion_total %>% mutate(diferencia = crecimiento_pet - crecimiento_gas)
produccion_total

# Hasta 2017 el crecimiento de la produccion de gas era mayor al del petroleo;
# de 2018 en adelante, el crecimiento del petroleo supero ampliamente al del gas.

# Es interesante observar el crecimiento de la produccion de petroleo y gas junto
# con lo observado en el inciso 1) sobre los players: hay una coincidencia temporal
# entre la entrada de nuevos players, una menor concentracion de mercado y el
# aumento exponencial de la produccion de petroleo.

# 3) Shale o Tight

# Shale o Tight complementa los incisos anteriores. Vimos que hacia 2018-2020
# aumento la produccion de petroleo (exponencialmente) y de gas (mas suavemente),
# que la concentracion de mercado cayo y que entraron nuevos players.
# Recordemos que extraer recursos de roca Shale es mas complejo y caro, pero a su
# vez produce una cantidad notablemente mayor que explotar roca Tight. Mayor
# explotacion de roca Shale se relaciona con mayor confianza, inversion y madurez
# del proyecto.

# Participacion de Shale y de Tight en los pozos activos, año a año
shale_tight <- clean_data %>% group_by(anio) %>%
  summarise(share_shale = n_distinct(idpozo[sub_tipo_recurso == "SHALE"]) / n_distinct(idpozo)) %>%
  mutate(share_tight = 1 - share_shale)
shale_tight

# Hasta 2021 habia una dominancia de la roca Tight; ese año la tendencia se
# revirtio y la explotacion de roca Shale siguio creciendo hasta hoy.

grafico_shale_tight <- ggplot(shale_tight, aes(x = anio)) +
  geom_line(aes(y = share_shale, color = "Shale")) + geom_line(aes(y = share_tight, color = "Tight")) +
  labs(x = "Año", y = "Participacion en pozos activos", color = "Tipo de roca",
       title = "Shale y tight: ¿cual se explota mas?")
grafico_shale_tight

# Conclusiones

# 1) Antes de 2014 habia pocas observaciones y ningun analisis hubiese sido
#    riguroso, por lo que lo descartamos del universo de analisis.
# 2) YPF es la empresa dominante tanto en gas como en petroleo, pero tanto su
#    participacion como la concentracion del mercado en general fueron cayendo
#    desde la segunda mitad de los 2010s.
# 3) La produccion de gas crecio mas hasta 2017; de 2018 en adelante el petroleo
#    paso a liderar el crecimiento y la tendencia continua.
# 4) En 2021 la explotacion de roca Shale supero a la de Tight, lo que valida la
#    hipotesis inicial. Mayor explotacion de roca Shale esta vinculada con una
#    industria mas madura, confiable y redituable.
# 5) Hacia finales de la decada de 2010 la industria cambio: entraron nuevos players
#    y el producto estrella paso a ser el petroleo. Años despues, la madurez de la
#    industria se reflejo en proyectos mas intensivos en capital y tecnologia
#    (explotacion de roca Shale).
