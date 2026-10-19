############################################################################
# ANÁLISIS ECONOMÉTRICO CON GAPMINDER                                      #
# Examen domiciliario de R - UTDT                                          #
# Burghardt, Kevin - Domanico, Santino - Roballos, Sebastián               #
#                                                                          #
# IMPORTANTE: correr con el working directory en la carpeta                #
# analisis_econometrico/ (donde están funciones.R y data/).                #
# En RStudio: Session > Set Working Directory > To Source File Location.   #
# Los gráficos (.png) se guardan en esa misma carpeta.                     #
############################################################################

rm(list = ls())   # limpia el Environment
set.seed(123)     # reproducibilidad

library(tidyverse)
source("funciones.R")

# ----------------------------------------------------------
# 0) Lectura y limpieza de datos
# ----------------------------------------------------------
gapminder <- read_csv("data/gapminder.csv")

# Códigos de dato faltante: "-" en la esperanza de vida femenina y -999 en la masculina
gapminder <- gapminder %>%
  mutate(life_expectancy_female = as.numeric(ifelse(life_expectancy_female == "-",
                                                    NA, life_expectancy_female)),
         life_expectancy_male = ifelse(life_expectancy_male == -999,
                                       NA, life_expectancy_male))

str(gapminder)

# ==========================================================
# PARTE 1: INGRESO POR PERSONA
# ==========================================================

# ----------------------------------------------------------
# 1) Evolución del ingreso por persona en Argentina
# ----------------------------------------------------------
arg <- gapminder %>%
  filter(country == "Argentina") %>%
  select(year, income_per_person) %>%
  arrange(year) %>%
  mutate(tiempo = year - min(year))   # tiempo = años desde el inicio de la serie

g_arg <- ggplot(arg, aes(x = year, y = income_per_person)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  labs(title = "Ingreso por persona en Argentina",
       x = "Año", y = "Ingreso por persona") +
  theme_minimal()
print(g_arg)
ggsave("fig1_ingreso_argentina.png", plot = g_arg, width = 7, height = 4.5)

# ----------------------------------------------------------
# 2) Tres modelos sobre el tiempo, con testeo en los últimos 10 años
# ----------------------------------------------------------
# Corte temporal: entrenamiento hasta 2000, testeo 2001-2010
corte <- max(arg$year) - 10
train <- arg %>% filter(year <= corte)
test <- arg %>% filter(year > corte)

modelos <- ajustar_modelos_ingreso(train)
nombres_modelos <- c("Lineal", "Grado 2", "Grado 10")

# poly() usa una base ortogonal: evita problemas numéricos del grado 10
# y predict() la extiende correctamente a los años de testeo.
metricas <- evaluar_modelos_ingreso(modelos, nombres_modelos, train, test)
print(metricas)

# Predicciones sobre una grilla que cubre entrenamiento y testeo
grilla <- tibble(tiempo = seq(min(arg$tiempo), max(arg$tiempo), length.out = 300)) %>%
  mutate(year = tiempo + min(arg$year))
grilla$lineal <- predict(modelos$lineal, newdata = grilla)
grilla$grado2 <- predict(modelos$grado2, newdata = grilla)
grilla$grado10 <- predict(modelos$grado10, newdata = grilla)

# Gráfico completo
g_ajustes <- ggplot() +
  geom_point(data = train, aes(x = year, y = income_per_person), color = "blue", alpha = 0.7) +
  geom_point(data = test, aes(x = year, y = income_per_person), color = "red", alpha = 0.7) +
  geom_line(data = grilla, aes(x = year, y = lineal, color = "Lineal"), linewidth = 1) +
  geom_line(data = grilla, aes(x = year, y = grado2, color = "Grado 2"), linewidth = 1) +
  geom_line(data = grilla, aes(x = year, y = grado10, color = "Grado 10"), linewidth = 1) +
  scale_color_manual(values = c("Lineal" = "darkgreen", "Grado 2" = "purple", "Grado 10" = "orange")) +
  scale_y_continuous(labels = function(x) scales::comma(x, big.mark = ".", decimal.mark = ",")) +   # eje y sin notación científica y con punto como separador de miles
  labs(title = "Ajustes polinómicos del ingreso por persona (Argentina)",
       subtitle = "Azul = entrenamiento, rojo = testeo",
       x = "Año", y = "Ingreso por persona", color = "Modelo") +
  theme_minimal()
print(g_ajustes)
ggsave("fig2_ajustes_completo.png", plot = g_ajustes, width = 7, height = 4.5)

# Gráfico acotado: se omite el grado 10 donde supera 30000 (explota fuera de la muestra)
grilla_acotada <- grilla %>% filter(grado10 < 30000)
g_acotado <- ggplot() +
  geom_point(data = train, aes(x = year, y = income_per_person), color = "blue", alpha = 0.7) +
  geom_point(data = test, aes(x = year, y = income_per_person), color = "red", alpha = 0.7) +
  geom_line(data = grilla, aes(x = year, y = lineal, color = "Lineal"), linewidth = 1) +
  geom_line(data = grilla, aes(x = year, y = grado2, color = "Grado 2"), linewidth = 1) +
  geom_line(data = grilla_acotada, aes(x = year, y = grado10, color = "Grado 10"), linewidth = 1) +
  scale_color_manual(values = c("Lineal" = "darkgreen", "Grado 2" = "purple", "Grado 10" = "orange")) +
  labs(title = "Mismos ajustes, omitiendo el grado 10 donde supera 30000",
       subtitle = "Azul = entrenamiento, rojo = testeo",
       x = "Año", y = "Ingreso por persona", color = "Modelo") +
  theme_minimal()
print(g_acotado)
ggsave("fig3_ajustes_acotado.png", plot = g_acotado, width = 7, height = 4.5)

# ----------------------------------------------------------
# 3) Correlaciones entre Argentina y cuatro países sudamericanos
# ----------------------------------------------------------
paises <- c("Argentina", "Brazil", "Chile", "Uruguay", "Peru")

ingresos_long <- gapminder %>%
  filter(country %in% paises) %>%
  select(country, year, income_per_person) %>%
  arrange(country, year)

# (a) Matriz de correlaciones entre los niveles de ingreso
ingresos_wide <- ingresos_long %>%
  pivot_wider(names_from = country, values_from = income_per_person)
cor_niveles <- cor(select(ingresos_wide, -year))
print(round(cor_niveles, 3))

# (b) Matriz de correlaciones entre variaciones porcentuales anuales (dY/Y)
crecimiento_long <- ingresos_long %>%
  group_by(country) %>%
  mutate(crecimiento = variacion_pct(income_per_person)) %>%
  ungroup() %>%
  filter(year > min(year))   # el primer año no tiene variación

crecimiento_wide <- crecimiento_long %>%
  select(country, year, crecimiento) %>%
  pivot_wider(names_from = country, values_from = crecimiento)
cor_crecimiento <- cor(select(crecimiento_wide, -year))
print(round(cor_crecimiento, 3))

g_niveles <- ggplot(ingresos_long, aes(x = year, y = income_per_person, color = country)) +
  geom_line(linewidth = 0.9) +
  labs(title = "Ingreso por persona", x = "Año", y = "Ingreso por persona", color = "País") +
  theme_minimal()
print(g_niveles)
ggsave("fig4_ingresos_paises.png", plot = g_niveles, width = 7, height = 4.5)

# ==========================================================
# PARTE 2: ESPERANZA DE VIDA Y GÉNERO (año 2010, el último del dataset)
# ==========================================================

# Muestra: países con datos completos de esperanza de vida total, femenina e ingreso
datos_2010 <- gapminder %>%
  filter(year == 2010) %>%
  filter(!is.na(life_expectancy) & !is.na(life_expectancy_female) & !is.na(income_per_person))
n_2010 <- nrow(datos_2010)
cat("Países en la muestra de 2010:", n_2010, "\n")

# Haiti (esperanza total muy baja en 2010) es un valor atípico: se mantiene en la
# muestra y se muestra la sensibilidad de los resultados sin él.
datos_sin_haiti <- datos_2010 %>% filter(country != "Haiti")

# ----------------------------------------------------------
# 5) Esperanza de vida total vs. femenina
# ----------------------------------------------------------
print(cor(datos_2010$life_expectancy, datos_2010$life_expectancy_female))

# Recta de 45 grados (total = femenina) como referencia
recta_45 <- tibble(x = range(datos_2010$life_expectancy_female),
                   y = range(datos_2010$life_expectancy_female))
atipicos <- datos_2010 %>% filter(country == "Haiti")
print(atipicos %>% select(country, life_expectancy, life_expectancy_female))
cat("Países excluidos en la sensibilidad:", nrow(datos_2010) - nrow(datos_sin_haiti), "\n")

g_5 <- ggplot(datos_2010, aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(data = recta_45, aes(x = x, y = y), color = "red", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") +
  geom_text(data = atipicos, aes(label = country), vjust = 2, size = 3) +
  labs(title = "Esperanza de vida total vs. femenina, 2010",
       subtitle = "Rojo punteado = recta de 45 grados; verde = ajuste lineal",
       x = "Esperanza de vida femenina", y = "Esperanza de vida total") +
  theme_minimal()
print(g_5)
ggsave("fig5_total_vs_femenina.png", plot = g_5, width = 7, height = 4.5)

# ----------------------------------------------------------
# 6) Regresión simple
# ----------------------------------------------------------
mod_simple <- lm(life_expectancy ~ life_expectancy_female, data = datos_2010)
res_simple <- summary(mod_simple)
print(res_simple)
cat(sprintf("R2 = %.4f\n", res_simple$r.squared))

# Test H0: b1 = 1 (bilateral), hecho a mano con la distribución t
b1 <- coef(res_simple)[2, "Estimate"]
se_b1 <- coef(res_simple)[2, "Std. Error"]
t_b1 <- (b1 - 1) / se_b1
gl_b1 <- n_2010 - 2
p_b1 <- 2 * pt(-abs(t_b1), df = gl_b1)
cat(sprintf("H0: b1 = 1 -> t = %.3f, gl = %.0f, p-valor = %.4g\n", t_b1, gl_b1, p_b1))

# Sensibilidad: sin el valor atípico
res_sin_haiti <- summary(lm(life_expectancy ~ life_expectancy_female, data = datos_sin_haiti))
print(res_sin_haiti)

# ----------------------------------------------------------
# 7) H0: femenina = total vs. H1: femenina > total (datos pareados)
# ----------------------------------------------------------
# Cada país aporta dos medidas sobre la misma población: se trabaja con la diferencia
dif <- datos_2010$life_expectancy_female - datos_2010$life_expectancy
n_dif <- length(dif)
t_dif <- mean(dif) / (sd(dif) / sqrt(n_dif))
p_dif <- pt(-t_dif, df = n_dif - 1)   # p-valor unilateral (H1: media de la diferencia > 0)
ic_dif <- mean(dif) + qt(c(0.025, 0.975), df = n_dif - 1) * sd(dif) / sqrt(n_dif)
cota_inf <- mean(dif) - qt(0.95, df = n_dif - 1) * sd(dif) / sqrt(n_dif)

print(summary(dif))
cat(sprintf("Diferencia media = %.3f, t = %.3f, gl = %.0f, p-valor unilateral = %.3g\n",
            mean(dif), t_dif, n_dif - 1, p_dif))
cat(sprintf("IC 95%% bilateral = [%.3f, %.3f]; cota inferior unilateral 95%% = %.3f\n",
            ic_dif[1], ic_dif[2], cota_inf))
print(mean(dif > 0))   # proporción de países donde femenina > total

# Sensibilidad sin el valor atípico
dif_sin <- datos_sin_haiti$life_expectancy_female - datos_sin_haiti$life_expectancy
t_sin <- mean(dif_sin) / (sd(dif_sin) / sqrt(length(dif_sin)))
cat(sprintf("Sin atípico: diferencia media = %.3f, t = %.3f\n", mean(dif_sin), t_sin))

# ----------------------------------------------------------
# 8) Regresión múltiple con ingreso (en miles de dólares)
# ----------------------------------------------------------
datos_2010 <- datos_2010 %>% mutate(ingreso_miles = income_per_person / 1000)

mod_mult <- lm(life_expectancy ~ life_expectancy_female + ingreso_miles, data = datos_2010)
res_mult <- summary(mod_mult)
print(res_mult)
print(coef(res_mult)["ingreso_miles", "Pr(>|t|)"])

# Comparación simple vs. múltiple: R2, R2 ajustado y RECM de testeo
# (promedio sobre 200 particiones 70/30, las mismas para ambos modelos)
set.seed(123)
splits_8 <- generar_particiones(n_2010)
formulas_8 <- list(life_expectancy ~ life_expectancy_female,
                   life_expectancy ~ life_expectancy_female + ingreso_miles)
tabla_8 <- comparar_modelos(formulas_8, c("Simple", "Múltiple (con ingreso)"),
                            datos_2010, splits_8)
print(tabla_8 %>% mutate(R2 = round(R2, 4), R2_adj = round(R2_adj, 4),
                         RECM_test = round(RECM_test, 3)))

# ----------------------------------------------------------
# 9) Modelo de hasta tres covariables
# ----------------------------------------------------------
# Variables excluidas del modelo (la justificación completa está en el informe):
# life_expectancy_female (lo pide la consigna), life_expectancy_male, country, year,
# income_groups, world_region, main_religion y population.
# Candidatas: income_per_person, child_mortality, children_per_woman e is_oecd.

# Nombres reales del CSV: child_mortality, children_per_woman, is_oecd
datos_9 <- gapminder %>%
  filter(year == 2010) %>%
  rename(mortalidad = child_mortality, fecundidad = children_per_woman, ocde = is_oecd) %>%
  mutate(ocde = as.numeric(ocde)) %>%   # is_oecd es TRUE/FALSE: pasa a 1/0
  select(country, life_expectancy, income_per_person, mortalidad, fecundidad, ocde) %>%
  filter(!is.na(life_expectancy) & !is.na(income_per_person) & !is.na(mortalidad) &
           !is.na(fecundidad) & !is.na(ocde))
n_9 <- nrow(datos_9)
cat("Países en la muestra del punto 9:", n_9, "\n")

# El ingreso tiene pocos países muy ricos y rendimientos decrecientes: se prueba su logaritmo
print(cor(datos_9$life_expectancy, datos_9$income_per_person))
print(cor(datos_9$life_expectancy, log(datos_9$income_per_person)))
print(round(cor(select(datos_9, life_expectancy, income_per_person, mortalidad, fecundidad, ocde)), 3))

nombres_9 <- c("Ingreso", "log(Ingreso)", "Mort. <5", "log(Mort. <5)",
               "Mort. + log(Ing.)", "log(Mort.) + log(Ing.)",
               "Mort. + log(Ing.) + Fecund.", "Mort. + log(Ing.) + OCDE",
               "log(Ing.) + Fecund. + OCDE")
# Cada fórmula va en el mismo orden que su etiqueta en nombres_9
formulas_9 <- list(
  life_expectancy ~ income_per_person,
  life_expectancy ~ log(income_per_person),
  life_expectancy ~ mortalidad,
  life_expectancy ~ log(mortalidad),
  life_expectancy ~ mortalidad + log(income_per_person),
  life_expectancy ~ log(mortalidad) + log(income_per_person),
  life_expectancy ~ mortalidad + log(income_per_person) + fecundidad,
  life_expectancy ~ mortalidad + log(income_per_person) + ocde,
  life_expectancy ~ log(income_per_person) + fecundidad + ocde
)

# Mismas 200 particiones 70/30 para todos los modelos
set.seed(123)
splits_9 <- generar_particiones(n_9)
tabla_9 <- comparar_modelos(formulas_9, nombres_9, datos_9, splits_9)
print(tabla_9 %>% mutate(R2 = round(R2, 3), R2_adj = round(R2_adj, 3),
                         RECM_test = round(RECM_test, 3)))

# Modelo final: el de menor RECM de testeo (sin redondear)
ordenada <- tabla_9 %>% arrange(RECM_test)
nombre_final <- ordenada$Modelo[1]
id_final <- ordenada$id[1]
cat("Modelo elegido:", nombre_final, "\n")

modelo_final <- lm(formulas_9[[id_final]], data = datos_9)
print(formulas_9[[id_final]])   # muestra la fórmula del modelo ganador
print(summary(modelo_final))
print(confint(modelo_final))
# Países peor explicados por el modelo
peores <- datos_9 %>%
  mutate(ajustado = fitted(modelo_final), residuo = resid(modelo_final)) %>%
  arrange(desc(abs(residuo))) %>%
  select(country, life_expectancy, ajustado, residuo) %>%
  head(8)
print(peores)

# Gráfico observado vs. ajustado
recta_45_9 <- tibble(x = range(fitted(modelo_final)), y = range(fitted(modelo_final)))
g_9 <- ggplot(datos_9 %>% mutate(ajustado = fitted(modelo_final)),
              aes(x = ajustado, y = life_expectancy)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_line(data = recta_45_9, aes(x = x, y = y), color = "red", linetype = "dashed") +
  geom_text(data = peores %>% head(3), aes(x = ajustado, y = life_expectancy, label = country),
            vjust = 1.5, size = 3) +
  labs(title = "Modelo final: observado vs. ajustado",
       x = "Esperanza de vida ajustada", y = "Esperanza de vida observada") +
  theme_minimal()
print(g_9)
ggsave("fig6_modelo_final.png", plot = g_9, width = 7, height = 4.5)