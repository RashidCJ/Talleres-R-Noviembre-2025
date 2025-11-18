# ============================================================
# Taller: Introducción a la computación estadística en R
# Tema: Continuando con visualizaciones, modelos inferenciales y diagnósticos
# Fecha: 18 de noviembre de 2025
# Autor: Rashid C. J. Marcano Rivera, Ph.D., M.S., M.A.
# Propósito: Archivo anotado para participantes del taller
# simplificado (alternativa a Quarto)
# ============================================================


# ------------------------------------------------------------
# 0. Nota inicial
# ------------------------------------------------------------
# Esta guía acompaña la presentación de Quarto.
# Incluye explicaciones breves antes de cada bloque de código,
# para que puedan repasar de manera autónoma.
# Cargar paquetes

library(tidyverse)
library(dslabs)

# Datos que usaremos
data(murders)

############################################################
# 1. Lienzo vacío con ggplot
############################################################
# ggplot siempre empieza con los datos y un lienzo vacío.
murders |> ggplot()

############################################################
# 2. Añadir una geometría: puntos (dispersión)
############################################################
# Un gráfico de dispersión necesita una geometría geom_point().
murders |>
  ggplot(aes(x = population/1e6, y = total)) +
  geom_point()

############################################################
# 3. Guardar un gráfico en un objeto
############################################################
# Guardamos en 'p' para seguir añadiendo capas.
p <- murders |>
  ggplot(aes(population/1e6, total)) +
  geom_point(size = 2)

p

############################################################
# 4. Añadir etiquetas de texto a cada punto
############################################################
# geom_text añade el texto; nudge_x mueve la etiqueta.
p + geom_text(aes(label = abb), nudge_x = 0.5)

############################################################
# 5. Añadir color por región
############################################################
# Se añade la estética "colour = region" dentro de aes().
p <- murders |>
  ggplot(aes(population/1e6, total, colour = region)) +
  geom_point(size = 3)

p

############################################################
# 6. Añadir etiquetas con color por región
############################################################
# Las etiquetas se colorean automáticamente según la región.
p <- p + geom_text(aes(label = abb), nudge_x = 0.5)
p

############################################################
# 7. Añadir títulos y nombres de ejes
############################################################
# Usamos labs() para añadir títulos descriptivos.
p + labs(
  x = "Población (en millones)",
  y = "Homicidios con arma de fuego",
  colour = "Región",
  title = "Homicidios en Estados Unidos, 2010"
)

############################################################
# 8. Transformaciones de escala (log10)
############################################################
# scale_x_continuous y scale_y_continuous permiten reescalar variables.
p2 <- murders |>
  ggplot(aes(population/1e6, total, colour = region)) +
  geom_point(size = 3) +
  geom_text(aes(label = abb), nudge_x = 0.05)

p2 <- p2 +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(
    x = "Población (millones, escala log)",
    y = "Homicidios (escala log)",
    colour = "Región",
    title = "Homicidios vs población por región (escala log-log)"
  )

p2

############################################################
# 9. Añadir temas visuales
############################################################
library(ggthemes)
p2 + theme_economist()

############################################################
# 10. Línea de referencia (tasa promedio)
############################################################
# Calculamos la tasa promedio de asesinatos por 100,000 habitantes.
tasa_prom <- murders |>
  summarise(tasa = sum(total) / sum(population) * 1e5) |>
  pull(tasa)

# Convertimos regiones a español (solo como ejemplo rápido)
murders2 <- murders |>
  mutate(region_es = case_when(
    region == "Northeast" ~ "Noreste",
    region == "South" ~ "Sur",
    region == "West" ~ "Oeste",
    region == "North Central" ~ "Centro-Norte"
  ))

# Creamos el gráfico con la línea
library(ggrepel)
t <- murders |> 
  summarise(tasa = sum(total) /  sum(population) * 10^6) |>
  pull(tasa)

murders |> 
  mutate(región=case_when(
    region == "Northeast" ~ "Noreste",
    region == "North Central" ~ "el Midwest",
    region == "West" ~ "Oeste",
    region == "South" ~ "el Sur"))|>
  ggplot(aes(population/10^6, total)) +   
  geom_abline(intercept = log10(t), lty = 2, color = "darkgrey") +
  geom_point(aes(col = región), size = 3) +
  geom_text_repel(aes(label = abb)) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Homicidios en EEUU con arma de fuego en 2010",
       x = "Población (en millones, escala log.)", 
       y = "Homicidios por arma de fuego (en escala log.)",
       color = "Región") +
  theme_solarized()

############################################################
# 11. Gráficos rápidos con qplot()
############################################################
# qplot permite graficar sin escribir todas las capas.
murdersg <- murders |>
  mutate(
    tasa = total/population * 1e5,
    grupo = case_when(
      abb %in% c("ME","NH","VT","MA","RI","CT") ~ "Nueva Inglaterra",
      abb %in% c("WA","OR","CA") ~ "Costa Pacífico",
      region == "South" ~ "Sur",
      TRUE ~ "Otros"
    )
  )

# Densidad con relleno
qplot(
  tasa,
  data = murdersg,
  geom = "density",
  fill = grupo,
  linetype = grupo
)

############################################################
# 12. Varias geometrías con qplot() usando heights
############################################################
data(heights)

# Caja
qplot(sex, height, data = heights, geom = "boxplot", fill = sex)

# Violín
qplot(sex, height, data = heights, geom = "violin", fill = sex)

# Densidad
qplot(height, data = heights, geom = "density", fill = sex)

############################################################
# 13. Densidades con transparencia
############################################################
mu_alt <- heights |>
  group_by(sex) |>
  summarise(media = mean(height))

c <- heights |> ggplot(aes(height))

# Transparencia con alpha
c + geom_density(aes(fill = sex), alpha = 0.4)

############################################################
# 14. Densidades con colores manuales
############################################################
c +
  geom_density(aes(color = sex)) +
  geom_vline(
    data = mu_alt,
    aes(xintercept = media, color = sex),
    linetype = "dashed"
  ) +
  scale_color_manual(values = c("#999999", "#E69F00"))

############################################################
# 15A. Introducción a pruebas inferenciales
# Para principiantes:
# Las pruebas inferenciales se usan cuando queremos decir algo
# sobre una población grande, usando una muestra pequeña.
# Cada prueba responde una pregunta distinta sobre "diferencias"
# o "asociaciones". Abajo tienes ejemplos básicos y prácticos.
############################################################



############################################################
# 15B. Prueba t de una muestra
# Para qué se usa:
# - Comparar un promedio observado vs un valor específico.
# - Ejemplo: ¿los estudiantes estudian más/menos que 20 horas?
#
# Se usa cuando:
# - La variable es numérica.
# - Los datos son más o menos normales.
# H0: la media es igual a un valor hipotético (mu0)
############################################################

horas <- c(4, 8, 15, 8, 10, 10, 9, 23)
t.test(horas, mu = 20)



############################################################
# 15C. Prueba t de dos muestras independientes
# Para qué se usa:
# - Comparar promedios entre dos grupos distintos.
# - Ejemplo: ¿ingresos promedios diferentes entre H y M?
#
# Se usa cuando:
# - Los dos grupos no se mezclan (H vs M, Público vs Privado).
# - La variable es numérica.
# - Distribuciones normales y varianzas similares.
# H0: medias iguales entre dos grupos independientes
############################################################

ingreso <- c(1200,1350,1500,1600,1700,1100,1250,1400)
sexo <- c("Hombre","Hombre","Hombre","Hombre",
          "Mujer","Mujer","Mujer","Mujer")

t.test(ingreso ~ sexo)



############################################################
# 15Ch. Prueba t pareada (antes vs después)
# Para qué se usa:
# - Comparar dos mediciones sobre las MISMAS personas.
# - Ejemplo: ansiedad antes/después de un taller.
#
# Se usa cuando:
# - Las observaciones son emparejadas una a una.
# - Las diferencias siguen distribución normal.
# H0: la media de las diferencias (antes-después) = 0
############################################################

antes <- c(25, 30, 28, 32, 27)
despues <- c(20, 27, 26, 30, 23)

t.test(antes, despues, paired = TRUE)



############################################################
# 15D. ANOVA de una vía
# Para qué se usa:
# - Comparar promedios entre 3 o más grupos.
# - Ejemplo: comparar satisfacción entre regiones.
#
# Se usa cuando:
# - Variable numérica, grupos independientes.
# - Normalidad en cada grupo y varianzas similares.
# H0: todas las medias poblacionales son iguales
############################################################

satisfaccion <- c(4,2,6,4,5,7,
                  9,7,6,8,7,5,
                  2,4,5,6,3,5)

region <- factor(c(rep("Metro",6),
                   rep("Oeste",6),
                   rep("Sur",6)))

modelo <- aov(satisfaccion ~ region)
summary(modelo)

# Comparaciones post-hoc: ¿qué grupos difieren?
TukeyHSD(modelo)



############################################################
# 15E. Wilcoxon pareada
# Para qué se usa:
# - Comparar antes vs después cuando los datos NO son normales.
# - Ejemplo: estrés antes/después de una intervención.
#
# Se usa cuando:
# - Datos ordinales o con outliers.
# - No se cumple normalidad.
# Alternativa no paramétrica del t pareado
############################################################

antes <- c(35, 40, 28, 30, 33, 38)
despues <- c(30, 36, 25, 27, 29, 35)

wilcox.test(antes, despues, paired = TRUE)



############################################################
# 15F. Mann–Whitney U (rango-suma)
# Para qué se usa:
# - Comparar dos grupos independientes sin asumir normalidad.
# - Ejemplo: confianza en instituciones (pública vs privada).
#
# Se usa cuando:
# - Variable ordinal o numérica no-normal.
# - Grupos independientes.
# Alternativa no paramétrica del t independiente
############################################################

confianza <- c(6,7,8,5,7,6, 4,5,6,4,5,5)
tipo_uni <- c(rep("Pública",6), rep("Privada",6))

wilcox.test(confianza ~ tipo_uni)



############################################################
# 15G. Ji-cuadrada: bondad de ajuste
# Para qué se usa:
# - Ver si una distribución observada coincide con una esperada.
# - Ejemplo: religiones en la muestra vs censo.
#
# Se usa cuando:
# - Categorías (nominal/ordinal).
# - Frecuencias esperadas ≥ 5.
# H0: distribución observada = distribución teórica
############################################################

observado <- c(90, 70, 40)
esperado <- c(0.40, 0.35, 0.25)

chisq.test(x = observado, p = esperado)



############################################################
# 15H. Ji-cuadrada: independencia
# Para qué se usa:
# - Ver si dos variables categóricas están asociadas.
# - Ejemplo: género × preferencia partidista.
#
# Se usa cuando:
# - Tabla de contingencia con frecuencias adecuadas.
# H0: dos variables categóricas son independientes
############################################################

tabla <- matrix(c(30, 20,
                  25, 25),
                nrow = 2, byrow = TRUE,
                dimnames = list(
                  Genero = c("Hombre","Mujer"),
                  Partido = c("A","B")
                ))

chisq.test(tabla)



############################################################
# 15I. Fisher exacta
# Para qué se usa:
# - Evaluar asociación en tablas 2x2 pequeñas.
# - Ejemplo: taller (sí/no) × voto (sí/no).
#
# Se usa cuando:
# - Alguna celda tiene menos de 5 observaciones.
# H0: independencia entre filas y columnas
############################################################

tabla <- matrix(c(8, 2,
                  1, 9),
                nrow = 2,
                dimnames = list(
                  Taller = c("Sí","No"),
                  Voto = c("A favor","En contra")
                ))

fisher.test(tabla)



############################################################
# 15J. Prueba de una proporción
# Para qué se usa:
# - Ver si un porcentaje observado coincide con uno teórico.
# - Ejemplo: ¿apoyo = 50%?
# H0: proporción poblacional = p0
############################################################

prop.test(62, 100, p = 0.50)



############################################################
# 15K. Prueba de dos proporciones
# Para qué se usa:
# - Comparar porcentajes entre dos grupos independientes.
# - Ejemplo: apoyo entre jóvenes vs mayores.
# H0: proporciones iguales entre dos grupos
############################################################

prop.test(c(120, 60), c(200, 200))


############################################################
# 15L. Correlación de Pearson
# Para qué se usa:
# - Medir relación lineal entre dos variables numéricas.
# - Ejemplo: ingresos vs % universitarios.
# H0: r = 0 (relación lineal nula)
############################################################

ingreso <- c(18000,22000,25000,27000,30000,32000,15000,21000,24000,28000)
universitarios <- c(12,18,20,23,28,30,10,15,19,25)

cor.test(ingreso, universitarios, method = "pearson")



############################################################
# 15LL. Correlación de Spearman
# Para qué se usa:
# - Relación monótona con datos ordinales o no normales.
# H0: rho = 0 (relación monótona nula)
############################################################

pobreza_rank <- c(1,3,2,5,4,6)
criminalidad_rank <- c(2,3,1,6,4,5)

cor.test(pobreza_rank, criminalidad_rank, method = "spearman")



############################################################
# 15M. Correlación de Kendall
# Para qué se usa:
# - Asociación entre rangos con muchos empates.
# H0: tau = 0
############################################################

participacion <- c(1,4,3,2,5)
confianza <- c(2,5,3,1,4)

cor.test(participacion, confianza, method = "kendall")

############################################################

library(wooldridge)
base <- wage1   # datos clásicos de Wooldridge
#names(base)    # para ver nombres
#?wage1         # documentación

############################################################
# Variables que utilizaremos (recordatorio en comentarios)
# - wage: salario promedio por hora
# - educ: años de educación
# - exper: años de experiencia potencial
# - tenure: años con el empleador actual
# - nonwhite: 1 = no blanco
# - female: 1 = mujer
# - married: 1 = casado/a
############################################################


############################################################
# 16.A Renombrar variables (tenure → antigüedad)
############################################################

names(base)[4]  <- "antigüedad"      # tenure → antigüedad
names(base)[24] <- "antigüedadcuad"  # squared tenure


############################################################
# 16.B Seleccionar subconjunto de variables relevantes
############################################################

# columnas 1–7 + log(wage)=col22 + exper^2=col23 + antigüedad^2=col24
base1 <- base[, c(1:7, 22:24)]

head(base1, n = 10)   # primeras filas


############################################################
# 16.C Subconjuntos básicos usando nombres
############################################################

datos1 <- base[, c("wage","educ","exper","antigüedad")]
head(datos1, n = 5)


############################################################
# 16.Ch Subconjuntos con dplyr::select()
############################################################

#library(tidyverse)
# NOTA para principiantes:
# Si no especificas dplyr::select(), a veces sobrescribe select() de otros paquetes
datos2 <- base |>
  select(wage, educ, exper, antigüedad)

head(datos2, n = 7)


############################################################
# 16.D Filtrar solo las personas casadas
############################################################

datos3 <- base |>
  dplyr::select(wage, educ, exper, antigüedad, married) |>
  filter(married == 1)

# datos3 contiene solo casados/as


############################################################
# 16.E Correlaciones (exploración inicial)
############################################################

# Diagrama de dispersión para todas las combinaciones
plot(datos2)

# Matriz de correlaciones
cor(datos2)

#Una matriz más linda de correlaciones:
library(GGally)
ggpairs(datos2)

############################################################
# 16.F Exploración con ggplot2 (dispersión simple)
############################################################

ggplot(datos2, aes(x = educ, y = wage)) +
  geom_point() +
  theme_light() +
  ggtitle("Relación entre salario y educación")


############################################################
# 16.G Dispersión con color por estado marital
############################################################

wage1 |>
  mutate(marital = factor(married,
                          levels = c(0,1),
                          labels = c("Soltero","Casado"))) |>
  ggplot(aes(educ, wage)) +
  geom_point(aes(colour = marital), size = 3) +
  labs(title   = "Relación entre educación y salario",
       x       = "Educación",
       y       = "Salario",
       color   = "Estado marital",
       caption = "Datos de Wooldridge (wage1)")


############################################################
# 16.H  Modelos de regresión lineal (simple y múltiple)
#       Con anotaciones para principiantes
############################################################

# Recordatorio para principiantes:
# lm(y ~ x)        → regresión simple
# lm(y ~ x1 + x2)  → regresión múltiple de 2 regresores
# lm(y ~ ., ...)   → usa todas las variables del data frame (no se usará aquí)

################################################################################
# 16.H.a  MODELO 1: Regresión lineal simple (MCO/OLS)
#         wage = β0 + β1*educ + error
################################################################################

mod1 <- lm(wage ~ educ, data = datos2)

summary(mod1)

# Interpretación para principiantes:
# β0 → salario esperado si educ = 0 (intercepto)
# β1 → aumento esperado en salario por cada año adicional de educación

# --- Predicciones y residuos para el gráfico con residuos ---
predicciones <- predict(mod1)
residuos      <- residuals(mod1)

datos2$predicciones <- predicciones
datos2$residuos      <- residuos

# --- Gráfico cool nº1: Salario vs educación con residuos dibujados ---
ggplot(datos2, aes(x = educ, y = wage)) +
  geom_point() +
  geom_segment(aes(xend = educ, yend = predicciones),
               color = 'red', linetype = 'dashed') +
  geom_point(aes(y = predicciones), color = 'red') +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
  theme_light() +
  labs(title = "Modelo 1: Salario vs Educación (con residuos)",
       x = "Años de educación",
       y = "Salario por hora")


################################################################################
# 16.I MODELO 2: Regresión múltiple (educ + antigüedad)
#         wage = β0 + β1*educ + β2*antigüedad + error
################################################################################

mod2 <- lm(wage ~ educ + antigüedad, data = base1)
summary(mod2)

# Interpretación para principiantes:
# β1 → efecto de la educación manteniendo fija la antigüedad
# β2 → efecto de la antigüedad manteniendo fija la educación

# --- Gráfico cool nº2A: Visualización 3D interactiva con plotly ---
library(plotly)
library(ggplot2)

plot_ly(
  x = base1$educ,
  y = base1$antigüedad,
  z = base1$wage,
  type = "scatter3d",
  mode = "markers",
  color = base1$wage
) |>
  layout(
    scene = list(
      xaxis = list(title = 'Educación (años)'),
      yaxis = list(title = 'Antigüedad (años)'),
      zaxis = list(title = 'Salario (USD/h)')
    )
  )

# --- Gráfico cool nº2B: Scatterplot3D con plano de regresión ---
library(scatterplot3d)

graf <- scatterplot3d(
  x = base1$educ,
  y = base1$antigüedad,
  z = base1$wage,
  pch = 16,
  highlight.3d = TRUE,
  type = "h",
  xlab = 'Años de educación',
  ylab = 'Antigüedad (años)',
  zlab = 'Salario (USD/h)'
)

graf$plane3d(mod2, lty.box = "solid", col = 'mediumblue')


################################################################################
# 16.J  MODELO 3: Regresión múltiple extendida
#         wage = β0 + educ + exper + antigüedad + female
################################################################################

mod3 <- lm(wage ~ educ + exper + antigüedad + female, data = base1)
summary(mod3)

# Interpretación breve:
# Cada coeficiente controla por las demás variables.
# female = 1 mujeres; female = 0 hombres.


################################################################################
# 16.K COEFPlots para comparar modelos
################################################################################

library(coefplot)

coefplot(mod1, title = "Gráfico de coeficientes de modelo 1")
coefplot(mod2, title = "Gráfico de coeficientes de modelo 2")
coefplot(mod3, title = "Gráfico de coeficientes de modelo 3")

################################################################################
# 16.L Stargazer para tablas de modelos
################################################################################

library(stargazer)
stargazer(mod1, mod2, mod3,
          type = "text",
          title = "Resultados de modelos de regresión",
          dep.var.labels = c("Salario por hora"),
          covariate.labels = c("Educación (años)",
                               "Antigüedad (años)",
                               "Experiencia (años)",
                               "Mujer (1=Sí)"),
          no.space = TRUE,
          omit.stat = c("f", "ser"))

stargazer(mod1, mod2, mod3,
          type = "latex",
          title = "Resultados de modelos de regresión",
          dep.var.labels = c("Salario por hora"),
          covariate.labels = c("Educación (años)",
                               "Antigüedad (años)",
                               "Experiencia (años)",
                               "Mujer (1=Sí)"),
          no.space = TRUE,
          omit.stat = c("f", "ser"))


############################################################
# 17. Diagnósticos y supuestos del modelo lineal
#     (usando principalmente el modelo mod2)
############################################################


############################################################
# 17.A Gráficos de diagnóstico base de R para mod2
#      (Residuals vs Fitted, QQ-plot, etc.)
############################################################

# Estos cuatro gráficos vienen "de fábrica" con objetos lm:
# Ayudan a mirar:
# 1) Forma de la relación (linealidad + homoscedasticidad)
# 2) Normalidad de residuos
# 3) Varianza constante
# 4) Puntos influyentes (apalancamiento + Cook)

par(mfrow = c(2, 2))    # 4 gráficos en la misma ventana
plot(mod2)              # gráficos 1–4 automáticos
par(mfrow = c(1, 1))    # regresamos a 1 gráfico


############################################################
# 17.B Verificando linealidad con gráficos de dispersión
#      para cada regresor en mod2
############################################################

# Idea: y (wage) vs cada x, con recta de regresión
# Si se ve muy curvo, hay problemas de linealidad.

library(ggplot2)

# 17.B.1 Salario vs educación
ggplot(base1, aes(x = educ, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    title = "Salario vs Educación",
    x = "Años de educación",
    y = "Salario por hora"
  ) +
  theme_minimal()

# 17.B.2 Salario vs antigüedad
ggplot(base1, aes(x = antigüedad, y = wage)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    title = "Salario vs Antigüedad",
    x = "Años de antigüedad",
    y = "Salario por hora"
  ) +
  theme_minimal()


############################################################
# 17.C Independencia de errores (Durbin–Watson)
############################################################

# La prueba de Durbin–Watson mira si los residuos están
# autocorrelacionados (importante en series de tiempo).
# H0: no hay autocorrelación de primer orden.
# Valores cercanos a 2 ≈ independencia.

library(lmtest)

dwtest(mod2)

#hay problemas
############################################################
# 17.Ch Homoscedasticidad (varianza constante)
############################################################

# 17.Ch.1 Gráfico residuos vs valores ajustados
# Si la nube se "abre" (forma de embudo), hay heteroscedasticidad.

ggplot(
  data = base1,
  aes(x = mod2$fitted.values, y = mod2$residuals)
) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", col = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs Valores Ajustados (mod2)",
    x = "Valores ajustados (fitted)",
    y = "Residuos"
  ) +
  theme_minimal()

# 17.Ch.2 Prueba de Breusch–Pagan
# H0: varianza constante de los errores (homoscedasticidad).
# p-valor pequeño → evidencia de heteroscedasticidad.

bptest(mod2)


############################################################
# 17.D Normalidad de errores
############################################################

# Supuesto útil para pruebas t, F e intervalos de confianza.
# Revisamos gráficos + prueba Shapiro–Wilk.

library(e1071)  # para skewness()

# 17.D.1 QQ-plot de residuos
qqnorm(mod2$residuals)
qqline(mod2$residuals, col = "red")

# 17.D.2 Histograma de residuos
hist(
  mod2$residuals,
  breaks = 20,
  main   = "Histograma de residuos (mod2)",
  xlab   = "Residuos",
  col    = "lightblue"
)

# 17.D.3 Densidad de residuos + asimetría
plot(
  density(mod2$residuals),
  main = "Densidad de residuos (mod2)",
  xlab = "Residuos",
  sub  = paste("Asimetría:", round(skewness(mod2$residuals), 2))
)
polygon(density(mod2$residuals), col = "red")

# 17.D.4 Prueba de Shapiro–Wilk
# H0: los residuos siguen una distribución normal.
shapiro.test(mod2$residuals)


############################################################
# 17.E Residuos contra regresores individuales
############################################################

# Idea: si los residuos tienen patrón (curva, abanico, etc.)
# frente a cada x, algo anda mal (no linealidad, mala especificación).

library(gridExtra)

# 17.E.1 Residuos de mod2 vs educación
plot1 <- ggplot(base1, aes(x = educ, y = mod2$residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs Educación",
    x = "Años de educación",
    y = "Residuos"
  ) +
  theme_bw()

# 17.E.2 Residuos de mod2 vs antigüedad
plot2 <- ggplot(base1, aes(x = antigüedad, y = mod2$residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuos vs Antigüedad",
    x = "Años de antigüedad",
    y = "Residuos"
  ) +
  theme_bw()

grid.arrange(plot1, plot2, ncol = 2)


############################################################
# 17.F Multicolinealidad entre regresores
############################################################

# 17.F.1 Matriz de correlaciones entre x
vars_indep <- base1[, c("educ", "antigüedad")]
cor(vars_indep)

# 17.F.2 Factor de inflación de varianza (VIF)
# VIF ≈ 1 → poca multicolinealidad.
# VIF > 5–10 → problema serio.

library(car)
vif(mod2)


############################################################
# 17.G Observaciones influyentes (apalancamiento y Cook)
############################################################

# 17.G.1 Gráfico de residuos estandarizados vs leverage
# (esto también sale como gráfico 5 en plot(mod2))
plot(mod2, which = 5)

# 17.G.2 Gráfico de distancia de Cook
# (gráfico 4 en plot(mod2))
plot(mod2, which = 4)

# 17.G.3 Identificar observaciones con alta Distancia de Cook
cooksd <- cooks.distance(mod2)

# Regla rápida: puntos con Cook's D > 4/n se consideran influyentes
umbral_cook <- 4 / nrow(base1)
influential <- which(cooksd > umbral_cook)

influential              # índices de filas influyentes
base1[influential, ]     # filas completas para inspeccionar en datos
### ¿Qué hacemos?

#Como detectamos problemas de heterocedasticidad y normalidad de errores, podemos probar
#transformando la variable dependiente. Es posible que al incorporar variables 
#relevantes que puedan explicar
#mejor la variabilidad en el salario, mejore el ajuste del modelo. Por otro lado, es
#posible que transformar la variable dependiente o algunas independientes para corregir
#violaciones a los supuestos del modelo lineal arreglen estos problemas.
# Alternativamente, podríamos ir con otros modelos menos rígidos, pero eso es tema para 
#otro taller.
