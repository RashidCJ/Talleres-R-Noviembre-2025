############################################################
# Taller breve de visualización en R con tidyverse y ggplot2
# Duración: ~30 minutos
# Todo el contenido explicado se coloca como comentarios.
############################################################

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
  theme_linedraw()

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