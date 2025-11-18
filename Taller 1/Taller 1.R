# ============================================================
# Taller: Introducción a la computación estadística en R
# Introducción y primeros pasos con tidyverse
# Fecha: 13 de noviembre de 2025
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


# ============================================================
# 1. Instalar y cargar paquetes necesarios
# ============================================================

# Instala los paquetes sólo si NO los tienes.
install.packages(c("dslabs", "wooldridge", "tidyverse"))

# Cargar el tidyverse (conjunto de funciones útiles para análisis)
library(tidyverse)



# ============================================================
# 2. R como calculadora básica
# ============================================================
# R ejecuta operaciones matemáticas directamente en la consola.

1 + 2
13 / 2
2 ^ 6
5 * (2 + 3)
sqrt(81)



# ============================================================
# 3. Asignación de valores a objetos
# ============================================================
# Usamos <- para guardar valores en un objeto.

x <- 4   # asignamos el valor 4 a x
x        # llamamos a x
x + 2    # realizamos operaciones

x <- 8   # reasignamos el valor
x

#Podemos igual usar la 'flecha' en sentido inverso
8->x #resulta en lo mismo.

# Igualdad lógica (comparación)
2 == 2
2 == 3



# ============================================================
# 4. Objetos de texto y valores lógicos
# ============================================================

# Texto (caracteres) - noten las comillas; su ausencia llevaría a errores
y <- "Puerto Rico"
y

# TRUE y FALSE (asignación de valores lógicos, verdadero y falso)
vivo  <- TRUE
despierto <- FALSE

vivo == despierto #dará falso
# Comparaciones
2 < 3 #menor que
3 <= 3 # menor o igual que
3 != 4   # "no es igual a"

# ============================================================
# 4.a Operaciones especiales en R base: %in%, ==, !=, &, |
# ============================================================
# Estas operaciones permiten filtrar, comparar y manipular datos
# sin usar tidyverse. Son esenciales para manejar objetos como
# vectores, data.frames y matrices solo con R base.


# ------------------------------------------------------------
# %in%  : verifica si valores están dentro de un conjunto
# ------------------------------------------------------------

# Ejemplo: identificar cuáles números están en un conjunto específico
x <- c(3, 7, 10, 12)
x %in% c(7, 12) 
# Devuelve: TRUE FALSE FALSE TRUE

# Filtrar en un data.frame con R base
subset(mtcars, cyl %in% c(4, 6))   # solo autos de 4 o 6 cilindros

# ------------------------------------------------------------
# == : igualdad exacta
# ------------------------------------------------------------

mtcars[ mtcars$cyl == 4 , ]   # autos con 4 cilindros

# ------------------------------------------------------------
# != : “no es igual a”
# ------------------------------------------------------------

mtcars[ mtcars$gear != 5 , ]  # autos cuya transmisión NO es de 5 cambios

# ------------------------------------------------------------
# & : “y” lógico (ambas condiciones deben cumplirse)
# ------------------------------------------------------------

# Autos de 6 cilindros Y con mpg > 20
mtcars[ mtcars$cyl == 6 & mtcars$mpg > 20 , ]

# ------------------------------------------------------------
# | : “o” lógico (al menos una condición debe cumplirse)
# ------------------------------------------------------------

# Autos automáticos O que pesan más de 3.5 (wt > 3.5)
mtcars[ mtcars$am == 0 | mtcars$wt > 3.5 , ]

# ------------------------------------------------------------
# which() para obtener índices lógicos
# ------------------------------------------------------------

# Índices de autos con hp mayor de 200
which(mtcars$hp > 200)

# Extraer usando los índices
mtcars[ which(mtcars$hp > 200) , ]

# ------------------------------------------------------------
# ifelse() para clasificar datos en R Base
# ------------------------------------------------------------

# Clasificación simple: "alto" si mpg ≥ 25, "regular" si no
mtcars$mpg_clas <- ifelse(mtcars$mpg >= 25, "alto", "regular")
head(mtcars$mpg_clas)

# ------------------------------------------------------------
# Operadores combinados en condiciones más complejas
# ------------------------------------------------------------

# "alto millaje" si mpg ≥ 30 Y cilindros = 4, de lo contrario "otro"
mtcars$tipo_mpg <- ifelse(
  mtcars$mpg >= 30 & mtcars$cyl == 4,
  "alto millaje",
  "otro"
)

head(mtcars[c("mpg", "cyl", "tipo_mpg")])

# ============================================================
# 4.b Bucles "for" en R base
# ============================================================
# Los bucles for permiten repetir una acción varias veces,
# recorriendo elementos de un vector, lista o filas de un data.frame.
# Son útiles cuando necesitamos procesos paso a paso.

# ------------------------------------------------------------
# Ejemplo 1: recorrer un vector y mostrar cada valor
# ------------------------------------------------------------

v <- c(10, 20, 30, 40)

for(i in v){
  print(i)
}

# ------------------------------------------------------------
# Ejemplo 2: recorrer un vector usando índices
# ------------------------------------------------------------
# Esto permite usar la posición del elemento (útil para modificarlo).

for(i in 1:length(v)){
  cat("Posición:", i, "— Valor:", v[i], "\n")
}

# ------------------------------------------------------------
# Ejemplo 3: crear un vector nuevo dentro del for
# ------------------------------------------------------------

resultados <- numeric(length(v))   # vector vacío del tamaño correcto

for(i in 1:length(v)){
  resultados[i] <- v[i] * 2        # operación simple
}

resultados

# ------------------------------------------------------------
# Ejemplo 4: recorrer columnas de un data.frame
# ------------------------------------------------------------

# Queremos la media de cada columna numérica de mtcars
df <- mtcars

for(col in names(df)){
  if(is.numeric(df[[col]])){
    cat("Promedio de", col, "=", mean(df[[col]]), "\n")
  }
}

# ------------------------------------------------------------
# Ejemplo 5: bucle con condicionales (if)
# ------------------------------------------------------------

for(i in v){
  if(i > 20){
    cat(i, "es mayor que 20\n")
  } else {
    cat(i, "no es mayor que 20\n")
  }
}

# ------------------------------------------------------------
# Ejemplo 6: anidar bucles ("for" dentro de "for")
# ------------------------------------------------------------
# Útil para matrices o comparaciones cruzadas sencillas.

m <- matrix(1:9, nrow = 3)

for(i in 1:nrow(m)){
  for(j in 1:ncol(m)){
    cat("fila", i, "columna", j, "=>", m[i, j], "\n")
  }
}

# ------------------------------------------------------------
# Ejemplo 7: construir una tabla manualmente con un for
# ------------------------------------------------------------

# Supongamos que queremos las desviaciones estándar de las columnas numéricas

desv <- c()

for(col in names(df)){
  if(is.numeric(df[[col]])){
    desv[col] <- sd(df[[col]])
  }
}

desv

# ============================================================
# 5. Operaciones con varios valores a la vez
# ============================================================
# Usamos c() para crear un vector, concatenando valores.

a <- c(2, 5, 7)

print(a)   # imprime con formato más estándar
cat(a)     # imprime en línea corrida

sum(a) #suma de valores
mean(a) #media/promedio
sd(a) #desv.est
var(a) #varianza




# ============================================================
# 6. Crear una función sencilla
# ============================================================

mediana_mín_máx <- function(x){
  qs <- quantile(x, c(0.5, 0, 1))
  data.frame(
    mediana = qs[1],
    mín     = qs[2],
    máx     = qs[3]
  )
}

# Puedes probarla así:
mediana_mín_máx(a)


# ============================================================
# 7. Importar datos desde archivos externos
# ============================================================

library(tidyverse)

# Ejemplo: escribir un archivo CSV
# write_csv(mtcars, "mtcars.csv")

# Cargar un archivo CSV, estos están en el github del taller
d <- read_csv("mtcars.csv")


# También podemos usar paquete haven para leer datos de Stata o SPSS
library(haven)

haven::write_dta(mtcars, "mtcars.dta")
d2 <- haven::read_dta("mtcars.dta")

haven::write_sav(mtcars, "mtcars.sav")
d3 <- haven::read_sav("mtcars.sav")



# ============================================================
# 8. Explorar los datos
# ============================================================

names(d)     # nombres de variables
summary(d)   # resumen estadístico
head(d)      # primeras filas
tail(d)      # últimas filas
str(d)       # estructura: tipo de cada variable

help(mtcars) # ayuda interna sobre este conjunto de datos: dará información de variables y conjunto
# esto saldrá en la pestaña Help del panel de archivos (files)

# ============================================================
# 9. Tipos de datos en R
# ============================================================

str(d)       # vemos los tipos actuales

# Convertir una variable a factor (categórica)
d$am <- as.factor(d$am)
str(d$am)



# ============================================================
# 10. Usar el operador $ para acceder variables dentro de objeto
# ============================================================

d$mpg   # accede a la variable mpg dentro del conjunto d

# ============================================================
# 10.1. otros operadores de acceso existen para listas y marcos de datos
d[ , "mpg"]      # accede a la variable mpg (data frame)
d[ , 1]          # accede a la primera variable (data frame)
d[1, ]           # accede a la primera fila (data frame)
d[1, "mpg"]     # accede al valor de mpg en la primera

# ============================================================
# 11. attach() y detach() (opcional, con cautela)
# ============================================================

# attach(d)  # NO RECOMENDADO para uso frecuente
# sus operaciones varias con las variables internas
# detach(d) # siempre cerrar si lo usas

# ============================================================
# 12. Funciones útiles para explorar variables
# ============================================================
d$am <- as.numeric(d$am)
tapply(d$mpg, d$gear, mean)          # media (tercer argumento) de primer argumento por segundo argumento
sapply(d, mean)                      # promedios de todas las columnas
summary(d$mpg)                       # resumen básico de una variable
table(d$carb, d$am)                  # tabla cruzada
addmargins(table(d$carb, d$am))      # añade totales marginales

# Gráficas básicas de R base
hist(d$wt, col = "skyblue", main = "Distribución del peso")
boxplot(d$wt, ylab = "Peso (en miles)")
plot(d$wt ~ d$mpg, xlab = "Millas por galón", ylab = "Peso (en miles)")



# ============================================================
# 13. Paquete psych para descripciones más completas
# ============================================================

library(psych)

describe(d$hp)          # estadísticas ampliadas
describeBy(d$mpg, d$am) # separados por categoría/grupos



# ============================================================
# 14. Tabulación cruzada con fórmulas
# ============================================================

xtabs(mpg ~ cyl + am, data = d)

# Calcular promedios por categoría
suma_mpg   <- xtabs(mpg ~ cyl + am, data = d)
cantidad   <- xtabs(~ cyl + am, data = d)
media_mpg  <- suma_mpg / cantidad

media_mpg

# ============================================================
# 15. Entender y manipular datos con el Tidyverse
# ============================================================
# Aquí usamos herramientas de dplyr para buscar valores extremos,
# crear variables, filtrar subconjuntos, resumir datos y agrupar.

# ------------------------------------------------------------
# 15.1 Encontrar máximos y mínimos (ejemplo con mpg)
# ------------------------------------------------------------

#library(ggplot2), parte del tidyverse
data("mpg")

summary(mpg)          # vistazo general al conjunto de datos
max(mpg$hwy)          # máximo valor de millaje en carretera

# which.max() devuelve el índice donde ocurre el valor máximo
i_max <- which.max(mpg$hwy)
mpg$model[i_max]      # modelo del vehículo con mejor millaje

# Forma equivalente usando tidyverse
mpg |>
  top_n(1, hwy)       # devuelve la fila con el hwy más alto


# ------------------------------------------------------------
# 15.2 Ejemplo con criminalidad en EE.UU. (dslabs)
# ------------------------------------------------------------

library(dslabs) # acompañante de libro de ciencia de datos de Irizarry.
data(murders)

summary(murders)

min(murders$total)    # número mínimo de asesinatos registrados
i_min <- which.min(murders$total)
murders$state[i_min]  # estado con menor número total de asesinatos

# ------------------------------------------------------------
# 15.3 Crear nuevas variables
# ------------------------------------------------------------
# Creamos una tasa de asesinatos por cada 100 mil habitantes.

murders$tasa_100k <- murders$total / murders$population * 10^5
head(murders)

# Forma equivalente en tidyverse usando mutate()
murders <- murders |>
  mutate(tasa = total / population * 10^5)

head(murders)

# ------------------------------------------------------------
# 15.4 Crear subconjuntos de datos con filter()
# ------------------------------------------------------------

# Vehículos con millaje hwy >= 35
filter(mpg, hwy >= 35)

# ------------------------------------------------------------
# 15.5 Seleccionar columnas con select() y combinar con filter()
# ------------------------------------------------------------

tabla_nueva_mpg <- select(mpg, model, year, cty, hwy)
filter(tabla_nueva_mpg, hwy >= 35)

# Lo mismo, pero evitando crear objetos intermedios:
mpg |>
  select(model, year, cty, hwy) |>
  filter(hwy >= 35)

# ------------------------------------------------------------
# 15.6 Uso del operador de canalización: |> o %>%
# ------------------------------------------------------------
# El pipe permite encadenar pasos sin crear múltiples objetos.
# Funciona bien cuando el primer argumento de la función son los datos.
# Este será el caso para todas las funciones dentro de los paquetes del
# tidyverse.


# ------------------------------------------------------------
# 15.7 Resumir datos con summarise() y group_by()
# ------------------------------------------------------------

# Promedio y desviación estándar por fabricante
mpg |>
  group_by(manufacturer) |>
  summarise(
    mpg_grupal = mean(hwy),
    ds_grupal  = sd(hwy)
  )

# Agrupado por tipo de transmisión
mpg |>
  group_by(trans) |>
  summarise(
    mpg_grupal = mean(hwy),
    ds_grupal  = sd(hwy)
  )

# Aplicando nuestra función creada anteriormente
mpg |>
  group_by(manufacturer) |>
  summarise(mediana_mín_máx(hwy))



# ------------------------------------------------------------
# 15.8 Ejemplo más complejo: tasas regionales con case_when()
# ------------------------------------------------------------
# Usamos mutate() + case_when() para crear una variable categórica
# y luego resumimos por grupos. El case_when es una versión de un "if else"

murders |>
  mutate(
    group = case_when(
      abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "Nueva Inglaterra",
      abb %in% c("WA", "OR", "CA")                  ~ "Costa del Pacífico",
      region == "South"                             ~ "el Sur",
      TRUE                                          ~ "Otras regiones"
    )
  ) |>
  group_by(group) |>
  summarise(
    tasa_100k = sum(total) / sum(population) * 10^5
  )
