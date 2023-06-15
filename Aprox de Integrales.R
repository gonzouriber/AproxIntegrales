# Cargamos las librerías necesarias
library(tidyverse)
library(gridExtra)
library(grid) # Para usar la función grid.draw()

# Definimos las funciones que vamos a integrar
f1 <- function(x) exp(-x^3)
f2 <- function(x) ifelse(x == 0, 1, (1 - cos(x))/x)
f3 <- function(u) (4*u^2 - 4*u + 1)/sqrt(1-(2*u-1)^2) # Considerando la transformación u=(x+1)/2
f4 <- function(v) log((4*v+1)^2-(4*v+1)) # Considerando la transformación v=(x-1)/4

# Definimos los tamaños de muestra
n_values <- c(100, 1000, 10000, 100000)

# Calculamos las aproximaciones para cada tamaño de muestra y cada función
approximations <- tibble(
  n = rep(n_values, 4),
  f = rep(c("f1", "f2", "f3", "f4"), each = length(n_values)),
  approximation = mapply(function(n, f) {
    x <- runif(n, 0, 1)
    if (f == "f1") {
      mean(f1(x))
    } else if (f == "f2") {
      mean(f2(x))
    } else if (f == "f3") {
      mean(f3(x))
    } else {
      mean(f4(x))
    }
  }, n, f)
)

# Calculamos los valores exactos utilizando la función integrate de R
exact_values <- tibble(
  f = c("f1", "f2", "f3", "f4"),
  exact_value = sapply(c(f1, f2, f3, f4), function(f) integrate(f, 0, 1)$value)
)

# Unimos los dos data frames y calculamos los errores
results <- left_join(approximations, exact_values, by = "f") %>%
  mutate(error = abs(approximation - exact_value))

# Creamos la tabla
table <- results %>%
  rename("Número de puntos" = n,
         "Función" = f,
         "Aproximación" = approximation,
         "Valor verdadero" = exact_value,
         "Error" = error) %>%
  mutate_at(vars(Aproximación, `Valor verdadero`, Error), 
            list(~ format(round(., 4), nsmall = 4)))

# Convertimos la tabla en un data frame y luego en un "grafico" de tabla
table_df <- as.data.frame(table)
table_grob <- tableGrob(table_df, theme = ttheme_default(core = list(fg_params=list(cex = 0.7))))

# Visualizamos la tabla
grid.draw(table_grob)
