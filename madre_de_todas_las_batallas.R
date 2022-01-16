
## 1 - Armo panel
anio1 <- 2021
trim1 <- 2

anio2 <- 2021
trim2 <- 3

#periodo <- glue::glue("t{trim1}_{trim2}_{anio1}")
periodo <- glue::glue("t{trim1}_{anio1} - t{trim2}_{anio2}")

### --> PASO 1
source("script/1_Armado de Panel.R")
table(b_total$periodo)


### --> PASO 2
source("script/2_Analisis_de_flujo.R")


### --> PASO 3
source("script/4_Gráfico - Slopegraph_funcion.R")
