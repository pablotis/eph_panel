
## 1 - Armo panel
anio1 <- 2016
trim1 <- 4

anio2 <- 2017
trim2 <- 1

#periodo <- glue::glue("t{trim1}_{trim2}_{anio1}")
periodo <- glue::glue("t{trim1}_{anio1} - t{trim2}_{anio2}")

### --> PASO 1
source("script/1_Armado de Panel.R")
table(b_total$periodo)

### --> PASO 2
source("script/2_Analisis_de_flujo.R")
