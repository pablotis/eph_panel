
#install.packages("pacman") 
pacman::p_load(tidyverse, readxl, questionr, alluvial, foreign, xtable, gmodels, ggplot2, ggQC,
       ggalluvial, gridExtra, xlsx, eph)


### Cargo función para crear paneles
source("Funciones/1_creo_panel.R")


data1 <- eph::get_microdata(anio1,trim1)
data2 <- eph::get_microdata(anio2,trim2)

data <- rbind(data1, data2)



####### Creamos identificador único para los casos.
b_total <- data %>% 
  rename(trimestre = TRIMESTRE, ano4 = ANO4, aglomerado = AGLOMERADO, componente = COMPONENTE) %>% 
  mutate(codigo_unico = paste(CODUSU, aglomerado, NRO_HOGAR, componente, sep = ""), 
         periodo      = paste0(ano4,"_",trimestre,"t"))


### Función para crear panel
creo_panel(t1 = glue::glue("{anio1}_{trim1}t"), t2 = glue::glue("{anio2}_{trim2}t"))

