
pacman::p_load(dplyr, ggplot2, openxlsx, reshape2, gghighlight, readxl)
#devtools::install_github("brooke-watson/BRRR")

source("funciones/4_creo_grafico_slope.R", encoding = "UTF-8")


prueba <- colnames(base)

anios <- c("t2_2016 - t3_2016", "t3_2016 - t4_2016", "t4_2016 - t1_2017", 
           "t1_2017 - t2_2017", "t2_2017 - t3_2017", "t3_2017 - t4_2017", "t4_2017 - t1_2018",
           "t1_2018 - t2_2018", "t2_2018 - t3_2018", "t3_2018 - t4_2018", "t4_2018 - t1_2019", 
           "t1_2019 - t2_2019", "t2_2019 - t3_2019", "t3_2019 - t4_2019", "t4_2019 - t1_2020", 
           "t1_2020 - t2_2020", "t2_2020 - t3_2020", "t3_2020 - t4_2020", "t4_2020 - t1_2021",
           "t1_2021 - t2_2021")
anios_lab <- c("t2_3_2016", "t3_4_2016", "t4_2016 - t1_2017",
               "t1_2_2017", "t2_3_2017", "t3_4_2017", "t4_2017 - t1_2018",
               "t1_2_2018", "t2_3_2018", "t3_4_2018", "t4_2018 - t1_2019",
               "t1_2_2019", "t2_3_2019", "t3_4_2019", "t4_2019 - t1_2020",
               "t1_2_2020", "t2_3_2020", "t3_4_2020", "t4_2020 - t1_2021",
               "t1_2_2021")

#periodo <- "CABA - t1_2_2018"
#periodo <- "Partidos del GBA - t1_2_2018"
periodo <- "Total aglomerados - 2016 a 2021"

#titulo3 <- "CABA"
#titulo3 <- "Partidos del GBA"
titulo3 <- "Total aglomerados"

bases <- c("permanencia", "old_ocup_Tant", "old_des_Tant", "old_inac_Tant",
           "old_ocup_Tpos", "old_des_Tpos", "old_inac_Tpos")


### Creo gráficos
for (i in bases) {
  grafico_flujo(base = i,
                tit3 = titulo3,
                anio = anios,
                periodo = periodo)
}; BRRR::skrrrahh("birdman")


  