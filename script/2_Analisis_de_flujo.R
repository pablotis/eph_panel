#source("1_Armado de Panel.R")
pacman::p_load(tidyverse, readxl, questionr, alluvial, foreign, xtable, gmodels, ggplot2, ggQC,
       ggalluvial, gridExtra, xlsx, eph)



## Para trabajar total aglomerado -->
b_panel <- b_panel_orig

## Total Aglomerados - por sexo
# b_panel <- b_panel_orig %>% 
#   filter(CH04)

## Para trabajar por aglomerado -->
#b_panel <- b_panel_orig %>% 
#  filter(aglomerado.x == 33)

table(b_panel$periodo_ta)
table(b_panel$periodo_tp)


for (i in c(2:4, 6:ncol(b_panel))) {
  b_panel[,i] <- as.numeric(b_panel[,i])
}

calculate_tabulates(b_panel, "CH04_ta", "CH04_tp", add.totals = "row", add.percentage = "col",
                    weights = "PONDERA_tp")

calculate_tabulates(b_panel, "CH04_ta", "CH04_tp", add.totals = "row")

inconsistencia <- b_panel %>% 
  filter(CH04_ta == 1 & CH04_tp == 2)




##################################### PANEL ################################
################################################################################################
################################################################################################

source(file = "Funciones/2_creo_flujo.R")

creo_flujo(fecha_panel = glue::glue("t{trim1}_{trim2}_{anio}"))

############################################### SE CORRE POR PRIMERA VEZ UNICAMENTE
###############################################-->
###############################################-->
source(file = "Funciones/3_creo_excel.R")

### Renombro columna de fecha
colnames(b_act_Tant) [ncol(b_act_Tant)] <- periodo
colnames(b_act_Tpost)[ncol(b_act_Tpost)] <- periodo
colnames(b_des_Tant) [ncol(b_des_Tant)] <- periodo
colnames(b_des_Tpos) [ncol(b_des_Tpos)] <- periodo
colnames(b_ocup_Tant)[ncol(b_ocup_Tant)] <- periodo
colnames(b_ocup_Tpos)[ncol(b_ocup_Tpos)] <- periodo
colnames(b_inac_Tant)[ncol(b_inac_Tant)] <- periodo
colnames(b_inac_Tpos)[ncol(b_inac_Tpos)] <- periodo
colnames(b_permanencia)[ncol(b_permanencia)] <- periodo

creo_excel(fecha_excel = periodo)
