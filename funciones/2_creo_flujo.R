
### Función para crear excel con flujo

# Armado de panel
# Formato del parámetro fecha_panel = t2_3_2019"
#fecha_panel <- "t2_3_2019"
creo_flujo <- function(fecha_panel){

  
  ### Población que permanece en la misma condición de actividad entre dos trimestres consecutivos -->
  b_permanencia <<- b_panel %>% 
    summarise(act_act     = sum(PONDERA_tp[(ESTADO_tp == 1 | ESTADO_tp ==2) & 
                                             (ESTADO_ta == 1 | ESTADO_ta ==2)]),
              act_tp      = sum(PONDERA_tp[(ESTADO_tp  == 1 | ESTADO_tp ==2)]),
              "Permanecen activos" = round(act_act / act_tp*100,1),
              inact_inact = sum(PONDERA_tp[ESTADO_tp == 3 & ESTADO_ta == 3]),
              inact_tp    = sum(PONDERA_tp[ESTADO_tp == 3]),
              "Permanecen inactivos" = round(inact_inact/inact_tp*100,1),
              oc_oc       = sum(PONDERA_tp[ESTADO_tp==1 & ESTADO_ta==1]),
              oc_tp       = sum(PONDERA_tp[ESTADO_tp==1]),
              "Permanecen ocupados" = round(oc_oc/oc_tp*100,1),
              des_des     = sum(PONDERA_tp[ESTADO_tp==2 & ESTADO_ta==2]),
              des_tp      = sum(PONDERA_tp[ESTADO_tp==2]),
              "Permanecen desocupados" = round(des_des/des_tp*100,1),
              inac_inac   = sum(PONDERA_tp[ESTADO_tp==3 & ESTADO_ta==3]),
              inac_tp     = sum(PONDERA_tp[ESTADO_tp==3]),
              "Permanecen inactivos" = round(inac_inac/inac_tp*100,1),
              men_men     = sum(PONDERA_tp[ESTADO_tp==4 & ESTADO_ta==4]),
              men_tp      = sum(PONDERA_tp[ESTADO_tp==4]),
              "Permanecen menores" = round(men_men/men_tp*100,1),
              imp_imp     = sum(PONDERA_tp[ESTADO_tp==0 & ESTADO_ta==0]),
              imp_tp      = sum(PONDERA_tp[ESTADO_tp==0]),
              "Permanecen sin imputar" = round(imp_imp/imp_tp*100, 1),
              pob_tot_ta  = sum(PONDERA_tp[ESTADO_ta>=1 & ESTADO_ta<=3]),
              pob_tot_tp  = sum(PONDERA_tp[ESTADO_tp>=1 & ESTADO_tp<=3]),
              pob_mov     = sum(PONDERA_tp[(ESTADO_tp>=1 & ESTADO_tp<=3) &
                                             (ESTADO_ta!=ESTADO_tp)]),
              "Tasa de movilidad" = round(pob_mov/pob_tot_ta*100, 1)) %>% 
    select("Permanecen activos", "Permanecen inactivos", "Permanecen ocupados",
           "Permanecen desocupados","Permanecen inactivos",
           "Permanecen menores","Tasa de movilidad") %>% 
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_permanencia
  
  
  ######################## Flujo de la condición de actividad entre trimestres consecutivos
  
  ## Flujo desde la Actividad -->
  b_act_Tant <<- b_panel %>% 
    filter(ESTADO_ta == 1 | ESTADO_ta == 2) %>% 
    summarise("Activos"   = round(sum(PONDERA_tp[ESTADO_tp == 1 | ESTADO_tp == 2] /
                                        sum(PONDERA_tp) * 100), 1),
              "Inactivos" = round(sum(PONDERA_tp[ESTADO_tp == 3] /
                                        sum(PONDERA_tp) * 100), 1),
              "Menores de 10 años / No imputados" = round(sum(PONDERA_tp[ESTADO_tp==4 | ESTADO_tp==0]/
                                                                sum(PONDERA_tp) * 100), 1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_act_Tant
  
  
  ## Flujo desde la Ocupación -->
  b_ocup_Tant <<- b_panel %>% 
    filter(ESTADO_ta==1) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_tp == 1 | ESTADO_tp == 2)] / 
                                          sum(PONDERA_tp) * 100), 1),
              "Ocupados"    = round(sum(PONDERA_tp[ESTADO_tp == 1] / sum(PONDERA_tp) * 100), 1),
              "Desocupados" = round(sum(PONDERA_tp[ESTADO_tp == 2] / sum(PONDERA_tp) * 100), 1),
              "Inactivos"   = round(sum(PONDERA_tp[ESTADO_tp == 3] / sum(PONDERA_tp) * 100), 1),
              "Menores de 10 años / No imputados"=round(sum(PONDERA_tp[ESTADO_tp==4 | ESTADO_tp==0] / 
                                                              sum(PONDERA_tp) * 100), 1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_ocup_Tant
  
  ### Flujo desde la Desocupación -->
  b_des_Tant <<- b_panel %>% 
    filter(ESTADO_ta==2) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_tp == 1 | ESTADO_tp == 2)] / 
                                          sum(PONDERA_tp) * 100), 1),
              "Ocupados"    = round(sum(PONDERA_tp[ESTADO_tp == 1] / sum(PONDERA_tp) * 100),1),
              "Desocupados" = round(sum(PONDERA_tp[ESTADO_tp == 2] / sum(PONDERA_tp) * 100),1),
              "Inactivos"   = round(sum(PONDERA_tp[ESTADO_tp == 3] / sum(PONDERA_tp) * 100),1),
              "Menores de 10 años / No imputados" = round(sum(PONDERA_tp[ESTADO_tp==4 | ESTADO_tp==0]
                                                              /sum(PONDERA_tp) * 100),1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_des_Tant
  
  ### Flujo desde la Inactividad -->
  b_inac_Tant <<- b_panel %>% 
    filter(ESTADO_ta==3) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_tp == 1 | ESTADO_tp == 2)] / 
                                          sum(PONDERA_tp) * 100), 1),
              "Ocupados"    = round(sum(PONDERA_tp[ESTADO_tp == 1] / sum(PONDERA_tp) * 100),1),
              "Desocupados" = round(sum(PONDERA_tp[ESTADO_tp == 2] / sum(PONDERA_tp) * 100),1),
              "Inactivos"   = round(sum(PONDERA_tp[ESTADO_tp == 3] / sum(PONDERA_tp) * 100),1),
              "Menores de 10 años / No imputados" = round(sum(PONDERA_tp[ESTADO_tp==4 | ESTADO_tp==0]
                                                              /sum(PONDERA_tp)*100),1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_inac_Tant
  
  ## Flujo hacia la Actividad -->
  b_act_Tpost <<- b_panel %>% 
    filter(ESTADO_tp == 1 | ESTADO_tp == 2) %>% 
    summarise("Activos"   = round(sum(PONDERA_tp[ESTADO_ta == 1 | ESTADO_ta == 2] /
                                        sum(PONDERA_ta) * 100), 1),
              "Inactivos" = round(sum(PONDERA_tp[ESTADO_ta == 3] /
                                        sum(PONDERA_ta) * 100), 1),
              "Menores de 10 años / No imputados" = round(sum(PONDERA_tp[ESTADO_ta==4 | ESTADO_ta==0]/
                                                                sum(PONDERA_tp) * 100), 1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_act_Tant
  
  ### Flujo hacia la Ocupación -->
  b_ocup_Tpos <<- b_panel %>% 
    filter(ESTADO_tp==1) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_ta == 1) | ESTADO_ta == 2] / 
                                          sum(PONDERA_tp) * 100),1),
              "Ocupados"    = round(sum(PONDERA_tp[ESTADO_ta == 1] / sum(PONDERA_tp) * 100),1),
              "Desocupados" = round(sum(PONDERA_tp[ESTADO_ta == 2] / sum(PONDERA_tp) * 100),1),
              "Inactivos"   = round(sum(PONDERA_tp[ESTADO_ta == 3] / sum(PONDERA_tp) * 100),1),
              "Menores de 10 años / No imputados"=round(sum(PONDERA_tp[ESTADO_ta==4 | ESTADO_ta==0]
                                                            /sum(PONDERA_tp)*100),1)) %>%
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_ocup_Tpos
  
  ### Flujo hacia la Desocupación -->
  b_des_Tpos <<- b_panel %>% 
    filter(ESTADO_tp==2) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_ta == 1) | ESTADO_ta == 2] / 
                                          sum(PONDERA_tp) * 100),1),
              "Ocupados"    =round(sum(PONDERA_tp[ESTADO_ta == 1] / sum(PONDERA_tp) * 100),1),
              "Desocupados" =round(sum(PONDERA_tp[ESTADO_ta == 2] / sum(PONDERA_tp) * 100),1),
              "Inactivos"   =round(sum(PONDERA_tp[ESTADO_ta == 3] / sum(PONDERA_tp) * 100),1),
              "Menores de 10 años / No imputados"=round(sum(PONDERA_tp[ESTADO_ta==4 | ESTADO_ta==0]
                                                            /sum(PONDERA_tp)*100),1)) %>%
    gather(Tasas, fecha_panel,1:ncol(.))
  
  b_des_Tpos
  
  ### Flujo hacia la Inactividad -->
  b_inac_Tpos <<- b_panel %>% 
    filter(ESTADO_tp==3) %>% 
    summarise("Activos"     = round(sum(PONDERA_tp[(ESTADO_ta == 1) | ESTADO_ta == 2] / 
                                          sum(PONDERA_tp) * 100),1),
              "Ocupados"    = round(sum(PONDERA_tp[ESTADO_ta == 1] / sum(PONDERA_tp) * 100),1),
              "Desocupados" = round(sum(PONDERA_tp[ESTADO_ta == 2] / sum(PONDERA_tp) * 100),1),
              "Inactivos"   = round(sum(PONDERA_tp[ESTADO_ta == 3] / sum(PONDERA_tp) * 100),1),
              "Menores de 10 años / No imputados" = round(sum(PONDERA_tp[ESTADO_ta == 4 | ESTADO_ta == 0]
                                                              / sum(PONDERA_tp)*100),1)) %>% 
    gather(Tasas, fecha_panel, 1:ncol(.))
  
  b_inac_Tpos
}


  
