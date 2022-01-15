
### Función para crear el panel

## Ejemplo de estrucura de valores
# unique(b_total$periodo)
# t1 <- "2019_2t"
# t2 <- "2019_3t"

creo_panel <- function(t1, t2){
  
  #Separamos las bases por trimestre para distinguir luego las variables de cada uno.
  p_t_ant <-  b_total %>% 
    filter(periodo==glue::glue("{t1}"))
  p_t_post <-  b_total %>% 
    filter(periodo==glue::glue("{t2}"))
  
  table(p_t_ant$trimestre,  p_t_ant$ano4)
  table(p_t_post$trimestre, p_t_post$ano4)
  
  # Para armar la base panel, elegimos las variables con las que vamos a trabajar:
  p_t_ant  <-  p_t_ant %>% 
    select(ano4,trimestre,aglomerado,codigo_unico,periodo,
           ESTADO,CAT_OCUP,CH03,CH04,PONDERA,PP07C,PP07H)
  p_t_post  <-  p_t_post %>% 
    select(ano4,trimestre,aglomerado,codigo_unico,periodo,
           ESTADO,CAT_OCUP,CH03,CH04,PONDERA,PP07C,PP07H)
  
  # Cambiamos los nombres de todas las variables que utilizaremos en panel, para ambas bases,
  # menos las que permiten identificar al individuo,
  names(p_t_ant)[5:12] <- paste0(names(p_t_ant)[5:12],"_ta")
  names(p_t_post)[5:12] <- paste0(names(p_t_post)[5:12],"_tp")
  #Chequeamos los nombres de variables luego del paso anterior
  names(p_t_ant)
  names(p_t_post)
  
  ### Armamos la base panel, fusionando las creadas para t_ant y t_post -->
  b_panel_orig <<- merge(p_t_ant,p_t_post,
                         by = "codigo_unico")
  
}
