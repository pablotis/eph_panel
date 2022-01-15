
### Ruta donde está / va a ir la tabla
ruta <- "data/series/aglomerado_tot"


creo_excel <- function(fecha_excel){

  if(file.exists("data/series/aglomerado_tot/Serie flujo condicion de actividad.xlsx") == TRUE){
   
    ### Una vez creado, cargo el archivo al objeto wb.
    
    old_permanencia <- read_excel(glue::glue('{ruta}/Serie flujo condicion de actividad.xlsx'), 
                                  sheet = "b_permanencia", skip = 1)
    new_permanencia <- cbind(old_permanencia, fecha_excel = b_permanencia[ ,2])
    colnames(new_permanencia)[ncol(new_permanencia)] <- periodo
    
    
    old_ocup_Tant <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                                sheet = "b_ocup_Tant", skip = 1)
    new_ocup_Tant <- cbind(old_ocup_Tant, fecha_excel = b_ocup_Tant[ ,2])
    colnames(new_ocup_Tant)[ncol(new_ocup_Tant)] <- periodo
    
    
    old_des_Tant <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                               sheet = "b_des_Tant", skip = 1)
    new_des_Tant <- cbind(old_des_Tant, fecha_excel = b_des_Tant[ ,2])
    colnames(new_des_Tant)[ncol(new_des_Tant)] <- periodo
    
    
    old_inac_Tant <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                                sheet = "b_inac_Tant", skip = 1)
    new_inac_Tant <- cbind(old_inac_Tant, fecha_excel = b_inac_Tant[ ,2])
    colnames(new_inac_Tant)[ncol(new_inac_Tant)] <- periodo
    
    
    old_ocup_Tpos <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                                sheet = "b_ocup_Tpos", skip = 1)
    new_ocup_Tpos <- cbind(old_ocup_Tpos, fecha_excel = b_ocup_Tpos[ ,2])
    colnames(new_ocup_Tpos)[ncol(new_ocup_Tpos)] <- periodo
    
    
    old_des_Tpos <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                               sheet = "b_des_Tpos", skip = 1)
    new_des_Tpos <- cbind(old_des_Tpos, fecha_excel = b_des_Tpos[ ,2])
    colnames(new_des_Tpos)[ncol(new_des_Tpos)] <- periodo
    
    
    old_inac_Tpos <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                                sheet = "b_inac_Tpos", skip = 1)
    new_inac_Tpos <- cbind(old_inac_Tpos, fecha_excel = b_inac_Tpos[ ,2])
    colnames(new_inac_Tpos)[ncol(new_inac_Tpos)] <- periodo
    
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "b_permanencia")
    openxlsx::addWorksheet(wb, "b_ocup_Tant")
    openxlsx::addWorksheet(wb, "b_des_Tant")
    openxlsx::addWorksheet(wb, "b_inac_Tant")
    openxlsx::addWorksheet(wb, "b_ocup_Tpos")
    openxlsx::addWorksheet(wb, "b_des_Tpos")
    openxlsx::addWorksheet(wb, "b_inac_Tpos")
    ### Escribe el dato "título"
    openxlsx::writeData(wb, sheet = "b_permanencia", x = "Tasa de permanencia en la condición de actividad entre dos trimestres consecutivos. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_ocup_Tant",   x = "Población ocupada en el trimestre anterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_des_Tant",    x = "Población desocupada en el trimestre anterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_inac_Tant",   x = "Población inactiva en el trimestre anterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_ocup_Tpos",   x = "Población ocupada en el trimestre posterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_des_Tpos",    x = "Población desocupada en el trimestre posterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    openxlsx::writeData(wb, sheet = "b_inac_Tpos",   x = "Población inactiva en el trimestre posterior por condición de actividad en el trimestre posterior. Total aglomerados.", startCol = 1, startRow = 1)
    ### Escribe el dato "contenido"
    openxlsx::writeData(wb, sheet = "b_permanencia", x = new_permanencia, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_ocup_Tant",   x = new_ocup_Tant, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_des_Tant",    x = new_des_Tant, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_inac_Tant",   x = new_inac_Tant, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_ocup_Tpos",   x = new_ocup_Tpos, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_des_Tpos",    x = new_des_Tpos, startCol = 1, startRow = 2)
    openxlsx::writeData(wb, sheet = "b_inac_Tpos",   x = new_inac_Tpos, startCol = 1, startRow = 2)
    
    openxlsx::saveWorkbook(wb, glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), overwrite = TRUE)
    
    table(b_panel$aglomerado.x)
    table(b_panel$trimestre.x, b_panel$ano4.x)
    table(b_panel$trimestre.y, b_panel$ano4.y)
  
  } else {
    
    source("script/0_Generador de serie flujo ESTADO por primera vez.R")
    openxlsx::saveWorkbook(wb, glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), overwrite = TRUE) 
  }
}


  

  
    
  