

############# Source para generar la serie de panel por flujo entre las categorías de 
############# la condición de actividad, primera vez -->
### Graba el primer resultado de la serie, se corre por única vez.
### Crea espacio de trabajo en excel
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "b_permanencia")
openxlsx::addWorksheet(wb, "b_ocup_Tant")
openxlsx::addWorksheet(wb, "b_des_Tant")
openxlsx::addWorksheet(wb, "b_inac_Tant")
openxlsx::addWorksheet(wb, "b_ocup_Tpos")
openxlsx::addWorksheet(wb, "b_des_Tpos")
openxlsx::addWorksheet(wb, "b_inac_Tpos")

### Escribe el dato "título"
openxlsx::writeData(wb, sheet = "b_permanencia", x = "Tasa de permanencia en la condición de actividad entre dos trimestres consecutivos.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_ocup_Tant",   x = "Población ocupada en el trimestre anterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_des_Tant",    x = "Población desocupada en el trimestre anterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_inac_Tant",   x = "Población inactiva en el trimestre anterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_ocup_Tpos",   x = "Población ocupada en el trimestre posterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_des_Tpos",    x = "Población desocupada en el trimestre posterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)
openxlsx::writeData(wb, sheet = "b_inac_Tpos",   x = "Población inactiva en el trimestre posterior por condición de actividad en el trimestre posterior.", startCol = 1, startRow = 1)

### Escribe el dato "contenido"
openxlsx::writeData(wb, sheet = "b_permanencia", x = b_permanencia, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_ocup_Tant",   x = b_ocup_Tant, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_des_Tant",    x = b_des_Tant, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_inac_Tant",   x = b_inac_Tant, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_ocup_Tpos",   x = b_ocup_Tpos, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_des_Tpos",    x = b_des_Tpos, startCol = 1, startRow = 2)
openxlsx::writeData(wb, sheet = "b_inac_Tpos",   x = b_inac_Tpos, startCol = 1, startRow = 2)
### Salvo archivo


