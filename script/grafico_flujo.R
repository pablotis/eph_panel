### Librerías
library(eph)
library(tidyverse)
library(stringr)
library(ggforce)
library(glue)


### Armo panel
eph_t1_2019 <- get_microdata(2019,1)
eph_t2_2019 <- get_microdata(2019,2)

eph_panel <- organize_panels(bases = list(eph_t1_2019, eph_t2_2019), 
                             variables = c("ESTADO", "PONDERA"), 
                             window = "trimestral") 


### Porcentaje de macheo:
round(nrow(eph_panel) / nrow(eph_t1_2019) * 100)


tabla_alluvial <- eph_panel %>% 
  count(ESTADO, ESTADO_t1, 
        wt = PONDERA) %>% 
  group_by(ESTADO) %>% 
  mutate(pct = round(n / sum(n), 1)) %>% 
  ungroup() %>% 
  gather_set_data(1:2) %>% 
  mutate(x = case_when(x == "ESTADO" ~ "Origen",
                       x == "ESTADO_t1" ~ "Destino"),
         x = factor(x, levels = c("Origen", "Destino")),
         y = case_when(y == 1 ~ "Ocupado",
                       y == 2 ~ "Desocupado",
                       y == 3 ~ "Inactivo",
                       y == 4 ~ "Menor de 10"),
         y = factor(y, levels = c("Ocupado", "Desocupado", "Inactivo", "Menor de 10")),
         pct_2 = case_when(x == "Destino" & y == "Ocupado" ~ sum()))


tabla_alluvial %>% 
  ggplot(aes(x, 
             id = id, 
             split = ifelse(x == "Destino",
                             paste0(y, "\n", glue("({str_trim(format(sum(n)/2, big.mark = '.'))})")),
                             paste0(y, glue(" ({str_trim(format(pct, decimal.mark = ',', digits = 2))}%)"))), 
             #split = glue("{y}({pct}"),
             # split = ifelse(x == "Destino", 
             #                paste0(y, "\n", glue("({str_trim(format(sum(n)/2, big.mark = '.'))})")), 
             #                paste0(y, glue(" ({str_trim(format(pct * 100, decimal.mark = ',', digits = 2))}%)"))), 
             value = n)) +
  geom_parallel_sets(aes(fill = as.factor(ESTADO)), alpha = 0.7, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(colour = 'black', 
                            angle = 0,
                            #family = familia_fuente, 
                            hjust = "outward",
                            size = 3.8) +
  scale_fill_manual(values = c("#EE3D8F","#9283BE", "#50B8B1", "#F7941E")) +
  #theme_fivethirtyeight() +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank())
         