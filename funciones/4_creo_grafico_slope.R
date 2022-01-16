plot_slopegraph <- function(df) {
  ylabs <- subset(df, x==head(x,1))$group
  yvals <- subset(df, x==head(x,1))$ypos
  fontSize <- 4.5
  
  gg <- ggplot(df,aes(x=x,y=ypos)) +
    geom_line(aes(group=group),colour="grey80") +
    geom_point(colour="white",size=8) +
    geom_text(aes(label=y), size=fontSize, family="American Typewriter") +
    scale_y_continuous(name="", breaks=yvals, labels=ylabs)
  return(gg)
}    


tufte_sort <- function(df, x="periodo", y="valor", group="Tasas",
                       method="tufte", min.space=0.05) {
  ids <- match(c(x, y, group), names(df))
  df <- df[,ids]
  names(df) <- c("x", "y", "group")
  
  ## Expand grid to ensure every combination has a defined value
  tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
  tmp <- merge(df, tmp, all.y=TRUE)
  df <- mutate(tmp, y=ifelse(is.na(y), 0, y))
  
  ## Cast into a matrix shape and arrange by first column
  require(reshape2)
  tmp <- dcast(df, group ~ x, value.var="y")
  ord <- order(tmp[,2])
  tmp <- tmp[ord,]
  
  min.space <- min.space*diff(range(tmp[,-1]))
  yshift <- numeric(nrow(tmp))
  
  ## Start at "bottom" row
  ## Repeat for rest of the rows until you hit the top
  for (i in 2:nrow(tmp)) {
    ## Shift subsequent row up by equal space so gap between two entries is >= minimum
    mat <- as.matrix(tmp[(i-1):i, -1])
    d.min <- min(diff(mat))
    yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
  }
  
  tmp <- cbind(tmp, yshift=cumsum(yshift))
  
  scale <- 1
  tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
  
  ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y
  tmp <- transform(tmp, ypos=y + scale*yshift)
  return(tmp)
  
}



grafico_flujo <- function(base, tit3, anio, periodo)  {
  if(base == "permanencia") {
    
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_permanencia", skip = 1)
    
    tit1 <- "Tasa de permanencia"
    
    tit2 <- "Tasa de permanencia en la condición de 
    actividad entre dos trimestres consecutivos."
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_ocup_Tant") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_ocup_Tant", skip = 1)
    
    tit1 <- "Población ocupada en el trimestre anterior"
    
    tit2 <- "Población ocupada en el trimestre anterior por 
    condición de actividad del trimestre posterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_des_Tant") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_des_Tant", skip = 1)
    
    tit1 <- "Población desocupada en el trimestre anterior"
    
    tit2 <- "Población desocupada en el trimestre anterior por
  condición de actividad del trimestre posterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_inac_Tant") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_inac_Tant", skip = 1)
    
    tit1 <- "Población inactiva en el trimestre anterior"
    
    tit2 <- "Población inactiva en el trimestre anterior por
  condición de actividad del trimestre posterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_ocup_Tpos") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_ocup_Tpos", skip = 1)
    
    tit1 <- "Población ocupada en el trimestre posterior"
    
    tit2 <- "Población ocupada en el trimestre posterior por
  condición de actividad del trimestre anterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_des_Tpos") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_des_Tpos", skip = 1)
    
    tit1 <- "Población desocupada en el trimestre posterior"
    
    tit2 <- "Población desocupada en el trimestre posterior por
  condición de actividad del trimestre anterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  if(base == "old_inac_Tpos") {
    base <- read_excel(glue::glue("{ruta}/Serie flujo condicion de actividad.xlsx"), 
                       sheet = "b_inac_Tpos", skip = 1)
    
    tit1 <- "Población inactiva en el trimestre posterior"
    
    tit2 <- "Población inactiva en el trimestre posterior por
  condición de actividad del trimestre anterior"
    
    new_data <- tidyr::gather(base, periodo, valor, 2:ncol(base))
    
  }
  
  df <- new_data
  
  ## Prepare data    
  df <- tufte_sort(df, x="periodo", y="valor", group="Tasas", method="tufte", min.space=0.05)
  
  df <- transform(df, 
                  x=factor(x, levels=anios, 
                           labels=anios_lab), 
                  y=round(y,1))
  
  theme_set(theme_classic())
  
  plot_slopegraph(df) + 
    labs(title = tit2,
         subtitle = paste(periodo),
         caption = "Fuente: EPH-INDEC.") + 
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(
            size = 20,
            hjust = 1,
            vjust = 10,
            family = "American Typewriter",
            face = "bold",
            lineheight = 1),
          plot.subtitle = element_text(
            size   = 16,
            hjust  = 1,
            vjust  = 5,
            family = "American Typewriter",
            face   = "italic"),
          plot.caption = element_text(
            size   = 10, 
            hjust  = 1,
            vjust  = -6,
            family = "American Typewriter",
            face   ="plain"),
          axis.text.x = element_text(
            size   = 12, 
            family = "American Typewriter",
            face   = "bold",
            angle  = 35, 
            vjust  = 1,
            hjust  = 1),
          axis.text.y = element_text(
            size   = 12, 
            family = "American Typewriter",
            face   = "bold"),
          plot.margin = unit(c(2.5,1,1,1), "cm"))
  
  ggsave(paste0("salidas/aglomerados_tot", "/", tit1, " - ", titulo3, ".png"), plot = last_plot(),
            width = 16, height = 12, units = 'in', dpi = 300)
  #width = 8, height = 12, units = 'in', res = 300, margin(2.5, 1, 1, 1))
  
  memory.limit()
}

