#Paquetes 
install.packages(c("tidyverse", "ggplot2", "png", "janitor", "grid"))
library(tidyverse)
library(ggplot2)
library(png)
library(janitor)
library(grid)

#Importar datos
df <- read.csv("sem-cvs.csv", sep=";")

# Limpieza ###########################
df <- df %>% remove_empty(c("rows", "cols"))
df <- df %>% select(-N.) %>% select(-Click)

#Ganados y Perdidos
df <- unite(df, GanPer, Des6, Des7, sep="")
df$GanPer <- ifelse(df$GanPer != "Ganado" & df$GanPer != "Perdido" , "", df$GanPer)

#Halfs
df <- unite(df, Half, Des28, Des29, sep="")

#Zonas
df <- unite(df, Zona, Des30, Des31, Des32, Des33, sep="")
df$Zona <- ifelse(df$Zona != "Cárcel" & df$Zona != "Transición" & df$Zona != "Gestación" & df$Zona != "Definición", "", df$Zona)

#Coordenadas
df$XY <- ifelse(nchar(df$XY) > 6, "", df$XY)
df <- separate(df, XY, c("x", "y"), sep=";", fill = "left")

# Tiempos
# Function to convert time from min:sec format to seconds
time_to_seconds <- function(time_str) {
  parts <- strsplit(time_str, ":")[[1]]
  minutes <- as.numeric(parts[1])
  seconds <- as.numeric(parts[2])
  total_seconds <- minutes * 60 + seconds
  return(total_seconds)
}

df$Inicio_seg <- sapply(df$Inicio, time_to_seconds)
df$Fin_seg <- sapply(df$Fin, time_to_seconds)

df$Duracion_seg <- df$Fin_seg - df$Inicio_seg
df <- select(df, -Inicio_seg, -Fin_seg)

#Valores vacios a N/A
df <- apply(df, 2, function(x) ifelse(x == "", NA, x))
df <- data.frame(df)
df$x <- as.numeric(df$x)
df$y <- as.numeric(df$y)
df$Duracion_seg <- as.numeric(df$Duracion_seg)

# Graficas ############################

# Fondo cancha para coordenadas
background <- rasterGrob(png::readPNG("cancha.png"), interpolate = TRUE)

# Posesión
df_pos <- filter(df, Categoría == "Posesión propia" | Categoría == "Posesión rival")
df_pos_sum <- df_pos %>% group_by(Categoría) %>% summarise(sum(Duracion_seg))

df_pos_sum$Duracion_min <- df_pos_sum$`sum(Duracion_seg)` %/% 60
df_pos_sum$Duracion_seg <- df_pos_sum$`sum(Duracion_seg)` %% 60
df_pos_sum <- unite(df_pos_sum, Duracion, Duracion_min, Duracion_seg,sep=":")
df_pos_sum$Duracion <- ifelse(nchar(df_pos_sum$Duracion) < 5, paste(substr(df_pos_sum$Duracion, 1, 3), "0", substr(df_pos_sum$Duracion, 4, nchar(df_pos_sum$Duracion)), sep = ""),df_pos_sum$Duracion)

ggplot(df_pos_sum, aes(x="", y=`sum(Duracion_seg)`, fill=Categoría)) +
  geom_bar(stat="identity") +
  coord_polar("y", start=0) +
  labs(title= "Posesión", x = NULL, y = NULL) + # Quitar los títulos
  theme(axis.title.x = element_blank(),   # Quitar los títulos de los ejes
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),    # Quitar los números de los ejes
        axis.text.y = element_blank())+
  scale_fill_brewer(name = "",palette = "Set1", direction = -1)
# annotate(geom = "text", x = 1, y = 650, label = "A")+
# annotate(geom = "text", x = 1, y = 2050, label = "B")

#Lines
df_lines <- filter(df, Categoría == "Lineout propio" | Categoría == "Lineout rival")
df_lines$x <- ifelse(df_lines$x < 50, 0, 70)

ggplot(data=df_lines)+
  geom_bar(mapping = aes(x = Categoría, fill= GanPer))+
  labs(title= "Lineouts", x = NULL, y = NULL) + # Quitar los títulos
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)

ggplot(data=df_lines)+
  annotation_custom(background, xmin = -6, xmax = 76, ymin = -20, ymax = 120)+
  scale_x_continuous(limits = c(-3, 73)) +
  scale_y_continuous(limits = c(-1, 101))+
  coord_fixed(ratio = 1.18)+
  geom_point(mapping = aes(x = x, y = 100 - y , color=Categoría))+
  labs(title= "Lineouts", x = NULL, y = NULL) + # Quitar los títulos
  theme(axis.title.x = element_blank(),   # Quitar los títulos de los ejes
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),    # Quitar los números de los ejes
        axis.text.y = element_blank())+
  scale_color_brewer(name = "",type = "qual", palette = "Set2", direction = 1)

#ds_scrum <- filter(ds, Categoría == "Scrum Propio" | Categoría == "Scrum Rival")

