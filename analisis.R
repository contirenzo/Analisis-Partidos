#Paquetes 
install.packages(c("tidyverse", "ggplot2", "png", "janitor", "grid"))
library(tidyverse)
library(ggplot2)
library(png)
library(janitor)
library(grid)

#Importar datos
df <- read.csv("int_sem-cvs.csv", sep=";")

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

# Eliminar titulos y ticks de graficas
clean_graph <- theme(axis.title.x = element_blank(),   
              axis.title.y = element_blank(), 
              axis.text.x = element_blank(),    
              axis.text.y = element_blank(),
              axis.ticks = element_blank())

# Fondo cancha para coordenadas
background <- rasterGrob(png::readPNG("cancha.png"), interpolate = TRUE)
cancha_y_coord <- list(annotation_custom(background, xmin = -6, xmax = 76, ymin = -20, ymax = 120),
                scale_x_continuous(limits = c(-3, 73)) ,
                scale_y_continuous(limits = c(-1, 101)),
                coord_fixed(ratio = 1.18),
                  annotate(geom = "text", x = 35, y = 18, label = "Cárcel", color= "white", alpha = 0.5),
                  annotate(geom = "text", x = 35, y = 40, label = "Transición", color= "white", alpha = 0.5),
                  annotate(geom = "text", x = 35, y = 60, label = "Gestación", color= "white", alpha = 0.5),
                  annotate(geom = "text", x = 35, y = 82, label = "Definición", color= "white", alpha = 0.5),
                    geom_point(mapping = aes(x = x, y = 100 - y , color=Categoría)),
                        scale_color_brewer(name = "", palette = "Set2", direction = 1),
                          clean_graph)

# Posesión
df_pos <- filter(df, Categoría == "Posesión propia" | Categoría == "Posesión rival")
df_pos_sum <- df_pos %>% group_by(Categoría) %>% summarise(suma_seg= sum(Duracion_seg))

df_pos_sum$percent <- df_pos_sum$suma_seg/sum(df_pos_sum$suma_seg)*100
df_pos_sum$Duracion_min <- df_pos_sum$suma_seg %/% 60
df_pos_sum$Duracion_seg <- df_pos_sum$suma_seg %% 60
df_pos_sum <- unite(df_pos_sum, Duracion, Duracion_min, Duracion_seg,sep=":")
#df_pos_sum$Duracion <- ifelse(nchar(df_pos_sum$Duracion) < 5, paste(substr(df_pos_sum$Duracion, 1, 3), "0", substr(df_pos_sum$Duracion, 4, nchar(df_pos_sum$Duracion)), sep = ""),df_pos_sum$Duracion)

ggplot(df_pos_sum, aes(x="", y=suma_seg, fill=Categoría)) +
  geom_bar(stat="identity") +
    coord_polar("y", start=0) +
      labs(title= NULL, x = NULL, y = NULL) +
        clean_graph+
          scale_fill_brewer(name = "",palette = "Set1", direction = -1)+
            geom_text(aes(label = paste0(round(percent, digits = 1), "%")), position = position_stack(vjust = 0.5), color = "white")

# annotate(geom = "text", x = 1, y = 650, label = "A")+
# annotate(geom = "text", x = 1, y = 2050, label = "B")

#Zonas
df_zonas <- filter(df, Categoría == "Cárcel" | Categoría == "Transición" | Categoría == "Gestación" | Categoría == "Definición")
df_zonas_sum <- df_zonas %>% group_by(Categoría) %>% summarise(suma_seg = sum(Duracion_seg))

df_zonas_sum$percent <- df_zonas_sum$suma_seg/sum(df_zonas_sum$suma_seg)*100
df_zonas_sum$Duracion_min <- df_zonas_sum$suma_seg %/% 60
df_zonas_sum$Duracion_seg <- df_zonas_sum$suma_seg %% 60
df_zonas_sum <- unite(df_zonas_sum, Duracion, Duracion_min, Duracion_seg,sep=":")
#df_zonas_sum$Duracion <- ifelse(nchar(df_zonas_sum$Duracion) < 5, paste(substr(df_zonas_sum$Duracion, 1, 3), "0", substr(df_zonas_sum$Duracion, 4, nchar(df_zonas_sum$Duracion)), sep = ""),df_zonas_sum$Duracion)

ggplot(df_zonas_sum, aes(x="", y=suma_seg, fill=Categoría)) +
  geom_bar(stat="identity") +
    coord_polar("y", start=0) +
      labs(title= NULL, x = NULL, y = NULL) + 
        clean_graph+
          scale_fill_brewer(name = "",palette = "Set1", direction = 1)+
            geom_text(aes(label = paste0(round(percent, digits = 1), "%")), position = position_stack(vjust = 0.5), color = "white")


#Penales
df_penales <- filter(df, Categoría == "Penalti concedido propio" | Categoría == "Penalti concedido rival")
df_penales_conc_riv <- filter(df, Categoría == "Penalti concedido rival")
df_penales_conc_riv_sum <- df_penales_conc_riv %>% group_by(Zona) %>% summarise(suma=n())
df_penales_conc_riv_sum$percent <- df_penales_conc_riv_sum$suma/sum(df_penales_conc_riv_sum$suma)*100

ggplot(df_penales_conc_riv_sum, aes(x="", y=suma, fill=Zona)) +
  geom_bar(stat="identity") +
    coord_polar("y", start=0) +
      labs(title= NULL, x = NULL, y = NULL) + 
        clean_graph+
          scale_fill_brewer(name = "",palette = "Set1", direction = 1)+
            geom_text(aes(label = paste0(round(percent, digits = 1), "%")), position = position_stack(vjust = 0.5), color = "white")

  

ggplot(data=df_penales)+
  cancha_y_coord+
    labs(title= NULL, x = NULL, y = NULL)

#Tackles
df_tackles <- filter(df, Categoría == "Tackle")
df_tackles_sum <- df_tackles %>% group_by(Zona) %>% summarise(suma=n())
df_tackles_sum$percent <- df_tackles_sum$suma/sum(df_tackles_sum$suma)*100


ggplot(df_tackles_sum, aes(x="", y=suma, fill=Zona)) +
  geom_bar(stat="identity") +
    coord_polar("y", start=0) +
      labs(title= NULL, x = NULL, y = NULL) +
        clean_graph+
          scale_fill_brewer(name = "",palette = "Set1", direction = 1)+
            geom_text(aes(label = paste0(round(percent, digits = 1), "%")), position = position_stack(vjust = 0.5), color = "white")

ggplot(data=df_tackles)+
  cancha_y_coord+
  labs(title= NULL, x = NULL, y = NULL)

#Lineouts
df_lines <- filter(df, Categoría == "Lineout propio" | Categoría == "Lineout rival")
df_lines$x <- ifelse(df_lines$x < 50, 0, 70)
df_lines_sum <- df_lines %>%
  count(Categoría, GanPer) %>%
  group_by(Categoría) %>%
  mutate(total = sum(n)) %>%
  ungroup()

ggplot(df_lines_sum, aes(x = Categoría, y = n, fill = GanPer)) +
  geom_bar(stat = "identity") +  # Crear el gráfico de barras apiladas
  scale_y_continuous(breaks = 1:nrow(df_lines))+
  labs(title= NULL, x = NULL, y = NULL)+
  scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
  geom_text(aes(label = n, group = GanPer),  # Agregar etiquetas con los valores de las sumas
            position = position_stack(vjust = 0.5),  # Alinear las etiquetas en el centro de cada segmento apilado
            color = "white",      # Establecer el color del texto
            size = 3)             # Establecer el tamaño del texto

ggplot(data=df_lines)+
    cancha_y_coord+
      labs(title= NULL, x = NULL, y = NULL) 
       

#Scrums
df_scrum <- filter(df, Categoría == "Scrum propio" | Categoría == "Scrum rival")
df_scrum_sum <- df_scrum %>%
                  count(Categoría, GanPer) %>%
                    group_by(Categoría) %>%
                      mutate(total = sum(n)) %>%
                        ungroup()

ggplot(df_scrum_sum, aes(x = Categoría, y = n, fill = GanPer)) +
  geom_bar(stat = "identity") +  # Crear el gráfico de barras apiladas
    scale_y_continuous(breaks = 1:nrow(df_scrum))+
      labs(title= NULL, x = NULL, y = NULL)+
        scale_fill_brewer(name = "", palette = "Set1", direction = -1)+
          geom_text(aes(label = n, group = GanPer),  # Agregar etiquetas con los valores de las sumas
                    position = position_stack(vjust = 0.5),  # Alinear las etiquetas en el centro de cada segmento apilado
                    color = "white",      # Establecer el color del texto
                    size = 3)             # Establecer el tamaño del texto

ggplot(data=df_scrum)+
  cancha_y_coord+
    labs(title= NULL, x = NULL, y = NULL)
