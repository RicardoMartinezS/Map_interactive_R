# HTML UBICACION CLIENTES --------------------------------------------------------------

##LIBRERIAS NECESARIAS -----------------------------------------------------------------

library(leaflet)  # libreria para graficar mapas interactivos
library(sf)  # manejo de informacion geografica 
library(viridis)  # paletas de colores
library(RColorBrewer)  # mas paletas de colores
library(dplyr)  # manejo de bases de datos
library(htmlwidgets)  # para guardar el mapa
library(ggplot2)        # Ploteo de graficos
library(scales)
library(htmltools)
library(stringr)

## CARGA DE SHAPEFILES --------------------------------------------------------------------------

ruta <- '...'
shape <- st_read(paste0(ruta, '...'))

## COLOR PALETA ----------------------------------------------------------------------------------
shape <- shape %>% 
  mutate(color = case_when(str_detect(Sector.d_1, "Wimpeshi") ~ "#a6cee3",
                           str_detect(Sector.d_1, "Urraichipa") ~ "#b2df8a",
                           str_detect(Sector.d_1, "Amalipa") ~ "#fb9a99" ,
                           str_detect(Sector.d_1, "Riritana") ~ "#fdbf6f"))


colores <- c('#a6cee3', '#b2df8a', '#fb9a99','#fdbf6f')

palfac <- colorFactor( palette = colores, domain = shape$Sector.de)
pal <- colorFactor(palette = c("#a6cee3", "#b2df8a", "#fb9a99","#fdbf6f"), domain = levels(shape$Sector.de))


#ggplot() + geom_sf(data = shape)

#specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

## ELABORACIÓN POP UP ---------------------------------------------------------------------

Aprobado <- shape %>% 
  filter( shape$Estado.Enc == "Aprobado")

pop_Aprobado <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Aprobado$Numero.de), "<br>", 
                       "<b>", "Encuestador: ", "</b>", as.character(Aprobado$Encuestado), "<br>", 
                       "<b>", "Supervisor: ", "</b>", as.character(Aprobado$Supervisor), "<br>",
                       "<b>", "Sector de ubicación: ", "</b>", as.character(Aprobado$Sector.d_1), "<br>",
                       "<b>", "Tipo de vivienda: ", "</b>", as.character(Aprobado$Tipo.de.vi), "<br>",
                       "<b>", "Material predominante paredes exteriores: ", "</b>", as.character(Aprobado$Material.p), "<br>",
                       "<b>", "Material predominante pisos: ", "</b>", as.character(Aprobado$Material_1), "<br>",
                       "<b>", "Hogares que habitan en la vivienda: ", "</b>", as.character(Aprobado$X.Cuántos), "<br>",
                       "<b>", "Estado Encuesta: ", "</b>", as.character(Aprobado$Estado.Enc), "<br>",
                       "<b>", "Fecha Creación: ", "</b>", as.character(Aprobado$Fecha.Crea), "<br>",
                       "<b>", "Fecha Cargue: ", "</b>", as.character(Aprobado$Fecha.Crgu), "<br>",
                       "<b>", "Usuario Creación: ", "</b>", as.character(Aprobado$Usuario.Cr), "<br>",
                       "<b>", "Activo: ", "</b>", as.character(Aprobado$Activo), "<br>")

Enviado <- shape %>% 
  filter( shape$Estado.Enc == "Enviado")

pop_Enviado <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Enviado$Numero.de), "<br>", 
                         "<b>", "Encuestador: ", "</b>", as.character(Enviado$Encuestado), "<br>", 
                         "<b>", "Supervisor: ", "</b>", as.character(Enviado$Supervisor), "<br>",
                         "<b>", "Sector de ubicación: ", "</b>", as.character(Enviado$Sector.d_1), "<br>",
                         "<b>", "Tipo de vivienda: ", "</b>", as.character(Enviado$Tipo.de.vi), "<br>",
                         "<b>", "Material predominante paredes exteriores: ", "</b>", as.character(Enviado$Material.p), "<br>",
                         "<b>", "Material predominante pisos: ", "</b>", as.character(Enviado$Material_1), "<br>",
                         "<b>", "Hogares que habitan en la vivienda: ", "</b>", as.character(Enviado$X.Cuántos), "<br>",
                         "<b>", "Estado Encuesta: ", "</b>", as.character(Enviado$Estado.Enc), "<br>",
                         "<b>", "Fecha Creación: ", "</b>", as.character(Enviado$Fecha.Crea), "<br>",
                         "<b>", "Fecha Cargue: ", "</b>", as.character(Enviado$Fecha.Crgu), "<br>",
                         "<b>", "Usuario Creación: ", "</b>", as.character(Enviado$Usuario.Cr), "<br>",
                         "<b>", "Activo: ", "</b>", as.character(Enviado$Activo), "<br>")

Proceso <- shape %>% 
  filter( shape$Estado.Enc == "Proceso")

pop_Proceso <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Proceso$Numero.de), "<br>", 
                      "<b>", "Encuestador: ", "</b>", as.character(Proceso$Encuestado), "<br>", 
                      "<b>", "Supervisor: ", "</b>", as.character(Proceso$Supervisor), "<br>",
                      "<b>", "Sector de ubicación: ", "</b>", as.character(Proceso$Sector.d_1), "<br>",
                      "<b>", "Tipo de vivienda: ", "</b>", as.character(Proceso$Tipo.de.vi), "<br>",
                      "<b>", "Material predominante paredes exteriores: ", "</b>", as.character(Proceso$Material.p), "<br>",
                      "<b>", "Material predominante pisos: ", "</b>", as.character(Proceso$Material_1), "<br>",
                      "<b>", "Hogares que habitan en la vivienda:: ", "</b>", as.character(Proceso$X.Cuántos), "<br>",
                      "<b>", "Estado Encuesta: ", "</b>", as.character(Proceso$Estado.Enc), "<br>",
                      "<b>", "Fecha Creación: ", "</b>", as.character(Proceso$Fecha.Crea), "<br>",
                      "<b>", "Fecha Cargue: ", "</b>", as.character(Proceso$Fecha.Crgu), "<br>",
                      "<b>", "Usuario Creación: ", "</b>", as.character(Proceso$Usuario.Cr), "<br>",
                      "<b>", "Activo: ", "</b>", as.character(Proceso$Activo), "<br>")


# HTML INDICADOR -----------------------------------------------------------------------------------------------------------------------------


mapa <- leaflet(shape) %>%
  
  addProviderTiles("CartoDB.Voyager") %>%
  
  addCircleMarkers (data = Aprobado,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Aprobado,
                    label = Aprobado$Sector.d_1,
                    #clusterOptions = markerOptions(Wimpeshi$Estado.Enc),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 4,
                    color = Aprobado$color,
                    group = "Aprobado")%>% 
  
  addCircleMarkers (data = Enviado,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Enviado,
                    label =Enviado$Sector.d_1,
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = ~Enviado$color,
                    group = "Enviado")%>% 
  
  addCircleMarkers (data = Proceso,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Proceso,
                    label =Proceso$Sector.d_1,
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = Proceso$color,
                    group = "Proceso")%>% 
  
  
  addLegend(position = "bottomright", pal = palfac, values = ~shape$Sector.de,
            title = "Encuestas por Sector:", opacity = 1, 
            group = "Leyenda") %>% 
  
  
  addLayersControl(overlayGroups = c("Aprobado","Enviado","Proceso"), 
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Leyenda")

mapa

htmlwidgets::saveWidget(mapa, "..../Mapa1.HTML")




