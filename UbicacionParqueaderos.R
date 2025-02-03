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

ruta <- '.../Codigos/R/V_PilasII'
shape <- st_read(paste0(ruta, '/V_PilasII.shp'))

## COLOR PALETA ----------------------------------------------------------------------------------
shape <- shape %>% 
  mutate(color = case_when(str_detect(Estado.Enc, "Aprobado") ~ "#a6cee3",
                           str_detect(Estado.Enc, "Enviado") ~ "#b2df8a",
                           str_detect(Estado.Enc, "Proceso") ~ "#fb9a99"))



#ggplot() + geom_sf(data = shape)

#specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

## ELABORACIÓN POP UP ---------------------------------------------------------------------

Wimpeshi <- shape %>% 
  filter( shape$Sector.de == "1, Wimpeshi")

pop_Wimpeshi <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Wimpeshi$Numero.de), "<br>", 
                         "<b>", "Encuestador: ", "</b>", as.character(Wimpeshi$Encuestado), "<br>", 
                         "<b>", "Supervisor: ", "</b>", as.character(Wimpeshi$Supervisor), "<br>",
                         "<b>", "Sector de ubicación: ", "</b>", as.character(Wimpeshi$Sector.de), "<br>",
                         "<b>", "Tipo de vivienda: ", "</b>", as.character(Wimpeshi$Tipo.de.vi), "<br>",
                         "<b>", "Material predominante paredes exteriores: ", "</b>", as.character(Wimpeshi$Material.p), "<br>",
                         "<b>", "Material predominante pisos: ", "</b>", as.character(Wimpeshi$Material_1), "<br>",
                         "<b>", "Hogares que habitan en la vivienda: ", "</b>", as.character(Wimpeshi$X.Cuántos), "<br>",
                         "<b>", "Estado Encuesta: ", "</b>", as.character(Wimpeshi$Estado.Enc), "<br>",
                         "<b>", "Fecha Creación: ", "</b>", as.character(Wimpeshi$Fecha.Crea), "<br>",
                         "<b>", "Fecha Cargue: ", "</b>", as.character(Wimpeshi$Fecha.Crgu), "<br>",
                         "<b>", "Usuario Creación: ", "</b>", as.character(Wimpeshi$Usuario.Cr), "<br>",
                         "<b>", "Activo: ", "</b>", as.character(Wimpeshi$Activo), "<br>")

Urraichipa <- shape %>% 
  filter( shape$Sector.de == "2, Urraichipa")

pop_Urraichipa <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Urraichipa$Numero.de), "<br>", 
                "<b>", "Encuestador: ", "</b>", as.character(Urraichipa$Encuestado), "<br>", 
                "<b>", "Supervisor: ", "</b>", as.character(Urraichipa$Supervisor), "<br>",
                "<b>", "Sector de ubicación: ", "</b>", as.character(Urraichipa$Sector.de), "<br>",
                "<b>", "Tipo de vivienda: ", "</b>", as.character(Urraichipa$Tipo.de.vi), "<br>",
                "<b>", "Material predominante paredes exteriores: ", "</b>", as.character(Urraichipa$Material.p), "<br>",
                "<b>", "Material predominante pisos: ", "</b>", as.character(Urraichipa$Material_1), "<br>",
                "<b>", "¿Cuántos hogares habitan en la vivienda?: ", "</b>", as.character(Urraichipa$X.Cuántos), "<br>",
                "<b>", "Estado Encuesta: ", "</b>", as.character(Urraichipa$Estado.Enc), "<br>",
                "<b>", "Fecha Creación: ", "</b>", as.character(Urraichipa$Fecha.Crea), "<br>",
                "<b>", "Fecha Cargue: ", "</b>", as.character(Urraichipa$Fecha.Crgu), "<br>",
                "<b>", "Usuario Creación: ", "</b>", as.character(Urraichipa$Usuario.Cr), "<br>",
                "<b>", "Activo: ", "</b>", as.character(Urraichipa$Activo), "<br>")

Amalipa <- shape %>% 
  filter( shape$Sector.de == "3, Amalipa")

pop_Amalipa <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Amalipa$Numero.de), "<br>", 
                         "<b>", "Encuestador: ", "</b>", as.character(Amalipa$Encuestado), "<br>", 
                         "<b>", "Supervisor: ", "</b>", as.character(Amalipa$Supervisor), "<br>",
                         "<b>", "Sector de ubicación: ", "</b>", as.character(Amalipa$Sector.de), "<br>",
                         "<b>", "Tipo de vivienda: ", "</b>", as.character(Amalipa$Tipo.de.vi), "<br>",
                         "<b>", "Material predominante de las paredes exteriores: ", "</b>", as.character(Amalipa$Material.p), "<br>",
                         "<b>", "Material predominante de los pisos: ", "</b>", as.character(Amalipa$Material_1), "<br>",
                         "<b>", "¿Cuántos hogares habitan en la vivienda?: ", "</b>", as.character(Amalipa$X.Cuántos), "<br>",
                         "<b>", "Estado Encuesta: ", "</b>", as.character(Amalipa$Estado.Enc), "<br>",
                         "<b>", "Fecha Creación: ", "</b>", as.character(Urraichipa$Fecha.Crea), "<br>",
                         "<b>", "Fecha Cargue: ", "</b>", as.character(Urraichipa$Fecha.Crgu), "<br>",
                         "<b>", "Usuario Creación: ", "</b>", as.character(Amalipa$Usuario.Cr), "<br>",
                         "<b>", "Activo: ", "</b>", as.character(Amalipa$Activo), "<br>")

Riritana <- shape %>% 
  filter( shape$Sector.de == "4, Riritana")

pop_Riritana <- paste0("<b>", "Numero de encuesta: ", "</b>", as.character(Riritana$Numero.de), "<br>", 
                         "<b>", "Encuestador: ", "</b>", as.character(Riritana$Encuestado), "<br>", 
                         "<b>", "Supervisor: ", "</b>", as.character(Riritana$Supervisor), "<br>",
                         "<b>", "Sector de ubicación: ", "</b>", as.character(Riritana$Sector.de), "<br>",
                         "<b>", "Tipo de vivienda: ", "</b>", as.character(Riritana$Tipo.de.vi), "<br>",
                         "<b>", "Material predominante de las paredes exteriores: ", "</b>", as.character(Riritana$Material.p), "<br>",
                         "<b>", "Material predominante de los pisos: ", "</b>", as.character(Riritana$Material_1), "<br>",
                         "<b>", "¿Cuántos hogares habitan en la vivienda?: ", "</b>", as.character(Riritana$X.Cuántos), "<br>",
                         "<b>", "Estado Encuesta: ", "</b>", as.character(Riritana$Estado.Enc), "<br>",
                         "<b>", "Fecha Creación: ", "</b>", as.character(Riritana$Fecha.Crea), "<br>",
                         "<b>", "Fecha Cargue: ", "</b>", as.character(Urraichipa$Fecha.Crgu), "<br>",
                         "<b>", "Usuario Creación: ", "</b>", as.character(Riritana$Usuario.Cr), "<br>",
                         "<b>", "Activo: ", "</b>", as.character(Riritana$Activo), "<br>")

## PALETA DE COLORES --------------------------------------------------------------------------------------------------
#palfac <- colorFactor("RdBu", domain = shape$Estado.Enc)

colores <- c('#a6cee3', '#b2df8a', '#fb9a99')


# HTML INDICADOR -----------------------------------------------------------------------------------------------------------------------------


mapa <- leaflet(shape) %>%
  
  addProviderTiles("CartoDB.DarkMatter") %>%
  
  addCircleMarkers (data = Wimpeshi,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Wimpeshi,
                    label = Wimpeshi$Sector.de,
                    #clusterOptions = markerOptions(Wimpeshi$Estado.Enc),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 4,
                    color = ~color,
                    group = "Wimpeshi")%>% 
  
  addCircleMarkers (data = Urraichipa,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Urraichipa,
                    label =Urraichipa$Sector.de,
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = ~color,
                    group = "Urraichipa")%>% 
  
  addCircleMarkers (data = Amalipa,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Amalipa,
                    label =Amalipa$Sector.de,
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = ~color,
                    group = "Amalipa")%>% 
  
  addCircleMarkers (data = Riritana,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_Riritana,
                    label = Riritana$Sector.de,
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = ~color,
                    group = "Riritana")%>% 
  
  
      #addLegend(position = "bottomleft", pal = paleta() , values = ~shape$Estado.Enc,
         # title = "Sector de ubicación", opacity = 1, 
        #  group = "Leyenda") %>% 
  
  
addLayersControl(overlayGroups = c("Wimpeshi","Urraichipa","Amalipa","Riritana"), 
                 options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("Leyenda")
    
    mapa



htmlwidgets::saveWidget(mapa, "C:/Users/fernando.zubieta/Documents/FZ_RM/Otras solicitudes/Parqueaderos/HTML/ParqueaderoGeoRef.HTML")



