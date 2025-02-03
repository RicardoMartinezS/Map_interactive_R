#-----------------------------------------------------HTML UBICACION CLIENTES--------------------------------------#


#-------------------------------------------------------LIBRERIAS NECESARIAS---------------------------------------#

library(leaflet)  # libreria para graficar mapas interactivos
library(sf)  # manejo de informacion geografica 
library(viridis)  # paletas de colores
library(RColorBrewer)  # mas paletas de colores
library(dplyr)  # manejo de bases de datos
library(htmlwidgets)  # para guardar el mapa
library(ggplot2)        # Ploteo de graficos
library(scales)
library(htmltools)
#-----------------------------------------------------------CARGA DE SHAPEFILES------------------------------------#

root <- 'C:/Users/ricardo.martinez/Documents/RM_FZ/Otras solicitudes/Parqueaderos'
shape <- st_read(paste0(root, '/Parqueaderos_Georef.shp'))
#ggplot() + geom_sf(data = shape)



# POP UP---------------------------------------------------------------------------------------------------------------------------------------

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall = k))

# Elaboracion del popup:




ALMACEN <- shape %>% 
  filter(TIPO.DE.SI == "ALMACEN")

pop_alm <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(ALMACEN$TIPO.DE.SI), "<br>", 
                "<b>", "Nombre del sitio de interes: ", "</b>", as.character(ALMACEN$NOMBRE.SIT), "<br>", 
                "<b>", "Dirección: ", "</b>", as.character(ALMACEN$Direccion), "<br>",
                "<b>", "Parqueaderos Carro: ", "</b>", format(ALMACEN$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                "<b>", "Parqueaderos motos: ", "</b>", format(ALMACEN$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                "<b>", "Parqueaderos bicicletas: ", "</b>", format(ALMACEN$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                "<b>", "Total parqueaderos: ", "</b>", format(ALMACEN$Total,0, big.mark=",", decimal.mark="."), "<br>")

COMPLEJO <- shape %>% 
  filter(TIPO.DE.SI == "COMPLEJO MEDICO")

pop_com <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(COMPLEJO$TIPO.DE.SI), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(COMPLEJO$NOMBRE.SIT), "<br>", 
                  "<b>", "Dirección: ", "</b>", as.character(COMPLEJO$Direccion), "<br>",
                  "<b>", "Parqueaderos Carro: ", "</b>", format(COMPLEJO$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos motos: ", "</b>", format(COMPLEJO$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos bicicletas: ", "</b>", format(COMPLEJO$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Total parqueaderos: ", "</b>", format(COMPLEJO$Total,0, big.mark=",", decimal.mark="."), "<br>")

CORPORATIVO <- shape %>% 
  filter(TIPO.DE.SI == "CORPORATIVO")

pop_cor <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(CORPORATIVO$TIPO.DE.SI), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(CORPORATIVO$NOMBRE.SIT), "<br>", 
                  "<b>", "Dirección: ", "</b>", as.character(CORPORATIVO$Direccion), "<br>",
                  "<b>", "Parqueaderos Carro: ", "</b>", format(CORPORATIVO$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos motos: ", "</b>", format(CORPORATIVO$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos bicicletas: ", "</b>", format(CORPORATIVO$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Total parqueaderos: ", "</b>", format(CORPORATIVO$Total,0, big.mark=",", decimal.mark="."), "<br>")

GYM <- shape %>% 
  filter(TIPO.DE.SI == "GYM")

pop_gym <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(GYM$TIPO.DE.SI), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(GYM$NOMBRE.SIT), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(GYM$Direccion), "<br>",
                  "<b>", "Parqueaderos Carro: ", "</b>", format(GYM$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos motos: ", "</b>", format(GYM$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos bicicletas: ", "</b>", format(GYM$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Total parqueaderos: ", "</b>", format(GYM$Total,0, big.mark=",", decimal.mark="."), "<br>")

LOTE <- shape %>% 
  filter(TIPO.DE.SI == "LOTE")

pop_lot <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(LOTE$TIPO.DE.SI), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(LOTE$NOMBRE.SIT), "<br>", 
                  "<b>", "Dirección: ", "</b>", as.character(LOTE$Direccion), "<br>",
                  "<b>", "Parqueaderos Carro: ", "</b>", format(LOTE$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos motos: ", "</b>", format(LOTE$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos bicicletas: ", "</b>", format(LOTE$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Total parqueaderos: ", "</b>", format(LOTE$Total,0, big.mark=",", decimal.mark="."), "<br>")

MALL <- shape %>% 
  filter(TIPO.DE.SI == "MALL")

pop_mall <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(MALL$TIPO.DE.SI), "<br>", 
                  "<b>", "Nombre del sitio de interes: ", "</b>", as.character(MALL$NOMBRE.SIT), "<br>", 
                  "<b>", "Dirección: ", "</b>", as.character(MALL$Direccion), "<br>",
                  "<b>", "Parqueaderos Carro: ", "</b>", format(MALL$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos motos: ", "</b>", format(MALL$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Parqueaderos bicicletas: ", "</b>", format(MALL$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                  "<b>", "Total parqueaderos: ", "</b>", format(MALL$Total,0, big.mark=",", decimal.mark="."), "<br>")

RESTAURANTE <- shape %>% 
  filter(TIPO.DE.SI == "RESTAURANTE")

pop_rest <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(RESTAURANTE$TIPO.DE.SI), "<br>", 
                   "<b>", "Nombre del sitio de interes: ", "</b>", as.character(RESTAURANTE$NOMBRE.SIT), "<br>", 
                   "<b>", "Dirección: ", "</b>", as.character(RESTAURANTE$Direccion), "<br>",
                   "<b>", "Parqueaderos Carro: ", "</b>", format(RESTAURANTE$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Parqueaderos motos: ", "</b>", format(RESTAURANTE$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Parqueaderos bicicletas: ", "</b>", format(RESTAURANTE$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Total parqueaderos: ", "</b>", format(RESTAURANTE$Total,0, big.mark=",", decimal.mark="."), "<br>")

UNIVERSIDAD <- shape %>% 
  filter(TIPO.DE.SI == "UNIVERSIDAD")

pop_u <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(UNIVERSIDAD$TIPO.DE.SI), "<br>", 
                   "<b>", "Nombre del sitio de interes: ", "</b>", as.character(UNIVERSIDAD$NOMBRE.SIT), "<br>", 
                   "<b>", "Dirección: ", "</b>", as.character(UNIVERSIDAD$Direccion), "<br>",
                   "<b>", "Parqueaderos Carro: ", "</b>", format(UNIVERSIDAD$Carro,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Parqueaderos motos: ", "</b>", format(UNIVERSIDAD$Moto,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Parqueaderos bicicletas: ", "</b>", format(UNIVERSIDAD$Bici,0, big.mark=",", decimal.mark="."), "<br>",
                   "<b>", "Total parqueaderos: ", "</b>", format(UNIVERSIDAD$Total,0, big.mark=",", decimal.mark="."), "<br>")

CENTRO_COMERCIAL <- shape %>% 
  filter(TIPO.DE.SI == "CENTRO COMERCIAL")

pop_cc <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(CENTRO_COMERCIAL$TIPO.DE.SI), "<br>", 
                "<b>", "Nombre del sitio de interes: ", "</b>", as.character(CENTRO_COMERCIAL$NOMBRE.SIT), "<br>", 
                "<b>", "Dirección: ", "</b>", as.character(CENTRO_COMERCIAL$Direccion), "<br>")

PROYECTO_VIV <- shape %>% 
  filter(TIPO.DE.SI == "PROYECTO DE VIVIENDA")

pop_pv <- paste0("<b>", "Tipo de sitio de interes: ", "</b>", as.character(PROYECTO_VIV$TIPO.DE.SI), "<br>", 
                 "<b>", "Nombre del sitio de interes: ", "</b>", as.character(PROYECTO_VIV$NOMBRE.SIT), "<br>", 
                 "<b>", "Dirección: ", "</b>", as.character(PROYECTO_VIV$Direccion), "<br>")


colores <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c',
             '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a')

# HTML INDICADOR -----------------------------------------------------------------------------------------------------------------------------

ID <- levels(as.factor(shape$TIPO.DE.SI))
paleta <- colorFactor(palette = colores, domain = ID)



mapa <- leaflet(shape) %>%
  
  addProviderTiles("CartoDB.Voyager") %>%
  
  
  addCircleMarkers (data = ALMACEN,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_alm,
                    label =ALMACEN$NOMBRE.SIT,
                    clusterOptions = markerOptions(ALMACEN$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#a6cee3',
                    group = "ALMACEN")%>% 
  
  addCircleMarkers (data = CENTRO_COMERCIAL,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_cc,
                    label =CENTRO_COMERCIAL$NOMBRE.SIT,
                    clusterOptions = markerOptions(CENTRO_COMERCIAL$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#1f78b4',
                    group = "CENTRO COMERCIAL") %>% 
  
  addCircleMarkers (data = COMPLEJO,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_com,
                    label =COMPLEJO$NOMBRE.SIT,
                    clusterOptions = markerOptions(COMPLEJO$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#b2df8a',
                    group = "COMPLEJO MEDICO") %>% 

  
  addCircleMarkers (data = CORPORATIVO,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_cor,
                    label =CORPORATIVO$NOMBRE.SIT,
                    clusterOptions = markerOptions(CORPORATIVO$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#33a02c',
                    group = "CORPORATIVO")%>% 
  
  addCircleMarkers (data = GYM,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_gym,
                    label =GYM$NOMBRE.SIT,
                    clusterOptions = markerOptions(GYM$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#fb9a99',
                    group = "GYM") %>% 
  
  addCircleMarkers (data = LOTE,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_lot,
                    label =LOTE$NOMBRE.SIT,
                    clusterOptions = markerOptions(LOTE$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#e31a1c',
                    group = "LOTE")%>% 
  
  addCircleMarkers (data = MALL,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_mall,
                    label =MALL$NOMBRE.SIT,
                    clusterOptions = markerOptions(MALL$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#fdbf6f',
                    group = "MALL") %>% 

  
  
  addCircleMarkers (data = PROYECTO_VIV,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_pv,
                    label =PROYECTO_VIV$NOMBRE.SIT,
                    clusterOptions = markerOptions(PROYECTO_VIV$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#ff7f00',
                    group = "PROYECTO DE VIVIENDA") %>% 
  
  
  addCircleMarkers (data = RESTAURANTE,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_rest,
                    label =RESTAURANTE$NOMBRE.SIT,
                    clusterOptions = markerOptions(RESTAURANTE$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#cab2d6',
                    group = "RESTAURANTE")%>% 
  
  
  addCircleMarkers (data = UNIVERSIDAD,
                    weight = 1, 
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    popup = pop_u,
                    label =UNIVERSIDAD$NOMBRE.SIT,
                    clusterOptions = markerOptions(UNIVERSIDAD$TIPO.DE.SI),
                    labelOptions = labelOptions(direction = "auto"),
                    radius = 5,
                    color = '#6a3d9a',
                    group = "UNIVERSIDAD") %>% 
  

  
  
  
    addLegend(position = "bottomleft", pal = paleta, values = ~shape$TIPO.DE.SI,
          title = "SITIO DE INTERES", opacity = 1, 
          group = "Leyenda") %>% 

addLayersControl(overlayGroups = c("ALMACEN","CENTRO COMERCIAL","COMPLEJO MEDICO","CORPORATIVO","GYM","LOTE", "MALL","PROYECTO DE VIVIENDA","RESTAURANTE","UNIVERSIDAD"), 
                 options = layersControlOptions(collapsed = TRUE)) %>% 
  hideGroup("Leyenda")
    
    mapa



htmlwidgets::saveWidget(mapa, "C:/Users/fernando.zubieta/Documents/FZ_RM/Otras solicitudes/Parqueaderos/HTML/ParqueaderoGeoRef.HTML")



