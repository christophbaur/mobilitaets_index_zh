fct_karte <- function(data){
  
 #Dateninput
  geo_info <- data
  
  #als sf
  geo_info_sf <- st_as_sf(geo_info, coords = c("EKoord", "NKoord"), 
                          crs = 2056, agr = "constant")
  
  #transformieren
  geo_info_sf <- st_transform(geo_info_sf, crs = st_crs(4326))
  
  
  #Karte
  map <- leaflet() %>%
    
    # add basemap
    addProviderTiles(providers$CartoDB.Positron) %>%
    #MIV
    addCircles(
      data = geo_info_sf[geo_info_sf$vsys=="miv",],
      radius = 100,
      stroke = FALSE,
      label = ~ZSName,
      group = "MIV",
      color="blue"
    )%>%
    #Velo/Fuss
    addCircles(
      data = geo_info_sf[geo_info_sf$vsys=="fuss/velo",],
      radius = 100,
      stroke = FALSE,
      label = ~ZSName,
      group = "Fuss/Velo",
      color ="green"
    )%>%
    #öV
    #Velo/Fuss
    addCircles(
      data = geo_info_sf[geo_info_sf$vsys=="oev",],
      radius = 100,
      stroke = FALSE,
      label = ~ZSName,
      group = "öV",
      color ="red"
    )%>%
    addLayersControl(overlayGroups = c(
      'MIV',
      'Fuss/Velo',
      'öV'),
      options = layersControlOptions(collapsed = TRUE),
      position = 'topright')%>%
    
    addLegend(colors=c("blue", "green", "red"),
              labels = c("MIV","Fuss/Velo","öV"),
              position = 'topright')%>%
    addMiniMap(position = 'bottomleft',
               toggleDisplay = TRUE,
               tiles = providers$CartoDB.Positron)
  
  
  return(map)
  
}