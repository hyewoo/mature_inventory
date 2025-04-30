
fire_text <- reactive({
  
  fire_text <- paste0("<p>Recent wildfire activity has impacted a number of 
                      ground samples across some management units. All 
                      design-based ground samples were revisited following 
                      three notable fire seasons (2017, 2018, 2021) for 
                      live/dead tree status and their mortality impacts were 
                      computed. This ensures consistency when comparing ground 
                      measurements against inventory attributes, since the 
                      published VRI also incorporates attribute adjustments by 
                      burn severity class for these recent wildfires.</p>")
  
  return(fire_text)
})

output$fire_text <- renderUI({
  HTML(fire_text())
})



firesample <- reactive({
  req(input$SelectVar)
  
  firesample <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    filter(!is.na(fire_year)) %>%
    select(fire_year, ba_mortality) %>%
    mutate(fire_year_f = factor(fire_year, 
                                levels = c(2017, 2018, 2021)),
           ba_mortality_f = ifelse(ba_mortality < 0.25, "0-25", ifelse(
             ba_mortality < 0.5, "25-50", ifelse(ba_mortality < 0.75, "50-75", "75-100")
           ))) %>%
    mutate(ba_mortality_f = factor(ba_mortality_f, 
                                   levels = c("75-100", "50-75", "25-50", "0-25")))
  
  return(firesample)
})


fig8 <- reactive({
  
  firesample <- firesample()
  
  fig8 <- if(nrow(firesample) > 0){
    
    ggplot(firesample) +
      geom_bar(aes(fill=ba_mortality_f, x=fire_year_f), width = 0.7, show.legend=TRUE) +
      scale_fill_manual(name = "%BA \nMortality", 
                        values = c(hcl.colors(4, palette = "YlOrRd")), drop = F) +
      scale_y_continuous(
        breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1),
        expand = expand_scale(mult = c(0, 0.05))
      ) +
      scale_x_discrete(drop = F) +
      labs(x = "Wildfire Season", y = "# Ground Samples",
           title = "Ground Samples Impacted by Recent Wildfires",
           caption = "Figure 8. Number of ground samples impacted by widfires, grouped \nby percent basal area mortality.") +
      theme(rect = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption = element_text(hjust = 0, size=15, face = "bold"),
            plot.caption.position = "plot")
    
    
  } else {
    ggplot() + 
      theme_void(15) +
      geom_text(aes(0,0,label='No sample impacted by fire')) +
      xlab(NULL)
  }
  
  return(fig8)
})



output$fig8 <- renderPlot({
  
  fig8()
  
})



#firemap <- reactive({
#  
#  firesample <- firesample()
#  
#  firemap <- if(nrow(firesample) > 0){
#    
#  location <- sample_data %>% 
#    filter(CLSTR_ID %in% clstr_id()) %>% 
#    group_by(SITE_IDENTIFIER) %>% 
#    mutate(#design = ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP"), "GRID", "PHASE2"), 
#      #design_icon = ifelse(design == "GRID", 1, 2),
#      visit_num = length(VISIT_NUMBER),
#      visit_year = paste0(MEAS_YR, collapse  = ',')) %>%
#    select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, BECsub,
#           MGMT_UNIT, TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR, GRID_SIZE, Design, design_icon,
#           BC_ALBERS_X, BC_ALBERS_Y, Latitude, Longitude, fire_year, ntwb_mortality) %>% 
#    distinct()
#  
#  location <- st_as_sf(x = location,                         
#                       coords = c("Longitude", "Latitude"),
#                       crs = 4326)
#  
#  location_fire <- location %>%
#    filter(!is.na(fire_year))
#  
#  aoimap <- tsa_sp %>%
#    filter(TSA_NUMBER %in% substr(unique(sample_data[sample_data$SITE_IDENTIFIER %in% location$SITE_IDENTIFIER,]$MGMT_UNIT), 4, 5))
#  
#  firemap <- st_filter(fire_sp, aoimap)
#  
#  lng1 = as.numeric(st_bbox(aoimap)[1])
#  lat1 = as.numeric(st_bbox(aoimap)[2])
#  lng2 = as.numeric(st_bbox(aoimap)[3])
#  lat2 = as.numeric(st_bbox(aoimap)[4])
#  
#  iconFile1 <- pchIcons(3, 20, 20, 
#                        col = "brown3", lwd = 3)
#  iconFile2 <- pchIcons(2, 20, 20, 
#                        col = "chartreuse4", lwd = 3)
#  
#  plotIcons <- iconList(darkred = makeIcon(iconFile1, iconWidth = 15, iconHeight = 15),
#                        darkolivegreen4 = makeIcon(iconFile2, iconWidth = 10, iconHeight = 10))
#  
#  pal <- colorFactor(c("brown3", "chartreuse4"), factor(location$Design))
#  
#  
#  firemap <- leaflet() %>% 
#    addTiles() %>% 
#    addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
#    addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
#    addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
#    setMaxBounds(lng1 = -142,
#                 lat1 = 46, 
#                 lng2 = -112,
#                 lat2 =  62) %>%
#    fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
#    addLayersControl(
#      baseGroups = c("Base map", "Terrain only", "Satellite view"),
#      options = layersControlOptions(collapsed = FALSE),
#    ) %>%
#    addPolygons(data = aoimap, stroke = TRUE, color = "#3c8dbc", weight = 2,
#                opacity = 0.9, fill = TRUE, fillOpacity = 0.2) %>%
#    addPolygons(data = firemap, stroke = TRUE, color = "#FF7F00", weight = 2,
#                opacity = 0.9, fill = TRUE, fillOpacity = 0.2,
#                popup = paste(sep = "<br/>",
#                              paste(paste("<b>Fire number</b> - ", firemap$FIRE_NO, "<br/>"),
#                                    paste("<b>Fire year</b> - ", firemap$FIRE_YEAR, "<br/>"),
#                                    paste("<b>Fire size</b> - ", firemap$SIZE_HA, " ha<br/>"),
#                                    paste("<b>Fire cause</b> - ", firemap$FIRE_CAUSE, "<br/>")))) %>%
#    addMarkers(data = location,
#               icon =  ~plotIcons[design_icon],
#               popup = paste(sep = "<br/>",
#                             paste(paste("<b>Management unit</b> - ", location$MGMT_UNIT, "<br/>"),
#                                   paste("<b>Sample ID</b> - ", location$SITE_IDENTIFIER, "<br/>"),
#                                   paste("<b>Sample type</b> - ", location$SAMPLE_ESTABLISHMENT_TYPE, "<br/>"),
#                                   paste("<b>BEC zone</b> - ", location$BEC_ZONE, "<br/>"), 
#                                   paste("<b>BEC subzone</b> - ", location$BEC_SBZ, "<br/>"),
#                                   paste("<b>BEC variant</b> - ", location$BEC_VAR, "<br/>"), 
#                                   paste("<b># of measures</b> - ", location$visit_num, "<br/>"),
#                                   paste("<b>Visited year</b> - ",location$visit_year, "<br/>")))) %>%
#    addMarkers(data = location_fire,
#               popup = paste(sep = "<br/>",
#                             paste(paste("<b>Management unit</b> - ", location_fire$MGMT_UNIT, "<br/>"),
#                                   paste("<b>Sample ID</b> - ", location_fire$SITE_IDENTIFIER, "<br/>"),
#                                   paste("<b>Sample type</b> - ", location_fire$SAMPLE_ESTABLISHMENT_TYPE, "<br/>"),
#                                   paste("<b>BEC zone</b> - ", location_fire$BEC_ZONE, "<br/>"), 
#                                   paste("<b>BEC subzone</b> - ", location_fire$BEC_SBZ, "<br/>"),
#                                   paste("<b>BEC variant</b> - ", location_fire$BEC_VAR, "<br/>"), 
#                                   paste("<b># of measures</b> - ", location_fire$visit_num, "<br/>"),
#                                   paste("<b>Visited year</b> - ",location_fire$visit_year, "<br/>"),
#                                   paste("<b>Fire year</b> - ", location_fire$fire_year, "<br/>"),
#                                   paste("<b>Volume mortality</b> - ",location_fire$ntwb_mortality, "<br/>")))) %>%
#    addLegend(data = firemap,
#              position = "bottomright",
#              colors = "#FF7F00", 
#              labels = "Fire perimeter",
#              #pal = pal, values = ~Design,
#              #pch = ,
#              title = NULL,
#              opacity = 0.5)
#  } else {
#    ggplot() + 
#      theme_void() +
#      xlab(NULL)
#  }
#  
#  return(firemap)
#})




firemap <- reactive({
  
  firesample <- firesample()
  samplemap <- samplemap()
  
  firemap <- if(nrow(firesample) > 0){
    
    location <- sample_data %>% 
      filter(CLSTR_ID %in% clstr_id()) %>% 
      group_by(SITE_IDENTIFIER) %>% 
      mutate(#design = ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP"), "GRID", "PHASE2"), 
        #design_icon = ifelse(design == "GRID", 1, 2),
        visit_num = length(VISIT_NUMBER),
        visit_year = paste0(MEAS_YR, collapse  = ',')) %>%
      select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, BECsub,
             MGMT_UNIT, TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR, GRID_SIZE, Design, design_icon,
             BC_ALBERS_X, BC_ALBERS_Y, Latitude, Longitude, fire_year, ntwb_mortality) %>% 
      distinct() %>%
      left_join(lead_vol %>% 
                  filter(CLSTR_ID %in% clstr_id()) %>%
                  select(SITE_IDENTIFIER, SPECIES, AGET_TLSO, NTWB_NVAF_LS),
                by = "SITE_IDENTIFIER")
    
    location <- st_as_sf(x = location,                         
                         coords = c("Longitude", "Latitude"),
                         crs = 4326)
    
    location_fire <- location %>%
      filter(!is.na(fire_year))
    
    aoimap <- tsa_sp %>%
      filter(TSA_NUMBER %in% substr(unique(sample_data[sample_data$CLSTR_ID %in% clstr_id(),]$MGMT_UNIT), 4, 5))
    
    firemap <- st_filter(fire_sp, aoimap)
    
    fireicon <- awesomeIconList(
      fire = makeAwesomeIcon(icon = "fire", library = "fa",
                             iconColor = 'black',
                             markerColor = 'darkred', squareMarker = F)
    )
    
    firemap <- samplemap %>%
      addPolygons(data = firemap, stroke = TRUE, color = "#FF7F00", weight = 2,
                  opacity = 0.9, fill = TRUE, fillOpacity = 0.2,
                  popup = paste(sep = "<br/>",
                                paste(paste("<b>Fire number</b>: ", firemap$FIRE_NO, "<br/>"),
                                      paste("<b>Fire year</b>: ", firemap$FIRE_YEAR, "<br/>"),
                                      paste("<b>Fire size</b>: ", firemap$SIZE_HA, " ha<br/>"),
                                      paste("<b>Fire cause</b>: ", firemap$FIRE_CAUSE, "<br/>")))) %>%
      addAwesomeMarkers(data = location_fire,
                 icon = fireicon,
                 popup = paste(sep = "<br/>",
                               paste(paste("<b>Management unit</b>: ", location_fire$MGMT_UNIT, "<br/>"),
                                     paste("<b>Sample ID</b>: ", location_fire$SITE_IDENTIFIER, "<br/>"),
                                     paste("<b>Sample type</b>: ", location_fire$SAMPLE_ESTABLISHMENT_TYPE, "<br/>"),
                                     paste0("<b>BEC/subzone/variant</b>: ", location$BEC_ZONE, "/",location$BEC_SBZ, "/",ifelse(is.na(location$BEC_VAR), "-", location$BEC_VAR),"<br/>"), 
                                     paste("<b># of measures</b>: ", location_fire$visit_num, "<br/>"),
                                     #paste("<b>Visited year</b> - ",location_fire$visit_year, "<br/>"),
                                     paste("<b>Fire year</b>: ", location_fire$fire_year, "<br/>"),
                                     paste("<b>Volume mortality</b>: ",location_fire$ntwb_mortality*100, "%<br/>")))) %>%
      addLegend(data = firemap,
                position = "bottomright",
                colors = "#FF7F00", 
                labels = "Fire perimeter",
                #pal = pal, values = ~Design,
                #pch = ,
                title = NULL,
                opacity = 0.5) %>%
      addLegend(data = location_fire,
                position = "bottomright",
                value=iconSet,colors = "darkred",labels='Fire impact-adjusted samples', 
                #pch = ,
                title = NULL,
                opacity = 1) 
  } else {
    ggplot() + 
      theme_void() +
      xlab(NULL)
  }
  
  return(firemap)
})


output$firemap <- renderLeaflet({
  firemap()
})

