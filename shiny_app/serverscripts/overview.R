

output$overview_header <- renderUI({
  
  HTML(paste0("<h3>Mature Inventory Assessment for ", title()," </h3>"))
  
})

description <- reactive({
  req(input$SelectVar)
  
  text <- paste0("<p>A comparison of total age, height, basal area, net 
                 merchantable volume and species, between ground sample data 
                 maintained by the B.C. Forest Analysis and Inventory Branch, 
                 vs the 2023 Forest Vegetation Composite Rank 1 Layer (Inventory), 
                 for design-based ground samples in the mature inventory 
                 population (vegetated treed >50yr old) in <b>", 
                 title(),"</b>.</p>  ")
  
  return(text)
})

output$description <- renderUI({
  HTML(description())
})


output$samplemap_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 1. Overview map of ",title(), 
              ", with ground sample plot locations colour-coded by sample design.</h5>"))
  
})


samplemap <- reactive({
  req(input$SelectVar)
  if(!is.null(clstr_id_all())){
    
    location <- sample_data %>% 
      filter(CLSTR_ID %in% clstr_id_all()) %>% 
      group_by(SITE_IDENTIFIER) %>% 
      mutate(Design = factor(Design, levels = c("GRID", "PHASE2")),
             visit_num = length(VISIT_NUMBER),
             visit_year = paste0(MEAS_YR, collapse  = ',')) %>%
      select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, BECsub,
             MGMT_UNIT, TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR, GRID_SIZE, Design, design_icon,
             BC_ALBERS_X, BC_ALBERS_Y, Latitude, Longitude) %>% 
      distinct() %>%
      left_join(lead_vol %>% 
                  filter(CLSTR_ID %in% clstr_id()) %>%
                  select(SITE_IDENTIFIER, SPECIES, AGET_TLSO, NTWB_NVAF_LS),
                by = "SITE_IDENTIFIER")
    
    location <- st_as_sf(x = location,                         
                         coords = c("Longitude", "Latitude"),
                         crs = 4326)
    
    aoimap <- tsa_sp %>%
      filter(TSA_NUMBER %in% substr(unique(sample_data[sample_data$CLSTR_ID %in% clstr_id(),]$MGMT_UNIT), 4, 5))
    
    lng1 = as.numeric(st_bbox(aoimap)[1])
    lat1 = as.numeric(st_bbox(aoimap)[2])
    lng2 = as.numeric(st_bbox(aoimap)[3])
    lat2 = as.numeric(st_bbox(aoimap)[4])
    
    
    iconFile1 <- pchIcons(3, 20, 20, 
                          col = "brown3", lwd = 3)
    iconFile2 <- pchIcons(2, 20, 20, 
                          col = "chartreuse4", lwd = 3)
    
    plotIcons <- iconList(darkred = makeIcon(iconFile1, iconWidth = 15, iconHeight = 15),
                          darkolivegreen4 = makeIcon(iconFile2, iconWidth = 10, iconHeight = 10))
    
    pal <- colorFactor(c("brown3", "chartreuse4"), location$Design)
    
    
    samplemap <- leaflet() %>% 
      addTiles() %>% 
      addProviderTiles("Esri.WorldImagery", group = "Satellite view") %>%
      addProviderTiles("Esri.WorldTerrain", group = "Terrain only") %>%
      addProviderTiles("Esri.WorldTopoMap", group = "Base map") %>%
      setMaxBounds(lng1 = -142,
                   lat1 = 46, 
                   lng2 = -112,
                   lat2 =  62) %>%
      fitBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
      addLayersControl(
        baseGroups = c("Base map", "Terrain only", "Satellite view"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>%
      addPolygons(data = aoimap, stroke = TRUE, color = "#3c8dbc", weight = 2,
                  opacity = 0.9, fill = TRUE, fillOpacity = 0.2) %>%
      addMarkers(data = location,
                 icon =  ~plotIcons[design_icon],
                 popup = paste(sep = "<br/>",
                               paste(paste("<b>Management unit</b>: ", location$MGMT_UNIT, "<br/>"),
                                     paste("<b>Sample ID</b>: ", location$SITE_IDENTIFIER, "<br/>"),
                                     paste("<b>Sample type</b>: ", location$SAMPLE_ESTABLISHMENT_TYPE, "<br/>"),
                                     #paste("<b>BEC zone</b> - ", location$BEC_ZONE, "<br/>"), 
                                     #paste("<b>BEC subzone</b> - ", location$BEC_SBZ, "<br/>"),
                                     #paste("<b>BEC variant</b> - ", location$BEC_VAR, "<br/>"), 
                                     paste0("<b>BEC/subzone/variant</b>: ", location$BEC_ZONE, "/",
                                            location$BEC_SBZ, "/",ifelse(is.na(location$BEC_VAR), "-", 
                                                                         location$BEC_VAR),"<br/>"), 
                                     #paste("<b>BEC subzone</b> - ", location$BEC_SBZ, "<br/>"),
                                     #paste("<b>BEC variant</b> - ", location$BEC_VAR, "<br/>"), 
                                     paste("<b># of measures</b>: ", location$visit_num, "<br/>"),
                                     paste("<b>Visited year</b>: ",location$visit_year, "<br/>"),
                                     paste("<b>Leading species</b>: ",location$SPECIES, "<br/>"),
                                     paste("<b>Total age of leading species</b>: ",location$AGET_TLSO, "(yrs)<br/>"),
                                     paste("<b>Live total volume</b>: ",round(location$NTWB_NVAF_LS, 1), 
                                           "(cubic m)<br/>")))) %>%
      addLegend(data = location,
                position = "bottomright",
                pal = pal, values = ~Design,
                #pch = ,
                title = "Sample Design",
                opacity = 1)
    
    unlink(iconFile1)
    unlink(iconFile2)
    
  }
  return(samplemap)
})


output$samplemap <- renderLeaflet({
  samplemap()
})





samplesize <- reactive({
  req(input$SelectVar)
  
  grid_size <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id(), Design == "GRID") %>%
    pull(grid_size) %>%
    unique()
  
  t1_1 <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    group_by(Design) %>%
    summarise(n = n(),
              max_meas_yr = max(MEAS_YR, na.rm = T)) %>%
    mutate(grid_size = ifelse(Design == "GRID", 
                              paste0("fixed area monitoring samples on a ", grid_size, " NFI grid"),
                              "temporary sample clusters using PPSWR selection")) %>%
    select(Design, n, grid_size, max_meas_yr) 
  
  t1 <- flextable(t1_1)
  
  t1 <- labelizor(
    x = t1, 
    part = "header", 
    labels = c("Design" = "Design",
               "n" = "Total", 
               "grid_size" = "Sampling method",
               "max_meas_yr" = "Last measured")) %>%
    bold(part = 'header', bold = TRUE) %>%
    colformat_num(j = 4, big.mark = "") %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Sample Size & Measurement Year by Ground Sample Design"))),
      fp_p = officer::fp_par(text.align = "left", padding = 3),
      align_with_table = FALSE) %>%
    autofit()
  
  
  return(t1)
})



output$samplesize <- renderUI({
  htmltools_value(samplesize())
})




correct_sp_lead <- reactive({
  req(input$SelectVar)
  
  correct_ls <- correct_ls()
  
  corsp_flex <- flextable(correct_ls)
  
  corsp_flex <- corsp_flex %>%
    mk_par(
      j = "correct_ls",
      value = as_paragraph(as_chunk(correct_ls, formatter = fmt_pct))) %>%
    align(align = "center", part = "all")
  
  corsp_flex <- delete_part(corsp_flex, part = "header") %>%
    border_remove() %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Leading Species Agreement \n(Inventory vs. Ground)"))),
      fp_p = officer::fp_par(text.align = "left", padding = 3),
      align_with_table = FALSE) %>%
    width(., width = 1.5)
  
  return(corsp_flex)
  
})


output$spcagree1 <- renderUI({
  htmltools_value(correct_sp_lead())
})



corsp_vol_flex <- reactive({
  req(input$SelectVar)
  
  spc_vol_dat <- spc_vol_dat()
  
  spc_vol_dat1 <- spc_vol_dat %>%
    group_by(Design, source) %>%
    reframe(#n = n(),
      SPECIES = SPECIES,
      livevolperc = livevol/sum(livevol, na.rm= T) * 100,
      deadvolperc = deadvol/sum(deadvol, na.rm= T) * 100) %>%
    ungroup() %>%
    group_by(Design) %>%
    pivot_wider(names_from = source ,
                values_from = c(livevolperc, deadvolperc)) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(livemin = min(livevolperc_Ground, livevolperc_Inventory),
           livemax = max(livevolperc_Ground, livevolperc_Inventory)) %>%
    group_by(Design) %>%
    summarise(liveminsum = sum(livemin, na.rm = T),
              livemaxsum = sum(livemax, na.rm = T)) %>%
    mutate(correct_pct = round(liveminsum/livemaxsum, 3)) %>%
    select(Design, correct_pct) %>%
    data.table
  
  corsp_vol_flex <- flextable(spc_vol_dat1)
  
  corsp_vol_flex <- corsp_vol_flex %>%
    mk_par(
      j = "correct_pct",
      value = as_paragraph(as_chunk(correct_pct, formatter = fmt_pct))) %>%
    align(align = "center", part = "all")
  
  corsp_vol_flex <- delete_part(corsp_vol_flex, part = "header") %>%
    border_remove() %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Overall Species Agreement \n(Inventory vs. Ground)"))),
      fp_p = officer::fp_par(text.align = "left", padding = 3),
      align_with_table = FALSE) %>%
    width(., width = 1.5)
    #autofit()
  
  return(corsp_vol_flex)
  
})


output$spcagree2 <- renderUI({
  htmltools_value(corsp_vol_flex())
})


fig2 <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  
  lead_vol_dat1 <- lead_vol_dat %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  fig2 <- lead_vol_dat1 %>% 
    filter(var != "voldead") %>%
    mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
    ggplot() +
    geom_hline(aes(yintercept = 0.9), linetype = 3, linewidth = 1.2, col = "darkgray") +
    geom_hline(aes(yintercept = 1.1), linetype = 3, linewidth = 1.2, col = "darkgray") +
    geom_point(aes(x = Design, y = rom, col = Design, group = Design), size = 3) +
    geom_point(aes(x = Design, y = l95rom, col = Design, group = Design), size = 3) +
    geom_point(aes(x = Design, y = u95rom, col = Design, group = Design), size = 3) +
    geom_segment(aes(y = l95rom, yend  = u95rom, x = Design, col = Design, group = Design), 
                 linewidth = 1.2, show.legend = TRUE) +
    scale_x_discrete(limit = c("PHASE2", "GRID")) +
    scale_color_manual(values = c("brown3", "chartreuse4"), drop = FALSE) +
    coord_flip() +
    facet_wrap(~var, scales = "free_y", ncol = 1, labeller = as_labeller(c(
      'age'="Age",
      'ba'="Basal Area",
      'ht'="Height",
      'vol'="Net Merchantable Volume"
    ))) +
    labs(x = "", y = "ROM", colour = NULL,
         caption = 'Figure 2. Overall Ratio of means (ground/inventory), \n95% confidence interval, & ROPE* interval for the \nlive attributes age, height, basal area & volume, \nacross each sample design.') +
    theme(
      plot.caption = element_text(hjust = 0, size=15, face = "bold"),
      plot.caption.position = "plot",
      legend.position = "top",
      legend.title = NULL,
      axis.ticks = element_blank(),
      panel.grid.major.y = element_blank(),
      #panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_blank(), axis.ticks.x = element_blank()
    ) 
  
  return(fig2)
  
})


output$fig2 <- renderPlot({
  
  fig2()
  
})


output$rope_desc <- renderUI({
  req(input$SelectVar)
  HTML("<p style='font-size:12px ;face=arial'>*For those strata with a least 8 observations, a region of practical 
       equivalence (ROPE) is pre-determined at 0.9-1.1 to assess if there is a 
       practical difference with the attribute assessed. A practical difference 
       (Y) occurs when the ratio-of-means (ROM) confidence interval (CI) sits 
       entirely outside the ROPE, no practical difference (N) occurs when the 
       ROM CI is entirely within the ROPE, all other situations are inconclusive (I).</p>")
  
})


test1 <- reactive({
  lead_vol_dat <- lead_vol_dat()
  
  lead_vol_dat1 <- lead_vol_dat %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  test1 <- lead_vol_dat1 %>%
    filter(var != "voldead") %>%
    select(Design, var, n, grd, inv, rom, l95rom, u95rom, sigrope) %>%
    mutate(var = fct_recode(var, "Age (yrs)" = "age", "HT (m)" = "ht",
                            "BA (m2/ha)" = "ba", "Volume (m3/ha)" = "vol"),
           inv = round(inv, 1),
           grd = round(grd, 1),
           rom = round(rom, 2),
           l95rom = round(l95rom, 2),
           u95rom = round(u95rom, 2),
           inv = round(inv, 1)
    ) %>%
    filter(sigrope %in% c("N", "Y")) %>%
    select(Design, var, sigrope, rom) %>%
    flextable() %>%
    align(j = 3, align = "center", part = "body") %>%
    color(i = ~ sigrope == "N", j = 3, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope == "Y", j = 3, color = 'red', part = "body") %>%
    bg(i = ~ sigrope == "N", j = 3, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope == "Y", j = 3, bg = "pink1", part = "body") %>%
    set_header_labels(
      values = list(var = "Attr.", sigrope = "Diff?", rom = "Ratio")) %>%
    add_header_lines(values = c("Overall Age, Ht, Volume Tests")) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(test1)
  
})


output$test1 <- renderUI({
  htmltools_value(test1())
})


test2 <- reactive({
  lead_vol_dat <- lead_vol_dat()
  
  lead_vol_dat1 <- lead_vol_dat %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  test2 <- lead_vol_dat1 %>%
    filter(var == "voldead") %>%
    select(Design, var, n, grd, inv, rom, l95rom, u95rom, sigrope) %>%
    mutate(var = "Volume (m3/ha)",
           inv = round(inv, 1),
           grd = round(grd, 1),
           rom = round(rom, 2),
           l95rom = round(l95rom, 2),
           u95rom = round(u95rom, 2),
           inv = round(inv, 1)
    ) %>%
    filter(sigrope %in% c("N", "Y")) %>%
    select(Design, var, sigrope, rom) %>%
    flextable() %>%
    align(j = 3, align = "center", part = "body") %>%
    color(i = ~ sigrope == "N", j = 3, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope == "Y", j = 3, color = 'red', part = "body") %>%
    bg(i = ~ sigrope == "N", j = 3, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope == "Y", j = 3, bg = "pink1", part = "body") %>%
    set_header_labels(
      values = list(var = "Attr.", sigrope = "Diff?", rom = "Ratio")) %>%
    add_header_lines(values = c("Dead Volume Test")) %>%
    bold(part = 'header', bold = TRUE) %>%
    autofit()
  
  return(test2)
  
})


output$test2 <- renderUI({
  htmltools_value(test2())
})




test3 <- reactive({
  invspc_vol_dat <- invspc_vol_dat()
  
  test3 <- invspc_vol_dat %>%
    filter(sigrope_vol %in% c("Y", "N") & n >= 8) %>%
    select(Design, SPC_GRP_INV, n, grd_vol, inv_vol, 
           rom_vol, l95rom_vol, u95rom_vol, sigrope_vol) %>%
    mutate(var = "Volume (m3/ha)",
           grd_vol = round(grd_vol, 1),
           inv_vol = round(inv_vol, 1),
           rom_vol = round(rom_vol, 2),
           l95rom_vol = ifelse(n >= 8,  round(l95rom_vol, 2), NA),
           u95rom_vol = ifelse(n >= 8,  round(u95rom_vol, 2), NA),
           sigrope_vol = ifelse(n >= 8,  sigrope_vol, "-")
    ) %>%
    select(Design, var, SPC_GRP_INV, sigrope_vol, rom_vol) %>%
    flextable() %>%
    merge_v(j = ~Design) %>%
    align(j = 4, align = "center", part = "body") %>%
    color(i = ~ sigrope_vol == "N", j = 4, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope_vol == "Y", j = 4, color = 'red', part = "body") %>%
    bg(i = ~ sigrope_vol == "N", j = 4, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope_vol == "Y", j = 4, bg = "pink1", part = "body")  %>%
    set_header_labels(
      values = list(var = "Attr.", SPC_GRP_INV = "Spc.", sigrope_vol = "Diff?", 
                    rom_vol = "Ratio")) %>%
    add_header_lines(values = c("Leading Species Live Volume Test")) %>%
    bold(part = 'header', bold = TRUE) 
  
  
  return(test3)
  
})


output$test3 <- renderUI({
  htmltools_value(test3())
})



output$test_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Listing of those Attributes where Ground:Inventory ratio of 
              means are practically different (Y) or not practically different 
              (N) from 1.0. Attributes which are not listed here have 
              inconclusive (I) results.</h5>"))
  
})