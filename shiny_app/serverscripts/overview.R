

output$overview_header <- renderUI({
  
  HTML(paste0("<h3>Mature Inventory Assessment for ", title()," </h3>"))
  
})

description <- reactive({
  req(input$SelectVar)
  
  #text <- paste0("<p>A comparison of total age, height, basal area, net 
  #               merchantable volume and species, between ground sample data 
  #               maintained by the B.C. Forest Analysis and Inventory Branch, 
  #               vs the 2023 Forest Vegetation Composite Rank 1 Layer (Inventory), 
  #               for design-based samples in the mature inventory 
  #               population (vegetated treed >50yr old) in <b>", 
  #               title(),"</b>.</p>",
  #               ifelse(input$SelectVar == "Quesnel TSA", 
  #                      "<p>The sample space for Quesnel TSA PHASE2 is confined 
  #                      to the eastern part of the TSA, as the western region 
  #                      lacked mature green stands at the time of the most recent 
  #                      PHASE1 sampling due to the MPB infestation. To enhance 
  #                      coverage, Quesnel TSA implemented intensified sampling 
  #                      grids with supplementary (SUP) samples. This enabled the 
  #                      delineation of three distinct assessment domains: 
  #                      Quesnel East, assessed using PHASE2 samples; Quesnel West, 
  #                      assessed using a combination of CMI and SUP samples; 
  #                      and the overall Quesnel TSA, assessed using CMI samples only.</p>", ""))
  
  text <- paste0("<p>This document provides a high-level technical summary of 
                 results compiled by FAIB for the mature stand ground sampling 
                 (>50 years) from our CMI and VRI PHASE2 ground sample programs: </p>",
                 "<ul><li><b>GRID -</b> Change Monitoring Inventory (CMI) ground 
                 samples have been established across BC. The target population 
                 for the CMI ground sample program is defined as all Crown 
                 forested stands in the Vegetation Resources Inventory (VRI) 
                 Vegcomp rank 1 layer. CMI ground samples (dots on map, below) 
                 are established on a 20km X 20km grid [40km x 40km in the three 
                 northernmost TSAs and one-time intensified grid; 10km x 10km in two and 10km x 20km in three 
                 TSAs (<b>SUP-GRID</b>)], with trees tagged in 0.04ha circular monitoring plots 
                 with a planned ten-year re-measurement cycle.</li>",
                 "<li><b>PHASE2 –</b> Following the completion of a photo-estimated 
                 inventory project, Vegetation Resource Inventory (VRI) audit 
                 plots will be established in the vegetated treed portion of the 
                 landbase to verify the accuracy of this new spatial inventory. 
                 VRI audit plots may be randomly established in Timber Supply 
                 Areas (TSAs), Tree Farm Licenses (TFLs), or other strata of 
                 interest. As an example, for a given TSA, the vegetated-treed, 
                 the Timber Harvest Land Base (THLB) or mature spruce portion 
                 may be sampled. Refer to the 'Reference' tab at the bottom of 
                 the page to find links to the sample designs for each PHASE2 
                 strata of interest.</li></ul>",
                 ifelse(input$SelectVar == "Quesnel TSA", 
                        "</br><p>The sample space for Quesnel TSA PHASE2 is confined 
                        to the eastern part of the TSA, as the western region 
                        lacked mature green stands at the time of the most recent 
                        PHASE1 sampling due to the MPB infestation. To enhance 
                        coverage, Quesnel TSA implemented intensified sampling 
                        grids with supplementary (SUP) samples. This enabled the 
                        delineation of three distinct assessment domains: 
                        Quesnel East, assessed using PHASE2 samples; Quesnel West, 
                        assessed using a combination of CMI and SUP samples; 
                        and the overall Quesnel TSA, assessed using CMI samples only.</p>", ""),
                 "<p>Some key objectives of this document are to: describe the 
                 characteristics and structure of mature stands, report on forest 
                 health condition, compare stand attributes of ground samples and 
                 Vegetation Resources Inventory (VRI), and estimate volume growth 
                 using growth models.</p>",
                 "<p>For summaries related to young stand ground sampling (15–50 years), 
                 please visit <a href='https://bcgov-env.shinyapps.io/YSM_techrep/' target='_blank'>YSM Technical Report</a>.</p>")
  
  return(text)
})

output$description <- renderUI({
  HTML(description())
})


output$samplemap_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 1. Overview map of ",title(), 
              ", with ground sample plot locations colour-coded by sample design. 
              The PHASE2 sample design plot locations are the actual spatial 
              coordinates while the GRID sample design plots are generalized to 
              the nearest 1*1km BC Albers grid location in the overview map. 
              Stand attributes are based on the latest measurement.</h5>"))
  
})


samplemap <- reactive({
  req(input$SelectVar)
  if(!is.null(clstr_id_all())){
    
    #location <- sample_data %>% 
    #  filter(CLSTR_ID %in% clstr_id_all()) %>% 
    #  group_by(SITE_IDENTIFIER) %>% 
    #  mutate(Design = factor(Design, levels = c("GRID", "PHASE2")),
    #         visit_num = length(VISIT_NUMBER),
    #         visit_year = paste0(MEAS_YR, collapse  = ',')) %>%
    #  select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, BECsub,
    #         MGMT_UNIT, TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR, GRID_SIZE, Design, design_icon,
    #         BC_ALBERS_X, BC_ALBERS_Y, Latitude, Longitude) %>% 
    #  distinct() %>%
    #  left_join(lead_vol %>% 
    #              filter(CLSTR_ID %in% clstr_id()) %>%
    #              select(SITE_IDENTIFIER, SPECIES, AGET_TLSO, NTWB_NVAF_LS),
    #            by = "SITE_IDENTIFIER")
    
    location <- sample_data %>% 
      filter(CLSTR_ID %in% clstr_id_all()) %>% 
      group_by(SITE_IDENTIFIER) %>% 
      mutate(#SAMPLE_ESTABLISHMENT_TYPE = ifelse(SAMPLE_ESTABLISHMENT_TYPE == "NFI", "CMI",
             #                                   SAMPLE_ESTABLISHMENT_TYPE),
             Design = ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "CMI-E"), "GRID", 
                             ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("SUP"), "SUP-GRID", "PHASE2")),
             Design = factor(Design, levels = c("GRID", "SUP-GRID","PHASE2")),
             design_icon = case_when(Design == "GRID" ~ 1,
                                     Design == "SUP-GRID" ~ 2,
                                     Design == "PHASE2" ~ 3,
                                     TRUE ~ NA),
             visit_num = length(VISIT_NUMBER),
             visit_year = paste0(MEAS_YR, collapse  = ','),
      ) %>%
      select(SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, visit_num, visit_year, BECsub,
             MGMT_UNIT, TSA_DESC, BEC_ZONE, BEC_SBZ, BEC_VAR,  Design, design_icon, #sampletype,
             BC_ALBERS_X, BC_ALBERS_Y, Latitude, Longitude,
      ) %>% 
      distinct() %>%
      left_join(lead_vol %>% 
                  filter(CLSTR_ID %in% clstr_id()) %>%
                  select(SITE_IDENTIFIER, SPECIES, AGET_TLSO, HT_TLSO, BA_HA_LS, NTWB_NVAF_LS, SPB_CPCT_LS),
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
    
    
    #iconFile1 <- pchIcons(3, 20, 20, 
    #                      col = "brown3", lwd = 3)
    #iconFile2 <- pchIcons(2, 20, 20, 
    #                      col = "chartreuse4", lwd = 3)
    #
    #plotIcons <- iconList(darkred = makeIcon(iconFile1, iconWidth = 15, iconHeight = 15),
    #                      darkolivegreen4 = makeIcon(iconFile2, iconWidth = 10, iconHeight = 10))
    #
    #pal <- colorFactor(c("brown3", "chartreuse4"), location$Design)
    
    
    iconFile1 <- pchIcons(3, 20, 20, 
                          col = "brown3", lwd = 3)
    iconFile3 <- pchIcons(3, 20, 20, 
                          col = "darkgoldenrod2", lwd = 3)
    iconFile2 <- pchIcons(2, 20, 20, 
                          col = "chartreuse4", lwd = 3)
    
    plotIcons <- iconList(darkred = makeIcon(iconFile1, iconWidth = 15, iconHeight = 15),
                          darkorange = makeIcon(iconFile3, iconWidth = 15, iconHeight = 15),
                          darkolivegreen4 = makeIcon(iconFile2, iconWidth = 10, iconHeight = 10))
    
    pal <- colorFactor(c("brown3","darkgoldenrod2", "chartreuse4"), location$Design)
    
    
    
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
                                     #paste("<b>Leading species</b>: ",location$SPECIES, "<br/>"),
                                     paste("<b>Species composition</b>: ",location$SPB_CPCT_LS, "<br/>"),
                                     paste("<b>Total age of leading species</b>: ",location$AGET_TLSO, "(yrs)<br/>"),
                                     paste("<b>Top height of leading species</b>: ",location$HT_TLSO, "(m)<br/>"),
                                     paste("<b>Live basal area</b>: ",location$BA_HA_LS, "(sq m)<br/>"),
                                     paste("<b>Live net merch volume</b>: ",round(location$NTWB_NVAF_LS, 1), 
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





samplenum <- reactive({
  req(input$SelectVar)
  
  table1_dat <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id_all()) %>%
    mutate(Design = factor(Design, levels = c("GRID", "SUP-GRID","PHASE2"))) %>%
    group_by(Design, SITE_IDENTIFIER) %>%
    arrange(VISIT_NUMBER) %>%
    mutate(visit_number_new = row_number()) %>%
    select(CLSTR_ID, Design, visit_number_new, MEAS_YR) %>%
    group_by(Design, visit_number_new, MEAS_YR) %>% tally()
  
  table1_dat <- table1_dat %>%
    arrange(MEAS_YR) %>%
    pivot_wider(id_cols = c("Design", "visit_number_new"),
                names_from = "MEAS_YR",
                values_from = "n")
  
  table1_dat <- table1_dat %>%
    arrange(Design, visit_number_new)
  
  totals <- cbind("Total", data.frame(as.list(colSums(table1_dat[,3:length(table1_dat)], na.rm = T))))
  
  flextable1 <- flextable(table1_dat) %>%
    color(~ Design == "GRID", color = "brown3") %>% 
    color(~ Design == "SUP-GRID", color = "darkgoldenrod2") %>% 
    color(~ Design == "PHASE2", color = "chartreuse4") %>%
    add_header_row(
      values = c("Design","visit_number_new", "MEAS_YR"),
      colwidths = c(1,1, length(table1_dat) -2)) %>%
    merge_v(part = "header", j = c(1, 2))  %>%
    merge_v(part = "body", j = 1)  %>%
    add_footer_row(values = totals,
                   colwidths = c(2, rep(1,length(table1_dat) -2)))
  
  flextable1 <- labelizor(
    x = flextable1, 
    part = "header", 
    labels = c("visit_number_new" = "Meas",
               "MEAS_YR" = "# Ground Samples by Year\n (end of growing season)"
    )) %>%
    bold(part = 'header', bold = TRUE) %>%
    align(align = "center",part = 'all') %>%
    set_caption(caption = as_paragraph(
      as_chunk("Table 1. Number of ground samples across all measurement years.", 
               props = fp_text_default(bold = TRUE))),
      align_with_table = FALSE,
      word_stylename = "Table Caption") %>%
    #set_caption(as_paragraph(
    #  as_b(as_chunk("Table 1. Number of ground samples across all measurement years.")))) %>%
    autofit()
  
  
  
  return(flextable1)
})



output$samplenum <- renderUI({
  htmltools_value(samplenum())
})






samplesize <- reactive({
  req(input$SelectVar)
  
  if (input$SelectVar == "Quesnel TSA"){
    
    grid_size <- sample_quesnel() %>%
      filter(CLSTR_ID %in% clstr_id()) %>%
      select(Design, grid_size) %>%
      unique()
    
    t1_dat <- sample_quesnel() %>%
      filter(CLSTR_ID %in% clstr_id())
    
  } else {
    
    grid_size <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id(), Design %in% c("GRID", "SUP-GRID")) %>%
      select(Design, grid_size) %>%
      unique()
    
    t1_dat <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id()) %>%
      mutate(Design = factor(Design, levels = c("GRID", "SUP-GRID","PHASE2"))) 
  }

  
  t1_1 <- t1_dat %>%
    group_by(Design) %>%
    summarise(n = n(),
              max_meas_yr = max(MEAS_YR, na.rm = T)) %>%
    mutate(grid_size = ifelse(Design %in% c("GRID", "Quesnel Overall"), 
                              paste0("fixed area monitoring samples on a ", 
                                     grid_size[grid_size$Design %in% c("GRID", "Quesnel Overall"),]$grid_size, 
                                     " NFI grid"),
                              ifelse(Design %in% c("SUP-GRID", "Quesnel West"), 
                                     paste0("fixed area temporary samples on a ", 
                                            grid_size[grid_size$Design %in% c("SUP-GRID", "Quesnel West"),]$grid_size, 
                                            " supplemental grid"),
                                     "temporary sample clusters using PPSWR selection")
    )) %>%
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
  
  design_col <- c("GRID" = "brown3", "PHASE2" = "chartreuse4")
  quesnel_col <- c("Quesnel Overall" = "brown3",
                   "Quesnel West" = "darkgoldenrod2",
                   "Quesnel East" = "chartreuse4")
  
  fig2 <- lead_vol_dat1 %>% 
    filter(var != "voldead") %>%
    #mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
    ggplot() +
    geom_rect(aes(ymin = 0.9, ymax = 1.1, xmin = -Inf, xmax = Inf, fill = "ROPE")) +
    #geom_hline(aes(yintercept = 0.9), linetype = 3, linewidth = 1.2, col = "darkgray") +
    #geom_hline(aes(yintercept = 1.1), linetype = 3, linewidth = 1.2, col = "darkgray") +
    geom_hline(yintercept = 1, linewidth =1.2, col = "gray30") +
    geom_point(aes(x = Design, y = rom, col = Design, group = Design), size = 3) +
    geom_point(aes(x = Design, y = l95rom, col = Design, group = Design), size = 3) +
    geom_point(aes(x = Design, y = u95rom, col = Design, group = Design), size = 3) +
    geom_segment(aes(y = l95rom, yend  = u95rom, x = Design, col = Design, group = Design), 
                 linewidth = 1.2, show.legend = TRUE) +
    #scale_x_discrete(limit = ifelse(input$SelectVar == "Quesnel TSA",
    #                                c("Quesnel West", "Quesnel East", "Quesnel Overall"),
    #                                c("PHASE2", "GRID"))) +
    #scale_color_manual(values = ifelse(input$SelectVar == "Quesnel TSA",
    #                                   quesnel_col,
    #                                   design_col), drop = FALSE) +
    #xlim(-1, 2) +
    ylim(floor(min(lead_vol_dat1[lead_vol_dat1$var != "voldead",]$l95rom)/.5)*.5, 
         max(ceiling(max(lead_vol_dat1[lead_vol_dat1$var != "voldead",]$u95rom)/.5)*.5, 
             1+ floor(min(lead_vol_dat1[lead_vol_dat1$var != "voldead",]$l95rom)/.5)*.5)) +
    coord_flip() +
    facet_wrap(~var, scales = "free_y", ncol = 2, labeller = as_labeller(c(
      'age'="Age",
      'ba'="Basal Area",
      'ht'="Height",
      'vol'="Net Merchantable Volume",
      'voldead' = "Dead Volume"
    ))) +
    labs(x = "", y = "ROM", colour = NULL) +
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
  
  
  if (input$SelectVar == "Quesnel TSA"){
    fig2 <- fig2 +
      scale_x_discrete(limit = c("Quesnel West", "Quesnel East", "Quesnel Overall")) +
      scale_color_manual(values = quesnel_col, drop = FALSE)  +
      scale_fill_manual(values = "gray90", drop = FALSE, name = NULL) 
  } else {
  fig2 <- fig2 +
    scale_x_discrete(limit = c("PHASE2", "GRID")) +
    scale_color_manual(values = design_col, drop = FALSE)  +
    scale_fill_manual(values = "gray90", drop = FALSE, name = NULL) 
  }
    
  
  return(fig2)
  
})


output$fig2 <- renderPlot({
  
  fig2()
  
})

output$fig2_desc <- renderUI({
  req(input$SelectVar)
  HTML("<h5>Overall Ratio of means (ground/inventory), 95% confidence 
       interval, & ROPE* interval for the live attributes age, height, basal 
       area & volume, across each sample design.</h5>")
  
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
    merge_v(j = ~Design) %>%
    align(j = 3, align = "center", part = "body") %>%
    color(i = ~ sigrope == "N", j = 3, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope == "Y", j = 3, color = 'red', part = "body") %>%
    bg(i = ~ sigrope == "N", j = 3, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope == "Y", j = 3, bg = "pink1", part = "body") %>%
    set_header_labels(
      values = list(var = "Attr.", sigrope = "Diff?", rom = "Ratio")) %>%
    add_header_lines(values = c("Overall Age, BA, Ht, Volume Tests")) %>%
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
    merge_v(j = ~Design) %>%
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
  HTML(paste0("<h5>Listing of those attributes where Ground:Inventory ratio of 
              means are practically different (Y) or not practically different 
              (N) from 1.0. Attributes which are not listed here have 
              inconclusive (I) results.</h5>"))
  
})




becplot <- reactive({
  req(input$SelectVar)
  if (!is.null(clstr_id())){
    
    figdata <- sample_data %>%
      filter(CLSTR_ID %in% clstr_id()) %>%
      mutate(Design = ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2")) %>%
      mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
      select(Design, BEClabel) %>%
      table()
    
    figdata1 <- data.frame(figdata)
    
    if (dim(figdata1)[2] == 1){
      figdata1 <- data.frame(figdata = row.names(figdata1), Freq = figdata1[1,1])
    }
    
    integer_breaks <- function(n = 5, ...) {
      fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
      }
      return(fxn)
    }
    
    #p <- ggplot(data.frame(rev(sort(table(figdata)))), aes(x = figdata, y = Freq)) +
    p <- ggplot(figdata1, aes(x = BEClabel, y = Freq)) +
      geom_bar(stat="identity", width=0.5, fill="steelblue") +
      facet_wrap(.~Design, drop = FALSE) +
      scale_x_discrete(guide = guide_axis(angle = -90)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                         breaks = integer_breaks()) +
      labs(x = "", y = "# of mature samples"#,
           #title = "Mature Sample Distribution by BEC subzone/variant"
           )  + 
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(colour="darkgray")
      )
    
  }
  return(p)
})


output$bec_dist <- renderPlot({
  
  becplot()
  
})

output$bec_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 2. Mature Sample Distribution by BEC subzone/variant, based on last measurement.</h5>"))
  
})
