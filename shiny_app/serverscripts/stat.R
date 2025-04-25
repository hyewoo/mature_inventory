
ysd <- reactive({
  
  ysd <-  HTML(paste0("Ratio of Means results are summarized between ground 
                      measurements and inventory projections (projected to 
                      ground measurement year) for stand age, height, basal 
                      area and net merchantable volume (live and dead), with 
                      volume further post-stratified by leading inventory 
                      species. Results of ROPE tests are highlighted if there 
                      are practical differences (red) or not (green)."))
  return(ysd)
})

output$description_text <- renderUI({
  
  ysd()
  
})


table1 <- reactive({
  table1_dat <- table1_dat()
  
  flextable1 <- proc_freq(table1_dat, "Design", "MEAS_YR",
                          include.row_total = F,
                          include.row_percent = F,
                          include.column_percent = F,
                          include.table_percent = F) 
  
  flextable1 <- labelizor(
    x = flextable1, 
    part = "header", 
    labels = c("visit_number_new" = "Meas", 
               "MEAS_YR" = "Measurement Year")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 1. Number of ground samples at latest measurement year.")))) %>%
    autofit()
  
  return(flextable1)
})

output$table1 <- renderUI({
  
  htmltools_value(table1())
  
})


table2 <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  
  lead_vol_dat1 <- lead_vol_dat %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  
  if (input$SelectVar == "Fraser TSA"){
    
    lead_vol_tsa30 <- lead_vol_tsa30()
    
    lead_vol_dat1_1 <- lead_vol_dat1 %>%
      filter(Design == "GRID") %>%
      rbind(lead_vol_tsa30)
    
    
  } else lead_vol_dat1_1 <- lead_vol_dat1
  
  
  table2 <- lead_vol_dat1_1 %>%
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
    flextable() %>%
    merge_v(j = ~Design)
  
  row_loc <- rle(cumsum(table2$body$spans$columns[,1]))$values
  
  table2 <- table2 %>%
    fix_border_issues(part = "all") %>%
    align(j = 9, align = "center", part = "body") %>%
    hline(i = row_loc[1], part = "body") %>%
    color(i = ~ sigrope == "N", j = 9, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope == "Y", j = 9, color = 'red', part = "body") %>%
    bg(i = ~ sigrope == "N", j = 9, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope == "Y", j = 9, bg = "pink1", part = "body") 
  
  table2 <- table2 %>%
    set_header_labels(
      values = list(var = "Attribute", inv = "Inventory", grd = "Ground", 
                    rom = "ROM", l95rom = "L95%", u95rom = "U95%",
                    sigrope = "ROPE Test")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 2. Live standing average age, height, basal area, volume, ROM (grd/inv), 95% confidence limits, and ROPE test.")))) %>%
    autofit()
  
  return(table2)
  
})


output$table2 <- renderUI({
  
  htmltools_value(table2())
  
})




table3 <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  
  lead_vol_dat1 <- lead_vol_dat %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  
  if (input$SelectVar == "Fraser TSA"){
    
    lead_vol_tsa30 <- lead_vol_tsa30()
    
    lead_vol_dat1_1 <- lead_vol_dat1 %>%
      filter(Design == "GRID") %>%
      rbind(lead_vol_tsa30)
    
  } else lead_vol_dat1_1 <- lead_vol_dat1
  
  
  table3 <- lead_vol_dat1_1 %>%
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
    flextable() %>%
    align(j = 9, align = "center", part = "body") %>%
    color(i = ~ sigrope == "N", j = 9, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope == "Y", j = 9, color = 'red', part = "body") %>%
    bg(i = ~ sigrope == "N", j = 9, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope == "Y", j = 9, bg = "pink1", part = "body") 
  
  
  table3 <- table3 %>%
    set_header_labels(
      values = list(var = "Attribute", inv = "Inventory", grd = "Ground", 
                    rom = "ROM", l95rom = "L95%", u95rom = "U95%",
                    sigrope = "ROPE Test")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 3. Dead standing average volume, ROM (grd/inv), 95% confidence limits, and ROPE test.")))) %>%
    autofit()
  
  
  return(table3)
  
})


output$table3 <- renderUI({
  
  htmltools_value(table3())
  
})


table4 <- reactive({
  
  invspc_vol_dat <- invspc_vol_dat()
  
  
  if (input$SelectVar == "Fraser TSA"){
    
    invspc_vol_tsa30 <- invspc_vol_tsa30()
    
    invspc_vol_dat1 <- invspc_vol_dat %>%
      filter(Design == "GRID") %>%
      rbind(invspc_vol_tsa30)
    
  } else invspc_vol_dat1 <- invspc_vol_dat
  
  
  table4 <- invspc_vol_dat1 %>%
    select(Design, SPC_GRP_INV, n, grd_vol, inv_vol, 
           rom_vol, l95rom_vol, u95rom_vol, sigrope_vol) %>%
    mutate(Design = factor(Design, levels = c("GRID", "PHASE2")),
           grd_vol = round(grd_vol, 1),
           inv_vol = round(inv_vol, 1),
           rom_vol = round(rom_vol, 2),
           l95rom_vol = ifelse(n >= 8,  round(l95rom_vol, 2), NA),
           u95rom_vol = ifelse(n >= 8,  round(u95rom_vol, 2), NA),
           sigrope_vol = ifelse(n >= 8,  sigrope_vol, "-")
    ) %>%
    flextable() %>%
    merge_v(j = ~Design) 
  
  row_loc <- rle(cumsum(table4$body$spans$columns[,1]))$values
  
  table4 <- table4%>%
    fix_border_issues(part = "all") %>%
    align(j = 9, align = "center", part = "body") %>%
    hline(i = row_loc[1], part = "body") %>%
    color(i = ~ sigrope_vol == "N", j = 9, color = 'darkgreen', part = "body")  %>%
    color(i = ~ sigrope_vol == "Y", j = 9, color = 'red', part = "body") %>%
    bg(i = ~ sigrope_vol == "N", j = 9, bg = "lightgreen", part = "body") %>%
    bg(i = ~ sigrope_vol == "Y", j = 9, bg = "pink1", part = "body") 
  
  
  table4 <- table4 %>%
    set_header_labels(
      values = list(SPC_GRP_INV = "Species Grp", inv_vol = "Inventory", grd_vol = "Ground", 
                    rom_vol = "ROM", l95rom_vol = "L95%", u95rom_vol = "U95%",
                    sigrope_vol = "ROPE Test")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 4. Live standing volume by leading inventory species group, ROM (grd/inv), 95% confidence limits, and ROPE test. Species groups defined by the top three leading inventory species (by # samples), with 'OTH' comprising remaining species.")))) %>%
    autofit()
  
  return(table4)
  
})



output$table4 <- renderUI({
  
  htmltools_value(table4())
  
})



bias_comp <- reactive({
  
  bias_comp <-  paste0("<p>The overall differences between Ground and 
                            Inventory values are further divided into components 
                            of bias (model vs attribute bias) to assess their 
                            individual contributions to total bias. Both basal 
                            area and volume attributes are assessed by design 
                            type.</p>
                       
                       <ul><li><u>Total Bias</u> = Ground measurement minus 
                       model projection (based on inventory input attributes). 
                       Assesses the total error between ground sample 
                       measurement & VDYP growth model projection.</li>
                       
                       <li><u>Model Bias</u> = Ground measurement minus model 
                       projection (based on ground sample input attributes). 
                       Assesses the error due to the growth model VDYP.</li>
                       
                       <li><u>Attribute Bias</u> = Total Bias minus Model Bias. 
                       Assesses the error due to the inventory classification 
                       attributes.</li></ul></br>")
  return(bias_comp)
})

output$bias_comp <- renderUI({
  
  HTML(bias_comp())
  
})

fig3 <- reactive({
  
  bias_source <- bias_source()
  
  fig3_dat <- bias_source %>%
    select(Design, starts_with("totbias_"),starts_with("modbias_"),starts_with("attrbias_"),
           starts_with("rom_"),starts_with("rommod_"),starts_with("romattr_"),
           starts_with("pctmodbias_"),starts_with("pctattrbias_"),
           starts_with("rommodbias_"),starts_with("romattrbias_")) %>%
    #select(-N_AG_TLSO, -N_HT_TLSO) %>%
    pivot_longer(cols = totbias_vol:romattrbias_ba,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  fig3_dat1 <- fig3_dat %>%
    select(Design:attrbias) %>%
    pivot_longer(cols = ends_with("bias"),
                 names_to = "comp",
                 names_prefix = "bias",
                 values_to = "bias")
  
  fig3 <- fig3_dat1 %>%
    mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
    ggplot(aes(x = comp, y = bias, group = Design)) +
    geom_bar(aes(fill = comp, group = Design), stat = 'identity',
             position = position_dodge2(width = 0.5), width = 0.7) +
    geom_hline(aes(yintercept = 0), col = "black") +
    geom_text(aes(label=round(bias, 1), col = comp,
                  hjust = ifelse(bias >= 0, -0.5, 1.5)), show.legend = F) +
    scale_x_discrete(limit = c("totbias", "modbias", "attrbias")) +
    scale_y_continuous(limits = c(NA, NA), expand = expansion(mult = c(0.2, 0.2))) +
    scale_fill_manual(name = "", values = c("palegreen3", "lightpink1", "steelblue"),
                      labels = c("Attribute Bias", "Model Bias", "Total Bias")) +
    scale_color_manual(name = "", values = c("palegreen3", "lightpink1", "steelblue")) +
    facet_grid(Design~var, scales = "free", switch = "y", #strip.position="left",
               labeller = as_labeller(c(
                 'ba'="Basal Area (m2/ha)",
                 'vol'="Volume (m3/ha)",
                 "GRID" = "GRID",
                 "PHASE2" = "PHASE2"
               ))) +
    labs(x = "", y = "", fill = NULL) +
    coord_flip() +
    theme(legend.position = "top",
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          #panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.placement = "outside")
  
  return(fig3)
  
})



output$fig3 <- renderPlot({
  
  fig3()
  
})




fig3_1 <- reactive({
  
  bias_source <- bias_source()
  
  fig3_1 <- bias_source %>%
    select(Design, domrombias_ba) %>%
    mutate(Design = paste0(Design, " Samples")) %>%
    flextable() %>%
    align(j = 2, align = "center", part = "body") %>%
    color(i = ~ domrombias_ba == "Attribute", j = 2, color = 'darkgreen', part = "body")  %>%
    color(i = ~ domrombias_ba == "Model", j = 2, color = 'red', part = "body") %>%
    bg(i = ~ domrombias_ba == "Attribute", j = 2, bg = "lightgreen", part = "body") %>%
    bg(i = ~ domrombias_ba == "Model", j = 2, bg = "pink1", part = "body") %>%
    bg(i = ~ domrombias_ba == "Model/Attribute", j = 2, bg = "slategray", part = "body") %>%
    delete_part(part = "header")  %>%
    border_remove() %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Dominant Bias (BA)")))) %>%
    autofit()
  
  return(fig3_1)
  
})


output$fig3_1 <- renderUI({
  
  htmltools_value(fig3_1())
  
  
})




fig3_2 <- reactive({
  
  bias_source <- bias_source()
  
  fig3_2 <- bias_source %>%
    select(Design, domrombias_vol) %>%
    mutate(Design = paste0(Design, " Samples")) %>%
    flextable() %>%
    align(j = 2, align = "center", part = "body") %>%
    color(i = ~ domrombias_vol == "Attribute", j = 2, color = 'darkgreen', part = "body")  %>%
    color(i = ~ domrombias_vol == "Model", j = 2, color = 'red', part = "body") %>%
    bg(i = ~ domrombias_vol == "Attribute", j = 2, bg = "lightgreen", part = "body") %>%
    bg(i = ~ domrombias_vol == "Model", j = 2, bg = "pink1", part = "body") %>%
    bg(i = ~ domrombias_vol == "Model/Attribute", j = 2, bg = "slategray", part = "body") %>%
    delete_part(part = "header")  %>%
    border_remove() %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Dominant Bias (Volume)")))) %>%
    autofit()
  
  return(fig3_2)
  
})


output$fig3_2 <- renderUI({
  
  htmltools_value(fig3_2())
  
})

