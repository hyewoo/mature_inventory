
spcomp_text <- reactive({
  req(input$SelectVar)
  
  text <- paste0("<p>Species comparisons between ground measurements and 
                 inventory labels include : 1) Leading species percent agreement 
                 (tables 6a & 6b), and 2) Overall 
                 species composition by percent basal area (figures 4 & 5).</p>")
  
  return(text)
})

output$spcomp_text <- renderUI({
  HTML(spcomp_text())
})



ld_table1 <- reactive({
  
  correct_ls <- correct_ls()
  
  LD_data <- lead_vol %>%
    filter(CLSTR_ID %in% clstr_id()) 
  
  LD_table <- proc_freq(#LD_data[LD_data$Design == "GRID",], "SPC_GRP2", "SPC_GRP1",
                        LD_data[LD_data$Design == "GRID",], "SPECIES_INV", "SPECIES",
                        include.row_total = T,
                        include.row_percent = F,
                        include.column_total = T,
                        include.column_percent = F,
                        include.table_percent = F) 
  
  LD_table <- set_caption(LD_table, caption = as_paragraph(
    as_chunk("Table 6a. Leading species cross-table comparison between Inventory and GRID samples (#).", 
             props = fp_text_default(bold = TRUE))),
    align_with_table = FALSE,
    word_stylename = "Table Caption") %>%
    add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate for the GRID Samples = ", 
                                                 correct_ls[Design == "GRID", ]$correct_ls*100, "%")))
  
  LD_table <- bg(x = LD_table, 
                 j = (1:(LD_table$body$content$ncol/2))*2, 
                 bg = "lightgray", part = "body")
  
  LD_table <- labelizor(x = LD_table, 
                        part = "header", 
                        #labels = c("SPC_GRP2" = "Inv", 
                        #           "SPC_GRP1" = "Grd")
                        labels = c("SPECIES_INV" = "Inventory", 
                                   "SPECIES" = "Ground"))  %>%
    align(align = "left", part = "header") %>%
    autofit()
  
  return(LD_table)
  
})


output$table5a <- renderUI({
  htmltools_value(ld_table1())
})





ld_table2 <- reactive({
  
  correct_ls <- correct_ls()
  
  LD_data <- lead_vol %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(SPECIES = ifelse(SPECIES %in% c(NA, ""), "Nonstock", SPECIES),
           SPECIES_INV = ifelse(SPECIES_INV %in% c(NA, ""), "Nonstock", SPECIES_INV))
  
  LD_table2 <- proc_freq(#LD_data[LD_data$Design == "PHASE2",], "SPC_GRP2", "SPC_GRP1",
                         LD_data[LD_data$Design == "PHASE2",], "SPECIES_INV", "SPECIES",
                         include.row_total = T,
                         include.row_percent = F,
                         include.column_total = T,
                         include.column_percent = F,
                         include.table_percent = F) 
  
  LD_table2 <- set_caption(LD_table2, caption = as_paragraph(
    as_chunk("Table 6b. Leading species cross-table comparison between Inventory and PHASE2 samples (#).", 
             props = fp_text_default(bold = TRUE))),
    align_with_table = FALSE,
    word_stylename = "Table Caption") %>%
    add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate for the PHASE2 Samples = ", 
                                                 correct_ls[Design == "PHASE2", ]$correct_ls*100, "%")))
  
  LD_table2 <- bg(x = LD_table2, 
                  j = (1:(LD_table2$body$content$ncol/2))*2, 
                  bg = "lightgray", part = "body")
  
  LD_table2 <- labelizor(x = LD_table2, 
                         part = "header", 
                         #labels = c("SPC_GRP2" = "Inv", 
                         #           "SPC_GRP1" = "Grd")
                         labels = c("SPECIES_INV" = "Inventory", 
                                    "SPECIES" = "Ground"))  %>%
    align(align = "left", part = "header") %>%
    autofit()
  
  return(LD_table2)
  
})


output$table5b <- renderUI({
  htmltools_value(ld_table2())
})



spcomp <- reactive({
  
  spc_vol_dat <- spc_vol_dat()
  
  spc_vol_dat1 <- spc_vol_dat %>%
    group_by(Design, source) %>%
    reframe(
      SPECIES = SPECIES,
      livevolperc = livevol/sum(livevol, na.rm= T) * 100,
      deadvolperc = deadvol/sum(deadvol, na.rm= T) * 100) %>%
    data.table
  
  return(spc_vol_dat1)
  
})


fig4 <- reactive({
  
  spcomp <- spcomp()
  correc_sp_vol <- correc_sp_vol() 
  
  correc_sp_vol <- correc_sp_vol %>%
    mutate(text = paste0("Overall Species Composition Overlap = ", correct_pct*100, "%")) %>%
    select(-correct_pct) %>%
    pivot_wider(names_from = Design,
                values_from = text)
    #mutate(ymax = ceiling(max(spcomp$livevolperc, na.rm = T)/ 5) * 5/100,
    #       ytext = ymax * 0.99,
    #       text = paste0("Overall Species Composition Overlap = ", correct_pct*100, "%"))
  
  fig4 <- spcomp %>%
    #mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
    ggplot() +
    geom_bar(aes(x = SPECIES, y = livevolperc/100, group = source, fill = source), 
             position = position_dodge2(preserve = "single"), width = 0.7, stat = "identity") +
    facet_wrap(~Design, ncol = 2, drop=FALSE) +
    #coord_cartesian(clip="off") +
    #geom_text(
    #  data=correc_sp_vol, 
    #  mapping=aes(y=ymax, x=1, label=text), hjust=0,
    #  fontface="bold", color="black"
    #) +
    scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL) +
    scale_x_discrete(drop=T) +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Species", y = "% of live net merch vol",
         title = "Live Species Composition - GRID / PHASE 2",
         caption = paste0()) +
    theme(
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      #rect = element_blank()
    ) 
  
  return(fig4)
  
})



output$fig4 <- renderPlot({
  
  fig4()
  
})

fig4_flex <- reactive({
  
  correc_sp_vol <- correc_sp_vol() 
  
  correc_sp_vol <- correc_sp_vol %>%
    mutate(text = paste0("Overall Species Composition Overlap = ", correct_pct*100, "%")) %>%
    select(-correct_pct) %>%
    pivot_wider(names_from = Design,
                values_from = text)
  
  fig4_flex <- flextable(correc_sp_vol) %>%
    #bold(part = 'all', bold = TRUE) %>%
    #align(j = 1, align = "center", part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "center", part = "all") %>%
    delete_part(part = "header") %>%
    border_remove() %>%
    width(., width = 3.7)
    #autofit()
  
  return(fig4_flex)
  
})


output$fig4_flex <- renderUI({
  req(input$SelectVar)
  htmltools_value(fig4_flex())
})


output$fig4_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 4. Overall live standing species composition (% of 
              total live merch volume reported in Table 3) between ground and 
              inventory, for both GRID (left) and PHASE 2 (right) sample designs. 
              Species percent is computed from the summed live merch volume 
              (m3/ha) by species of all plots combined. Species composition 
              overlap is a rough index, and expressed as the ratio between the 
              minimum in common relative to the maximum in common that could 
              have been.</h5>"))
  
})




fig5 <- reactive({
  
  spcomp <- spcomp()
  correc_sp_vol <- correc_sp_vol()
  
  fig5 <- spcomp %>%
    #mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
    ggplot() +
    geom_bar(aes(x = SPECIES, y = deadvolperc/100, group = source, fill = source), 
             position = position_dodge2(preserve = "single"), width = 0.7, stat = "identity") +
    facet_wrap(~Design, ncol = 2, drop=FALSE) +
    scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL) +
    scale_x_discrete(drop=T) +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Species", y = "% of live net merch vol",
         title = "Dead Species Composition - GRID / PHASE 2") +
    theme(
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
      #rect = element_blank()
    ) 
  
  return(fig5)
  
})


output$fig5 <- renderPlot({
  
  fig5()
  
})



output$fig5_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 5. Overall dead standing species composition (% of 
              total dead merch volume reported in Table 4) of ground samples, 
              for both GRID (left) and PHASE 2 (right) sample designs. Species 
              percent is computed from the summed dead merch volume (m3/ha) by 
              species of all plots combined. The inventory dead volume is 
              modeled primarily as Mountain Pine Beetle mortality, and therefore 
              assumed 100% allocated to PL.</h5>"))
  
})


output$scatter_text <- renderUI({
  req(input$SelectVar)
  HTML(paste0("For ROPE test results, <a href='#table2'>Go to Table 3</a>. "))
  
})



fig6 <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  top3spc <- top3spc()
  
  scatter <- lead_vol_dat %>%
    left_join(lead_vol %>% select(CLSTR_ID, SPECIES_INV, SPC_GRP2), by = "CLSTR_ID") %>%
    #left_join(top3spc %>% select(-n), by = c('Design', 'SPECIES_INV')) %>%
    mutate(#SPC_GRP_INV = ifelse(!is.na(top3) & top3 == "Y", SPECIES_INV, "OTH"),
           SPC_GRP_INV = ifelse(SPECIES_INV %in% top3spc$SPECIES_INV, SPECIES_INV, 'OTH'),
           #Design = factor(Design, levels = c("GRID", "PHASE2"))
           ) %>%
    select(Design, CLSTR_ID, SPECIES_INV, SPC_GRP2, SPC_GRP_INV,
           inv_ba = vdyp_ba, inv_age = PROJ_AGE_ADJ, inv_ht = vdyp_dom_ht, inv_vol = vdyp_vol_dwb, 
           grd_ba = BA_HA_LS, grd_age = AGET_TLSO, grd_ht = HT_TLSO, grd_vol = NTWB_NVAF_LS,
           rom_ba, rom_age, rom_ht, rom_vol) %>%
    #mutate(SPC_GRP_INV = ifelse(SPECIES_INV %in% top3spc, SPECIES_INV, "OTH")) %>%
    pivot_longer(cols = inv_ba:rom_vol,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  npal <- length(unique(scatter$SPC_GRP_INV))
  
  fig6 <- ggplot(scatter) +
    geom_point(aes(x = inv, y = grd, col = SPC_GRP_INV), size = 2) +
    geom_abline(aes(intercept = 0, slope = 1, linetype = "1:1"), color = "darkgray") + 
    geom_abline(aes(intercept = 0, slope = rom, linetype = "ROM"), linewidth = 1,  color = "red") +
    geom_text(aes(Inf, -Inf, label = paste0("ROM = ",round(rom, 2))), hjust = 1.2, 
              vjust = -1, size = 5) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_linetype_manual(name = NULL, 
                          values = c(2, 1),
                          guide = guide_legend(override.aes = list(color = c("darkgray", "red")))) +
    scale_color_manual(name = "SPECIES", values = (hcl.colors(npal, palette = "Dark 3"))) +
    labs(x = "Inventory", y = "Ground") +
    facet_wrap(var ~  Design , scales = "free", ncol = 2,drop=FALSE,
               labeller = as_labeller(c(
                 'ba' = 'Basal Area (m2/ha)',
                 'age'="Total Age (yrs)",
                 'ht' = 'Height (m)',
                 'vol'="Volume (m3/ha)",
                 "GRID" = "GRID",
                 "PHASE2" = "PHASE2",
                 "Quesnel West" = "Quesnel West",
                 "Quesnel East" = "Quesnel East",
                 "Quesnel Overall" = "Quesnel Overall"
               ))) +
    theme(#legend.position="bottom",
      strip.background.x = element_rect(fill=NA, colour=NA,size=1))
  
  return(fig6)
})



output$fig6 <- renderPlot({
  
  fig6()
  
})


output$fig6_caption <- renderUI({
  req(input$SelectVar)
  HTML(paste0("<h5>Figure 6. Scatter plots of ground to inventory age (top), 
              height (middle), and volume (bottom), for GRID samples (left) and 
              PHASE 2 samples (right), colour-themed by inventory leading 
              species group. Each graph also includes overlays of the computed 
              ratio of means (red line) and 1:1 reference (grey line).</h5>"))
  
})








stockplot <- reactive({
  req(input$SelectVar)
  if (!is.null(clstr_id())){
    
    fig5_dat <- tree_fh_data %>%
      filter(CLSTR_ID %in% clstr_id(), DAM_NUM == 1, LV_D == "L") %>%
      left_join(sample_data %>% select(CLSTR_ID, Design), by = "CLSTR_ID") %>%
      mutate(Design = ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2")) %>%
      mutate(Design = factor(Design, levels = c("GRID", "PHASE2"))) %>%
      group_by(Design)%>%
      mutate(VOL_WSV_HA = VOL_WSV*PHF_TREE,
             PERC_TOT_VOL_HA = VOL_WSV_HA/sum(VOL_WSV_HA, na.rm = T),
             DBH_CLASS = round(DBH/5)*5) %>% 
      mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
      mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))
    
    fig5_dat <- fig5_dat %>%
      group_by(Design, SPC_GRP1, DBH_CLASS) %>%
      summarise(PERC_TOT_VOL_HA_SPC = sum(PERC_TOT_VOL_HA, na.rm = T)) %>%
      ungroup()
    
    fig5_dat_label <- fig5_dat %>%
      group_by(Design, SPC_GRP1) %>%
      summarize(TOT_VOL_HA = sum(PERC_TOT_VOL_HA_SPC, na.rm = T)) %>%
      arrange(Design, desc(TOT_VOL_HA)) %>%
      mutate(order = row_number())
    
    fig5_dat <- fig5_dat %>%
      left_join(fig5_dat_label, by = c("Design", "SPC_GRP1"))
    
    fig5_dat <- fig5_dat %>%
      mutate(SPC_GRP2 = ifelse(order <= 6, SPC_GRP1, 'Other'),
             SPC_GRP2 = ifelse(SPC_GRP2 == 'DE', 'Decid', SPC_GRP2),
             DBH_CLASS_relevel = cut(DBH_CLASS, breaks = c(seq(-1, 59, 5), Inf), 
                                     labels = c(seq(0, 55, 5), "60+")))
    
    p <- ggplot(fig5_dat, aes(x = DBH_CLASS_relevel, y = PERC_TOT_VOL_HA_SPC, fill = SPC_GRP2)) + 
      geom_bar(stat = "identity") + 
      facet_wrap(.~Design, drop = FALSE) +
      scale_fill_brewer(name = "", palette = "Set2") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), labels = scales::percent) +
      scale_x_discrete(drop=FALSE) +
      labs(x = "DBH class (cm)", y = "% of total vol/ha",
           title = "Stock Table - live trees") +
      theme(
        #axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom"
        #rect = element_blank()
      ) 
    
  }
  return(p)
})


output$stock_table <- renderPlot({
  
  stockplot()
  
})




stdcode_text <- reactive({
  
  stdcode_text <- paste0("<p>Inventory standards of data collection vary across 
                         the province. The VRI attribute 'inventory_standard_cd' 
                         is summarized across all spatially intersected ground 
                         sample locations, as a percent of the total number of 
                         ground samples. This helps assess the reliability of 
                         Inventory attributes when compared against ground 
                         measurements.</p>")
  
  return(stdcode_text)
  
})


output$stdcode_text <- renderUI({
  HTML(stdcode_text())
})



fig7 <- reactive({
  
  invcode <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(Design = factor(ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2"))) %>%
    select(Design, INVENTORY_STANDARD_CD) %>%
    table()  %>%
    as.data.table
  
  
  fig7 <- invcode %>%
    mutate(INVENTORY_STANDARD_CD = factor(INVENTORY_STANDARD_CD,
                                          levels = c("V", "I", "F", "L"))) %>%
    
    ggplot(aes(x = Design , y = N, fill = INVENTORY_STANDARD_CD)) + 
    geom_bar(position = "fill",stat = "identity", width = 0.7, show.legend = TRUE) +
    coord_flip() +
    scale_x_discrete(limit = c("PHASE2", "GRID")) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(name = NULL, values = c("V" = hcl.colors(4, palette = "Dynamic")[2],
                                              "I" = hcl.colors(4, palette = "Dynamic")[3],
                                              "F" = hcl.colors(4, palette = "Dynamic")[1],
                                              "L" = hcl.colors(4, palette = "Dynamic")[4]),
                      labels = as_labeller(c("V" = "V: Full VRI", "I" = "I: Incomplete VRI",
                                             "F" = "F: FIP Rollover", "L" = "L: LVI")),
                      drop = FALSE) +
    labs(x = "", y = "",
         caption = "Figure 7. Inventory standard code as a % of total ground samples by design.") +
    theme(legend.title = NULL,
          rect = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.caption = element_text(hjust = 0, size=15, face = "bold"),
          plot.caption.position = "plot")
  
  return(fig7)
  
})


output$fig7 <- renderPlot({
  
  fig7()
  
})




invyear_flex <- reactive({
  
  invyear <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    mutate(Design = factor(ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2"))) %>%
    mutate(INTERPRETATION_YEAR = as.numeric(substr(INTERPRETATION_DATE, 1, 4))) %>%
    group_by(Design, INVENTORY_STANDARD_CD) %>%
    summarise(ave_INTERPRETATION_YEAR = round(mean(INTERPRETATION_YEAR, na.rm = T), 0))
  
  
  invyear_flex <- flextable(invyear) %>%
    merge_v(part = "body", j = 1) %>%
    colformat_num(j = 3, big.mark = "")
  
  invyear_flex <- labelizor(
    x = invyear_flex, 
    part = "header", 
    labels = c("INVENTORY_STANDARD_CD" = "Code",
               "ave_INTERPRETATION_YEAR" = "Average Interpretation Year"
    )) %>%
    bold(part = 'header', bold = TRUE) %>%
    align(align = "center",part = 'all') %>%
    set_caption(caption = as_paragraph(
      as_chunk("Table 8. Average interpretation year by inventory standard.", 
               props = fp_text_default(bold = TRUE))),
      align_with_table = FALSE,
      word_stylename = "Table Caption") %>%
    #set_caption(as_paragraph(
    #  as_b(as_chunk("Table 8. Average interpretation year by inventory standard.")))) %>%
    autofit()
  
  return(invyear_flex)
  
})


output$invyear_flex <- renderUI({
  htmltools_value(invyear_flex())
})



invyear_text <- reactive({
  
  invyear_text <- paste0("
<ul>
<li><strong>V</strong> &ndash; Full VRI.&nbsp; A full VRI involves estimating vegetation polygon characteristics from aerial photographs.&nbsp; This is commonly referred to as a PHASE 1 VRI.</li>
<li><strong>F</strong> &ndash; Forest Inventory Planning (FIP).&nbsp; These records were collected prior to the development of the VRI standard.&nbsp; Records were sourced from a combination of aerial photographs and silviculture surveys.&nbsp; &nbsp;&nbsp;&nbsp;</li>
<li><strong>I </strong>&ndash; Incomplete.&nbsp; When a full set of VRI attributes are not collected.&nbsp; The most common source of incomplete records are forest cover updates from the RESULTS silviculture reporting system.</li>
<li><strong>L</strong> &ndash; Landscape Vegetation Inventory (LVI). &nbsp;The LVI was developed in 2016 and is used for strategic (or landscape level) planning and reporting and is typically generated at a lower spatial resolution than the V or I standard. The LVI design includes three basic components:</li>
<ul>
<li>Landsat multispectral imagery for polygon delineation and basic land cover classification,</li>
<li>low-level digital image sampling and photo interpretation to provide forest attributes, and</li>
<li>nearest neighbor classification for extrapolation to provide landscape level spatial and attribute products.</li>
</ul>
</ul>")
  
  return(invyear_text)
  
})


output$invyear_text <- renderUI({
  HTML(invyear_text())
})

