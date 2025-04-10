
spcomp_text <- reactive({
  req(input$SelectVar)
  
  text <- paste0("<p>Species comparisons between ground measurements and 
                 inventory labels include : 1) Leading species percent agreement 
                 (for up to 7 leading species, tables 5a & 5b), and 2) Overall 
                 species composition by percent basal area (for up to 14 
                 individual species, figures 4 & 5). In each case, 'OTH' 
                 represents the sum of all remaining species present.</p>")
  
  return(text)
})

output$spcomp_text <- renderUI({
  HTML(spcomp_text())
})



ld_table1 <- reactive({
  
  correct_ls <- correct_ls()
  
  LD_data <- lead_vol %>%
    filter(CLSTR_ID %in% clstr_id()) 
  
  LD_table <- proc_freq(LD_data[LD_data$Design == "GRID",], "SPC_GRP2", "SPC_GRP1",
                        include.row_total = T,
                        include.row_percent = F,
                        include.column_total = T,
                        include.column_percent = F,
                        include.table_percent = F) 
  
  LD_table <- set_caption(LD_table, caption = as_paragraph(
    as_chunk("Table 5a. Leading species cross-table comparison between Inventory and Grid samples (#).", 
             props = fp_text_default(bold = TRUE))),
    align_with_table = FALSE,
    word_stylename = "Table Caption") %>%
    add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate = ", 
                                                 correct_ls[Design == "GRID", ]$correct_ls*100, "%")))
  
  LD_table <- bg(x = LD_table, 
                 j = (1:(LD_table$body$content$ncol/2))*2, 
                 bg = "lightgray", part = "body")
  
  LD_table <- labelizor(x = LD_table, 
                        part = "header", 
                        labels = c("SPC_GRP2" = "Inv", 
                                   "SPC_GRP1" = "Grd"))  %>%
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
    filter(CLSTR_ID %in% clstr_id()) 
  
  LD_table2 <- proc_freq(LD_data[LD_data$Design == "PHASE2",], "SPC_GRP2", "SPC_GRP1",
                         include.row_total = T,
                         include.row_percent = F,
                         include.column_total = T,
                         include.column_percent = F,
                         include.table_percent = F) 
  
  LD_table2 <- set_caption(LD_table2, caption = as_paragraph(
    as_chunk("Table 5b. Leading species cross-table comparison between Inventory and Phase2 ground samples (#).", 
             props = fp_text_default(bold = TRUE))),
    align_with_table = FALSE,
    word_stylename = "Table Caption") %>%
    add_footer_lines(value = as_paragraph(paste0("Correct Leading Species Classification Rate = ", 
                                                 correct_ls[Design == "PHASE2", ]$correct_ls*100, "%")))
  
  LD_table2 <- bg(x = LD_table2, 
                  j = (1:(LD_table2$body$content$ncol/2))*2, 
                  bg = "lightgray", part = "body")
  
  LD_table2 <- labelizor(x = LD_table2, 
                         part = "header", 
                         labels = c("SPC_GRP2" = "Inv", 
                                    "SPC_GRP1" = "Grd"))  %>%
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
  
  fig4 <- spcomp %>%
    ggplot() +
    geom_bar(aes(x = SPECIES, y = livevolperc/100, group = source, fill = source), 
             position = position_dodge2(preserve = "single"), width = 0.7, stat = "identity") +
    facet_wrap(~Design, ncol = 2) +
    scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL) +
    scale_x_discrete(drop=T) +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Species", y = "% of live net merch vol",
         title = "Live Species Composition - GRID / Phase 2") +
    theme(
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      #rect = element_blank()
    ) 
  
  return(fig4)
  
})



output$fig4 <- renderPlot({
  
  fig4()
  
})



fig5 <- reactive({
  
  spcomp <- spcomp()
  
  fig5 <- spcomp %>%
    ggplot() +
    geom_bar(aes(x = SPECIES, y = deadvolperc/100, group = source, fill = source), 
             position = position_dodge2(preserve = "single"), width = 0.7, stat = "identity") +
    facet_wrap(~Design, ncol = 2) +
    scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL) +
    scale_x_discrete(drop=T) +
    scale_y_continuous(labels = scales::label_percent(), expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Species", y = "% of live net merch vol",
         title = "Dead Species Composition - GRID / Phase 2") +
    theme(
      panel.grid.major.y = element_line(color = 'darkgray'), 
      plot.caption = element_text(hjust=0, size=rel(1.2)),
      legend.position="top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      #rect = element_blank()
    ) 
  
  return(fig5)
  
})


output$fig5 <- renderPlot({
  
  fig5()
  
})


fig6 <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  top3spc <- top3spc()
  
  scatter <- lead_vol_dat %>%
    left_join(lead_vol %>% select(CLSTR_ID, SPECIES_INV), by = "CLSTR_ID") %>%
    select(Design, CLSTR_ID, SPECIES_INV, 
           inv_age = PROJ_AGE_ADJ, inv_ht = vdyp_dom_ht, inv_vol = vdyp_vol_dwb, 
           grd_age = AGET_TLSO, grd_ht = HT_TLSO, grd_vol = NTWB_NVAF_LS,
           rom_age, rom_ht, rom_vol) %>%
    mutate(SPC_GRP_INV = ifelse(SPECIES_INV %in% top3spc, SPECIES_INV, "OTH")) %>%
    pivot_longer(cols = inv_age:rom_vol,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  
  fig6 <- ggplot(scatter) +
    geom_point(aes(x = inv, y = grd, col = SPC_GRP_INV), size = 2) +
    geom_abline(aes(intercept = 0, slope = 1, linetype = "1:1"), color = "darkgray") + 
    geom_abline(aes(intercept = 0, slope = rom, linetype = "ROM"), linewidth = 1,  color = "red") +
    geom_text(aes(Inf, -Inf, label = paste0("ROM = ",round(rom, 2))), hjust = 1.2, 
              vjust = -1) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    scale_linetype_manual(name = NULL, 
                          values = c(2, 1),
                          guide = guide_legend(override.aes = list(color = c("darkgray", "red")))) +
    scale_color_manual(name = "SPECIES", values = (hcl.colors(4, palette = "Dark 3"))) +
    labs(x = "Inventory", y = "Ground") +
    facet_wrap(var ~  Design , scales = "free", ncol = 2,
               labeller = as_labeller(c(
                 'age'="Total Age (yrs)",
                 'ht' = 'Height (m)',
                 'vol'="Volume (m3/ha)",
                 "GRID" = "GRID",
                 "PHASE2" = "PHASE2"
               ))) +
    theme(#legend.position="bottom",
      strip.background.x = element_rect(fill=NA, colour=NA,size=1))
  
  return(fig6)
})



output$fig6 <- renderPlot({
  
  fig6()
  
})


stdcode_text <- reactive({
  
  stdcode_text <- paste0("<p>Inventory standards of data collection vary across 
                         TSAs and management units. The VRI attribute 
                         'inventory_standard_cd' is summarized across all 
                         spatially intersected ground sample locations, as a 
                         percent of the total number of ground samples. This 
                         helps assess the reliability of Inventory attributes 
                         when compared against ground measurements.</p>")
  
  return(stdcode_text)
  
})


output$stdcode_text <- renderUI({
  HTML(stdcode_text())
})



fig7 <- reactive({
  
  invcode <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
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
                      labels = as_labeller(c("V" = "Full VRI", "I" = "Incomplete VRI",
                                             "F" = "FIP Rollover", "L" = "LVI")),
                      drop = FALSE) +
    labs(x = "", y = "") +
    theme(legend.title = NULL,
          rect = element_blank(),
          panel.grid.major.y = element_blank(),)
  
  return(fig7)
  
})


output$fig7 <- renderPlot({
  
  fig7()
  
})
