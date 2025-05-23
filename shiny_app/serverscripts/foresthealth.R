
coctext <- reactive({
  
  remeas_plot <- remeas_plot()
  total_remeas_plot <- length(remeas_plot)
  
  period <- sample_data %>%
    filter(CLSTR_ID %in% remeas_plot) %>%
    #select(CLSTR_ID, SAMPLE_ESTABLISHMENT_TYPE, PERIOD) %>%
    pull(PERIOD) %>%
    median()
  
  coctext <- HTML(paste0("<p>Growth and mortality of the n=", "<b>", total_remeas_plot, 
                         "</b>", " re-measured mature ground samples (GRID) are summarized into 
                         components of change for all tagged trees, separately for each measurement period. </p>", 
                         "<p>The components of change across only the last two measurements are shown 
  (figure below), representing a median period of ", round(period, 0),
                         " years. The components of change include:</p>",
                         "</br><ul><li><b><i>Survivor</i></b> Trees that are alive at both measurements </li>",
                         "<li><b><i>Mortality</i></b> Trees that died between measurements </li>",
                         "<li><b><i>Ingrowth</i></b> New trees that grow into the minimum tagging limit </li>",
                         "<li><b><i>Dead</i></b> Trees that are dead standing at both measurements </li></br>"))
  return(coctext)
})



output$coctext <- renderUI({
  
  coctext()
  
})




cocfig <- reactive({
  
  remeas_plot <- remeas_plot()
  total_remeas_plot <- length(remeas_plot)
  
  fig8_dat <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id_last2(), DAM_NUM==1) 
  
  fig8 <- fig8_dat %>%
    filter(CLSTR_ID %in% remeas_plot) %>%
    mutate(baha = BA_TREE*phf_coc) %>%
    group_by(comp_chg_coc) %>%
    summarize(BA = sum(baha, na.rm = T)/total_remeas_plot,
              stem = sum(phf_coc, na.rm = T)/total_remeas_plot) %>%
    data.table
  
  ratio = round(max(fig8$BA)/max(fig8$stem), 2)
  
  fig8 <- melt(fig8, measure.vars = c( "stem","BA"),
               variable.name = "variable", value.name = "value")
  
  fig8 <- fig8 %>%
    mutate(value_adj = ifelse(variable == "BA", value/ratio, value))
  
  p <- if (nrow(fig8) > 1){ ggplot(fig8, aes(x = comp_chg_coc)) +
      geom_bar(aes(y = value_adj, fill = variable, group = variable),
               stat = "identity", position = position_dodge2(), width = 0.7)  +
      labs(x = "", title = "Components of Change") + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, labels = c("Stems/ha", "BA/ha")) +
      #scale_fill_discrete(name = "", labels = c("Stems/ha", "BA/ha")) +
      scale_x_discrete("", labels = c("D" = "Dead", "I" = "Ingrowth", "M" = "Mortality", "S" = "Survivor")) +
      scale_y_continuous(name = "Stems (#/ha)", expand = c(0, 0), 
                         limits = c(0,max(fig8$value_adj)*1.1), 
                         sec.axis = sec_axis( trans=~.*ratio, 
                                              name=expression("Basal Area ("~m^{2}~"/ha)"))) +
      geom_text(mapping = aes(label = round(value, 1), 
                              x = comp_chg_coc, y = value_adj+max(value_adj)*0.05, 
                              group = variable,
                              colour = variable), 
                position = position_dodge(width = .9), show.legend  = FALSE) + 
      scale_color_manual(values = c("steelblue", "#B4464B")) +
      theme(
        axis.title.y = element_text(color="steelblue"),
        axis.title.y.right = element_text(colour = "#B4464B"),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.position = "inside",
        legend.position.inside = c(0.15, 0.87),
        legend.title = element_blank(),
        axis.line = element_line(colour="darkgray"), 
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank()
      ) 
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  p
  
  
})


output$cocfig <- renderPlot({
  
  cocfig()
  
})


curfhtext <- reactive({
  
  clstr_id_grid <- clstr_id_grid()
  
  curfhtext <- HTML(paste0("All tagged trees in the <b>n=", length(clstr_id_grid), 
  "</b> grid samples are assessed for up to five forest health damage agents
per tree. The mean incidence and 95% confidence intervals by damage
agent (expressed as a percent of total live stems/ha of all unique damage
agents recorded per tree) are computed at the latest measurement. Note
that up to the 2020 field season, all fork and crook occurrences were
recorded regardless of size and severity. Starting in the 2021 field
season, those forks and crooks with very small (<10%) offsets in stem
diameter (expected to have negligible/no impact on stem form) are no
longer recorded; therefore, earlier recorded incidences of forks and
crooks prior to 2021, were likely over-estimated. Since 2021, fork and
crook severity is further classified into minor (<50%) or major
(>=50%) diameter offsets. 
The severity of other scalable damages—such as fire, beetle, root rot, stem rust, 
and weevil—is summarized by damage class, if available (figure below). 
A full list of recorded damage agents and severity classes is under General Notes."))
  return(curfhtext)
})


output$health_inci <- renderUI({
  
  curfhtext()
  
})



curfhplot <- reactive({
  ## Current health (5 damage agent)
  
  FH_dat <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id_grid()) %>%
    mutate(n = length(clstr_id_grid()))
  
  FH_dat1 <- FH_dat  %>%
    filter(DAM_NUM == 1)  %>%
    group_by(CLSTR_ID, n, lvd_coc) %>%
    summarize(tot_ba_plot = sum(ba_ha, na.rm = T),
              tot_stems_plot = sum(phf_coc, na.rm = T),
              tot_vol_plot = sum(vol_ha, na.rm = T)) %>%
    group_by(n, lvd_coc) %>%
    mutate(avg_ba_allplot = sum(tot_ba_plot)/n,
           avg_stems_allplot = sum(tot_stems_plot)/n,
           avg_vol_allplot = sum(tot_vol_plot)/n)
  
  #*totals based on un expanded tree list (ie., sum of percent incidence can be 
  #*over 100 since multiple damage agents per tree);
  ### Total stems/ha of a damage agent in a plot
  FH_dat2 <- FH_dat %>%
    group_by(CLSTR_ID, n, lvd_coc, AGN) %>%
    summarize(tot_ba_dam_plot = sum(ba_ha, na.rm = T),
              tot_stems_dam_plot = sum(phf_coc, na.rm = T),
              tot_vol_dam_plot = sum(vol_ha, na.rm = T),
              n_agn = n())   ### n_agn: # of each damage within a plot
  
  ### Proportion of damaged stems within a plot
  FH_dat3 <- FH_dat1 %>%
    full_join(FH_dat2, by = c("CLSTR_ID", "n", "lvd_coc" )) %>%
    mutate(incid_dam_ba_plot = tot_ba_dam_plot/tot_ba_plot,
           incid_dam_stems_plot = tot_stems_dam_plot/tot_stems_plot,
           incid_dam_vol_plot = tot_vol_dam_plot/tot_vol_plot)
  
  ### Total stems/ha of a damage agent across plots
  FH_dat4 <- FH_dat3 %>%
    group_by(n, lvd_coc, AGN) %>%
    summarize(tot_ba_dam_allplot = sum(tot_ba_dam_plot),
              tot_stems_dam_allplot = sum(tot_stems_dam_plot),
              tot_vol_dam_allplot = sum(tot_vol_dam_plot),
              n_agn_plot = n()) %>%    #n_agn_all: # plots that has the damage agent
    mutate(avg_ba_dam_allplot = tot_ba_dam_allplot/n,
           avg_stems_dam_allplot = tot_stems_dam_allplot/n,
           avg_vol_dam_allplot = tot_vol_dam_allplot/n)
  
  FH_dat5 <- FH_dat4 %>%
    left_join(FH_dat3 %>% ungroup() %>% distinct(n, lvd_coc, avg_ba_allplot, avg_stems_allplot, avg_vol_allplot), 
              by = c("n", "lvd_coc")) %>%
    mutate(incid_stems_allplot = avg_stems_dam_allplot/avg_stems_allplot,
           incid_vol_allplot = avg_vol_dam_allplot/avg_vol_allplot)
  
  FH_dat6 <- FH_dat3 %>%
    left_join(FH_dat5, by = c("n", "lvd_coc", "AGN", "avg_ba_allplot", "avg_stems_allplot"))
  
  FH_dat6 <- FH_dat6 %>%
    mutate(incid_diffs_sqrd = (tot_stems_dam_plot - avg_stems_dam_allplot)^2)
  
  FH_dat7 <- FH_dat6 %>%
    group_by(n, lvd_coc, AGN) %>%
    summarize(sum_incid_diffs_sqrd = sum(incid_diffs_sqrd))
  
  FH_dat8 <- FH_dat5 %>%
    left_join(FH_dat7, by = c("n", "lvd_coc", "AGN")) %>%
    mutate(var_stems_incid = sum_incid_diffs_sqrd / (n*(n-1)*avg_stems_allplot^2),
           l95_stems = ifelse(n >1, incid_stems_allplot - qt(0.975, n-1) * sqrt(var_stems_incid), NA),
           u95_stems = ifelse(n >1, incid_stems_allplot + qt(0.975, n-1) * sqrt(var_stems_incid), NA))
  
  FH_dat8 <- FH_dat8 %>%
    mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
           dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
                                 dam_1letter == 'U' ~ 'Unknown',
                                 dam_1letter == 'N' ~ 'Abiotic',
                                 dam_1letter == 'D' ~ 'Disease',
                                 dam_1letter == 'I' ~ 'Insect',
                                 dam_1letter == 'T' ~ 'Trt',
                                 dam_1letter == 'A' ~ 'Animal',
                                 dam_1letter == 'X' ~ 'Frk_Crk_Btp',
                                 dam_1letter == 'V' ~ 'Vegetation',
                                 TRUE ~ '')) %>%
    mutate(dam_class = fct_reorder(dam_class, -incid_stems_allplot))
  
  FH_dat_final <- FH_dat8 %>% ungroup()
  
  limits <- FH_dat_final %>%
    filter(AGN != "O", lvd_coc == "L") %>%
    select(u95_stems) %>%
    max() 
  
  if (nrow(FH_dat_final) > 1){
    p <- ggplot(FH_dat_final %>% 
                  filter(lvd_coc == "L", AGN != "O")) + 
      geom_bar(stat = "identity", aes(x = AGN, y = incid_stems_allplot, fill = dam_class)) + 
      geom_errorbar(aes(x = AGN, ymin = l95_stems, ymax = u95_stems), width = .2,
                    position = position_dodge(.9)) +
      geom_linerange(aes(x = AGN, ymin = incid_stems_allplot, ymax = u95_stems)) +
      scale_x_discrete(drop = FALSE) +
      #scale_fill_brewer(name = NULL, palette = "Set2") +
      scale_fill_manual(name = NULL, values = dam_color) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                         limits = c(0, ceiling(limits / 0.05) * 0.05)) + 
      facet_grid(. ~ reorder(dam_class, -incid_stems_allplot, min), scales="free_x", space="free_x") +
      labs(x = "", y = "Incidence (%)",
           title = "Current Incidence",  
           subtitle = "(% of total live stems/ha of up to 5 unique damage agents recorded per tree)") +
      theme(
        axis.line = element_line(colour="darkgray"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.y = element_line(color = 'darkgray'), 
        #panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0, size=rel(1.2))
      )  
  } else {
    p <- ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  p
  
  
})



output$curr_fh_inci <- renderPlot({
  
  curfhplot()
})




sevplot <- reactive({
  
  FH_dat <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id_grid()) %>%
    mutate(n = length(clstr_id_grid()))
  
  sevplot_dat <- FH_dat %>%
    filter(lvd_coc == "L") %>%
    group_by(dam_class, sev_class) %>%
    summarise(n = sum(phf_coc)) %>%
    ungroup() %>%
    mutate(nprop = prop.table(n)) %>%
    filter(dam_class != "None", dam_class != "Unknown") %>%
    mutate(sev_class_f = factor(sev_class, level = c("HIGH", "MOD", "LOW", "UNKNOWN")),
           damsevclass = paste0(dam_class, "-", sev_class),
           #damsevclass_o = !is.na(match(damclass_color$Var4, damsevclass)),
           damsevclass_f = factor(damsevclass, levels = damclass_color$Var4)
    ) 
  
  damclass_col <- damclass_color[match(sevplot_dat$damsevclass, damclass_color$Var4), 3]
  
  p <- ggplot(sevplot_dat) +
    geom_bar(aes(x = reorder(dam_class, -nprop, min), y = nprop, fill = sev_class_f), width = 0.7,
             stat='identity', show.legend = TRUE) +
    scale_fill_manual(
      name = "SEVERITY",
      values = c(rev(RColorBrewer::brewer.pal(n = 3, "Blues")), "darkgray"),
      #labels = temp[order(temp$sev_class_f), ]$sev_class_f,
      drop = F
    ) +
    scale_y_continuous(labels = scales::percent, expand = c(0, 0)) + 
    labs(x = "", y = "Incidence (%)",
         title = "Damage Severity") +
    theme(
      axis.line = element_line(colour="darkgray"), 
      #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      rect = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.85, 0.85),
      legend.box.background = element_rect(fill = "white", color = NA),
      #legend.title = element_blank(),
      plot.caption = element_text(hjust=0, size=rel(1.2))
    )  
  
  p
  
  
})



output$sevplot <- renderPlot({
  
  sevplot()
})




fhcoctext1 <- reactive({
  
  remeas_plot <- remeas_plot()
  total_remeas_plot <- length(remeas_plot)
  
  period <- sample_data %>%
    filter(CLSTR_ID %in% remeas_plot) %>%
    pull(PERIOD) %>%
    median()
  
  fhcoctext1 <- HTML(paste0("Change in forest health incidence of the (<b>n = ","",
                            total_remeas_plot,"</b>",") re-measured mature samples are compared across 
  the last measurement with median period of ", "<b>", 
                            round(period, 0), "</b>", " years. Incidence across both 
  measurements is relative to the primary damage agent (first recorded and 
  most significant) on the same trees that were alive at the beginning of the 
  period (graph below)."))
  
  return(fhcoctext1)
})


output$fhcoctext1 <- renderUI({
  
  fhcoctext1()
  
})




cocfhplot <- reactive({
  
  fig10_dat <- fig10_dat()
  
  p <- if (nrow(fig10_dat) > 0) {
    ggplot(fig10_dat %>% filter(!(AGN %in% c("O", "")))) + 
      geom_bar(stat = "identity", aes(x = AGN, y = incid_stems, fill = factor(new_visit_number)), 
               position = position_dodge(), width=0.7) + 
      scale_fill_manual(values = c("steelblue", "#B4464B"), name = NULL, 
                        labels = c("Previous visit", "Latest visit")) +
      scale_x_discrete(drop = FALSE) +
      scale_y_continuous(labels = scales::percent, expand = c(0, 0),
                         limits = c(0, ceiling(max(fig10_dat[fig10_dat$AGN!="O",]$incid_stems)*20) / 20),
                         minor_breaks = seq(0, 
                                            ceiling(max(fig10_dat[fig10_dat$AGN!="O",]$incid_stems)*20) / 20, 
                                            by = 0.01)) + 
      facet_grid(. ~ reorder(dam_class, -incid_stems, min), scales="free_x", space="free_x") +
      labs(x = "", y = "Incidence (%)",
           title = "Change in primary damage agent incidence",
           subtitle = "(% of live trees)") +
      theme(
        axis.line = element_line(colour="darkgray"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major.y = element_line(color = 'darkgray'), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        rect = element_blank(),
        legend.box.background = element_rect(fill = "white", color = "lightgray"),
        legend.position = c(0.9, 0.8),
        legend.title = element_blank(),
        plot.caption = element_text(hjust=0, size=rel(1.2))
      )    
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  p
  
  
  
})



output$cocfhplot <- renderPlot({
  
  cocfhplot()
})


fhcoctext2 <- reactive({

  fhcoctext2 <- HTML("All trees that were alive at the beginning of the period, have either
survived or died between measurements. The table below summarizes
the components of change for those trees affected by 
primary damage agents with highest percent incidence. In addition, the 
probability of trees getting infected by a given damage agent and subsequently 
dying from it, is also calculated.")
  return(fhcoctext2)
})



output$fhcoctext2 <- renderUI({
  
  fhcoctext2()
  
})

fhcocflex <- reactive({
  
  fig10_dat <- fig10_dat()
  
  
  if (nrow(fig10_dat) > 0){
    tot_tree_alive <- round(unique(fig10_dat[fig10_dat$new_visit_number=='Last',]$totsph_comdem),0)
    
    table6_dat <- fig10_dat %>%
      arrange(desc(incid_stems)) %>%
      filter(new_visit_number == 'Last') %>%
      mutate(rank = row_number()) %>%
      mutate(rank_new = ifelse(rank>15, 16, rank),
             AGN_rank = ifelse(rank>15, "Rest", AGN)) %>%
      group_by(AGN_rank, rank_new) %>%
      summarise(incid_stems = sum(incid_stems, na.rm = T)*100,
                S.dam = sum(S.dam, na.rm = T),
                M.dam = sum(M.dam, na.rm = T),
                damsph_comdem = sum(damsph_comdem, na.rm = T),
                perc_mort = mean(perc_mort, na.rm = T)*100,
                prob_get_and_die = sum(prob_get_and_die, na.rm = T)*100) %>%
      ungroup() %>%
      arrange(rank_new) %>%
      mutate(total = "", 
             PDA = AGN_rank,
             Inci = round(incid_stems, 1),
             S = round(S.dam, 0),
             M = round(M.dam, 0), 
             Tot = round(damsph_comdem, 0),
             PM = round(perc_mort, 1),
             Prob = round(prob_get_and_die, 1)) %>%
      select(total, PDA, Inci, S, M, Tot, PM, Prob) %>%
      mutate(across(everything(), .fns = function(x) ifelse(x == 0, "", x)))
    
    totinc <- round(sum(as.numeric(table6_dat$Inci), na.rm = T),0)
    tots <- round(sum(as.numeric(table6_dat$S), na.rm = T),0)
    totm <- round(sum(as.numeric(table6_dat$M), na.rm = T),0)
    totmort <- round(totm/tot_tree_alive*100,1)
    
    table6_total <- cbind("", "Total", paste0(totinc, "%"), tots, totm,tot_tree_alive, 
                          paste0(totmort, "%"), paste0(totmort, "%"))
    
    flextable3 <- flextable(table6_dat) 
    
    flextable3 <- add_header_row(flextable3, top = TRUE, colwidths = c(3,4,1),
                                 values = c("", "Number of Affected Trees by Primary Damage Agent", "")) #%>%
    #align(align = "center", part = "all") %>%
    #merge_v(j = c(1:3,8), part = "header") 
    
    flextable3 <- add_footer_row(flextable3, top = FALSE, values =table6_total, 
                                 colwidths = c(1,1,1,1,1,1,1,1)) %>%
      align(align = "center", part = "all") %>%
      merge_v(j = c(1:3,8), part = "header") #%>%
    #hline_bottom(part = 'footer')
    
    flextable3 <- labelizor(
      x = flextable3, 
      part = "header", 
      labels = c("total" = 'Live trees at\n period start\n [a]\n (#/ha)', 
                 "PDA" = "Primary\n Damage\n Agent",
                 "Inci" = "Incidence\n [b]=e/a \n (%)",
                 "S" = "Survivor trees\n [c]\n (#/ha)",
                 "M" = "Mortality trees\n [d]\n (#/ha)",
                 "Tot" = "Total affected\n [e]=c+d (#/ha)",
                 "PM" = "Mortality\n [f]=d/e\n (%)",
                 "Prob" = "Prob. getting\n Infected &\n Dying [g]=b*f\n (%)")) %>%
      autofit()
    
    flextable3 <- merge_v(flextable3, j = 1:2, part = "header") 
    
    flextable3 <- flextable::compose(flextable3,
                                     i = 1, j = 1, 
                                     value = as_paragraph(round(tot_tree_alive, 0)))
    
  } else {
    flextable3 <- flextable(data.frame(matrix(rep("-", 8), ncol=8,nrow=1)))
    flextable3 <- add_header_row(flextable3, top = TRUE, colwidths = c(3,4,1),
                                 values = c("", "Number of Affected Trees by Primary Damage Agent", "")) %>%
      align(align = "center", part = "all")
    
    flextable3 <- labelizor(
      x = flextable3, 
      part = "header", 
      labels = c("X1" = 'Live trees at\n period start\n [a]\n (#/ha)', 
                 "X2" = "Primary\n Damage\n Agent",
                 "X3" = "Incidence\n [b]=e/a \n (%)",
                 "X4" = "Survivor trees\n [c]\n (#/ha)",
                 "X5" = "Mortality trees\n [d]\n (#/ha)",
                 "X6" = "Total affected\n [e]=c+d (#/ha)",
                 "X7" = "Mortality\n [f]=d/e\n (%)",
                 "X8" = "Prob. getting\n Infected &\n Dying [g]=b*f\n (%)")) %>%
      autofit()
    
    flextable3 <- merge_v(flextable3, j = 1:2, part = "header") 
    
  }
  
  return(flextable3)
  
})


output$fhcocflex <- renderUI({
  
  htmltools_value(fhcocflex())
  
})

