

growth_text <- reactive({
  growth_text <-  HTML("<p>The average VDYP7-projected volumes are plotted 
                       against the measured volumes from GRID samples to infer 
                       volume growth within the TSA. Data points are connected 
                       where re-measurements are available. 
                       Samples identified as harvested or affected by disturbance 
                       in the 2023 Forest Vegetation Composite Rank 1 layer 
                       during the re-measurement period are shown separately.</p>")
  return(growth_text)
})


output$growth_text <- renderUI({
  
  growth_text()
  
})


fig7_5 <- reactive({
  
  if (length(clstr_id_grid_all()) > 0){
  
  prj_dat1 <- prj_dat %>%
    filter(CLSTR_ID %in% clstr_id_grid_all()) %>%
    mutate(Design = ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2")) %>%
    #left_join(sample_data %>% select(CLSTR_ID, Design), by = "CLSTR_ID") %>%
    group_by(Design, PRJ_TOTAL_AGE) %>%
    summarise(n = n(),
              sumvol = sum(vdyp_vol_dwb, na.rm = T),
              meanvol = sumvol/n,
              sdvol = sd(vdyp_vol_dwb, na.rm = T),
              sevol = sdvol/sqrt(n)*qt(0.975, n-1)) %>%
    mutate(Design = factor(Design, levels = c("GRID", "PHASE2")))
    
    xlim <-  lead_vol %>%
      filter(CLSTR_ID %in% clstr_id_grid_all()) %>% 
      pull(PRJ_TOTAL_AGE) %>% 
      max(na.rm = T)
    
    dist_dat <- sample_data %>% 
      filter(CLSTR_ID %in% clstr_id_grid_all()) %>% 
      mutate(dist_yr = as.numeric(substr(EARLIEST_NONLOGGING_DIST_DATE, 1, 4)),
             harvest_yr = as.numeric(substr(HARVEST_DATE, 1, 4))) %>% 
      group_by(SITE_IDENTIFIER) %>% 
      mutate(dist = case_when(!is.na(harvest_yr) & harvest_yr <= max(MEAS_YR) & harvest_yr > min(MEAS_YR) ~ 'Harvested',
                              !is.na(dist_yr) & dist_yr <= max(MEAS_YR) & dist_yr > min(MEAS_YR) & 
                                EARLIEST_NONLOGGING_DIST_TYPE %in% c("B", "NB") ~ 'Wildfire',
                              !is.na(dist_yr) & dist_yr <= max(MEAS_YR) & dist_yr > min(MEAS_YR) & 
                                EARLIEST_NONLOGGING_DIST_TYPE %in% c("IBM") ~ 'MPB',
                              !is.na(dist_yr) & dist_yr <= max(MEAS_YR) & dist_yr > min(MEAS_YR) & 
                                EARLIEST_NONLOGGING_DIST_TYPE %in% c("IDL") ~ 'Western Hemlock Looper',
                              !is.na(dist_yr) & dist_yr <= max(MEAS_YR) & dist_yr > min(MEAS_YR) & 
                                EARLIEST_NONLOGGING_DIST_TYPE %in% c("NW") ~ 'Windthrow',
                              TRUE ~ NA)) %>%
      do(fill(.,dist)) %>%
      select(SITE_IDENTIFIER, dist) %>% distinct()
    
    vol_dat <- lead_vol %>%
      filter(CLSTR_ID %in% clstr_id_grid_all()) %>%
      mutate(Design = ifelse(Design %in% c("GRID", "SUP-GRID"), "GRID", "PHASE2")) %>%
      left_join(dist_dat, by = 'SITE_IDENTIFIER') 
    
  
    p <- ggplot() +
      geom_ribbon(data = prj_dat1 %>% filter(n >5), 
                  aes(x = PRJ_TOTAL_AGE, ymin = meanvol - sevol, ymax = meanvol + sevol, 
                      group = Design), 
                  fill = "lightgray") +
      geom_line(data = prj_dat1 %>% filter(n >5), 
                aes(x = PRJ_TOTAL_AGE, y = meanvol, col = Design), linewidth = 2) +
      scale_color_manual(values = c("darkorange"), labels = c("VDYP projection"), name = NULL) +
      #scale_color_manual(values = c("coral1", "palegreen3"), drop = FALSE, name = "VDYP projection") +
      ggnewscale::new_scale_color() +
      geom_point(data = vol_dat %>%
                   filter(Design == "GRID", !is.na(dist)), 
                 aes(x = PRJ_TOTAL_AGE, y = NTWB_NVAF_LS, col = dist), size = 3) +
      geom_line(data = vol_dat %>%
                  filter(Design == "GRID", !is.na(dist)),
                aes(x=PRJ_TOTAL_AGE, y=NTWB_NVAF_LS, col = dist, group= SITE_IDENTIFIER), linewidth = 1.2) +
      scale_colour_brewer(palette = "Accent", name = NULL,
                          guide = guide_legend(position = "inside", ncol = 1)) +
      ggnewscale::new_scale_color() +
      geom_point(data = vol_dat %>%
                   filter(Design == "GRID",is.na(dist)), 
                 aes(x = PRJ_TOTAL_AGE, y = NTWB_NVAF_LS, col = Design), size = 3) +
      geom_line(data = vol_dat %>%
                  filter(Design == "GRID",is.na(dist)),
                aes(x=PRJ_TOTAL_AGE, y=NTWB_NVAF_LS, col = Design, group= SITE_IDENTIFIER), linewidth = 1.2) +
      
      #geom_point(data = vol_dat %>%
      #             filter(Design == "PHASE2"), 
      #           aes(x = PRJ_TOTAL_AGE, y = NTWB_NVAF_LS, col = Design), size = 3) +
      scale_x_continuous(limit = c(0, xlim)) +
      scale_color_manual(values = c("brown3"), name = NULL) +
      #scale_color_manual(values = c("brown3", "chartreuse4"), drop = FALSE) +
      labs(x = "Age", y = "Volume") +
      #facet_wrap(~Design, scale = "free_x", drop = FALSE) +
      theme(
        plot.caption = element_text(hjust = 0, size=15, face = "bold"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.title = NULL,
        legend.position.inside = c(0.1, 0.85)
      ) 
    
  } else {
    p <- ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  p
  
  
})




output$fig7_5 <- renderPlot({
  
  fig7_5()
  
})



Fig75_dat <- reactive({
  
  pai_dat <- sample_data %>%
    filter(CLSTR_ID %in% clstr_id_all()) %>%
    arrange(SITE_IDENTIFIER, VISIT_NUMBER) %>%
    group_by(SITE_IDENTIFIER) %>%
    filter(VISIT_NUMBER == min(VISIT_NUMBER) | VISIT_NUMBER == max(VISIT_NUMBER)) %>%
    left_join(lead_vol %>% select(CLSTR_ID, NTWB_NVAF_LS, vdyp_vol_dwb), by = "CLSTR_ID") #%>%
    #mutate(DIST_yr = as.Date(EARLIEST_NONLOGGING_DIST_DATE))
  
  pai_dat <- pai_dat %>%
    mutate(grdnv = ifelse(is.na(NTWB_NVAF_LS), 0, NTWB_NVAF_LS),
           prednv = ifelse(is.na(vdyp_vol_dwb), 0, vdyp_vol_dwb),
           voldiff = grdnv - prednv)
  
  setDT(pai_dat)[, year_dff := MEAS_YR - lag(MEAS_YR), by = list(SITE_IDENTIFIER,Design)]
  setDT(pai_dat)[, grdnv_diff := grdnv - lag(grdnv), by = list(SITE_IDENTIFIER,Design)]
  setDT(pai_dat)[, prednv_diff := prednv - lag(prednv), by = list(SITE_IDENTIFIER,Design)]
  
  Fig75_dat <- pai_dat %>%
    select(SITE_IDENTIFIER, year_dff, grdnv_diff, prednv_diff) %>%
    filter(!is.na(year_dff)) %>%
    mutate(grdnv_pai = grdnv_diff/year_dff,
           prednv_pai = prednv_diff/year_dff)
  
  return(Fig75_dat)
})



pai_table1 <- reactive({
  
  Fig75_dat <- Fig75_dat()
  
  ### PAI summary table
  pai_table1 <- data.frame(attr = c("GRID", "VDYP"),
                           obs = rep(dim(Fig75_dat)[1],2),
                           Yrs = c(round(mean(Fig75_dat$year_dff),0), round(mean(Fig75_dat$year_dff),0)),
                           PAI = c(round(mean(Fig75_dat$grdnv_pai), 2),
                                   round(mean(Fig75_dat$prednv_pai), 2)))
  
  pai_table1 <- flextable(pai_table1) %>% 
    set_header_labels(values = c("attr.", "# obs", "Yrs", "PAI"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  pai_table1 <- pai_table1 %>% 
    add_header_lines(values = c("GRID vs. VDYP projections")) %>%
    bold(part = 'header', bold = TRUE) %>%
    #set_caption(as_paragraph(
    #  as_b(as_chunk("Test 1: Compare YSM actual growth vs. TSR yield table projections")))) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 9a. Comparison of GRID sample actual growth vs. predicted VDYP yield tables.")))) %>%
    autofit()
  
  return(pai_table1)
  
})

output$pai_table1 <- renderUI({
  
  htmltools_value(pai_table1())
})



pai_table2 <- reactive({
  
  Fig75_dat <- Fig75_dat()
  
  
  ### T test table
  if (nrow(Fig75_dat) > 1){
    
    pai_table2 <- broom::tidy(t.test(Fig75_dat$prednv_pai - Fig75_dat$grdnv_pai), conf.int = TRUE)
    
  } else {
    pai_table2 <- data.frame(matrix(rep("-", 4), ncol=4,nrow=1, 
                                    dimnames=list(NULL, c("p.value", "estimate", "conf.low", "conf.high"))))
  }
  
  pai_table2 <- pai_table2 %>%
    select(p.value, estimate, conf.low, conf.high)  %>%
    mutate(p.value = ifelse(is.numeric(p.value), round(p.value, 3),p.value),
           estimate = ifelse(is.numeric(estimate), round(estimate, 3),estimate),
           conf.low = ifelse(is.numeric(conf.low), round(conf.low, 3),conf.low),
           conf.high = ifelse(is.numeric(conf.high), round(conf.high, 3),conf.high))
  
  pai_table2 <- flextable(pai_table2) %>% 
    color(i = ~ p.value < 0.05, j = 1, color = 'red', part = "body") %>%
    set_header_labels(values = c("p-val", "Diff", "L95", "U95"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  pai_table2 <- pai_table2 %>% 
    add_header_lines(values = as_paragraph('VDYP-GRID diff (m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 9b. Predicted VDYP yield tables minus actual GRID sample actual (m3/ha/yr).")))) %>%
    autofit()
  
  return(pai_table2)
  
})


output$pai_table2 <- renderUI({
  
  htmltools_value(pai_table2())
})


pai_test <- reactive({
  
  req(input$SelectVar)
  
  Fig75_dat <- Fig75_dat()
  
  if(nrow(Fig75_dat) > 0 & all(!is.na(Fig75_dat$grdnv_pai)) & 
     length(Fig75_dat$prednv_pai - Fig75_dat$grdnv_pai) > 1){
    test1<-t.test(Fig75_dat$prednv_pai - Fig75_dat$grdnv_pai)
    test1est <- test1$estimate
    test1p <- test1$p.value
    test1result <- c(test1est, test1p)
  } else test1result <- c(NA, NA)
  
  return(test1result)
  
})

pai_comment <- reactive({
  
  req(input$SelectVar)
  
  if (length(remeas_plot()) > 0){
    
    pai_test <- pai_test()
    
    if (!is.null(pai_test) & !is.na(pai_test[2]) & pai_test[2] < 0.05){
      pai_comment <- paste0("VDYP is ", "<b>",
                              ifelse(!is.na(pai_test[1]) & pai_test[1] > 0, "over", "under"), "</b>",
                              "-estimating actual growth by ", "<b>", 
                              ifelse(!is.na(pai_test[1]), round(abs(pai_test[1]), 1), "-"), 
                              "</b>"," m<sup>3</sup>/ha/yr.</br>")
    } else if (!is.null(pai_test) & !is.na(pai_test[2]) & pai_test[2] >= 0.05){
      pai_comment <- "no significant difference between GRID and VDYP projection."
    } else if (is.null(pai_test) | all(is.na(pai_test))){
      pai_comment <- "(insufficient remeasured data)"
    }} else {
      pai_comment <- "No re-measred GRID samples"
    }
  
  return(pai_comment)
  
})

pai_text <- reactive({
  pai_text <-  HTML(paste0("<p><b>Periodic annual increment</b></p>
  <p>Test to Compare VDYP vs. Re-measured GRID Sample Periodic Annual Increment</p>
  <p>Periodic annual increment (PAI) in units of m3/ha/yr, is computed from all 
  re-measured GRID samples, and compared against predicted PAI from VDYP yield 
  tables over the same re-measurement period. Paired T-tests check for significant 
  differences in PAI (highlighted when significant at alpha=0.05, middle chart). 
  The following test helps evaluate if the VDYP growth assumptions are in line 
  with actual GRID sample growth rates. </p>",
                    "<p>The test indicates ", pai_comment(),"</p>"))
  return(pai_text)
})

  
output$pai_text <- renderUI({
    
  pai_text()
    
})



paidiff <- reactive({
  req(input$SelectVar)
  Fig75_dat <- Fig75_dat()
  
  if (nrow(Fig75_dat) > 1){
    all_test<-t.test(Fig75_dat$prednv_pai - Fig75_dat$grdnv_pai)
    mean_CI<-(all_test$conf.int[2]+all_test$conf.int[1])/2
    d <- data.frame(x = rep(0, 3), 
                    y = c((all_test$conf.int[2] + all_test$conf.int[1]) / 2,all_test$conf.int[1],all_test$conf.int[2]),
                    val = c("y", "ymin", "ymax"))
  } else {
    d <- data.frame(x = rep(NA, 3),
                    y = rep(NA, 3),
                    val = c("y", "ymin", "ymax"))
    all_test<-NA
  }
  
  
  
  p <- if (nrow(Fig75_dat) > 1){ ggplot(d, aes(x = x, y = y)) + 
      geom_hline(yintercept = 0, linetype = 2, size =1.2, col = "darkgray") +
      geom_line(linewidth = 1.2, col = "steelblue") +
      geom_text(aes(label = round(y,1)), col = "steelblue", 
                position = position_dodge(.9),  vjust = -1, size = 5) +
      geom_point(aes(x, y), size = 4, col = "steelblue")+
      xlim(-.1, .1) +
      ylim(floor(min(d$y)/5)*5, ceiling(max(d$y)/5)*5) +
      coord_flip() +
      labs(x = "", y = expression(m^3~"/ha/yr"),
           title = "PAI VDYP-GRID mean difference & 95% CI") +
      #theme_bw() + 
      theme(
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        rect = element_blank()
      )  
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  return(p)
})


output$paidiff <- renderPlot({
  
  paidiff()
})
  






curvoltext <- reactive({
  req(input$SelectVar)
  
  curvoltext <- paste0("<p>Field measured GRID volumes are compared to predicted volumes developed 
  for FAIB’s TSR analysis. FAIB’s TSR yield table development process has three 
  types of yield curves developed for stands greater than 50 years of age:</p>
  
  <ul><li><u>TSR TIPSY Opening Specific</u> – where the VRI feature links to a RESULTS 
  opening record that contains the minimum required yield table inputs. 
  These features are projected in TIPSY ver 4.4 using RESULTS opening specific 
  records as inputs.</li>
  <li><u>TSR TIPSY Aggregate</u> – where the VRI feature links to a RESULTS opening 
  record that does not contain the minimum required yield table inputs. 
  These features are projected in TIPSY ver 4.4 using the average of many 
  RESULTS opening specific yield curves within the same management unit and 
  Biogeoclimatic subzone.</li>
  <li><u>TSR VDYP</u> – where the VRI feature does not spatially match a RESULTS 
  opening record. These features are projected in VDYP ver 7 using VRI inventory 
  rank1 attributes.</li></ul>
  
  <p>TSR predicted volumes are compared to GRID volumes using the TSR input age 
       adjusted to the year of ground sampling. The first graph plots GRID actual 
       volume (points are joined where re-measurements are available), plus the 
       average of all spatially intersected TSR predicted yield tables (solid blue line). 
       The second graph illustrates the total bias (predicted minus actual volume) 
       at each individual GRID sample location, at the latest measurement. 
       TSR predicted volumes underestimate current YSM volume when the bias is 
       negative, and overestimate current GRID volume when positive.</p></br>")
  
  return(curvoltext)
})


output$curvoltext <- renderUI({
  
  HTML(curvoltext())
  
})




maturetsr <- reactive({
  req(input$SelectVar)
  Fig12_dat <- prj_msyt_vdyp %>%
    filter(CLSTR_ID %in% clstr_id_grid_all())
  
  ## Mean predicted volume (aggregated)
  agg_meanvol <- tsr_volproj %>%
    filter(CLSTR_ID %in% clstr_id_grid(), 
           AGE %in% seq(30, 120, 10)) %>% 
    group_by(AGE) %>%
    summarise(meanvol = mean(volTSR))
  
  p <- if (nrow(Fig12_dat) > 1){ ggplot() +
    geom_line(data = agg_meanvol, aes(x= AGE, y=meanvol), col="deepskyblue", linewidth = 3) +
    geom_point(data = Fig12_dat,
               aes(x=PROJ_AGE_ADJ, y=LIVE_VOL_PER_HA), col ="red", size = 3) +
    geom_line(data = Fig12_dat,
              aes(x=PROJ_AGE_ADJ, y=LIVE_VOL_PER_HA, group= SITE_IDENTIFIER), 
              col="red", linewidth = 1.2) +
    #scale_x_continuous(expand = c(0, 0.1))+ 
    scale_y_continuous(expand = c(0.01, 0.1), limits = c(-0.01, NA)) + 
    labs(x = "Total Age (yrs)", y = "Net Merch Volume (m3/ha)",
         title = "Mature Sample Remeasurements vs Average of TSR Yield Tables",
         subtitle = "(Spatially matched to each GRID sample location)") +
    #theme_bw() + 
    theme(
      plot.title = element_text(lineheight = 0.9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      rect = element_blank()
    ) 
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  return(p)
})


output$maturetsr <- renderPlot({
  
  maturetsr()
  
})



vol_bias <- reactive({
  req(input$SelectVar)
  Fig13_dat <- prj_msyt_vdyp %>%
    filter(CLSTR_ID %in% clstr_id_grid()) %>%
    mutate(grdnv = ifelse(is.na(grdnv), 0, grdnv),
           prednv = ifelse(is.na(prednv), 0, prednv),
           voldiffTSR = ifelse(is.na(voldiffTSR), 0, voldiffTSR)
    )
  
  p <- if (nrow(Fig13_dat) > 1){ ggplot() +
    geom_hline(yintercept = 0, col = "darkgray") +
    geom_point(data = Fig13_dat,
               aes(x=ref_age_adj, y=voldiffTSR, col = yt_source_f, shape = yt_source_f), 
               size= 3,, show.legend = TRUE) +
    scale_colour_manual(name = NULL, values=c("Managed" = "red","AGGREGATE"="deepskyblue" ,
                                              "VDYP" =  "green", "VDYP-fill_missed_tsr" = "darkmagenta"),
                        labels = c("TSR TIPSY Opening Specific", "TSR TIPSY Aggregate",
                                   "TSR VDYP", "TSR missed : VDYP filled"), drop = FALSE) +
    scale_shape_manual(name = NULL, 
                       labels = c("TSR TIPSY Opening Specific", "TSR TIPSY Aggregate",
                                  "TSR VDYP", "TSR missed : VDYP filled"),
                       values = c("Managed" =16, "AGGREGATE" =15, "VDYP" = 17, "VDYP-fill_missed_tsr" = 18), drop = FALSE) +
    #scale_x_continuous(expand = c(0, 0))+ 
    #scale_y_continuous(expand = c(0.01, 0), limits = c(-0.01, NA))+ 
    labs(x = "Total Age (yrs)", y = "Predicted - Actual (m3/ha)",
         title = "Total volume bias at each GRID location",
         subtitle = "(Predicted TSR yield table volume - Actual GRID volume)") +
    #theme_bw() + 
    theme(
      legend.position="top",
      #panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = 'darkgray'), 
      panel.grid.minor.y = element_line(color = 'darkgray'), 
      panel.grid.minor.x = element_blank(),
      rect = element_blank()
    ) 
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  return(p)
})



output$vol_bias <- renderPlot({
  
  vol_bias()
})




paitext <- reactive({
  req(input$SelectVar)
  
  #test1_comment <- test1_comment()
  tsrpaitest_comment <- tsrpaitest_comment()
  
  paitext <- HTML( paste0("<p>Periodic annual increment (PAI) in units of m3/ha/yr, 
                          is computed from all re-measured GRID samples, and 
                          compared against predicted PAI from TSR yield tables 
                          over the same re-measurement period. Paired T-tests 
                          check for significant differences in PAI (highlighted 
                          when significant at alpha=0.05, middle chart). The 
                          following test helps evaluate if the TSR growth 
                          assumptions are in line with actual GRID sample growth rates. </p> ",
                          
                          "<p>Results of test TSR yield tables vs. GRID show ", 
                          tsrpaitest_comment,"</p></br>"))
  return(paitext)
})



output$paitext <- renderUI({
  
  paitext()
  
})




tsr_pai_flex1 <- reactive({
  req(input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### TSR PAI summary table
  pai_table1 <- data.frame(attr = c("GRID", "TSR"),
                           obs = rep(dim(Fig15_dat)[1],2),
                           Yrs = c(round(mean(Fig15_dat$year_dff),0), round(mean(Fig15_dat$year_dff),0)),
                           PAI = c(round(mean(Fig15_dat$grdnv_pai), 2),
                                   round(mean(Fig15_dat$prednv_pai), 2)))
  
  pai_table1 <- flextable(pai_table1) %>% 
    set_header_labels(values = c("attr.", "# obs", "Yrs", "PAI"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  pai_table1 <- pai_table1 %>% 
    add_header_lines(values = c("GRID vs. TSR projections")) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 7a. Comparison of GRID sample actual growth vs. predicted TSR yield tables.")))) %>%
    autofit()
  
  return(pai_table1)   
})


output$tsr_pai_flex1 <- renderUI({
  
  htmltools_value(tsr_pai_flex1())
})




tsr_pai_flex2 <- reactive({
  req(input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  ### T test table
  if (nrow(Fig15_dat) > 1){
    
    pai_table2 <- broom::tidy(t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai), conf.int = TRUE)
    
  } else {
    pai_table2 <- data.frame(matrix(rep("-", 4), ncol=4,nrow=1, 
                                    dimnames=list(NULL, c("p.value", "estimate", "conf.low", "conf.high"))))
  }
  
  pai_table2 <- pai_table2 %>%
    select(p.value, estimate, conf.low, conf.high)  %>%
    mutate(p.value = ifelse(is.numeric(p.value), round(p.value, 3),p.value),
           estimate = ifelse(is.numeric(estimate), round(estimate, 3),estimate),
           conf.low = ifelse(is.numeric(conf.low), round(conf.low, 3),conf.low),
           conf.high = ifelse(is.numeric(conf.high), round(conf.high, 3),conf.high))
  
  pai_table2 <- flextable(pai_table2) %>% 
    color(i = ~ p.value < 0.05, j = 1, color = 'red', part = "body") %>%
    set_header_labels(values = c("p-val", "Diff", "L95", "U95"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  pai_table2 <- pai_table2 %>% 
    add_header_lines(values = as_paragraph('TSR-GRID diff (m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Table 7b. Predicted TSR yield tables minus actual GRID sample actual (m3/ha/yr).")))) %>%
    autofit()
  
  return(pai_table2)   
})


output$tsr_pai_flex2 <- renderUI({
  
  htmltools_value(tsr_pai_flex2())
})



pai_diff <- reactive({
  req(input$SelectVar)
  Fig15_dat <- Fig15_dat()
  
  if (nrow(Fig15_dat) > 1){
    all_test<-t.test(Fig15_dat$prednv_pai - Fig15_dat$grdnv_pai)
    mean_CI<-(all_test$conf.int[2]+all_test$conf.int[1])/2
    d <- data.frame(x = rep(0, 3), 
                    y = c((all_test$conf.int[2] + all_test$conf.int[1]) / 2,all_test$conf.int[1],all_test$conf.int[2]),
                    val = c("y", "ymin", "ymax"))
  } else {
    d <- data.frame(x = rep(NA, 3),
                    y = rep(NA, 3),
                    val = c("y", "ymin", "ymax"))
    all_test<-NA
  }
  
  p <- if (nrow(Fig15_dat) > 1){ 
    ggplot(d, aes(x = x, y = y)) + 
      geom_hline(yintercept = 0, linetype = 2, size =1.2, col = "darkgray") +
      geom_line(linewidth = 1.2, col = "steelblue") +
      geom_text(aes(label = round(y,1)), col = "steelblue", 
                position = position_dodge(.9),  vjust = -1, size = 5) +
      geom_point(aes(x, y), size = 4, col = "steelblue")+
      xlim(-.1, .1) +
      ylim(floor(min(d$y)/5)*5, ceiling(max(d$y)/5)*5) +
      coord_flip() +
      labs(x = "", y = expression(m^3~"/ha/yr"),
           title = "PAI TSR-GRID mean difference & 95% CI") +
      #theme_bw() + 
      theme(
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_blank(), axis.ticks.x = element_blank(),
        rect = element_blank()
      )  
  } else {
    ggplot() + 
      theme_void() +
      geom_text(aes(0,0,label='N/A')) +
      xlab(NULL)
  }
  
  
  return(p)
})


output$pai_diff <- renderPlot({
  
  pai_diff()
})