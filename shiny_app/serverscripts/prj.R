

growth_text <- reactive({
  growth_text <-  HTML("<p>The average VDYP7-projected volumes are plotted 
                       against the measured volumes from ground samples to infer 
                       volume growth within the TSA. Data points are connected 
                       where re-measurements are available (i.e., GRID samples). 
                       Samples identified as harvested or affected by disturbance 
                       in the 2023 Forest Vegetation Composite Rank 1 layer 
                       during the re-measurement period are shown separately.</p>")
  return(growth_text)
})


output$growth_text <- renderUI({
  
  growth_text()
  
})


fig7_5 <- reactive({
  
  
  prj_dat1 <- prj_dat %>%
    filter(CLSTR_ID %in% clstr_id()) %>%
    group_by(Design, PRJ_TOTAL_AGE) %>%
    summarise(n = n(),
              sumvol = sum(vdyp_vol_dwb, na.rm = T),
              meanvol = sumvol/n,
              sdvol = sd(vdyp_vol_dwb, na.rm = T),
              sevol = sdvol/sqrt(n)*qt(0.975, n-1)) %>%
    mutate(Design = factor(Design, levels = c("GRID", "PHASE2")))
    
    xlim <-  lead_vol %>%
      filter(CLSTR_ID %in% clstr_id_all()) %>% 
      pull(PRJ_TOTAL_AGE) %>% 
      max(na.rm = T)
    
    dist_dat <- sample_data %>% 
      filter(CLSTR_ID %in% clstr_id_all()) %>% 
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
      filter(CLSTR_ID %in% clstr_id_all()) %>%
      left_join(dist_dat, by = 'SITE_IDENTIFIER') %>%
      mutate(Design = factor(Design, levels = c("GRID", "PHASE2")))
    
    
    p <- ggplot() +
      geom_ribbon(data = prj_dat1 %>% filter(n >5), 
                  aes(x = PRJ_TOTAL_AGE, ymin = meanvol - sevol, ymax = meanvol + sevol, 
                      group = Design), 
                  fill = "lightgray") +
      geom_line(data = prj_dat1 %>% filter(n >5), 
                aes(x = PRJ_TOTAL_AGE, y = meanvol, col = Design), linewidth = 2) +
      scale_color_manual(values = c("coral1", "palegreen3"), drop = FALSE, name = "VDYP projection") +
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
      
      geom_point(data = vol_dat %>%
                   filter(Design == "PHASE2"), 
                 aes(x = PRJ_TOTAL_AGE, y = NTWB_NVAF_LS, col = Design), size = 3) +
      scale_x_continuous(limit = c(0, xlim)) +
      scale_color_manual(values = c("brown3", "chartreuse4"), drop = FALSE) +
      labs(x = "Age", y = "Volume") +
      facet_wrap(~Design, scale = "free_x", drop = FALSE) +
      theme(
        plot.caption = element_text(hjust = 0, size=15, face = "bold"),
        plot.caption.position = "plot",
        legend.position = "top",
        legend.title = NULL,
        legend.position.inside = c(0.1, 0.85)
      ) 
    
    
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
  pai_table1 <- data.frame(attr = c("Ground", "VDYP"),
                           obs = rep(dim(Fig75_dat)[1],2),
                           Yrs = c(round(mean(Fig75_dat$year_dff),0), round(mean(Fig75_dat$year_dff),0)),
                           PAI = c(round(mean(Fig75_dat$grdnv_pai), 2),
                                   round(mean(Fig75_dat$prednv_pai), 2)))
  
  pai_table1 <- flextable(pai_table1) %>% 
    set_header_labels(values = c("attr.", "# obs", "Yrs", "PAI"), lengths = colwidths) %>%
    align(align = "center", part = "header") 
  
  pai_table1 <- pai_table1 %>% 
    add_header_lines(values = c("Ground vs. VDYP projections")) %>%
    bold(part = 'header', bold = TRUE) %>%
    #set_caption(as_paragraph(
    #  as_b(as_chunk("Test 1: Compare YSM actual growth vs. TSR yield table projections")))) %>%
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
    add_header_lines(values = as_paragraph('Ground-VDYP diff (m',as_sup('3'),'/ha/yr)')) %>%
    bold(part = 'header', bold = TRUE) %>%
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
      pai_comment <- paste0("Prediction is ", "<b>",
                              ifelse(!is.na(pai_test[1]) & pai_test[1] > 0, "over", "under"), "</b>",
                              "-estimating actual growth by ", "<b>", 
                              ifelse(!is.na(pai_test[1]), round(abs(pai_test[1]), 1), "-"), 
                              "</b>"," m<sup>3</sup>/ha/yr.</br>")
    } else if (!is.null(pai_test) & !is.na(pai_test[2]) & pai_test[2] >= 0.05){
      pai_comment <- "no significant difference between Ground and VDYP projection."
    } else if (is.null(pai_test) | all(is.na(pai_test))){
      pai_comment <- "(insufficient remeasured data)"
    }} else {
      pai_comment <- "No re-measred Ground samples"
    }
  
  return(pai_comment)
  
})

pai_text <- reactive({
  pai_text <-  HTML(paste0("<p>Periodic annual increment (PAI; m3/ha/yr) is calculated 
                    for all re-measured grid-based ground plots and compared to 
                    corresponding PAI values predicted by VDYP projections. 
                    Paired t-test is conducted to assess statistically 
                    significant differences between observed and predicted PAI, 
                    with result highlighted in the middle panel when p < 0.05 
                    and sample size n > 8.</p>",
                    "<p>The test indicates ", pai_comment(),"</p>"))
  return(pai_text)
})

  
output$pai_text <- renderUI({
    
  pai_text()
    
})
  