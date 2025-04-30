
title <- reactive({
  req(input$SelectVar)
  title <- as.character(input$SelectVar)
  return(title)
})


clstr_id <- reactive({
  
  req(input$SelectVar)
  
  clstr_id <- sample_data %>% 
    filter(TSA_DESC %in% input$SelectVar, LAST_MSMT_new == "Y") %>%
    pull(CLSTR_ID)
  
  return(clstr_id)
  
})



clstr_id_all <- reactive({
  
  req(input$SelectVar)
  
  clstr_id_all <- sample_data %>% 
    filter(TSA_DESC %in% input$SelectVar) %>%
    pull(CLSTR_ID)
  
  return(clstr_id_all)
  
})



tsa30_ci <- reactive({
  
  req(input$SelectVar)
  
  if (input$SelectVar == "Fraser TSA"){
    
    tsa30_ci <- sample_data %>%
      filter(MGMT_UNIT == "TSA30_Fraser", Design == "PHASE2") %>%
      pull(CLSTR_ID)
    
  } else NULL
  
  return(tsa30_ci)
  
})

sample_tsa30 <- reactive({
  req(input$SelectVar)
  
  # *separate computation of rom for TSA30 phase2 samples;
  # *samples selected in two subpopulations (in IFPA, out IFPA);
  # *first analyze overall;
  #sample_tsa30 <- sample_data %>%
  #  filter(CLSTR_ID %in% tsa30_ci()) %>%
  #  mutate(proj_id = sub("_.*", "", SAMPLE_SITE_NAME),
  #         stratum = ifelse(proj_id == '0301', 'IFPA-OUT', 'IFPA-IN'),
  #         strat_area = ifelse(proj_id == '0301', 750000, 100000),
  #         strat_n = ifelse(proj_id == '0301', 87, 50),
  #         t_area_vt = 750000 + 100000,
  #         weight =  strat_area/(strat_n*t_area_vt)) %>%
  #  select(CLSTR_ID, Design, 
  #         stratum, strat_area, strat_n, t_area_vt, weight)
  
  sample_tsa30 <- sample_data %>%
    filter(CLSTR_ID %in% tsa30_ci()) %>%
    mutate(proj_id = sub("_.*", "", SAMPLE_SITE_NAME),
           stratum = ifelse(proj_id == '0301', 'IFPA-OUT', 'IFPA-IN'),
           strat_area = ifelse(proj_id == '0301', 750000, 100000),
           t_area_vt = 750000 + 100000) %>%
    group_by(stratum) %>%
    mutate(strat_n = n_distinct(CLSTR_ID),
           weight =  strat_area/(strat_n*t_area_vt)) %>%
    select(CLSTR_ID, Design, 
           stratum, strat_area, strat_n, t_area_vt, weight)
  
  
  return(sample_tsa30)
  
})



correct_ls <- reactive({
  req(input$SelectVar)
  
  correct_ls <- lead_vol %>%
    filter(CLSTR_ID %in%  clstr_id()) %>%
    mutate(SPECIES = ifelse(is.na(SPECIES), "", SPECIES),
           SPECIES_INV = ifelse(is.na(SPECIES_INV), "", SPECIES_INV)) %>%
    group_by(Design) %>%
    reframe(correct_ls = round(sum(SPECIES_INV == SPECIES)/n(), 3)) %>%
    data.table
  
  return(correct_ls)
  
})


top3spc <- reactive({
  req(input$SelectVar)

#top3spc <- lead_vol %>%  
#  filter(CLSTR_ID %in% clstr_id()) %>%
#  filter(!is.na(SPC_GRP2)) %>%  
#  group_by(Design, SPC_GRP2) %>%
#  count() %>% 
#  group_by(Design) %>%
#  top_n(3) %>%
#  arrange(Design, desc(n), desc(SPC_GRP2)) %>%
#  slice_head(n = 3) %>%
#  mutate(top3 = "Y") %>%
#  data.table
#  #group_by(SPC_GRP2) %>%
#  #count() %>% 
#  #ungroup() %>% 
#  #top_n(3) %>%
#  #pull(SPC_GRP2)

top3spc <- lead_vol %>%  
  filter(CLSTR_ID %in% clstr_id()) %>%
  filter(!is.na(SPECIES_INV)) %>%  
  group_by(Design, SPECIES_INV) %>%
  count() %>% 
  filter(n >= 8) %>% 
  mutate(top3 = "Y")

if (nrow(top3spc) < 3){
  
  top3spc <- lead_vol %>%  
    filter(CLSTR_ID %in% clstr_id()) %>%
    filter(!is.na(SPECIES_INV)) %>%  
    group_by(Design, SPECIES_INV) %>%
    count() %>% 
    group_by(Design) %>%
    top_n(3) %>%
    arrange(Design, desc(n), desc(SPECIES_INV)) %>%
    slice_head(n = 3) %>%
    mutate(top3 = "Y") %>%
    data.table
}

return(top3spc)

})


lead_vol_dat <- reactive({
  req(input$SelectVar)
  
  lead_vol_dat <- lead_vol %>%
    filter(CLSTR_ID %in%  clstr_id()) %>%
    select(CLSTR_ID, Design, 
           AGET_TLSO, BA_HA_LS, HT_TLSO, NTWB_NVAF_LS, NTWB_NVAF_DS,
           PROJ_AGE_ADJ, vdyp_ba, vdyp_dom_ht, vdyp_vol_dwb, DEAD_STAND_VOLUME_175)
  
  lead_vol_dat1 <- lead_vol_dat %>%
    group_by(Design) %>%
    summarise(n_age = sum(!is.na(AGET_TLSO)),
              n_ba = sum(!is.na(BA_HA_LS)),
              n_ht = sum(!is.na(HT_TLSO)),
              n_vol = sum(!is.na(NTWB_NVAF_LS)),
              n_voldead = sum(!is.na(NTWB_NVAF_DS)),
              inv_age = mean(PROJ_AGE_ADJ, na.rm = T),
              inv_ba = mean(vdyp_ba, na.rm = T),
              inv_ht = mean(vdyp_dom_ht, na.rm = T),
              inv_vol = mean(vdyp_vol_dwb, na.rm = T),
              inv_voldead = mean(DEAD_STAND_VOLUME_175, na.rm = T),
              grd_age = mean(AGET_TLSO, na.rm = T),
              grd_ba = mean(BA_HA_LS, na.rm = T),
              grd_ht = mean(HT_TLSO, na.rm = T),
              grd_vol = mean(NTWB_NVAF_LS, na.rm = T),
              grd_voldead = mean(NTWB_NVAF_DS, na.rm = T)) %>%
    ungroup() %>%
    mutate(rom_age = grd_age/inv_age,
           rom_ba = grd_ba/inv_ba,
           rom_ht = grd_ht/inv_ht,
           rom_vol = grd_vol/inv_vol,
           rom_voldead = grd_voldead/inv_voldead)
  
  
  lead_vol_dat2 <- lead_vol_dat %>%
    left_join(lead_vol_dat1, by = "Design")
  
  
  lead_vol_dat2 <- lead_vol_dat2 %>%
    mutate(diffssqrd_age = (AGET_TLSO - (rom_age * PROJ_AGE_ADJ))**2,
           diffssqrd_ba = (BA_HA_LS - (rom_ba * vdyp_ba))**2,
           diffssqrd_ht = (HT_TLSO - (rom_ht * vdyp_dom_ht))**2,
           diffssqrd_vol = (NTWB_NVAF_LS - (rom_vol * vdyp_vol_dwb))**2,
           diffssqrd_voldead = (NTWB_NVAF_DS - (rom_voldead * DEAD_STAND_VOLUME_175))**2)
  
  lead_vol_dat3 <- lead_vol_dat2 %>%
    group_by(Design) %>%
    summarise(n = n(),
              sumdiffssqrd_age = sum(diffssqrd_age, na.rm = T),
              sumdiffssqrd_ba = sum(diffssqrd_ba, na.rm = T),
              sumdiffssqrd_ht = sum(diffssqrd_ht, na.rm = T),
              sumdiffssqrd_vol = sum(diffssqrd_vol, na.rm = T),
              sumdiffssqrd_voldead = sum(diffssqrd_voldead, na.rm = T))
  
  lead_vol_dat4 <- lead_vol_dat2 %>%
    left_join(lead_vol_dat3, by = "Design")
  
  
  lead_vol_dat4 <- lead_vol_dat4 %>%
    #group_by(design) %>%
    rowwise() %>%
    mutate(varrom_ba = ifelse(n_ba >=2, sumdiffssqrd_ba / (n_ba * (n_ba-1) * inv_ba**2), NA),
           l95rom_ba  = ifelse(n_ba >=2, rom_ba - qt(0.975,n_ba-1) * sqrt(varrom_ba), NA),
           u95rom_ba  = ifelse(n_ba >=2, rom_ba + qt(0.975,n_ba-1) * sqrt(varrom_ba), NA),
           # *standard signficance test;
           sigrom_ba = ifelse(!is.na(l95rom_ba) & l95rom_ba < 1.0 & !is.na(u95rom_ba) & u95rom_ba > 1.0,
                              "N", "Y"),
           # *following review by p.ott 2021mar, recommend to use ROPE TO DEFINE ZONE OF PRACTICAL SIGNIFICANCE;
           # *suggested to use 0.95 to 1.05 range of ratio to establish zone, rationale is that 5% rom is a 1m SI difference from a known;
           # *SI of 20m, and results in a 10% change in culmination of MAI for a PL MSYT using TIPSY 4.4;
           
           # *95% confidence interval completely outside rope;
           sigrope_ba = case_when((!is.na(l95rom_ba) & l95rom_ba > upr_limit) | (!is.na(u95rom_ba) & u95rom_ba < lwr_limit) ~ "Y",
                                  # *95% confidence interval completely inside rope;
                                  (!is.na(l95rom_ba) & l95rom_ba > lwr_limit) & (!is.na(u95rom_ba) & u95rom_ba < upr_limit) ~ "N",
                                  # *one 95% confidence limit inside rope, but the other outside rope, then inconclusive;
                                  (!is.na(l95rom_ba) & l95rom_ba < upr_limit) & (!is.na(u95rom_ba) & u95rom_ba > upr_limit) ~ "I",
                                  (!is.na(l95rom_ba) & l95rom_ba < lwr_limit) & (!is.na(u95rom_ba) & u95rom_ba > lwr_limit) ~ "I",
                                  # *both 95% confidence limits outside rope, then inconclusive;
                                  (!is.na(l95rom_ba) & l95rom_ba < lwr_limit) & (!is.na(u95rom_ba) & u95rom_ba > upr_limit) ~ "I"
           ),
           varrom_age = ifelse(n_age >=2, sumdiffssqrd_age / (n_age * (n_age-1) * inv_age**2), NA),
           l95rom_age  = ifelse(n_age >=2, rom_age - qt(0.975,n_age-1) * sqrt(varrom_age), NA),
           u95rom_age  = ifelse(n_age >=2, rom_age + qt(0.975,n_age-1) * sqrt(varrom_age), NA),
           sigrom_age = ifelse(!is.na(l95rom_age) & l95rom_age < 1.0 & !is.na(u95rom_age) & u95rom_age > 1.0,
                               "N", "Y"),
           sigrope_age = case_when((!is.na(l95rom_age) & l95rom_age > upr_limit) | (!is.na(u95rom_age) & u95rom_age < lwr_limit) ~ "Y",
                                   (!is.na(l95rom_age) & l95rom_age > lwr_limit) & (!is.na(u95rom_age) & u95rom_age < upr_limit) ~ "N",
                                   (!is.na(l95rom_age) & l95rom_age < upr_limit) & (!is.na(u95rom_age) & u95rom_age > upr_limit) ~ "I",
                                   (!is.na(l95rom_age) & l95rom_age < lwr_limit) & (!is.na(u95rom_age) & u95rom_age > lwr_limit) ~ "I",
                                   (!is.na(l95rom_age) & l95rom_age < lwr_limit) & (!is.na(u95rom_age) & u95rom_age > upr_limit) ~ "I"
           ),
           varrom_ht = ifelse(n_ht >=2, sumdiffssqrd_ht / (n_ht * (n_ht-1) * inv_ht**2), NA),
           l95rom_ht  = ifelse(n_ht >=2, rom_ht - qt(0.975,n_ht-1) * sqrt(varrom_ht), NA),
           u95rom_ht  = ifelse(n_ht >=2, rom_ht + qt(0.975,n_ht-1) * sqrt(varrom_ht), NA),
           sigrom_ht = ifelse(!is.na(l95rom_ht) & l95rom_ht < 1.0 & !is.na(u95rom_ht) & u95rom_ht > 1.0,
                              "N", "Y"),
           sigrope_ht = case_when((!is.na(l95rom_ht) & l95rom_ht > upr_limit) | (!is.na(u95rom_ht) & u95rom_ht < lwr_limit) ~ "Y",
                                  (!is.na(l95rom_ht) & l95rom_ht > lwr_limit) & (!is.na(u95rom_ht) & u95rom_ht < upr_limit) ~ "N",
                                  (!is.na(l95rom_ht) & l95rom_ht < upr_limit) & (!is.na(u95rom_ht) & u95rom_ht > upr_limit) ~ "I",
                                  (!is.na(l95rom_ht) & l95rom_ht < lwr_limit) & (!is.na(u95rom_ht) & u95rom_ht > lwr_limit) ~ "I",
                                  (!is.na(l95rom_ht) & l95rom_ht < lwr_limit) & (!is.na(u95rom_ht) & u95rom_ht > upr_limit) ~ "I"
           ),
           varrom_vol = ifelse(n_vol >=2, sumdiffssqrd_vol / (n_vol * (n_vol-1) * inv_vol**2), NA),
           l95rom_vol  = ifelse(n_vol >=2, rom_vol - qt(0.975,n_vol-1) * sqrt(varrom_vol), NA),
           u95rom_vol  = ifelse(n_vol >=2, rom_vol + qt(0.975,n_vol-1) * sqrt(varrom_vol), NA),
           sigrom_vol = ifelse(!is.na(l95rom_vol) & l95rom_vol < 1.0 & !is.na(u95rom_vol) & u95rom_vol > 1.0,
                               "N", "Y"),
           sigrope_vol = case_when((!is.na(l95rom_vol) & l95rom_vol > upr_limit) | (!is.na(u95rom_vol) & u95rom_vol < lwr_limit) ~ "Y",
                                   (!is.na(l95rom_vol) & l95rom_vol > lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol < upr_limit) ~ "N",
                                   (!is.na(l95rom_vol) & l95rom_vol < upr_limit) & (!is.na(u95rom_vol) & u95rom_vol > upr_limit) ~ "I",
                                   (!is.na(l95rom_vol) & l95rom_vol < lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol > lwr_limit) ~ "I",
                                   (!is.na(l95rom_vol) & l95rom_vol < lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol > upr_limit) ~ "I"
           ),
           varrom_voldead = ifelse(n_voldead >=2, sumdiffssqrd_voldead / (n_voldead * (n_voldead-1) * inv_voldead**2), NA),
           l95rom_voldead  = ifelse(n_voldead >=2, rom_voldead - qt(0.975,n_voldead-1) * sqrt(varrom_voldead), NA),
           u95rom_voldead  = ifelse(n_voldead >=2, rom_voldead + qt(0.975,n_voldead-1) * sqrt(varrom_voldead), NA),
           sigrom_voldead = ifelse(!is.na(l95rom_voldead) & l95rom_voldead < 1.0 & !is.na(u95rom_voldead) & u95rom_voldead > 1.0,
                                   "N", "Y"),
           sigrope_voldead = case_when((!is.na(l95rom_voldead) & l95rom_voldead > upr_limit) | (!is.na(u95rom_voldead) & u95rom_voldead < lwr_limit) ~ "Y",
                                       (!is.na(l95rom_voldead) & l95rom_voldead > lwr_limit) & (!is.na(u95rom_voldead) & u95rom_voldead < upr_limit) ~ "N",
                                       (!is.na(l95rom_voldead) & l95rom_voldead < upr_limit) & (!is.na(u95rom_voldead) & u95rom_voldead > upr_limit) ~ "I",
                                       (!is.na(l95rom_voldead) & l95rom_voldead < lwr_limit) & (!is.na(u95rom_voldead) & u95rom_voldead > lwr_limit) ~ "I",
                                       (!is.na(l95rom_voldead) & l95rom_voldead < lwr_limit) & (!is.na(u95rom_voldead) & u95rom_voldead > upr_limit) ~ "I"
           )
    ) %>% data.table
  
  lead_vol_dat5 <- lead_vol_dat4 %>%
    select(Design, starts_with("n_"),starts_with("inv_"),starts_with("grd_"),
           starts_with("rom_"),starts_with("l95rom_"),starts_with("u95rom_"),
           starts_with("sigrom_"),starts_with("sigrope_")) %>%
    pivot_longer(cols = n_age:sigrope_voldead,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  
  return(lead_vol_dat4)
  
})


lead_vol_tsa30 <- reactive({
  req(input$SelectVar)
  
  #lead_vol_dat <- lead_vol_dat()
  
  if (input$SelectVar == "Fraser TSA"){
    sample_tsa30 <- sample_tsa30()
    
    tsa30_leadvol <- lead_vol %>%
      filter(CLSTR_ID %in% tsa30_ci()) %>%
      left_join(sample_tsa30, by = c('CLSTR_ID', 'Design')) %>%
      select(CLSTR_ID, Design, 
             stratum, strat_area, strat_n, t_area_vt, weight,
             grd_vol = NTWB_NVAF_LS, inv_vol = vdyp_vol_dwb, 
             grd_ba = BA_HA_LS, inv_ba = vdyp_ba, 
             grd_age = AGET_TLSO, inv_age = PROJ_AGE_ADJ, 
             grd_ht = HT_TLSO, inv_ht = vdyp_dom_ht, 
             grd_voldead = NTWB_NVAF_DS, inv_voldead = DEAD_STAND_VOLUME_175) 
    
    tsa30_leadvol_dat <- tsa30_leadvol %>%
      pivot_longer(cols = grd_vol:inv_voldead,
                   names_to = c("source", "variable"),
                   names_pattern = "(.*)_(.*)") %>%
      pivot_wider(names_from = source,
                  values_from = value) %>%
      replace_na(list(inv = 0)) %>%
      data.table
    
    
    stat_ratio_all <- data.frame()
    stat_xy_all <- data.frame()
    
    for (i in unique(tsa30_leadvol_dat$variable)){
      dat <- tsa30_leadvol_dat[variable == i,]
      dat <- dat[!is.na(grd),]
      
      svy_base <- svydesign(
        data = dat,
        strata = ~stratum,
        ids = ~CLSTR_ID,  
        weights = ~weight
      )
      
      set.seed(123)
      svy_design <- as.svrepdesign(
        design = svy_base,
        type = "bootstrap",
        replicates = 1000
      )
      
      stat_ratio <- tryCatch({
        ratio_est <- svyratio(~grd, ~inv, design = svy_design)
        confint_est <- confint(ratio_est)
        
        data.frame(
          variable = i,
          ratio = coef(ratio_est)[1],
          lower = confint_est[1],
          upper = confint_est[2],
          se = c(sqrt(ratio_est$var)),
          n = nrow(dat)
        )
      })
      
      stat_xy <- tryCatch({
        est <- svytotal(~grd + inv, design = svy_design)
        conf <- confint(est)
        
        data.frame(
          variable = i,
          ground_sum = coef(est)["grd"],
          ground_lcl = conf["grd", 1],
          ground_ucl = conf["grd", 2],
          pred_sum = coef(est)["inv"],
          pred_lcl = conf["inv", 1],
          pred_ucl = conf["inv", 2],
          n = nrow(dat)
        )
      })
      
      stat_ratio_all <- rbind(stat_ratio_all, stat_ratio)
      stat_xy_all <- rbind(stat_xy_all, stat_xy)
    }
    
    
    tsa30_leadvol_dat1 <- merge(stat_ratio_all, stat_xy_all, by = c('variable', 'n'))
    
    tsa30_leadvol_dat2 <- tsa30_leadvol_dat1 %>%
      select(var = variable, n, grd = ground_sum, inv = pred_sum, 
             rom = ratio, se) %>%
      mutate(Design = "PHASE2",
             l95rom = ifelse(n >=2, rom - qt(0.975,n-1) * se, NA),
             u95rom = ifelse(n >=2, rom + qt(0.975,n-1) * se, NA),
             sigrom = ifelse(!is.na(l95rom) & l95rom < 1.0 & !is.na(u95rom) & u95rom > 1.0,
                             "N", "Y"),
             sigrope = case_when((!is.na(l95rom) & l95rom > upr_limit) | (!is.na(u95rom) & u95rom < lwr_limit) ~ "Y",
                                 (!is.na(l95rom) & l95rom > lwr_limit) & (!is.na(u95rom) & u95rom < upr_limit) ~ "N",
                                 (!is.na(l95rom) & l95rom < upr_limit) & (!is.na(u95rom) & u95rom > upr_limit) ~ "I",
                                 (!is.na(l95rom) & l95rom < lwr_limit) & (!is.na(u95rom) & u95rom > lwr_limit) ~ "I",
                                 (!is.na(l95rom) & l95rom < lwr_limit) & (!is.na(u95rom) & u95rom > upr_limit) ~ "I"
             )) %>%
      select(Design, var, n, inv, grd, rom, l95rom, u95rom, sigrom, sigrope)
    
    
  }
  
  return(tsa30_leadvol_dat2)
  
})


invspc_vol_dat <- reactive({
  req(input$SelectVar)
  
  top3spc <- top3spc()
  
  #top3_grid <- top3spc[Design == "GRID",]$SPECIES_INV
  #top3_phase2 <- top3spc[Design == "PHASE2",]$SPECIES_INV
  
  #invspc_vol <- lead_vol %>%
  #  filter(CLSTR_ID %in%  clstr_id()) %>% 
  #  rowwise() %>%
  #  mutate(SPC_GRP_GRD = ifelse(SPC_GRP1 %in% top3spc, SPC_GRP1, "OTH"),
  #         SPC_GRP_INV = ifelse(SPC_GRP2 %in% top3spc, SPC_GRP2, "OTH")) %>%
  #  select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP1, SPC_GRP_GRD, SPC_GRP2, 
  #         SPC_GRP_INV, NTWB_NVAF_LS, vdyp_vol_dwb)
  
  invspc_vol <- lead_vol %>%
    filter(CLSTR_ID %in%  clstr_id()) %>% 
    left_join(top3spc %>% select(-n), by = c('Design', 'SPECIES_INV')) %>%
    mutate(#SPC_GRP_INV = ifelse(!is.na(top3) & top3 == "Y", SPC_GRP2, "OTH"),
      #SPC_GRP_INV = ifelse(!is.na(top3) & top3 == "Y", SPECIES_INV, "OTH"),
      SPC_GRP_INV = ifelse(SPECIES_INV %in% top3spc$SPECIES_INV, SPECIES_INV, 'OTH')) %>%
    replace_na(list(NTWB_NVAF_LS = 0, vdyp_vol_dwb = 0)) %>%
    #rowwise() %>%
    #mutate(SPC_GRP_INV = case_when(Design == "GRID" & SPC_GRP2 %in% top3_grid ~ SPC_GRP2,
    #                               Design == "GRID" & !(SPC_GRP2 %in% top3_grid) ~ "OTH",
    #                               Design == "PHASE2" & SPC_GRP2 %in% top3_phase2 ~ SPC_GRP2,
    #                               Design == "PHASE2" & !(SPC_GRP2 %in% top3_phase2) ~ "OTH",
    #                               TRUE ~ NA)) %>%
    select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP1, SPC_GRP2, 
           SPC_GRP_INV, NTWB_NVAF_LS, vdyp_vol_dwb)
  
  invspc_vol1 <- invspc_vol %>%
    group_by(Design, SPC_GRP_INV) %>%
    summarise(n = n(),
              grd_vol = mean(NTWB_NVAF_LS, na.rm = T),
              inv_vol = mean(vdyp_vol_dwb, na.rm = T)) %>%
    ungroup() %>%
    mutate(rom_vol = grd_vol/inv_vol)
  
  invspc_vol2 <- invspc_vol %>%
    left_join(invspc_vol1, by = c("Design","SPC_GRP_INV")) %>%
    select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP_INV, 
           NTWB_NVAF_LS, vdyp_vol_dwb, grd_vol, inv_vol, rom_vol)
  
  invspc_vol3 <- invspc_vol2 %>%
    mutate(diffssqrd_vol = (NTWB_NVAF_LS - (rom_vol * vdyp_vol_dwb))**2)
  
  invspc_vol4 <- invspc_vol3 %>%
    group_by(Design, SPC_GRP_INV) %>%
    summarise(n = n(),
              sumdiffssqrd_vol = sum(diffssqrd_vol, na.rm = T))
  
  invspc_vol5 <- invspc_vol3 %>%
    left_join(invspc_vol4, by = c("Design","SPC_GRP_INV"))
  
  
  invspc_vol5 <- invspc_vol5 %>%
    rowwise() %>%
    mutate(varrom_vol = ifelse(n >=2, sumdiffssqrd_vol / (n * (n-1) * inv_vol**2), NA),
           l95rom_vol  = ifelse(n >=2, rom_vol - qt(0.975,n-1) * sqrt(varrom_vol), NA),
           u95rom_vol  = ifelse(n >=2, rom_vol + qt(0.975,n-1) * sqrt(varrom_vol), NA),
           sigrom_vol = ifelse(!is.na(l95rom_vol) & l95rom_vol < 1.0 & !is.na(u95rom_vol) & u95rom_vol > 1.0,
                               "N", "Y"),
           sigrope_vol = case_when((!is.na(l95rom_vol) & l95rom_vol > upr_limit) | (!is.na(u95rom_vol) & u95rom_vol < lwr_limit) ~ "Y",
                                   (!is.na(l95rom_vol) & l95rom_vol > lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol < upr_limit) ~ "N",
                                   (!is.na(l95rom_vol) & l95rom_vol < upr_limit) & (!is.na(u95rom_vol) & u95rom_vol > upr_limit) ~ "I",
                                   (!is.na(l95rom_vol) & l95rom_vol < lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol > lwr_limit) ~ "I",
                                   (!is.na(l95rom_vol) & l95rom_vol < lwr_limit) & (!is.na(u95rom_vol) & u95rom_vol > upr_limit) ~ "I"
           )
    )
  
  invspc_vol6 <- invspc_vol5 %>%
    select(-SITE_IDENTIFIER, -CLSTR_ID, -NTWB_NVAF_LS, -vdyp_vol_dwb, 
           -varrom_vol, -diffssqrd_vol, -sumdiffssqrd_vol) %>%
    distinct() %>% 
    arrange(Design, SPC_GRP_INV) %>%
    data.table
  
  
  return(invspc_vol6)
  
})


spc_vol_dat <- reactive({
  
  spc_vol_dat <- spc_vol %>%
    filter(CLSTR_ID %in%  clstr_id()) %>%
    group_by(Design, source, SPECIES) %>%
    summarise(livevol = sum(LIVE_VOL_PER_HA, na.rm = T),
              deadvol = sum(DEAD_VOL_PER_HA, na.rm = T))
  
  return(spc_vol_dat)
  
})


correc_sp_vol <- reactive({
  
  spc_vol_dat <- spc_vol_dat()
  
  spc_vol_dat1 <- spc_vol_dat %>%
    group_by(Design, source) %>%
    reframe(
      SPECIES = SPECIES,
      livevolperc = livevol/sum(livevol, na.rm= T) * 100,
      deadvolperc = deadvol/sum(deadvol, na.rm= T) * 100) %>%
    data.table
  
  spc_vol_dat2 <- spc_vol_dat1 %>%
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
  
  return(spc_vol_dat2)
  
})



invspc_vol_tsa30 <- reactive({
  req(input$SelectVar)
  
  if (input$SelectVar == "Fraser TSA"){
    
    top3spc <- top3spc()
    #top3_phase2 <- top3spc[Design == "PHASE2",]$SPECIES_INV
    
    sample_tsa30 <- sample_tsa30()
    
    tsa30_spcvol <- lead_vol %>%
      filter(CLSTR_ID %in%  tsa30_ci()) %>% 
      left_join(top3spc %>% select(-n), by = c('Design', 'SPECIES_INV')) %>%
      mutate(#SPC_GRP_INV = ifelse(!is.na(top3) & top3 == "Y", SPC_GRP2, "OTH"),
             SPC_GRP_INV = ifelse(SPECIES_INV %in% top3spc$SPECIES_INV, SPECIES_INV, 'OTH')) %>%
      replace_na(list(#NTWB_NVAF_LS = 0, 
                      vdyp_vol_dwb = 0)) %>%
      select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP1, SPC_GRP2, 
             SPC_GRP_INV, NTWB_NVAF_LS, vdyp_vol_dwb) %>%
      left_join(sample_tsa30, by = c('CLSTR_ID', 'Design')) %>%
      select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP1, SPC_GRP2, SPC_GRP_INV, 
             stratum, strat_area, strat_n, t_area_vt, weight,
             grd_vol = NTWB_NVAF_LS, inv_vol = vdyp_vol_dwb) %>%
      data.table
    
    
    stat_ratio_spc <- data.frame()
    stat_xy_spc <- data.frame()
    
    for (i in unique(tsa30_spcvol$SPC_GRP_INV)){
      dat <- tsa30_spcvol[SPC_GRP_INV == i,]
      dat <- dat[!is.na(grd_vol),]
      
      svy_base <- svydesign(
        data = dat,
        strata = ~stratum,
        ids = ~CLSTR_ID,  
        weights = ~weight
      )
      
      set.seed(123)
      svy_design <- as.svrepdesign(
        design = svy_base,
        type = "bootstrap",
        replicates = 1000
      )
      
      stat_ratio <- tryCatch({
        ratio_est <- svyratio(~grd_vol, ~inv_vol, design = svy_design)
        confint_est <- confint(ratio_est)
        
        data.frame(
          variable = i,
          ratio = coef(ratio_est)[1],
          lower = confint_est[1],
          upper = confint_est[2],
          se = c(sqrt(ratio_est$var)),
          n = nrow(dat)
        )
      })
      
      stat_xy <- tryCatch({
        est <- svytotal(~grd_vol + inv_vol, design = svy_design)
        conf <- confint(est)
        
        data.frame(
          variable = i,
          ground_sum = coef(est)["grd_vol"],
          ground_lcl = conf["grd_vol", 1],
          ground_ucl = conf["grd_vol", 2],
          pred_sum = coef(est)["inv_vol"],
          pred_lcl = conf["inv_vol", 1],
          pred_ucl = conf["inv_vol", 2],
          n = nrow(dat)
        )
      })
      
      stat_ratio_spc <- rbind(stat_ratio_spc, stat_ratio)
      stat_xy_spc <- rbind(stat_xy_spc, stat_xy)
    }
    
    
    tsa30_spcvol_dat1 <- merge(stat_ratio_spc, stat_xy_spc, by = c('variable', 'n'))
    
    tsa30_spcvol_dat2 <- tsa30_spcvol_dat1 %>%
      select(var = variable, n, grd = ground_sum, inv = pred_sum, 
             rom = ratio, se) %>%
      mutate(Design = "PHASE2",
             l95rom = ifelse(n >=2, rom - qt(0.975,n-1) * se, NA),
             u95rom = ifelse(n >=2, rom + qt(0.975,n-1) * se, NA),
             sigrom = ifelse(!is.na(l95rom) & l95rom < 1.0 & !is.na(u95rom) & u95rom > 1.0,
                             "N", "Y"),
             sigrope = case_when((!is.na(l95rom) & l95rom > upr_limit) | (!is.na(u95rom) & u95rom < lwr_limit) ~ "Y",
                                 (!is.na(l95rom) & l95rom > lwr_limit) & (!is.na(u95rom) & u95rom < upr_limit) ~ "N",
                                 (!is.na(l95rom) & l95rom < upr_limit) & (!is.na(u95rom) & u95rom > upr_limit) ~ "I",
                                 (!is.na(l95rom) & l95rom < lwr_limit) & (!is.na(u95rom) & u95rom > lwr_limit) ~ "I",
                                 (!is.na(l95rom) & l95rom < lwr_limit) & (!is.na(u95rom) & u95rom > upr_limit) ~ "I"
             )) %>%
      select(Design, SPC_GRP_INV = var, grd_vol = grd, inv_vol = inv, rom_vol = rom,
             n, l95rom_vol = l95rom, u95rom_vol = u95rom, sigrom_vol = sigrom, sigrope_vol = sigrope)
    
    
  }
  
  return(tsa30_spcvol_dat2)
  
})



table1_dat <- reactive({
  req(input$SelectVar)
  
  table1_dat <- sample_data %>%  
    filter(CLSTR_ID %in% clstr_id()) %>%
    group_by(SITE_IDENTIFIER) %>%
    arrange(VISIT_NUMBER) %>%
    mutate(visit_number_new = row_number()) 
  
  return(table1_dat)
  
})


bias_source <- reactive({
  
  lead_vol_dat <- lead_vol_dat()
  
  VDYP_grd1 <- lead_vol_dat %>%
    left_join(vdyp_grd %>% select(CLSTR_ID, PRJ_BA, PRJ_DOM_HT, PRJ_VOL_DWB),
              by = "CLSTR_ID")
  
  #*for vdyp projections using ground attributes, whenever values are missing, then assume no model bias;
  #*and all bias originates with inventory attribute classification;
  VDYP_grd1 <- VDYP_grd1 %>%
    rowwise() %>%
    mutate(grdprj_vol = ifelse(is.na(PRJ_VOL_DWB), NTWB_NVAF_LS, PRJ_VOL_DWB),
           grdprj_ht = ifelse(is.na(PRJ_DOM_HT), HT_TLSO, PRJ_DOM_HT),
           grdprj_ba = ifelse(is.na(PRJ_BA), BA_HA_LS, PRJ_BA))
  
  
  VDYP_grd2 <- VDYP_grd1 %>%
    mutate(total_bias_vol = grd_vol - inv_vol,
           model_bias_vol = grd_vol - grdprj_vol,
           attr_bias_vol = total_bias_vol - model_bias_vol,
           total_bias_ba = grd_ba - inv_ba,
           model_bias_ba = grd_ba - grdprj_ba,
           attr_bias_ba = total_bias_ba - model_bias_ba
    )
  
  VDYP_grd3 <- VDYP_grd2 %>%
    group_by(Design) %>%
    #rowwise() %>%
    summarise(ntot_vol = sum(!is.na(total_bias_vol)),
              nmod_vol = sum(!is.na(model_bias_vol)),
              natt_vol = sum(!is.na(attr_bias_vol)),
              sdtot_vol = sd(total_bias_vol, na.rm = T),
              sdmod_vol = sd(model_bias_vol, na.rm = T),
              sdattr_vol = sd(attr_bias_vol, na.rm = T),
              sdinv_vol = sd(vdyp_vol_dwb, na.rm = T),
              sdgrd_vol = sd(NTWB_NVAF_LS, na.rm = T),
              sdgrdprj_vol = sd(grdprj_vol, na.rm = T),
              grd_vol = mean(NTWB_NVAF_LS, na.rm = T),
              inv_vol = mean(vdyp_vol_dwb, na.rm = T),
              grdprj_vol = mean(grdprj_vol, na.rm = T),
              totbias_vol = mean(total_bias_vol, na.rm = T),
              modbias_vol = mean(model_bias_vol, na.rm = T),
              attrbias_vol = mean(attr_bias_vol, na.rm = T),
              
              ntot_ba = sum(!is.na(total_bias_ba)),
              nmod_ba = sum(!is.na(model_bias_ba)),
              natt_ba = sum(!is.na(attr_bias_ba)),
              sdtot_ba = sd(total_bias_ba, na.rm = T),
              sdmod_ba = sd(model_bias_ba, na.rm = T),
              sdattr_ba = sd(attr_bias_ba, na.rm = T),
              sdinv_ba = sd(vdyp_ba, na.rm = T),
              sdgrd_ba = sd(BA_HA_LS, na.rm = T),
              sdgrdprj_ba = sd(grdprj_ba, na.rm = T),
              grd_ba = mean(BA_HA_LS, na.rm = T),
              inv_ba = mean(vdyp_ba, na.rm = T),
              grdprj_ba = mean(grdprj_ba, na.rm = T),
              totbias_ba = mean(total_bias_ba, na.rm = T),
              modbias_ba = mean(model_bias_ba, na.rm = T),
              attrbias_ba = mean(attr_bias_ba, na.rm = T),
    ) %>%
    ungroup() %>%
    mutate(rom_vol = grd_vol/inv_vol,
           rommod_vol = (inv_vol + modbias_vol) / inv_vol,
           romattr_vol = (inv_vol + attrbias_vol) / inv_vol,
           rom_ba = grd_ba/inv_ba,
           rommod_ba = (inv_ba + modbias_ba) / inv_ba,
           romattr_ba = (inv_ba + attrbias_ba) / inv_ba,
    ) %>%
    data.table
  
  
  VDYP_grd4 <- VDYP_grd3 %>%
    ### by absolute value
    mutate(pctmodbias_vol = abs(modbias_vol)/(abs(modbias_vol) + abs(attrbias_vol))*100,
           pctattrbias_vol = abs(attrbias_vol)/(abs(modbias_vol) + abs(attrbias_vol))*100,
           ### by rom
           rommodbias_vol = abs(rommod_vol-1)/(abs(rommod_vol-1) + abs(romattr_vol-1))*100,
           romattrbias_vol = abs(romattr_vol-1)/(abs(rommod_vol-1) + abs(romattr_vol-1))*100,
           
           pctmodbias_ba = abs(modbias_ba)/(abs(modbias_ba) + abs(attrbias_ba))*100,
           pctattrbias_ba = abs(attrbias_ba)/(abs(modbias_ba) + abs(attrbias_ba))*100,
           ### by rom
           rommodbias_ba = abs(rommod_ba-1)/(abs(rommod_ba-1) + abs(romattr_ba-1))*100,
           romattrbias_ba = abs(romattr_ba-1)/(abs(rommod_ba-1) + abs(romattr_ba-1))*100,
    ) %>%
    mutate(dompctbias_vol = case_when(pctmodbias_vol > 60 ~ "Model",
                                      pctmodbias_vol <= 40 ~ "Attribute",
                                      pctmodbias_vol <= 60 & pctmodbias_vol > 40 ~ "Model/Attribute",
                                      TRUE ~ ""),
           domrombias_vol = case_when(rommodbias_vol > 60 ~ "Model",
                                      rommodbias_vol <= 40 ~ "Attribute",
                                      rommodbias_vol <= 60 & rommodbias_vol > 40 ~ "Model/Attribute",
                                      TRUE ~ ""),
           dompctbias_ba = case_when(pctmodbias_ba > 60 ~ "Model",
                                     pctmodbias_ba <= 40 ~ "Attribute",
                                     pctmodbias_ba <= 60 & pctmodbias_ba > 40 ~ "Model/Attribute",
                                     TRUE ~ ""),
           domrombias_ba = case_when(rommodbias_ba > 60 ~ "Model",
                                     rommodbias_ba <= 40 ~ "Attribute",
                                     rommodbias_ba <= 60 & rommodbias_ba > 40 ~ "Model/Attribute",
                                     TRUE ~ "")
    )
  
  VDYP_grd5 <- VDYP_grd4 %>%
    select(Design, starts_with("totbias_"),starts_with("modbias_"),starts_with("attrbias_"),
           starts_with("rom_"),starts_with("rommod_"),starts_with("romattr_"),
           starts_with("pctmodbias_"),starts_with("pctattrbias_"),
           starts_with("rommodbias_"),starts_with("romattrbias_")) %>%
    #select(-N_AG_TLSO, -N_HT_TLSO) %>%
    pivot_longer(cols = totbias_vol:romattrbias_ba,
                 names_to =  c(".value", "var"),
                 names_pattern = "(.*)_(.*)") %>%
    distinct() %>% data.table
  
  VDYP_grd6 <- VDYP_grd5 %>%
    select(Design:attrbias) %>%
    pivot_longer(cols = ends_with("bias"),
                 names_to = "comp",
                 names_prefix = "bias",
                 values_to = "bias")
  
  return(VDYP_grd4)
  
})



clstr_id_grid <- reactive({
  
  req(input$SelectVar)
  
  clstr_id_grid <- sample_data %>% 
    filter(TSA_DESC %in% input$SelectVar, Design == "GRID", LAST_MSMT_new == "Y") %>%
    pull(CLSTR_ID)
  
  return(clstr_id_grid)
  
})


clstr_id_last2 <- reactive({
  
  req(input$SelectVar)
  
  clstr_id_last2 <- sample_data %>% 
    filter(TSA_DESC %in% input$SelectVar, Design == "GRID") %>%
    group_by(SITE_IDENTIFIER) %>%
    filter(n() > 1) %>% 
    arrange(VISIT_NUMBER) %>% 
    slice_tail(n = 2) %>%
    pull(CLSTR_ID)
  
  return(clstr_id_last2)
  
})



remeas_plot <- reactive({
  
  req(input$SelectVar)
  
  remeas_plot <- sample_data %>% 
    filter(CLSTR_ID %in% clstr_id_last2()) %>%
    group_by(SITE_IDENTIFIER) %>%
    arrange(VISIT_NUMBER) %>%
    mutate(meas_no = row_number()) %>%
    filter(meas_no == max(meas_no), meas_no != 1) %>%
    ungroup() %>%
    pull(CLSTR_ID)
  
  return(remeas_plot)
  
})




fig10_dat <- reactive({
  
  fig8_dat <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id_last2(), DAM_NUM==1) 
  
  # *prep file to compare incidence over time for subset of remeasured samples;
  # *get last two measurements for analysis;
  FH_dat_coc <- fig8_dat %>%
    # *only interested in remeasured samples, and only the last two measurements to compare change;
    filter(CLSTR_ID %in% clstr_id_last2()) %>%
    mutate(n_si = n_distinct(substr(clstr_id_last2(), 1, 7))) %>%
    group_by(SITE_IDENTIFIER) %>%
    # *rename first_last FP to P for reporting purposes, as this change analysis is based on the last two visits;
    mutate(new_visit_number = ifelse(VISIT_NUMBER == min(VISIT_NUMBER), 'First', 'Last')) %>%
    ungroup()
  
  # *when previous visit is also the first visit, no components of change.  
  # *to make damage agent change comparison between the last two visits;
  # *need to fill in components of change for first visit to "E" as a default (establishment);
  FH_dat_coc <- FH_dat_coc %>%
    rowwise() %>%
    mutate(#COMP_CHG_new = COMP_CHG,
      COMP_CHG_new = comp_chg_coc,
      COMP_CHG_new = ifelse(new_visit_number == 'First' & lvd_coc == "D", "D", COMP_CHG_new),
      COMP_CHG_new = ifelse(new_visit_number == 'First' & lvd_coc == "L", "E", COMP_CHG_new)) %>%
    data.table
  
  FH_dat_coc1 <- FH_dat_coc %>%
    #mutate(n = n_distinct(SITE_IDENTIFIER)) %>%
    filter(!is.na(BA_TREE), !is.na(phf_coc)) %>%
    ungroup()
  
  # *compute incidence by visit and by live dead, then merge and average all samples per mu;
  # *compute totals for common denominator between measurements;
  FH_dat_coc2 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n_si, SITE_IDENTIFIER, new_visit_number, COMP_CHG_new) %>%
    summarize(tot_ba_comp = sum(phf_coc*BA_TREE, na.rm = T),
              tot_stems_comp = sum(phf_coc, na.rm = T),
              n_tree = n())  %>%
    ungroup() %>%
    data.table()
  
  # *need to fill in missing records of each forest health pest by sample id , with zeros;
  if (nrow(FH_dat_coc2) > 1){
    FH_dat_coc2_1 <- FH_dat_coc2 %>%
      dcast(n_si + SITE_IDENTIFIER + new_visit_number ~ COMP_CHG_new,
            value.var = "tot_stems_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(totsph_comdem = E + M + S)
  } else {
    FH_dat_coc2_1 <- data.frame()
  }
  
  # *recompute incidence based on common demoninator between measurements;
  # *summarize totals by damage agent for each coc;
  FH_dat_coc3 <- FH_dat_coc1 %>%
    filter(DAM_NUM == 1)  %>%
    group_by(n_si, SITE_IDENTIFIER, new_visit_number, AGN, COMP_CHG_new) %>%
    summarize(tot_ba_dam_comp = sum(phf_coc*BA_TREE, na.rm = T),
              tot_stems_dam_comp = sum(phf_coc, na.rm = T),
              n_tree = n())  %>%
    ungroup() %>%
    data.table()
  
  # *sum the totals so to compare live standing at first measure, to those same trees at second measure, wheher still alive;
  # *of if they died during that period;
  if (nrow(FH_dat_coc3) > 1){
    FH_dat_coc3_1 <- FH_dat_coc3 %>%
      # *creates full join of all fh damage agents per sample;
      dcast(n_si + SITE_IDENTIFIER + new_visit_number + AGN ~ COMP_CHG_new,
            value.var = "tot_stems_dam_comp", drop=FALSE, fill=0, sep = "_") %>%
      mutate(damsph_comdem = E + M + S)
    
    FH_dat_coc4 <- FH_dat_coc3_1 %>%
      left_join(FH_dat_coc2_1, 
                by = c("n_si", "SITE_IDENTIFIER", "new_visit_number"),
                suffix = c(".dam", ".comp"))
    
    FH_dat_coc5 <- FH_dat_coc4 %>%
      ungroup() %>%
      group_by(n_si, new_visit_number, AGN) %>%
      reframe(across(where(is.double), sum)) %>%
      ungroup() %>%
      mutate(across(where(is.double), function(x) x/n_si))
    
    FH_dat_coc5 <- FH_dat_coc5 %>%
      ungroup() %>%
      mutate(incid_stems = damsph_comdem/totsph_comdem,
             perc_mort = M.dam/damsph_comdem,
             prob_get_and_die = incid_stems*perc_mort)
    
    fig10_dat_final <- FH_dat_coc5 %>%
      mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
             dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
                                   dam_1letter == 'U' ~ 'Unknown',
                                   dam_1letter == 'N' ~ 'Abiotic',
                                   dam_1letter == 'D' ~ 'Disease',
                                   dam_1letter == 'I' ~ 'Insect',
                                   dam_1letter == 'T' ~ 'Trt',
                                   dam_1letter == 'A' ~ 'Animal',
                                   dam_1letter == 'X' ~ 'Frk_Crk_Btp',
                                   TRUE ~ '')) %>%
      mutate(dam_class = fct_reorder(dam_class, -incid_stems)) 
  } else {
    fig10_dat_final <- data.frame()
  }
  
  return(fig10_dat_final)
  
})

