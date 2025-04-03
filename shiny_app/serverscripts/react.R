
title <- reactive({
  req(input$SelectVar)
  title <- as.character(input$SelectVar)
  return(title)
})


clstr_id <- reactive({
  
  req(input$SelectVar)
  
  clstr_id <- sample_data %>% 
    filter(TSA_DESC %in% input$SelectVar) %>%
    pull(CLSTR_ID)
  
  return(clstr_id)
  
})


top3spc <- reactive({
  req(input$SelectVar)

top3spc <- lead_vol %>%  
  filter(CLSTR_ID %in% clstr_id()) %>%
  group_by(SPC_GRP2) %>%
  count() %>% 
  ungroup() %>% 
  top_n(3) %>%
  pull(SPC_GRP2)

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


invspc_vol_dat <- reactive({
  req(input$SelectVar)
  
  top3spc <- top3spc()
  
  invspc_vol <- lead_vol %>%
    filter(CLSTR_ID %in%  clstr_id()) %>% 
    rowwise() %>%
    mutate(SPC_GRP_GRD = ifelse(SPC_GRP1 %in% top3spc, SPC_GRP1, "OTH"),
           SPC_GRP_INV = ifelse(SPC_GRP2 %in% top3spc, SPC_GRP2, "OTH")) %>%
    select(SITE_IDENTIFIER, CLSTR_ID, Design, SPC_GRP1, SPC_GRP_GRD, SPC_GRP2, 
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

