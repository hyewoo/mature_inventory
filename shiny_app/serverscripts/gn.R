
disclaimer <- reactive({
  disclaimer <- paste0(
    "<p>Ground sample measurement dates (and compilations) range between 2004 
    and 2023.</p>","
<p>In contrast the spatial inventory attributes (from the 2023 published 
Forest Vegetation Composite Rank 1 Layer and projected to the end of 2023), 
are adjusted back to the ground sample measurement year using VDYP7 Batch / 
Console processing.</p>
<p>Ground data include all available design-based CMI, VRI and supplemental 
ground samples at their latest measurement dates.</p>
<p>Each ground sample's GPS coordinate location is intersected with the 
spatial inventory to obtain paired inventory polygon attributes.</p>
<p>Ground total age and height are from suitable site trees in 0.01ha 
subplots of each sample, while compiled ground sample net merchantable 
volume is NVAF-adjusted.</p>
<p>All reported basal area and net merchantable volumes are at 12.5cm close 
utilization for the Pine component and 17.5cm for all other species (conifer 
and deciduous are included).</p>
<p>Only those ground samples in the inventory-based mature population are 
used (defined as the Crown vegetated treed landbase where the projected 
inventory age adjusted to ground sample measurement year is >50 years old).</p>
<p>Two available sampling designs include: 1) GRID : these are either CMI 
monitoring plots on the NFI 20km*20km grid, or an intensificiation of the 
NFI grid using CMI and/or VRI ground supplemental samples, and 2) PHASE 2 : 
these are from past designed-based projects using PPSWR selection methods 
from a known population.</p>
<p>Components of Bias are computed by re-running VDYP7 projections after 
replacing the inventory input attributes age, height, species composition, 
basal area and stems/ha with ground based measurements.</p>
<p>Ratio of means tests are computed for age, height, basal area and volume, 
with 95% confidence limits around the computed ratio.</p>
<p>For those strata with a least 8 observations, a region of practical 
equivalence (ROPE) is pre-determined at 0.9-1.1 to assess if there is a 
practical difference in age, height and volume, or if results are 
inconclusive. A practical difference (Y) occurs when the ratio-of-means 
(ROM) confidence interval (CI) sits entirely outside the ROPE, no practical 
difference (N) occurs when the ROM CI sites entirely within the ROPE, and 
all other situations are deemed inconclusive (I).</p>
<p>Weighted analysis (using the weighted bootstrap method) was applied where 
possible to those management units with different sub-unit sampling 
intensities, where estimation followed design-based principles that applied 
differing sampling weights to the plots.</p>
<p>All ground samples impacted by the 2017, 2018, and 2021 wildfires were 
revisited post-fire, with live basal area and net merchantable volumes 
recomputed.</p>
<p>Only project designs with at least 8 ground sample observations are 
included in these reports. TSA and TFL management units are assessed 
separately.</p>"
  )
  
  return(disclaimer)
})


output$disclaimer <- renderUI({
  HTML(disclaimer())
})



gn <- reactive({
  gn <- paste0(
    '<p>This app was last updated on 15 July, 2025.</p>
<br>
<p><strong>Overview</strong></p>
<p>* The Forest Analysis and Inventory Branch (FAIB) website contains a&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories" target="_blank">General Overview of FAIB Ground Sample Inventories.</a></p>
<p>* There are several layers of ground sample data quality assurance including&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/training-and-qualified-contractors" target="_blank">Certified Crews</a>, detailed&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring/standards" target="_blank">Ground Sampling Procedures</a>, electronic data loggers with hundreds of validation rules to reduce errors and omissions, detailed ground sampling quality assurance procedures (including a 10% random field audit), and a scripted ground sample&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring/standards" target="_blank">Data Compilation Process</a>.</p>
<p>* The BC Data Catalogue contains several inputs to this reporting including the&nbsp;<a href="https://catalogue.data.gov.bc.ca/dataset/824e684b-4114-4a05-a490-aa56332b57f4" target="_blank">Ground Sample Data Download</a>&nbsp;outputs from the&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/ground-sample-inventories/provincial-monitoring/standards" target="_blank">Data Compilation Process</a>.</p>
<p>* To match each ground samples with a TSR Predicted Yield Table, a spatial overlay of the ground sample GPS locations and&nbsp;<a href="https://catalogue.data.gov.bc.ca/dataset/6ba30649-14cd-44ad-a11f-794feed39f40" target="_blank">VRI</a>&nbsp;was completed.</p>
<p>* Weighted analysis (using the weighted bootstrap method) was applied to the PHASE2 where possible to those management units with different sub-unit sampling intensities. More information on the TSA specific PHASE2 sample designs and their sampling weights can be found in the Past assessment / Reference section.</p>
<p>* All reported net merchantable volumes are at 12.5cm close utilization for PL, 17.5cm for other species, and include all species (deciduous + conifer).</p>
<br><p><strong>Ground vs. inventory</strong></p>

<p>* Stand table &amp; species composition histograms are computed as pooled (weighted) averages of all mature samples combined for a given reporting strata (TSA, BEC Zone, etc.).</p>
<p>* Ground total age and height are from suitable site trees in 0.01ha subplots of each sample, while compiled ground sample net merchantable volume is NVAF-adjusted.</p>
<p>* Ratio of means tests are computed for age, height, basal area and volume, with 95% confidence limits around the computed ratio.</p>
<br>
<p><strong>Comparison with TSR</strong></p>
<p>* TSR TIPSY Opening Specific and TSR TIPSY Aggregate yield table estimates are based on&nbsp;<a href="https://bcgov.github.io/FAIB_MSYT_overview/" target="_blank">FAIB MSYTs</a>&nbsp;ver. 4. Where&nbsp;<a href="https://bcgov.github.io/FAIB_MSYT_overview/" target="_blank">FAIB MSYTs</a>&nbsp;are not available for a sample location, TSR VDYP existing natural stand yield table estimates are generated by running&nbsp;<a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/growth-and-yield-modelling/variable-density-yield-projection-vdyp" target="_blank">VDYP7</a>&nbsp;batch console using the published VRI VDYP Input&nbsp;<a href="https://catalogue.data.gov.bc.ca/dataset/57513aaa-c0a6-41a9-b2a8-b980b1604ee6" target="_blank">poly</a>&nbsp;and&nbsp;<a href="https://catalogue.data.gov.bc.ca/dataset/5aedccb7-e4b5-447d-8e38-9a2e5474f9db" target="_blank">layer</a>&nbsp;files.</p>
<p>* Note that in strata with TIPSY yield curves, the average TSR yield curve may have an adjustment at age 120.&nbsp; This adjustment is because TIPSY yield curves are only projected to age 120 years while VDYP yield curves are projected to 250 years.&nbsp;&nbsp;</p>
<p><br /> <strong>Standards of Data Collection</strong></p>
<p>* Learn more about photo VRI standards <a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/forest-cover-inventories" target="_blank">here</a>.</p>
<br>
<p><strong>Forest Health</strong></p>
<p>* Components of change are computed from individual tree changes (ie., survivor, ingrowth, mortality) between the last two measurements only. Trees initially tagged as subplot trees (4-9 cm DBH) and subsequently as main plot trees (&gt;9 cm DBH) have their "per-hectare-factors" (PHF) fixed relative to the first measurement.</p>
<p>* Forest health incidence histograms are computed as pooled (weighted) averages, together with 95% confidence intervals computed using the ""variance of a ratio estimator". Current incidence summaries for a given damage agent are based on all possible occurrences recorded for a given tree, which can be up to 5 damage agents per tree. Therefore, current incidence of all damage agents can together add up to more than 100%. Conversely, when summarizing change in forest health incidence over time, only the primary damage agent is included; therefore, the total change in average incidence will add up to 100%.</p>
<br><p><strong>Comparison with VDYP</strong></p>
<p>* The VRI earliest_nonlogging_dist_type and harvest_date attributes are used to identify the areas affected by disturbance between measurements.&nbsp;</p>
<p>* Learn more about VDYP <a href="https://www2.gov.bc.ca/gov/content/industry/forestry/managing-our-forest-resources/forest-inventory/growth-and-yield-modelling/variable-density-yield-projection-vdyp" target="_blank">here</a>.</p>
<br>
<p><strong>Components of Bias</strong></p>
<p>* Components of bias are computed by re-running VDYP7 projections after replacing the inventory input attributes age, height, species composition, basal area and stems/ha with ground-based measurements.</p>'
  )
  
  return(gn)
})


output$gn <- renderUI({
  HTML(gn())
})


output$sp_dam_header <- renderUI({
  
  HTML(paste0("<h3>Tree Species and Damage Agents Recorded from Mature Ground Samples in ", title(),"</h3>"))
  
})


spcdtab <- reactive({
 
  sp_cd <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id_grid(), S_F == "S") %>%
    pull(SPECIES) %>%
    unique()
  
  spcd1 <- spcd %>%
    filter(species %in% sp_cd)
  
  names(spcd1) <- c("Code", "Name")
  
  return(spcd1)
})

output$sp_table <- renderDT({
  datatable(spcdtab(), rownames= FALSE)
})


damcdtab <- reactive({
  
  dam_agn <- tree_fh_data  %>%
    filter(CLSTR_ID %in% clstr_id_grid(), S_F == "S") %>%
    pull(AGN) %>%
    unique()
  
  damcd1 <- damcd %>%
    filter(dam_agna %in% dam_agn)
  
  names(damcd1) <- c("Code", "Name")
  
  return(damcd1)
})


output$dam_table <- renderDT({
  datatable(damcdtab(), rownames= FALSE)
})


damsevtable <- reactive({
  
  dam_agn <- tree_fh_data %>%
    filter(CLSTR_ID %in% clstr_id_grid(), S_F == "S") %>%
    filter(! AGN %in% c("O", "U", "I")) %>%
    pull(AGN) %>%
    unique() 
  
  damsev1 <- damsev %>%
    filter(dam_3letter %in% dam_agn) %>%
    group_by(Group, Low, Mod, High) %>%
    summarise(AGN = str_c(dam_3letter, collapse  = ", ")) %>%
    flextable() %>%
    set_header_labels(
      values = list(Group = "Group", Low = "LOW", Mod = "MOD", 
                    High = "HIGH", AGN = "Damage Agent")) %>%
    bold(part = 'header', bold = TRUE) %>%
    align(j = 2:4, align = "center", part = "all") %>%
    set_caption(as_paragraph(
      as_b(as_chunk("Severity classification by damage agent group*"))),
      fp_p = officer::fp_par(text.align = "left", padding = 3),
      align_with_table = FALSE) %>%
    autofit()
  
  return(damsev1)
  
})



output$damsevtable <- renderUI({
  
  htmltools_value(damsevtable())
  
})





output$sev_desc <- renderUI({
  req(input$SelectVar)
  HTML("<p style='font-size:12px ;face=arial'>*This is an unofficial classification of damage agent severity, developed jointly by FAIB and Forest Health staff to group various severity ratings into three broad classes.</p>")
  
})


#ref <- reactive({
#  ref <-paste0("<p><a href='https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/ground-sample-inventories/vri-audits/planning-reports/tsa-vri-ground-sampling/arrowtsa_vrigs_vpip.pdf'>
#               https://www2.gov.bc.ca/assets/gov/farming-natural-resources-and-industry/forestry/stewardship/forest-analysis-inventory/ground-sample-inventories/vri-audits/planning-reports/tsa-vri-ground-sampling/arrowtsa_vrigs_vpip.pdf</a></p>")
#  return(ref)
#})
#
#
#output$ref <- renderUI({
#  HTML(ref())
#})


output$ref <- DT::renderDataTable(DT::datatable(reference, rownames = FALSE, escape = FALSE))
