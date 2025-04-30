
disclaimer <- reactive({
  disclaimer <- paste0(
    "<p>Ground sample measurement dates (and compilations) range between 2004 
    and 2023.</p>","
<p>In contrast the spatial inventory attributes (from the 2023 published 
Forest Vegetation Composite Rank 1 Layer and projected to the end of 2021), 
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
NFI grid using CMI and/or VRI ground supplemental samples, and 2) Phase 2 : 
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
      as_b(as_chunk("Severity classification by damage agent group"))),
      fp_p = officer::fp_par(text.align = "left", padding = 3),
      align_with_table = FALSE) %>%
    autofit()
  
  return(damsev1)
  
})



output$damsevtable <- renderUI({
  
  htmltools_value(damsevtable())
  
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


output$ref <- DT::renderDataTable(DT::datatable(reference, escape = FALSE))
