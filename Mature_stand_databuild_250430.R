
library(data.table)
library(dplyr)
library(tidyverse)
library(openxlsx)
library(RODBC)
library(haven)
library(sf)
library(leaflet)
library(flextable)


decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

lwr_limit = 0.9
upr_limit = 1.1

savepath <- ""
savepath <- "D:/R/mature_inventory/files/rds/250429"


### 1) Import ISMC compiled ground sample data
### Set data import location
folderloc <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/forpublish"  # Published BC ground samples
### Pick a compilation date (ex. 20240619)
compdate <- 20240619
### Need some unpublished data

comp_path <- file.path(paste0("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/Archive_nonPSP_", 
                              compdate))
indatapath <- file.path(folderloc, paste0("nonPSP_",compdate))

### Read ISMC compiled data (published)
faib_header <-fread(paste0(indatapath, "/faib_header.csv"))
faib_sample_byvisit <-fread(paste0(indatapath, "/faib_sample_byvisit.csv"))
faib_spcsmries <-fread(paste0(indatapath, "/faib_compiled_spcsmries.csv"))
faib_smeries <-fread(paste0(indatapath, "/faib_compiled_smeries.csv"))
faib_siteage <-fread(paste0(indatapath, "/faib_compiled_spcsmries_siteage.csv"))
faib_tree <-fread(paste0(indatapath, "/faib_tree_detail.csv"))
### Read ISMC compiled data (unpublished)
vi_d<-readRDS(paste0(comp_path, "/compilation_nonPSP_db/compiled_vi_d.rds"))
### Import IMSC damage agent for severity class lookup table
lookup_sev <-readRDS(paste0(comp_path, "/compilation_nonPSP_raw/ISMC_PROD_20240619_15pm_TreeDamageOccurrences.rds"))

external_path <-  "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!WorkArea/hwoo/ForestHealth/CopiedfromRdejong/forest_health/severity_rating_lookup_table"
### Import forest health severity data
#* severity rating lookup table created by D.Rusch;
lookup_rush <- read.xlsx(paste0(external_path, 
                                "/Severity_lookup_table_2021mar15.xlsx"),
                         sheet = 'input1')

#* corrections to severity rating - unknown to correct severity ratings created by D.Rusch 2021mar10;
sev_rusch <- read.xlsx(paste0(external_path, 
                              "/Unknown_severity_2021mar09.xlsx"))


# Read VRI data - this version is based on the published 2022 vri feature_id;
#vridatpath <- file.path(paste0(external_path2,  "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/1_All_VRI_Attributes_2024Jun11.accdb"))
external_path <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"
vridatpath <- file.path(paste0(external_path, "/spatial_overlay/ISMC_VRI_Overlay/2_All_VRI_Attributes_2024Jun11.accdb"))
channel<-odbcConnectAccess2007(vridatpath)
vegcomp1<-sqlFetch(channel,"All_VRI_Attributes")

# *import crosswalk table for feature_id based on 2022 vegcomp, 
# needed to provide linkage for latest tsr msyt tables;
vegcomp2<- read.xlsx(paste0(external_path, 
                            "/spatial_overlay/ISMC_VRI_Overlay/2022VegCompR1_Overlay/1_Plot_Overlay_Out_2024Jun18.xlsx"))
### Make sure the variable names match
names(vegcomp2) <-c('SITE_IDENTIFIER', 'Alb_x', 'Alb_y', 'FEATURE_ID_2022', 'PROJ_AGE_1_2022', 'PROJECTED_DATE_2022')

# *get master grid base msaccess database created by M.Makar 2019-jan-10, to define grid designs;
# *includes nfi grid and intensification of nfi grid down to 5*5km;
grid_lookup <- read.xlsx("D:/R/mature_inventory/Data/FAIB_Sampling_Grids_Dec_03_2021.xlsx")


# *get post 2018 wildfire grid based sample survey on 5*5km grid, which overlaps with CMI , VRI samples on the 10*20km grid;
# *cant use five_5_plot_numbers, since they were redefined early 2019.  so use intended utm coordinates instead to match;
# *affected tsas include morice, lakes, prince george;
# *round utm coordinates to 100m, noticed that one extra sample location matched by rounding east coordinate up by 100. no other combinations provided match;
fire_2018 <- read.xlsx("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/!Project/Forest Inventory Section/2018 Wildfires/nadina_fire_compilation/overlay_severity_classes/Burn Points to Sample_Original severity.xlsx")

# *get post 2017 and 2021 wildfire ground sample recompilations from Yong, where he computed percent mortality;
firesamples_2022 <- read.xlsx("//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_outputs/fire_samples/firesamples_2022jul21.xlsx")

### Reassign duplicated column name
names(fire_2018)[1] <- "five_k_plot_number"

fire_2018 <- fire_2018 %>%
  mutate(Loss.Percent = sub("N/A", "", Loss.Percent),
         Loss.Percent = as.numeric(Loss.Percent)) %>%
  filter(!is.na(UTM)) %>%
  mutate(x = round(Easting, -2),
         y = round(Northing, -2))


### Ground projected yield
VDYP_grd <- fread(paste0(external_path, 
                         "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/compiled_ISMC_inputs/VDYP7_OUTPUT_YLDTBL.csv"))

VDYP_grd <- VDYP_grd %>%
  mutate(CLSTR_ID = paste0(POLYGON_ID, "-", MAP_ID)) 
  


### Create list of mature sample data
sample_data1 <- faib_sample_byvisit %>% 
  left_join(faib_header, by = c("SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE")) %>%
  mutate(BEClabel = paste0(BEC_ZONE, BEC_SBZ, 
                           ifelse(!is.na(BEC_VAR), BEC_VAR, '')),
         BECsub = paste0(BEC_ZONE, BEC_SBZ),
         proj_id = sub("_.*", "", SAMPLE_SITE_NAME),
         OWN_SCHED = paste0(OWNER,"-",SCHEDULE),
         tsa_tfl = substr(MGMT_UNIT, 1, 3)) %>%
  ### Join with VRI data 
  left_join(vegcomp1,
            by = c("SITE_IDENTIFIER"), suffix = c("", "_vegcomp")) %>%
  mutate(bclcs = paste0(BCLCS_LEVEL_1, BCLCS_LEVEL_2)) %>%
  mutate(sample_change_case = case_when(
    SAMPLE_ESTABLISHMENT_TYPE == "VRI" & VISIT_TYPE == "REP" ~ 'rep_in_vri',
    SAMPLE_ESTABLISHMENT_TYPE == "CMI" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'eysm_in_cmi',
    SAMPLE_ESTABLISHMENT_TYPE == "YSM" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'eysm_in_ysm',
    SAMPLE_ESTABLISHMENT_TYPE == "SUP" & VISIT_TYPE == "REP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "A" ~ 'ysm_in_sup',
    SAMPLE_ESTABLISHMENT_TYPE == "SUP" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "Y" ~ 'eysm_in_sup',
    SAMPLE_ESTABLISHMENT_TYPE == "CNS" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE %in% c("D", "O") ~ 'tmp_in_cns',
    SAMPLE_ESTABLISHMENT_TYPE == "YNS" & VISIT_TYPE == "TMP" & SAMPLE_SITE_PURPOSE_TYPE_CODE == "L" ~ 'tmp_in_yns',
    MGMT_UNIT == "TFL60_TaanForest" & SAMPLE_ESTABLISHMENT_TYPE == "CNS" & VISIT_TYPE == "REP" ~ 'pre_post_trt',
    # *in merritt tsa, YSM and CMI/NFI samples are on different grids, 
    # so only one sample type can be retained.  for ysm analysis drop cmi,nfi;
    # *for mature assessment analysis, drop ysm;
    MGMT_UNIT == "TSA18_Merritt" & SAMPLE_ESTABLISHMENT_TYPE == "YSM" ~ 'popn_conflict',
    YSM_MAIN_LM == "Y" & PROJ_AGE_ADJ < 15 ~ 'ysm_too_young',
    TRUE ~"")) %>%
  # *subset population of interest;
  mutate(drop_reason = case_when(
    !(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "VRI", "SUP")) ~ 'not_mature_sample_type',
    MAT_MAIN_LM != "Y" ~ 'not_in_mat_main_lm',
    # *drop parks / conservancy areas / private / IR ;
    OWN_SCHED %in% c('63-N','50-N','51-N','53-N','54-N',
                     '67-N','64-N','60-N','40-N','41-N',
                     '52-N','72-A','77-A','79-A','80-N',
                     '99-N','81-U') ~ 'in_park_consrv_private_ir',
    # *only interested in vegetated treed land classification (note, 
    # if burned polygon since reclassified as VN, then burn impact ignored);
    bclcs != "VT" ~ 'not_VT',
    # *tsa only, no tfl coverage included;
    tsa_tfl == "TFL" ~ 'in_TFL',
    TRUE ~ ""))


sample_data2 <- sample_data1 %>%
  filter(sample_change_case == "") %>%
  filter(drop_reason == '') %>%
  mutate(design = ifelse(SAMPLE_ESTABLISHMENT_TYPE %in% c("CMI", "NFI", "SUP"), "GRID", "PHASE2"), 
         design_icon = ifelse(design == "GRID", 1, 2))

sample_data2 <- sample_data2 %>%
  left_join(vegcomp2 %>% select(SITE_IDENTIFIER, FEATURE_ID_2022, PROJ_AGE_1_2022),
            by = c("SITE_IDENTIFIER"), suffix = c("", "_vegcomp"))


# *get sample data set, use location checks with intended coordinates based on what is recorded for 
# last visit number record;
sample_data3_1 <- sample_data2 %>%
  # *match merge by site_identifier;
  left_join(grid_lookup, by = c("SITE_IDENTIFIER" = "Sample_Number")) %>%
  # *for those matching to the master grid, compute absolute distance;
  mutate(coord_within = sqrt((BC_ALBERS_X - BC_Albers_X)^2 + (BC_ALBERS_Y - BC_Albers_Y)^2),
         orphan = ifelse(is.na(OBJECTID) | coord_within > 400, 1, 0))


sample_data3_2 <- sample_data3_1 %>%
  filter(orphan == 1) %>%
  select(-c(OBJECTID:orphan)) %>%
  # *attempt alternate match to master grid, not by site_identifier, but by x y;
  mutate(BC_ALBERS_X_grd = round(BC_ALBERS_X, -1),
         BC_ALBERS_Y_grd = round(BC_ALBERS_Y, -1)) %>%
  left_join(grid_lookup %>% mutate(BC_ALBERS_X_grid = round(BC_Albers_X, -1),
                                   BC_ALBERS_Y_grid = round(BC_Albers_Y, -1)),
            by = c("BC_ALBERS_X_grd" = "BC_ALBERS_X_grid", "BC_ALBERS_Y_grd" = "BC_ALBERS_Y_grid")) %>%
  mutate(coord_within = 10,
         orphan = ifelse(is.na(OBJECTID), 1, 0))


sample_data3_3 <- sample_data3_2 %>%
  filter(orphan == 1) %>%
  select(-c(OBJECTID:orphan)) %>%
  # *2nd attempt alternate match to master grid, not by site_identifier, but by x y;
  mutate(BC_ALBERS_X_grd = ceiling(BC_ALBERS_X / 25) * 25,
         BC_ALBERS_Y_grd = ceiling(BC_ALBERS_Y / 25) * 25) %>%
  left_join(grid_lookup %>% mutate(BC_ALBERS_X_grid = ceiling(BC_Albers_X / 25) * 25,
                                   BC_ALBERS_Y_grid = ceiling(BC_Albers_Y / 25) * 25),
            by = c("BC_ALBERS_X_grd" = "BC_ALBERS_X_grid", "BC_ALBERS_Y_grd" = "BC_ALBERS_Y_grid")) %>%
  mutate(coord_within = 25,
         orphan = ifelse(is.na(OBJECTID), 1, 0))


sample_data3_4 <- sample_data3_3 %>%
  filter(orphan == 1) %>%
  select(-c(OBJECTID:orphan)) %>%
  # *3rd attempt alternate match to master grid, not by site_identifier, but by x y;
  mutate(BC_ALBERS_X_grd = round(BC_ALBERS_X, -3),
         BC_ALBERS_Y_grd = round(BC_ALBERS_Y, -3)) %>%
  left_join(grid_lookup %>% mutate(BC_ALBERS_X_grid = round(BC_Albers_X, -3),
                                   BC_ALBERS_Y_grid = round(BC_Albers_Y, -3)),
            by = c("BC_ALBERS_X_grd" = "BC_ALBERS_X_grid", "BC_ALBERS_Y_grd" = "BC_ALBERS_Y_grid")) %>%
  mutate(coord_within = 1000,
         orphan = ifelse(is.na(OBJECTID), 1, 0))


sample_data3_5 <- bind_rows(sample_data3_1 %>% filter(orphan == 0 | design == "PHASE2"),
                            sample_data3_2 %>% filter(orphan == 0, design == "GRID"),
                            sample_data3_3 %>% filter(orphan == 0, design == "GRID"),
                            sample_data3_4 %>% filter(orphan == 0, design == "GRID"))


# *generalize all on 20km grid as cmi, and drop those NFI areas where no other grid sampling has yet taken place;
# *one nfi sample in PG that is outside popn of interest in TSA (north tip of TSA);
# *other tsas with 40km*40km grid sampling, only keep those nfi samples on same 40km grid;
#sample_data4 <- sample_data3 %>%
sample_data4 <- sample_data3_5 %>%
  filter(!(SAMPLE_ESTABLISHMENT_TYPE == "NFI" & TSA %in% c(4,8,43) & Forty_By_Forty == "N")) %>%
  # *nfi PG outside pop of interest, nfi Mackenzie outside northern range of mature population in the mackenzie tsa;
  filter(!(SITE_IDENTIFIER %in% c(1211056, 1190456, 1197326))) %>%
  # *for williams lake and quesnel, analyze grid samples just for west half, ignore east grid samples;
  # *the east half is analyzed using the phase 2 data;
  # *this specific to quesnel using an east/west divide for splitting cmi samples;
  mutate(SAMPLE_ESTABLISHMENT_TYPE = ifelse(BC_ALBERS_X > 1226986 & 
                                              BC_ALBERS_Y > 828387 & 
                                              SAMPLE_ESTABLISHMENT_TYPE == "CMI" &
                                              TSA == 26, 
                                            "CMI-E", SAMPLE_ESTABLISHMENT_TYPE)) %>%
  filter(SAMPLE_ESTABLISHMENT_TYPE != "CMI-E") %>%
  mutate(SAMPLE_ESTABLISHMENT_TYPE = ifelse(SAMPLE_ESTABLISHMENT_TYPE == "NFI", "CMI", SAMPLE_ESTABLISHMENT_TYPE)) %>%
  ## *define design.  for specific tsas, intensified SUPplemental samples on a grid are combined with cmi;
  mutate(Design = case_when(TSA %in% c(26, 20) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI','SUP') ~ "GRID",
                            TSA %in% c(26, 20) & SAMPLE_ESTABLISHMENT_TYPE %in% c('VRI') ~ "PHASE2",
                            TSA %in% c(14, 16, 23) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI','SUP') ~ "GRID",
                            TSA %in% c(14, 16, 23) & SAMPLE_ESTABLISHMENT_TYPE %in% c('VRI') ~ "PHASE2",
                            TSA %in% c(4, 8, 43) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI') ~ "GRID",
                            TSA %in% c(4, 8, 43) & SAMPLE_ESTABLISHMENT_TYPE %in% c('VRI') ~ "PHASE2",
                            SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI') ~ "GRID",
                            SAMPLE_ESTABLISHMENT_TYPE %in% c('VRI') ~ "PHASE2",
                            TRUE ~ ""),
         grid_size = case_when(TSA %in% c(26, 20) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI','SUP') ~ "10km*10km",
                               TSA %in% c(14, 16, 23) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI','SUP') ~ "10km*20km",
                               TSA %in% c(4, 8, 43) & SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI') ~ "40km*40km",
                               SAMPLE_ESTABLISHMENT_TYPE %in% c('CMI') ~ "20km*20km",
                               TRUE ~ "")) %>%
  # *focus on just the latest PHASE2 VRI ground sample plan per tsa, to compare against;
  # *all other vri projects were based on an earlier project in a given tsa, therefore not relevant, 
  # ie interest in the most recent vri project;
  mutate(vri_in = case_when(Design == "PHASE2" & TSA == 1 & MEAS_YR %in% c(2004,2005) ~ "Y",
                            Design == "PHASE2" & TSA == 3 & MEAS_YR %in% c(2008,2009) ~ "Y",
                            Design == "PHASE2" & TSA == 4 ~ "Y",
                            Design == "PHASE2" & TSA == 7 & MEAS_YR %in% c(2003) ~ "Y",
                            Design == "PHASE2" & TSA == 8 & MEAS_YR %in% c(2002) ~ "Y",
                            Design == "PHASE2" & TSA == 9 & MEAS_YR %in% c(2007,2008) ~ "Y",
                            Design == "PHASE2" & TSA == 10 & MEAS_YR %in% c(2004,2008,2009) ~ "Y",
                            Design == "PHASE2" & TSA == 11 & MEAS_YR %in% c(2013,2014) ~ "Y",
                            Design == "PHASE2" & TSA == 13 & MEAS_YR %in% c(2011,2012) ~ "Y",
                            Design == "PHASE2" & TSA == 14 & MEAS_YR %in% c(2012) ~ "Y",
                            Design == "PHASE2" & TSA == 15 & MEAS_YR %in% c(1998,1999,2000,2001) ~ "Y",
                            Design == "PHASE2" & TSA == 16 & MEAS_YR %in% c(2011) ~ "Y",
                            Design == "PHASE2" & TSA == 17 & MEAS_YR %in% c(2008,2009,2010) ~ "Y",
                            Design == "PHASE2" & TSA == 18 & MEAS_YR %in% c(2012,2013) ~ "Y",
                            Design == "PHASE2" & TSA == 20 & MEAS_YR %in% c(2011,2012) ~ "Y",
                            Design == "PHASE2" & TSA == 22 & MEAS_YR %in% c(2006,2007,2008) & 
                              substr(MGMT_UNIT, 1, 5) == "TSA22" & !(substr(SAMPLE_SITE_NAME, 1, 4) %in% c('4721','022M')) ~ "Y",
                            Design == "PHASE2" & TSA == 22 & MEAS_YR %in% c(1997,1998,1999,2000,2006,2007,2008) &
                              substr(MGMT_UNIT, 1, 5) == "TFL49" & substr(SAMPLE_SITE_NAME, 1, 4) == '4721'~ "Y",
                            Design == "PHASE2" & TSA == 24 & MEAS_YR %in% c(2013,2014) ~ "Y",
                            Design == "PHASE2" & TSA == 25 & MEAS_YR %in% c(2015,2016) ~ "Y",
                            Design == "PHASE2" & TSA == 26 & MEAS_YR %in% c(2010) ~ "Y",
                            Design == "PHASE2" & TSA == 27 & MEAS_YR %in% c(2008, 2009, 2010) &
                              substr(MGMT_UNIT, 1, 5) == "TSA27" ~ "Y",
                            Design == "PHASE2" & TSA == 27 & MEAS_YR %in% c(2002) &
                              substr(MGMT_UNIT, 1, 5) == "TFL55" ~ "Y",
                            Design == "PHASE2" & TSA == 29 & MEAS_YR %in% c(2012,2013) ~ "Y",
                            # *note this phase 2 sampled both ifpa and non ifpa at differetn intensities;
                            # *analysis for this data set address this as a weighted analysis by strata;
                            Design == "PHASE2" & TSA == 30 & MEAS_YR %in% c(1999,2000) &
                              substr(SAMPLE_SITE_NAME, 1, 4) %in% c("0301","0302") ~ "Y",
                            Design == "PHASE2" & TSA == 38 & MEAS_YR %in% c(1998,2006,2007) ~ "Y",
                            Design == "PHASE2" & TSA == 39 & MEAS_YR %in% c(1997,1998) ~ "Y",
                            Design == "PHASE2" & TSA == 40 & MEAS_YR %in% c(2008,2009) ~ "Y",
                            Design == "PHASE2" & TSA == 41 & MEAS_YR %in% c(2000,2001) ~ "Y",
                            Design == "PHASE2" & TSA == 47 & MEAS_YR %in% c(2006,2007) ~ "Y",
                            # *include both strathcona and kincome vri phase 2 projects, and separate strata;
                            Design == "PHASE2" & TSA == 48 & MEAS_YR %in% c(2003,2006,2007) &
                              substr(SAMPLE_SITE_NAME, 1, 4) %in% c('0331','0332','0371') ~ "Y",
                            TRUE ~ ""
  )) %>%
  filter(Design == "GRID" | (Design == "PHASE2" & vri_in == "Y")) #%>%
  #filter(LAST_MSMT == "Y")


sample_data4_1 <- sample_data4 %>%
  ### Define a now visit number as VISIT_NUMBER may not be consecutive
  group_by(SITE_IDENTIFIER) %>%
  arrange(VISIT_NUMBER) %>%
  mutate(visit_number_new = row_number(),
         LAST_MSMT_new = ifelse(visit_number_new == max(visit_number_new), "Y", LAST_MSMT)) %>%
  ungroup()

sample_data4_2 <- sample_data4_1 %>%
  filter(Design == "GRID" | (Design == "PHASE2" & LAST_MSMT_new == "Y"))

n_by_mgmt <- sample_data4_2 %>%
  filter(LAST_MSMT_new == "Y") %>%
  group_by(MGMT_UNIT, Design) %>%
  #mutate(n = n()) %>%
  filter(n() >= 8) %>%
  pull(SITE_IDENTIFIER)

sample_data4_3 <- sample_data4_2 %>%
  filter(SITE_IDENTIFIER %in% n_by_mgmt)



sample_data5 <- sample_data4_3 %>%
  mutate(x = round(IP_EAST, -2),
         y = round(IP_NRTH, -2)) 


fire_2018 <- fire_2018 %>%
  left_join(sample_data5 %>% select(CLSTR_ID, x, y, IP_UTM), by = c("x", "y", "UTM" = "IP_UTM")) %>%
  filter(!is.na(CLSTR_ID)) %>%
  # *approximate air call percent burn as a volume loss factor, to be consistent with the 2017 fire calls;
  # *other attributes assume same % mortality estimate;
  mutate(ntwb_mortality = sub("N/A", "", Loss.Percent),
         ntwb_mortality = as.numeric(Loss.Percent),
         stem_mortality = ntwb_mortality,
         ba_mortality = ntwb_mortality,
         wsv_mortality = ntwb_mortality,
         fire_year = 2018) %>%
  select(CLSTR_ID, fire_year, stem_mortality, ba_mortality, wsv_mortality, 
         ntwb_mortality)

fire_2022 <- firesamples_2022 %>%
  # *approximate air call percent burn as a volume loss factor, to be consistent with the 2017 fire calls;
  # *other attributes assume same % mortality estimate;
  select(CLSTR_ID, fire_year = Fire.year, stem_mortality = Stem.mortality,
         ba_mortality = BA.mortality, wsv_mortality = WSV.mortality, 
         ntwb_mortality = NTWB.mortality)

# *append 2017 and 2018 fire impacted samples together; 
fire_sample <- rbind(fire_2018, fire_2022)

# *keep only results for latest fire in a given site_identifier;
fire_sample1 <- fire_sample %>%
  mutate(SITE_IDENTIFIER = as.numeric(substr(CLSTR_ID, 1, 7))) %>%
  group_by(SITE_IDENTIFIER) %>%
  arrange(desc(fire_year)) %>%
  slice(1) %>%
  ungroup()

fire_sample1 <- fire_sample1 %>%
  replace(is.na(.), 0) 


sample_data6 <- sample_data5 %>%
  left_join(fire_sample1, by = c("SITE_IDENTIFIER", "CLSTR_ID")) %>%
  mutate(MEAS_YR_new = ifelse(!is.na(fire_year) & fire_year >= MEAS_YR, fire_year, MEAS_YR))


sample_data7 <- sample_data6 %>%
  select(MGMT_UNIT, SITE_IDENTIFIER, VISIT_NUMBER, visit_number_new, CLSTR_ID, MEAS_YR, MEAS_YR_new,
         LAST_MSMT, LAST_MSMT_new, PERIOD,
         SAMPLE_ESTABLISHMENT_TYPE, SAMPLE_SITE_NAME, TSA, TSA_DESC, 
         Design, design_icon, GRID_SIZE, grid_size, Latitude, Longitude,
         IP_UTM, IP_NRTH, IP_EAST, BC_ALBERS_X, BC_ALBERS_Y, BC_Albers_X, BC_Albers_Y, 
         UTM_Zone, UTM_Northing, UTM_Easting, OWNER, SCHEDULE, OWN_SCHED, OWN_SCHED_DESCRIP,
         FEATURE_ID, FEATURE_ID_vegcomp, FEATURE_ID_2022, #FEATURE_ID_2022_vegcomp,
         MAP_ID, OPENING_NUMBER, OPENING_ID, LAYER_ID, INVENTORY_STANDARD_CD,
         BEC_ZONE, BEC_SBZ, BEC_VAR, BEClabel, BECsub,
         PROJECTED_DATE_vegcomp, PROJECTED_DATE, INTERPRETATION_DATE, REFERENCE_YEAR, 
         LINE_7B_DISTURBANCE_HISTORY, EARLIEST_NONLOGGING_DIST_TYPE, EARLIEST_NONLOGGING_DIST_DATE,
         HARVEST_DATE, BCLCS_LEVEL_1:BCLCS_LEVEL_5,
         SPECIES_CD_1:SPECIES_PCT_6, 
         PROJ_AGE_1, PROJ_AGE_1_2022, PROJ_AGE_1_vegcomp, #PROJ_AGE_1_2022_vegcomp, 
         PROJ_AGE_2, PROJ_AGE_ADJ,
         PROJ_HEIGHT_1, PROJ_HEIGHT_2, SITE_INDEX, CROWN_CLOSURE, BASAL_AREA,
         VRI_LIVE_STEMS_PER_HA, VRI_DEAD_STEMS_PER_HA, LIVE_STAND_VOLUME_125, LIVE_STAND_VOLUME_175,
         LIVE_VOL_PER_HA_SPP1_125, LIVE_VOL_PER_HA_SPP1_175,
         LIVE_VOL_PER_HA_SPP2_125, LIVE_VOL_PER_HA_SPP2_175,
         LIVE_VOL_PER_HA_SPP3_125, LIVE_VOL_PER_HA_SPP3_175,
         LIVE_VOL_PER_HA_SPP4_125, LIVE_VOL_PER_HA_SPP4_175,
         LIVE_VOL_PER_HA_SPP5_125, LIVE_VOL_PER_HA_SPP5_175,
         LIVE_VOL_PER_HA_SPP6_125, LIVE_VOL_PER_HA_SPP6_175,
         DEAD_VOL_PER_HA_SPP1_125, DEAD_VOL_PER_HA_SPP1_175,
         DEAD_VOL_PER_HA_SPP2_125, DEAD_VOL_PER_HA_SPP2_175,
         DEAD_VOL_PER_HA_SPP3_125, DEAD_VOL_PER_HA_SPP3_175,
         DEAD_VOL_PER_HA_SPP4_125, DEAD_VOL_PER_HA_SPP4_175,
         DEAD_VOL_PER_HA_SPP5_125, DEAD_VOL_PER_HA_SPP5_175,
         DEAD_VOL_PER_HA_SPP6_125, DEAD_VOL_PER_HA_SPP6_175,
         DEAD_STAND_VOLUME_125, DEAD_STAND_VOLUME_175,
         Five_By_Five, Five_By_Ten, Ten_By_Five, Ten_By_Ten, Ten_By_Twenty, 
         Twenty_By_Twenty_NFI, Twenty_By_Forty, Forty_By_Forty,
         fire_year, stem_mortality, ba_mortality, wsv_mortality, ntwb_mortality
  )

saveRDS(sample_data7, paste0(savepath, "/sample_data.rds"))



### Projected inventory
external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"

VDYP_all <- fread(paste0(external_path2, 
                         "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_OUTPUT_YLDTBL.csv"))

VDYP_input <- fread(paste0(external_path2, 
                           "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_INPUT_LAYER.csv"))

# Remove residual
VDYP_input1_1 <- VDYP_input %>%
  filter(VDYP7_LAYER_CD == "P") 

VDYP_all <- VDYP_all %>%
  left_join(VDYP_input1_1 %>% select(FEATURE_ID, LAYER_ID = LAYER_LEVEL_CODE), 
            by = c("FEATURE_ID", "LAYER_ID"))

#VDYP_proj <- VDYP_all %>%
#  filter(FEATURE_ID %in% unique(sample_data7$FEATURE_ID_2022))

#VDYP_proj1 <- VDYP_proj %>%
#  left_join(sample_data7 %>% select(FEATURE_ID_2022, CLSTR_ID, BEC_ZONE, PROJ_AGE_ADJ),
#            by = c("FEATURE_ID" = "FEATURE_ID_2022"))

VDYP_proj1 <- sample_data7 %>%
  select(FEATURE_ID, LAYER_ID, CLSTR_ID, BEC_ZONE, SPECIES_CD_1, PROJ_AGE_ADJ) %>%
  mutate(LAYER_ID = as.character(LAYER_ID)) %>%
  left_join(VDYP_all, by = c("FEATURE_ID", "LAYER_ID"))

#VDYP_proj2 <- VDYP_proj1 %>% 
#  select(CLSTR_ID, BEC_ZONE, FEATURE_ID, LAYER_ID, SPECIES_1_CODE, 
#         PROJ_AGE_ADJ, PRJ_BA, PRJ_TPH, PRJ_TOTAL_AGE, PRJ_DOM_HT, PRJ_VOL_DWB) %>%
#  mutate_at(vars(PRJ_BA, PRJ_TPH, PRJ_DOM_HT, PRJ_VOL_DWB), ~replace(., is.na(.), 0))

VDYP_proj2 <- VDYP_proj1 %>% 
  filter(!is.na(CLSTR_ID)) %>% 
  select(CLSTR_ID, BEC_ZONE, FEATURE_ID, LAYER_ID, SPECIES_CD_1, SPECIES_1_CODE, 
         PROJ_AGE_ADJ, PRJ_BA, PRJ_TPH, PRJ_TOTAL_AGE, PRJ_DOM_HT, PRJ_VOL_DWB) %>%
  mutate_at(vars(PRJ_BA, PRJ_TPH, PRJ_DOM_HT, PRJ_VOL_DWB), ~replace(., is.na(.), 0))

#vdyp_year <- VDYP_proj2 %>% 
#  filter(LAYER_ID != "D") %>% 
#  tidyr::expand(nesting(FEATURE_ID, CLSTR_ID, LAYER_ID), PRJ_TOTAL_AGE = full_seq(PRJ_TOTAL_AGE, 1))

vdyp_year <- VDYP_proj2 %>% 
  filter(!is.na(SPECIES_1_CODE)) %>% 
  tidyr::expand(nesting(FEATURE_ID, CLSTR_ID, LAYER_ID), PRJ_TOTAL_AGE = full_seq(PRJ_TOTAL_AGE, 1))

#VDYP_proj3 <- vdyp_year %>%
#  left_join(VDYP_proj2, by = c("FEATURE_ID", "CLSTR_ID", "LAYER_ID", "PRJ_TOTAL_AGE")) %>%
#  arrange(FEATURE_ID, CLSTR_ID, LAYER_ID, PRJ_TOTAL_AGE)

VDYP_proj3 <- vdyp_year %>%
  left_join(VDYP_proj2, by = c("FEATURE_ID", "CLSTR_ID", "LAYER_ID", "PRJ_TOTAL_AGE")) %>%
  arrange(FEATURE_ID, CLSTR_ID, LAYER_ID, PRJ_TOTAL_AGE)

VDYP_proj4 <- VDYP_proj3 %>% 
  filter(!is.na(CLSTR_ID)) %>% 
  group_by(FEATURE_ID, CLSTR_ID, LAYER_ID) %>% 
  mutate(CLSTR_ID = zoo::na.locf(CLSTR_ID, na.rm = FALSE),
         BEC_ZONE = zoo::na.locf(BEC_ZONE, na.rm = FALSE),
         SPECIES_CD_1 = zoo::na.locf(SPECIES_CD_1, na.rm = FALSE),
         SPECIES_1_CODE = zoo::na.locf(SPECIES_1_CODE, na.rm = FALSE),
         PROJ_AGE_ADJ = zoo::na.locf(PROJ_AGE_ADJ, na.rm = FALSE),
         vdyp_ba = zoo::na.approx(PRJ_BA, PRJ_TOTAL_AGE, rule = 2),
         vdyp_tph = zoo::na.approx(PRJ_TPH, PRJ_TOTAL_AGE, rule = 2),
         vdyp_dom_ht = zoo::na.approx(PRJ_DOM_HT, PRJ_TOTAL_AGE, rule = 2),
         vdyp_vol_dwb = zoo::na.approx(PRJ_VOL_DWB, PRJ_TOTAL_AGE, rule = 2)) %>% 
  ungroup()

saveRDS(VDYP_proj4, paste0(savepath, "/VDYP_proj_all.rds"))

#VDYP_proj5 <- VDYP_proj4 %>%
#  filter(!is.na(CLSTR_ID)) %>%
#  filter(ifelse(!is.na(PROJ_AGE_ADJ) & PROJ_AGE_ADJ < 526, PROJ_AGE_ADJ == PRJ_TOTAL_AGE, PRJ_TOTAL_AGE == 526))

VDYP_proj5 <- VDYP_proj4 %>%
  filter(!is.na(CLSTR_ID), PROJ_AGE_ADJ == PRJ_TOTAL_AGE) %>%
  select(-PRJ_BA, -PRJ_TPH, -PRJ_DOM_HT, -PRJ_VOL_DWB)

#VDYP_proj6 <- VDYP_proj5 %>%
#  group_by(CLSTR_ID, FEATURE_ID) %>%
#  arrange(desc(LAYER_ID)) %>%
#  slice(1)
#
#VDYP_proj6_1 <- VDYP_proj6 %>%
#  left_join(sample_data7 %>% select(CLSTR_ID, SPECIES_CD_1), by = "CLSTR_ID")

VDYP_proj6 <- VDYP_proj5  %>%
#VDYP_proj6 <- VDYP_proj6_1  %>%
  rowwise() %>%
  #*further adjustments, corrections to species codes based on bec zone & expected outcome;
  # *bec coast vs interior for species corrections;
  mutate(bec_i_c = ifelse(BEC_ZONE %in% c('CWH','CDF','MH','CMA'), "C", "I"), 
         
         SPECIES_INV = SPECIES_CD_1,
         SPECIES_INV = case_when(SPECIES_CD_1 == "A" ~ "AT",
                                 SPECIES_CD_1 == "AX" ~ "AC",
                                 SPECIES_CD_1 == "C" ~ "CW",
                                 SPECIES_CD_1 %in% c("D", "RA") ~ "DR",
                                 SPECIES_CD_1 %in% c("E", "EXP", "EA") ~ "EP",
                                 SPECIES_CD_1 == "J" ~ "JR",
                                 SPECIES_CD_1 == "L" ~ "LW",
                                 SPECIES_CD_1 %in% c("P", "PLI") ~ "PL",
                                 SPECIES_CD_1 %in% c("FDI", "FDC") ~ "FD",
                                 SPECIES_CD_1 %in% c("SX", "SXL", "SXW") ~ "SW",
                                 SPECIES_CD_1 == "SXE" ~ "SE",
                                 SPECIES_CD_1 == "T" ~ "TW",
                                 SPECIES_CD_1 %in% c("X", "XC") ~ "XC",
                                 SPECIES_CD_1 == "ZH" ~ "XH",
                                 TRUE ~ substr(SPECIES_CD_1, 1, 2)), 
         # *further adjustments, corrections to species codes based on bec zone & expected outcome;
         SPECIES_INV = case_when(grepl("S", SPECIES_INV) == T & BEC_ZONE == "ESSF" ~ "SE",
                                 grepl("S", SPECIES_INV) == T & bec_i_c == "I" & BEC_ZONE != "ESSF" ~ "SW",
                                 grepl("S", SPECIES_INV) == T & bec_i_c == "C" ~ "SS",
                                 grepl("B", SPECIES_INV) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "BG",
                                 grepl("B", SPECIES_INV) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "BA",
                                 grepl("B", SPECIES_INV) == T & bec_i_c == "I" ~ "BL",
                                 grepl("H", SPECIES_INV) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "HW",
                                 grepl("H", SPECIES_INV) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "HM",
                                 grepl("H", SPECIES_INV) == T & bec_i_c == "I" ~ "HW",
                                 TRUE ~ SPECIES_INV),#,
         #SPC_GRP_GRD = ifelse(SPECIES %in% decidspc, 'DE', SPECIES),
         SPC_GRP2 = ifelse(SPECIES_INV %in% decidspc, 'DE', SPECIES_INV),
                           
         SPECIES_INV_prj = SPECIES_1_CODE,
         SPECIES_INV_prj = case_when(SPECIES_1_CODE == "A" ~ "AT",
                                 SPECIES_1_CODE == "AX" ~ "AC",
                                 SPECIES_1_CODE == "C" ~ "CW",
                                 SPECIES_1_CODE %in% c("D", "RA") ~ "DR",
                                 SPECIES_1_CODE %in% c("E", "EXP", "EA") ~ "EP",
                                 SPECIES_1_CODE == "J" ~ "JR",
                                 SPECIES_1_CODE == "L" ~ "LW",
                                 SPECIES_1_CODE %in% c("P", "PLI") ~ "PL",
                                 SPECIES_1_CODE %in% c("FDI", "FDC") ~ "FD",
                                 SPECIES_1_CODE %in% c("SX", "SXL", "SXW") ~ "SW",
                                 SPECIES_1_CODE == "SXE" ~ "SE",
                                 SPECIES_1_CODE == "T" ~ "TW",
                                 SPECIES_1_CODE %in% c("X", "XC") ~ "XC",
                                 SPECIES_1_CODE == "ZH" ~ "XH",
                                 TRUE ~ substr(SPECIES_1_CODE, 1, 2)), 
         # *further adjustments, corrections to species codes based on bec zone & expected outcome;
         SPECIES_INV_prj = case_when(grepl("S", SPECIES_INV_prj) == T & BEC_ZONE == "ESSF" ~ "SE",
                                 grepl("S", SPECIES_INV_prj) == T & bec_i_c == "I" & BEC_ZONE != "ESSF" ~ "SW",
                                 grepl("S", SPECIES_INV_prj) == T & bec_i_c == "C" ~ "SS",
                                 grepl("B", SPECIES_INV_prj) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "BG",
                                 grepl("B", SPECIES_INV_prj) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "BA",
                                 grepl("B", SPECIES_INV_prj) == T & bec_i_c == "I" ~ "BL",
                                 grepl("H", SPECIES_INV_prj) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "HW",
                                 grepl("H", SPECIES_INV_prj) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "HM",
                                 grepl("H", SPECIES_INV_prj) == T & bec_i_c == "I" ~ "HW",
                                 TRUE ~ SPECIES_INV_prj),#,
         #SPC_GRP_GRD = ifelse(SPECIES %in% decidspc, 'DE', SPECIES),
         SPC_GRP2_prj = ifelse(SPECIES_INV_prj %in% decidspc, 'DE', SPECIES_INV_prj)#,
         #LIVE_VOL_PER_HA = ifelse(SPECIES == "PL", LIVE_VOL_PER_HA_125, LIVE_VOL_PER_HA_175),
         #DEAD_VOL_PER_HA = ifelse(SPECIES == "PL", DEAD_VOL_PER_HA_125, DEAD_VOL_PER_HA_175),
         #source = "Inventory"
  ) %>%
  as.data.table()


saveRDS(VDYP_proj6, paste0(savepath, "/VDYP_dat.rds"))



################################################################################
### Leading species
### Ground
# *get leading species by ba at 12.5cm util;
grd_lead <-faib_spcsmries  %>% 
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID, UTIL == 12.5) %>%
  mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
  mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1)) %>%  
  group_by(CLSTR_ID) %>% 
  arrange(desc(SP_PCT_BA_LS)) %>% 
  slice(1) %>% 
  select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1)  

### Age and ht of leading species
grd_by_sp <- grd_lead %>%
  left_join(faib_siteage, by = c("CLSTR_ID", "SPECIES"))

### volume: 12.5 for PL 17.5 for others
grd_bastemvol <-faib_spcsmries  %>% 
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID, UTIL == ifelse(SPECIES == "PL", 12.5, 17.5)) %>%
  group_by(CLSTR_ID) %>% 
  select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, UTIL, 
         BA_HA_LS, BA_HA_DS, 
         STEMS_HA_LS, STEMS_HA_DS,
         VHA_NTWB_NVAF_LS, VHA_NTWB_NVAF_DS) %>% 
  # *sum volumes ba and tph at specified utilization limits across all species;
  summarise(BA_HA_LS = sum(BA_HA_LS, na.rm = T),
            BA_HA_DS = sum(BA_HA_DS, na.rm = T),
            STEMS_HA_LS = sum(STEMS_HA_LS, na.rm = T),
            STEMS_HA_DS = sum(STEMS_HA_DS, na.rm = T),
            NTWB_NVAF_LS = sum(VHA_NTWB_NVAF_LS, na.rm = T),
            NTWB_NVAF_DS = sum(VHA_NTWB_NVAF_DS, na.rm = T))

grd_by_sp1 <- grd_by_sp %>%
  left_join(grd_bastemvol, by = c("CLSTR_ID"))


grd_by_sp2 <- grd_by_sp1  %>% 
  left_join(VDYP_proj6, by = "CLSTR_ID") %>% 
  #mutate_at(c('id','pages'), ~replace_na(.,0)) %>%
  data.table

grd_by_sp3 <- grd_by_sp2 %>%
  left_join(sample_data7 %>% 
              select(MGMT_UNIT, CLSTR_ID, Design, MEAS_YR,
                     DEAD_STAND_VOLUME_175,
                     fire_year:ntwb_mortality), 
            by = "CLSTR_ID") %>%
  mutate(DEAD_STAND_VOLUME_175 = replace_na(DEAD_STAND_VOLUME_175, 0)) 

# *adjust burn impact on ground sample compiled volume and basal area, for those samples 
# visited post burn, only if last full measurement;
# *was prior to burn.  if a full remeasurement completed post burn, then do not adjust;
grd_by_sp4 <- grd_by_sp3 %>%
  mutate(BA_HA_LS = ifelse(!is.na(fire_year) & fire_year >= MEAS_YR, 
                           BA_HA_LS * (1-ba_mortality), BA_HA_LS),
         STEMS_HA_LS = ifelse(!is.na(fire_year) & fire_year >= MEAS_YR, 
                              STEMS_HA_LS * (1-stem_mortality), STEMS_HA_LS),
         NTWB_NVAF_LS = ifelse(!is.na(fire_year) & fire_year >= MEAS_YR, 
                               NTWB_NVAF_LS * (1-ntwb_mortality), NTWB_NVAF_LS)
  )


saveRDS(grd_by_sp4, paste0(savepath, "/VDYP_proj.rds"))
saveRDS(grd_by_sp3, paste0(savepath, "/lead_vol.rds"))


################################################################################
### Volume by species
### Ground
grdspc_vol <-faib_spcsmries  %>% 
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID, UTIL == ifelse(SPECIES == "PL", 12.5, 17.5)) %>%
  mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
  mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1)) %>%  
  group_by(CLSTR_ID) %>% 
  select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1, 
         LIVE_VOL_PER_HA = VHA_NTWB_NVAF_LS, 
         DEAD_VOL_PER_HA = VHA_NTWB_NVAF_DS) %>%
  replace(is.na(.), 0) %>% 
  ungroup() %>%
  mutate(source = "Ground") %>%
  data.table


### Inventory
inv_temp <- sample_data7 %>%
  select(MGMT_UNIT, CLSTR_ID, SITE_IDENTIFIER, BEC_ZONE, SAMPLE_ESTABLISHMENT_TYPE, Design,
         SPECIES_CD_1:SPECIES_CD_6, SPECIES_PCT_1:SPECIES_PCT_6, 
         LIVE_VOL_PER_HA_SPP1_125:DEAD_VOL_PER_HA_SPP6_175)

inv_temp1 <- inv_temp %>%
  select(MGMT_UNIT,CLSTR_ID, SITE_IDENTIFIER, BEC_ZONE, SAMPLE_ESTABLISHMENT_TYPE, Design,
         SPECIES_CD_1, SPECIES_CD_2, SPECIES_CD_3, SPECIES_CD_4, SPECIES_CD_5, SPECIES_CD_6) %>%
  pivot_longer(cols = starts_with("SPECIES_CD_"), 
               names_to = "NUM",
               names_prefix = "SPECIES_CD_",
               values_to = "SPECIES")

inv_temp2 <- inv_temp %>%
  select(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, Design,
         SPECIES_PCT_1, SPECIES_PCT_2, SPECIES_PCT_3, SPECIES_PCT_4, SPECIES_PCT_5, SPECIES_PCT_6) %>%
  pivot_longer(cols = starts_with("SPECIES_PCT_"), 
               names_to = "NUM",
               names_prefix = "SPECIES_PCT_",
               values_to = "SPECIES_PCT") 

inv_temp3 <- inv_temp %>%
  select(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, Design,
         LIVE_VOL_PER_HA_SPP1_125, LIVE_VOL_PER_HA_SPP2_125, LIVE_VOL_PER_HA_SPP3_125, 
         LIVE_VOL_PER_HA_SPP4_125, LIVE_VOL_PER_HA_SPP5_125, LIVE_VOL_PER_HA_SPP6_125) %>%
  pivot_longer(cols = starts_with("LIVE_VOL_PER_HA_"), 
               names_to = "NUM",
               names_prefix = "LIVE_VOL_PER_HA_SPP",
               values_to = "LIVE_VOL_PER_HA_125") %>% 
  mutate(NUM = gsub("(\\d+)_.*", "\\1", NUM))

inv_temp4 <- inv_temp %>%
  select(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, Design,
         LIVE_VOL_PER_HA_SPP1_175, LIVE_VOL_PER_HA_SPP2_175, LIVE_VOL_PER_HA_SPP3_175, 
         LIVE_VOL_PER_HA_SPP4_175, LIVE_VOL_PER_HA_SPP5_175, LIVE_VOL_PER_HA_SPP6_175) %>%
  pivot_longer(cols = starts_with("LIVE_VOL_PER_HA_"), 
               names_to = "NUM",
               names_prefix = "LIVE_VOL_PER_HA_SPP",
               values_to = "LIVE_VOL_PER_HA_175") %>% 
  mutate(NUM = gsub("(\\d+)_.*", "\\1", NUM))

inv_temp5 <- inv_temp %>%
  select(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, Design,
         DEAD_VOL_PER_HA_SPP1_125, DEAD_VOL_PER_HA_SPP2_125, DEAD_VOL_PER_HA_SPP3_125, 
         DEAD_VOL_PER_HA_SPP4_125, DEAD_VOL_PER_HA_SPP5_125, DEAD_VOL_PER_HA_SPP6_125) %>%
  pivot_longer(cols = starts_with("DEAD_VOL_PER_HA_"), 
               names_to = "NUM",
               names_prefix = "DEAD_VOL_PER_HA_",
               values_to = "DEAD_VOL_PER_HA_125") %>% 
  mutate(NUM = gsub("(\\d+)_.*.*", "\\1", NUM))

inv_temp6 <- inv_temp %>%
  select(CLSTR_ID, SITE_IDENTIFIER, SAMPLE_ESTABLISHMENT_TYPE, Design,
         DEAD_VOL_PER_HA_SPP1_175, DEAD_VOL_PER_HA_SPP2_175, DEAD_VOL_PER_HA_SPP3_175, 
         DEAD_VOL_PER_HA_SPP4_175, DEAD_VOL_PER_HA_SPP5_175, DEAD_VOL_PER_HA_SPP6_175) %>%
  pivot_longer(cols = starts_with("DEAD_VOL_PER_HA_"), 
               names_to = "NUM",
               names_prefix = "DEAD_VOL_PER_HA_SPP",
               values_to = "DEAD_VOL_PER_HA_175") %>% 
  mutate(NUM = gsub("(\\d+)_.*", "\\1", NUM))


inv_vol <- inv_temp1 %>%
  left_join(inv_temp2, by = c("CLSTR_ID", "SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE", "Design", "NUM")) %>%
  left_join(inv_temp3, by = c("CLSTR_ID", "SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE", "Design", "NUM")) %>%
  left_join(inv_temp4, by = c("CLSTR_ID", "SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE", "Design", "NUM")) %>%
  left_join(inv_temp5, by = c("CLSTR_ID", "SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE", "Design", "NUM")) %>%
  left_join(inv_temp6, by = c("CLSTR_ID", "SITE_IDENTIFIER", "SAMPLE_ESTABLISHMENT_TYPE", "Design", "NUM")) %>%
  filter(!is.na(SPECIES)) %>%
  #mutate(DEAD_VOL_PER_HA_125 = replace_na(DEAD_VOL_PER_HA_125, 0),
  #       DEAD_VOL_PER_HA_175 = replace_na(DEAD_VOL_PER_HA_175, 0)) %>%
  replace(is.na(.), 0) %>% 
  rowwise() %>%
  # *further adjustments, corrections to species codes based on bec zone & expected outcome;
  # *bec coast vs interior for species corrections;
  mutate(bec_i_c = ifelse(BEC_ZONE %in% c('CWH','CDF','MH','CMA'), "C", "I"), 
         SPECIES_INV = SPECIES,
         SPECIES = case_when(SPECIES == "A" ~ "AT",
                             SPECIES == "AX" ~ "AC",
                             SPECIES == "C" ~ "CW",
                             SPECIES %in% c("D", "RA") ~ "DR",
                             SPECIES %in% c("E", "EXP", "EA") ~ "EP",
                             SPECIES == "J" ~ "JR",
                             SPECIES == "L" ~ "LW",
                             SPECIES %in% c("P", "PLI") ~ "PL",
                             SPECIES %in% c("FDI", "FDC") ~ "FD",
                             SPECIES %in% c("SX", "SXL", "SXW") ~ "SW",
                             SPECIES == "SXE" ~ "SE",
                             SPECIES == "T" ~ "TW",
                             SPECIES %in% c("X", "XC") ~ "XC",
                             SPECIES == "ZH" ~ "XH",
                             TRUE ~ substr(SPECIES, 1, 2)), 
         # *further adjustments, corrections to species codes based on bec zone & expected outcome;
         SPECIES = case_when(grepl("S", SPECIES) == T & BEC_ZONE == "ESSF" ~ "SE",
                             grepl("S", SPECIES) == T & bec_i_c == "I" & BEC_ZONE != "ESSF" ~ "SW",
                             grepl("S", SPECIES) == T & bec_i_c == "C" ~ "SS",
                             grepl("B", SPECIES) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "BG",
                             grepl("B", SPECIES) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "BA",
                             grepl("B", SPECIES) == T & bec_i_c == "I" ~ "BL",
                             grepl("H", SPECIES) == T & bec_i_c == "C" & BEC_ZONE %in% c('CWH','CDF') ~ "HW",
                             grepl("H", SPECIES) == T & bec_i_c == "C" & !(BEC_ZONE %in% c('CWH','CDF')) ~ "HM",
                             grepl("H", SPECIES) == T & bec_i_c == "I" ~ "HW",
                             TRUE ~ SPECIES),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPECIES),
         LIVE_VOL_PER_HA = ifelse(SPECIES == "PL", LIVE_VOL_PER_HA_125, LIVE_VOL_PER_HA_175),
         DEAD_VOL_PER_HA = ifelse(SPECIES == "PL", DEAD_VOL_PER_HA_125, DEAD_VOL_PER_HA_175),
         DEAD_VOL_PER_HA = DEAD_VOL_PER_HA_175,
         source = "Inventory") %>%
  as.data.table()

inv_vol <- inv_vol %>%
  select(SITE_IDENTIFIER, CLSTR_ID, SPECIES, SPC_GRP1, LIVE_VOL_PER_HA, DEAD_VOL_PER_HA, source)


vol_comp <- rbind(grdspc_vol, inv_vol)

vol_comp <- vol_comp %>%
  left_join(sample_data7 %>% select(CLSTR_ID, Design),
            by = "CLSTR_ID")

saveRDS(vol_comp, paste0(savepath, "/spc_vol.rds"))



VDYP_grd <- VDYP_grd %>%
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID)

saveRDS(VDYP_grd, paste0(savepath, "/VDYP_grd.rds"))



################################################################################
### Tree data for forest health analysis
tree_data <- faib_tree %>%
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID) %>%
  left_join(vi_d[, .(CLSTR_ID, PLOT, TREE_NO, SPECIES,
                     LOSS1_IN,LOSS2_IN,LOSS3_IN,LOSS4_IN,LOSS5_IN,
                     LOC1_FRO,LOC2_FRO,LOC3_FRO,LOC4_FRO,LOC5_FRO,
                     FREQ1,FREQ2,FREQ3,FREQ4,FREQ5)], 
            by = c('CLSTR_ID', 'PLOT', 'TREE_NO', 'SPECIES')) 

FH_dat <- tree_data %>%
  filter(!is.na(PHF_TREE)) %>%
  ### Force all NA to blank for further processing
  mutate(DAM_AGNA = ifelse(is.na(DAM_AGNA), '', DAM_AGNA),
         DAM_AGNB = ifelse(is.na(DAM_AGNB), '', DAM_AGNB),
         DAM_AGNC = ifelse(is.na(DAM_AGNC), '', DAM_AGNC),
         DAM_AGND = ifelse(is.na(DAM_AGND), '', DAM_AGND),
         DAM_AGNE = ifelse(is.na(DAM_AGNE), '', DAM_AGNE),
         ### DAM_AGNA should be counted
         DAM_AGNA = ifelse(DAM_AGNA == '', 'O', DAM_AGNA),
         ba_ha = BA_TREE * PHF_TREE,
         vol_ha = VOL_WSV * PHF_TREE) %>%
  left_join(sample_data7 %>% select(CLSTR_ID, MEAS_YR), by = "CLSTR_ID")

### Function for updating U trees based on loss information
update_dam_loss <- function(dam_agnt, loss) {
  case_when(
    dam_agnt == "U" & loss %in% c('FRK') ~ 'UF',
    dam_agnt == "U" & loss %in% c('CRO', 'CRK') ~ 'UCR',
    dam_agnt == "U" & loss %in% c('BTP') ~ 'UBT',
    dam_agnt == "U" & loss %in% c('DTP') ~ 'UDT',
    TRUE ~ dam_agnt)
}

#*redefine unknown fhf associated with specific stem form damage;
FH_dat1 <- FH_dat %>%
  mutate(
    DAM_AGNA = update_dam_loss(DAM_AGNA, LOSS1_IN),
    DAM_AGNB = update_dam_loss(DAM_AGNB, LOSS2_IN),
    DAM_AGNC = update_dam_loss(DAM_AGNC, LOSS3_IN),
    DAM_AGND = update_dam_loss(DAM_AGND, LOSS4_IN),
    DAM_AGNE = update_dam_loss(DAM_AGNE, LOSS5_IN)
  )

# *ignore minor incidence of UF and UCR (ie,. where severity=N, as recorded in 2021 and later measurements);
FH_dat <- FH_dat %>%
  mutate(DAM_AGNA = ifelse(DAM_AGNA %in% c('UF', 'UCR') & SEV_A == "N", "O", DAM_AGNA),
         DAM_AGNB = ifelse(DAM_AGNB %in% c('UF', 'UCR') & SEV_B == "N", "O", DAM_AGNB),
         DAM_AGNC = ifelse(DAM_AGNC %in% c('UF', 'UCR') & SEV_C == "N", "O", DAM_AGNC),
         DAM_AGND = ifelse(DAM_AGND %in% c('UF', 'UCR') & SEV_D == "N", "O", DAM_AGND),
         DAM_AGNE = ifelse(DAM_AGNE %in% c('UF', 'UCR') & SEV_E == "N", "O", DAM_AGNE))

### If other damage agent exists, remove O
FH_dat <- FH_dat %>%
  mutate(DAM_AGNA = ifelse(DAM_AGNA == 'O' & !(DAM_AGNB %in% c(NA, '')), '', DAM_AGNA),
         
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNB),
         DAM_AGNB = ifelse(DAM_AGNB == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGNB),
         
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNC),
         DAM_AGNC = ifelse(DAM_AGNC == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGNC),
         
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGND),
         DAM_AGND = ifelse(DAM_AGND == 'O' & !(DAM_AGNE %in% c('O', '', NA)), '', DAM_AGND),
         
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNA %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNB %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGNC %in% c('O', '', NA)), '', DAM_AGNE),
         DAM_AGNE = ifelse(DAM_AGNE == 'O' & !(DAM_AGND %in% c('O', '', NA)), '', DAM_AGNE))

#### If the DAM_AGNA is U and there are other loss indicator exists
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNA = ifelse(DAM_AGNA == 'U' & !is.na(DAM_AGNB) & grepl('U', DAM_AGNB), DAM_AGNB, DAM_AGNA))

#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNB = ifelse(DAM_AGNA == DAM_AGNB & grepl('U', DAM_AGNA, fixed = TRUE), 'U', DAM_AGNB))

# *look at numeric values only in severity class;
FH_dat <- FH_dat %>%
  mutate(SEVPERC_A = as.numeric(gsub("[^\\d]+", "", SEV_A, perl = T)),
         SEVPERC_B = as.numeric(gsub("[^\\d]+", "", SEV_B, perl = T)),
         SEVPERC_C = as.numeric(gsub("[^\\d]+", "", SEV_C, perl = T)),
         SEVPERC_D = as.numeric(gsub("[^\\d]+", "", SEV_D, perl = T)),
         SEVPERC_E = as.numeric(gsub("[^\\d]+", "", SEV_E, perl = T))) %>%
  data.frame

## *only output unknown damage agents if leading damage agent only; - didnt seem to be applied
#update_undefined_dam <- function(dam_agna, dam_agnt) {
#  case_when(
#    grepl('U', dam_agna, fixed = TRUE) & dam_agnt %in% c(NA, '','UF','UCR','UBT','UDT','U') ~ '',
#    # *all other damage agents get output regardless of position;
#    TRUE ~ dam_agnt)
#}
#
#FH_dat <- FH_dat %>%
#  mutate(DAM_AGNB = update_undefined_dam(DAM_AGNA, DAM_AGNB),
#         DAM_AGNC = update_undefined_dam(DAM_AGNA, DAM_AGNC),
#         DAM_AGND = update_undefined_dam(DAM_AGNA, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNA, DAM_AGNE),
#         DAM_AGNC = update_undefined_dam(DAM_AGNB, DAM_AGNC),
#         DAM_AGND = update_undefined_dam(DAM_AGNB, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNB, DAM_AGNE),
#         DAM_AGND = update_undefined_dam(DAM_AGNC, DAM_AGND),
#         DAM_AGNE = update_undefined_dam(DAM_AGNC, DAM_AGNE),
#         DAM_AGNE = update_undefined_dam(DAM_AGND, DAM_AGNE))

# *expande tree data so up to 5 damage agents per tree each on their own record for tracking all agents per tree;
FH_dat1 <- melt(setDT(FH_dat),
                id.vars = c('SITE_IDENTIFIER', 'CLSTR_ID', 'VISIT_NUMBER', 'PLOT', 'MEAS_YR', 'MEAS_INTENSE',
                            'TREE_NO', 'RESIDUAL', 'TREE_WT', 'WALKTHRU_STATUS', 'TH_TREE', 
                            'DBH', 'HEIGHT', 'SPECIES', 'LV_D', 'S_F', 'COMP_CHG', 'PHF_TREE', 
                            'VOL_WSV', 'VOL_MER', 'VOL_NTWB', 'AGE_BH', 'AGE_TOT', 
                            'SI_TREE', "SUIT_TR", "SUIT_HT", "SUIT_SI",
                            'BA_TREE', 'ba_ha', 'vol_ha'),
                measure.vars = patterns("^DAM_AGN", "^SEV_", "^SEVPERC_"),
                variable.name = "DAM_NUM",
                value.name = c("AGN", "SEV", "SEVPERC"))

FH_dat1 <- FH_dat1 %>%
  mutate(SPC_GRP1 = substr(SPECIES,1,2)) %>%
  # *further grouping of multiple species labels, and for all deciduous;
  mutate(SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

FH_dat1 <- FH_dat1 %>%
  mutate(AGN = case_when(
    # *further corrections, error caught by H.Kope;
    #AGN %in% c('DBS') & SEV %in% c('BC', 'SC') ~ 'DSB',
    AGN %in% c('IAG') & SPC_GRP1 %in% c('DE') ~ '',
    AGN %in% c('DBS') & SPC_GRP1 %in% c('PW') ~ '',
    AGN %in% c('DFE') & SPC_GRP1 %in% c('FD') ~ '',
    AGN %in% c('DFL') & SPC_GRP1 %in% c('BL','FD') ~ '',
    AGN %in% c('DSB') & SPC_GRP1 %in% c('PL') ~ '',
    AGN %in% c('DSG') & SPC_GRP1 %in% c('BL') ~ '',
    AGN %in% c('IBM') & SPC_GRP1 %in% c('BL','SW') ~ '',
    AGN %in% c('IBS') & SPC_GRP1 %in% c('PL') ~ '',
    #AGN %in% c('IDE') & SPC_GRP1 %in% c('PL') ~ '',
    #AGN %in% c('DFB') & SPC_GRP1 %in% c('FD','SW') ~ '',
    #AGN %in% c('DM') & SPC_GRP1 %in% c('BL') ~ 'DBF',
    #AGN %in% c('DM') & SPC_GRP1 %in% c('PL') ~ 'DMP',
    #AGN %in% c('ISP') & SPC_GRP1 %in% c('BL') ~ '',
    # *edit corrections, error caught by T.Ebata on spruce budworm on Fd;
    AGN %in% c('IDE') & SPC_GRP1 %in% c('FD') & 
      CLSTR_ID %in% c(sample_data7[sample_data7$MGMT_UNIT %in% c('TSA11_Kamloops','TSA29_Williams_Lake'),]$CLSTR_ID) ~ 'IDW',
    TRUE ~ AGN))

#FH_dat1 <- FH_dat1 %>%
#  mutate(AGN = ifelse(DAM_NUM == 1 & AGN == "", "O", AGN))

FH_dat1_temp <- FH_dat1 %>%
  group_by(CLSTR_ID, PLOT, TREE_NO) %>%
  mutate(agn_all = paste(AGN, collapse = "")) %>%
  ungroup() %>%
  mutate(AGN = ifelse(agn_all == "" & DAM_NUM == 1, 'O', AGN))

# *for multiple occurrences of the same damage agent, only keep the lowest position 
# *(ie, lowest 1st digit in severity class);
FH_dat1_1_1 <- FH_dat1 %>%
  filter(!is.na(AGN), AGN != '') %>%
  arrange(CLSTR_ID, PLOT, TREE_NO, AGN, DAM_NUM) %>%
  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
  slice(1)

### Assign new DAM_NUM
FH_dat1_1_1 <- FH_dat1_1_1 %>%
  ungroup() %>%
  mutate(DAM_NUM_old = DAM_NUM) %>%
  arrange(CLSTR_ID, PLOT, TREE_NO, DAM_NUM_old) %>%
  group_by(CLSTR_ID, PLOT, TREE_NO) %>%
  mutate(DAM_NUM = row_number(DAM_NUM_old))

#### If a tree has multiple records of same damage agent with different severity, leave the most severity one only
#FH_dat1_1 <- FH_dat1 %>%
#  filter(!is.na(AGN), AGN != '') %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
#  mutate(dups = ifelse(n()>1, "Yes", "No"),
#         selected = ifelse(SEVPERC == max(SEVPERC), "Yes", "No"),
#         selected = ifelse(is.na(selected), "Yes", selected))

#### If duplicated damage agent for a same tree has no severity information, leave only one
#FH_dat1_1_1 <- FH_dat1_1 %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN) %>%
#  mutate(SEVPERC_new = ifelse(DAM_NUM == 1 & selected == "No", max(SEVPERC), SEVPERC),
#         selected = ifelse(SEVPERC_new == max(SEVPERC_new), "Yes", "No"),
#         selected = ifelse(is.na(selected), "Yes", selected)) %>%
#  filter(selected == "Yes") %>%
#  arrange(CLSTR_ID, PLOT, TREE_NO, AGN, DAM_NUM, desc(dups), desc(selected)) %>%
#  group_by(CLSTR_ID, PLOT, TREE_NO, AGN, dups, selected) %>%
#  slice(1)


### Restructure the severity rating data
sev_rusch1 <- sev_rusch %>%
  pivot_longer(
    cols = starts_with("unkn_sev"),
    names_to = "unkn_sev",
    names_prefix = 'unkn_sev_',
    values_to = "SEV",
    values_drop_na = TRUE
  )

# *merge corrections to severity classification;
FH_dat1_1_2 <- FH_dat1_1_1 %>%
  left_join(sev_rusch1[, c('dam_3letter', 'SEV', 'corr_sev')], 
            by = c('AGN' = 'dam_3letter', 'SEV'))

# *replace unknown severity with corrected severity, where matches present;
FH_dat1_1_2 <- FH_dat1_1_2 %>%
  mutate(dam_severity = ifelse(is.na(corr_sev), SEV, corr_sev)) %>%
  data.table

# *start categorizing severity;
# *first import ismc damage agent to severity class lookup table;
lookup_sev1 <- lookup_sev %>%
  mutate(dam_3letter = DAMAGE_AGENT_CODE,
         severity_grp = DAMAGE_AGENT_SEVERITY_CODE) %>%
  select(dam_3letter, severity_grp) %>%
  distinct()

# *next, import severity rating lookup table created by D.Rush 2021-jan;
lookup_rush2 <- lookup_rush %>%
  mutate_at(vars(matches("dam")), trimws)

lookup_rush3 <- lookup_rush2 %>%
  pivot_longer(cols = starts_with("dam"), names_to = "dam", 
               values_to = "dam_3letter",values_drop_na = TRUE) %>%
  distinct()

lookup_sev2 <- lookup_sev1 %>%
  left_join(lookup_rush3, by = c("dam_3letter"))

### Hard coded
lookup_sev3 <- lookup_sev2[!(lookup_sev2$dam_3letter %in% c('NW', 'NWS', "NWT") & lookup_sev2$allowed == "1-100"),]

FH_dat1_2 <- FH_dat1_1_2 %>%
  left_join(lookup_sev3, by = c("AGN" = "dam_3letter"))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(len_dam = nchar(AGN))  %>%
  rowwise() %>% 
  mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
         dam_2letter = toupper(substr(AGN, 1, min(len_dam,2))),
         dam_3letter = toupper(substr(AGN, 1, min(len_dam,3))))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
                               dam_1letter == 'U' ~ 'Unknown',
                               dam_1letter == 'N' ~ 'Abiotic',
                               dam_1letter == 'D' ~ 'Disease',
                               dam_1letter == 'I' ~ 'Insect',
                               dam_1letter == 'T' ~ 'Treatment',
                               dam_1letter == 'A' ~ 'Animal',
                               dam_1letter == 'X' ~ 'Frk_Crk_Btp',
                               dam_1letter == 'V' ~ 'Vegetation',
                               TRUE ~ ''))

FH_dat1_2 <- FH_dat1_2 %>%
  ungroup() %>%
  mutate(dam_severity_adj = gsub(' ', '', toupper(SEV)),
         sev_char = gsub("[0-9]", "", dam_severity_adj),
         sev_num = as.numeric(gsub("[A-Za-z]", "", dam_severity_adj)))

FH_dat1_2 <- FH_dat1_2 %>%
  mutate(sev_class = case_when(severity_grp %in% c("BBEETLE", "FIRE", "ROOTROT", "STEMRUST2", "TWEEVIL") & 
                                 sev_char %in% c(Low_class1, Low_class2) ~ 'LOW',
                               severity_grp %in% c("BBEETLE", "FIRE", "ROOTROT", "STEMRUST2", "TWEEVIL") & 
                                 sev_char %in% c(Mod_class1, Mod_class2) ~ 'MOD',
                               severity_grp  %in% c("BBEETLE", "FIRE", "ROOTROT", "STEMRUST2", "TWEEVIL") & 
                                 sev_char %in% c(High_class1, High_class2) ~ 'HIGH',
                               
                               severity_grp %in% c("PERCENTAGE", "HAWKSWORTH") & 
                                 sev_num >= Low_min & sev_num <= Low_max ~ 'LOW',
                               severity_grp %in% c("PERCENTAGE", "HAWKSWORTH") & 
                                 sev_num >= Mod_min & sev_num <= Mod_max ~ 'MOD',
                               severity_grp  %in% c("PERCENTAGE", "HAWKSWORTH") & 
                                 sev_num >= High_min ~ 'HIGH',
                               
                               # *use only percent encirclemt for most recent stem rust severity ratings;
                               severity_grp == "STEMRUST1" & is.na(sev_num) & 
                                 sev_char %in% c(Low_class1, Low_class2) ~ 'LOW',
                               severity_grp == "STEMRUST1" & is.na(sev_num) & 
                                 sev_char %in% c(Mod_class1, Mod_class2) ~ 'MOD',
                               severity_grp == "STEMRUST1" & is.na(sev_num) & 
                                 sev_char %in% c(High_class1, High_class2) ~ 'HIGH',
                               
                               # *get percent encirclement for stem rust severity collected since 2017;
                               severity_grp == "STEMRUST1" & !is.na(sev_num) & 
                                 as.numeric(substr(dam_severity, 2, 2)) >= Low_min & 
                                 as.numeric(substr(dam_severity, 2, 2)) <= Low_max ~ 'LOW',
                               severity_grp == "STEMRUST1" & !is.na(sev_num) & 
                                 as.numeric(substr(dam_severity, 2, 2)) >= Mod_min & 
                                 as.numeric(substr(dam_severity, 2, 2)) <= Mod_max ~ 'MOD',
                               severity_grp == "STEMRUST1" & !is.na(sev_num) & 
                                 as.numeric(substr(dam_severity, 2, 2)) >= High_min ~ 'HIGH',
                               
                               # *default to severity class when no match;
                               sev_char == "" ~  "UNKNOWN",
                               TRUE ~ "UNKNOWN"
  ))


FH_dat2 <- FH_dat1_2 %>%
  ungroup() %>%
  filter(!is.na(AGN), AGN != '') %>%
  mutate(n = n_distinct(SITE_IDENTIFIER),
         SPC_GRP1 = substr(SPECIES,1,2),
         SPC_GRP1 = ifelse(SPECIES %in% decidspc, 'DE', SPC_GRP1))

### Immediate vs. incremental mortality
# *assign a tree either as a current mortality (lethal agents), 
# or future mortality (assigned as an incremental 0.25%/yr mort rate;
FH_dat2 <- FH_dat2 %>%
  # *immediate / imminent mortality, at 90%;
  mutate(mort_code = case_when(AGN %in% c('AB','DRA','DRB','DRC','DRL','DRN','DRR','DRT','DSB', 
                                          'IB', 'IBB', 'IBI', 'IBM', 'IBP', 'IBS', 
                                          'IBW', 'ISW', 'ND', 'NF', 'NS', 'NW', 'NWS', 
                                          'NY', 'TC','DSC') ~ 1,
                               # *further assessment on fire damage (exclude FD);
                               # *immediate / imminent mortality, at 90%;
                               AGN == 'NB' & !(SPC_GRP1 %in% c('FD','LW')) ~ 1,
                               # *immediate only for the following fh agents greater than 80% severity;
                               AGN %in% c('DF','DFE','DFS') & SEVPERC >= 80 ~ 1,
                               # *incremental mortality, at 0.25%/yr for the following conifer diseases;
                               AGN %in% c('DB', 'DM', 'DMH', 'DMP', 'DSA', 'DSE', 'DSG','DSS') ~ 2,
                               #AGN %in% c('DSG','DSS') & MEAS_YR < 2017 ~ 2,
                               # *incremental growth loss, at 0.25%/yr for insects with percent of tree affected, 
                               # use 80%;
                               AGN %in% c('IAB', 'IDW', 'IDB', 'IDE', 'IDH', 'IDI', 'IDT') &
                                 SEVPERC >= 80 ~ 2,
                               # *incremental growth loss, at 0.25%/yr, for terminal weevil, 
                               # for all cases other than minor crooks / forks;
                               AGN %in% c('IWP', 'IWS') & grepl('N', SEV) == FALSE ~ 2,
                               TRUE ~ 3))

### If two or more mort_code appear on a same trees, use the most severe one.
FH_dat2 <- FH_dat2 %>%
  group_by(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, PLOT, TREE_NO) %>%
  mutate(mort_flag = min(mort_code),
         AGN_new = ifelse(mort_flag != mort_code, AGN[which.min(mort_code)], AGN))

FH_dat2 <- FH_dat2 %>%
  # *create new resid flag, which combines different sources;
  mutate(RESID_GRP = ifelse(RESIDUAL %in% c('Y','R','V') | TH_TREE == 'V', 'Y', 'N'),
         # *create dbh classes;
         DBH_CLASS = round(DBH/5)*5)

#FH_dat2 <- FH_dat2 %>%
#  mutate(len_dam = nchar(AGN))  %>%
#  rowwise() %>% 
#  mutate(dam_1letter = toupper(substr(AGN, 1, 1)),
#         dam_2letter = toupper(substr(AGN, 1, min(len_dam,2))),
#         dam_3letter = toupper(substr(AGN, 1, min(len_dam,3))))
#
#FH_dat2 <- FH_dat2 %>%
#  mutate(dam_class = case_when(dam_1letter %in% c('O', '') ~ 'None',
#                               dam_1letter == 'U' ~ 'Unknown',
#                               dam_1letter == 'N' ~ 'Abiotic',
#                               dam_1letter == 'D' ~ 'Disease',
#                               dam_1letter == 'I' ~ 'Insect',
#                               dam_1letter == 'T' ~ 'Treatment',
#                               dam_1letter == 'A' ~ 'Animal',
#                               dam_1letter == 'X' ~ 'Frk_Crk_Btp',
#                               TRUE ~ ''))

FH_dat3 <- FH_dat2 %>%
  mutate(AGN = ifelse(AGN == '', 'O', AGN))

Tree_FH_data <- FH_dat3 %>%
  select(SITE_IDENTIFIER, CLSTR_ID, VISIT_NUMBER, PLOT, TREE_NO, RESIDUAL, MEAS_YR, MEAS_INTENSE,
         TREE_WT, TH_TREE, DBH, HEIGHT, SPECIES, SPC_GRP1, LV_D, S_F, COMP_CHG, WALKTHRU_STATUS, 
         PHF_TREE, VOL_WSV, VOL_MER, VOL_NTWB, AGE_BH, AGE_TOT,
         SI_TREE, SUIT_TR, SUIT_HT, SUIT_SI,
         BA_TREE, ba_ha, vol_ha, DAM_NUM, AGN, SEV, SEVPERC, 
         severity_grp, dam, dam_severity_adj, 
         sev_char, sev_num, sev_class, n, mort_code, mort_flag, AGN_new,
         RESID_GRP, DBH_CLASS, dam_1letter, dam_2letter, dam_3letter,
         dam_class, corr_sev, dam_severity)

### if a tree is identified as a residual in any visit, then it is a residual across all visits.
Tree_FH_data <- Tree_FH_data %>%
  rename(RESIDUAL_old = RESIDUAL) %>%
  group_by(SITE_IDENTIFIER, TREE_NO) %>%
  mutate(RESIDUAL = case_when("Y" %in% RESIDUAL_old ~ "Y", 
                              TRUE ~ RESIDUAL_old)) %>%
  ungroup() %>%
  data.table

Tree_FH_data1 <- Tree_FH_data %>%
  left_join(sample_data7 %>% select(SITE_IDENTIFIER, VISIT_NUMBER, visit_number_new),
            by = c('SITE_IDENTIFIER', 'VISIT_NUMBER')) %>%
  mutate(tree_id = paste0(SITE_IDENTIFIER, "-", PLOT, "-", TREE_NO),
         phf_coc = PHF_TREE,
         resid_coc = RESIDUAL,
         comp_chg_coc = COMP_CHG,
         species_coc = SPECIES,
         lvd_coc = LV_D,
         sf_coc = S_F)

# *run checks across measurements;
### Takes a while..
for (i in unique(Tree_FH_data1$tree_id)){
  
  #max_meas <- max(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new)
  meas_no <- sort(unique(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new))
  
  if (length(meas_no) > 1){
    
    for (j in meas_no){
      
      a1 <- Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                            Tree_FH_data1$DAM_NUM == 1 &
                            Tree_FH_data1$visit_number_new == j, ]
      a2 <- Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                            Tree_FH_data1$DAM_NUM == 1 &
                            Tree_FH_data1$visit_number_new == j + 1, ]
      # *ingress trees;
      if (nrow(a1) == 0){
        Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                        Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- ifelse(a2$lvd_coc == "L", "I", "M")
      } else if (nrow(a2) == 0){
        ## *live at first msmt, missing at second msmt, assign as mortality, and assume dead fallen;
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$comp_chg_coc <- "M"
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$lvd_coc <- "D"
        #Tree_FH_data1[Tree_FH_data1$tree_id == i & 
        #                Tree_FH_data1$visit_number_new == j, ]$sf_coc <- "F"
      } else {
        # *where tree was previously recorded as dead, but subsequently recorded as live, 
        # then believe second measure;
        # *and redefine tree as alive at first measure;
        if (a1$lvd_coc == "D" & a2$lvd_coc == "L"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$lvd_coc <- a2$lvd_coc
        }
        # *for components of change analysis, need to constrain phf to first measure;
        if (!is.na(a2$phf_coc) & a1$phf_coc != a2$phf_coc){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$phf_coc <- a1$phf_coc
        }
        # *fill in residual classification if recorded at one measurement , but not the next;
        # *assign as residual across both measurements;
        if (a1$RESIDUAL != a2$RESIDUAL & (a1$RESIDUAL == "Y" | a2$RESIDUAL == "Y")){
          Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$resid_coc <- "Y"
        }
        # *components of change;
        ### Dead trees;
        if (a1$lvd_coc == "D" & a2$lvd_coc == "D"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "D"
        }
        # *survivor trees;
        if (a1$lvd_coc == "L" & a2$lvd_coc == "L" & a2$sf_coc == "S"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "S"
        }
        # *fallen live, assume this will become mortality;
        if (a1$lvd_coc == "L" & a2$lvd_coc == "L" & a2$sf_coc == "F"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$lvd_coc <- "D"
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "M"
        }
        # *mortality : trees that died between measurements;
        if (a1$LV_D == "L" & a2$lvd_coc == "D"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j + 1, ]$comp_chg_coc <- "M"
        }
        # *if second measure is unknown then use first measure species;
        if (a2$SPECIES == "XC"){
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$species_coc <- a1$SPECIES
          # *otherwise resolve inconsistencies by believing second measure;
        } else if (a1$SPECIES != a2$SPECIES) {
          Tree_FH_data1[Tree_FH_data1$tree_id == i & 
                          Tree_FH_data1$visit_number_new == j, ]$species_coc <- a2$SPECIES
        }
      }
    }
  }
  if (length(meas_no) == 1){
    if (unique(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$visit_number_new) == 1){
      Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc <- ""
    } else {
      Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc <- 
        ifelse(Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$lvd_coc == "L", "I", 
               Tree_FH_data1[Tree_FH_data1$tree_id == i, ]$comp_chg_coc)
    }
  }
}


### Save data for application
saveRDS(Tree_FH_data1, paste0(savepath, "/Tree_FH_data.rds"))


