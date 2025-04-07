
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
    MGMT_UNIT == "TSA18_Merritt" & SAMPLE_ESTABLISHMENT_TYPE == "CMI" ~ 'popn_conflict',
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
                              substr(MGMT_UNIT, 1, 5) == "TSA22" & substr(SAMPLE_SITE_NAME, 1, 4) != '4721' ~ "Y",
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
  filter(Design == "GRID" | (Design == "PHASE2" & vri_in == "Y")) %>%
  filter(LAST_MSMT == "Y")


sample_data5 <- sample_data4 %>%
  group_by(MGMT_UNIT, Design) %>%
  filter(n() >= 8) %>%
  ungroup()

sample_data5 <- sample_data5 %>%
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
  mutate(MEAS_YR = ifelse(!is.na(fire_year) & fire_year >= MEAS_YR, fire_year, MEAS_YR))


sample_data7 <- sample_data6 %>%
  select(MGMT_UNIT, SITE_IDENTIFIER, VISIT_NUMBER, CLSTR_ID, MEAS_YR,
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

saveRDS(sample_data7, "D:/R/mature_inventory_rep/shiny_app/data/sample_data.rds")



### Projected inventory
external_path2 <- "//sfp.idir.bcgov/s164/S63016/!Workgrp/Inventory/Compilation/ismc/external_inputs"

VDYP_all <- fread(paste0(external_path2, 
                         "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_OUTPUT_YLDTBL_old.csv"))

VDYP_input <- fread(paste0(external_path2, 
                           "/spatial_overlay/ISMC_VRI_Overlay/VDYP7/VDYP7_INPUT_LAYER.csv"))

# Remove residual
VDYP_input1_1 <- VDYP_input %>%
  filter(VDYP7_LAYER_CD == "P") 

VDYP_all <- VDYP_all %>%
  left_join(VDYP_input1_1 %>% select(FEATURE_ID, LAYER_ID = LAYER_LEVEL_CODE), 
            by = c("FEATURE_ID", "LAYER_ID"))

VDYP_proj <- VDYP_all %>%
  filter(FEATURE_ID %in% unique(sample_data7$FEATURE_ID_2022))


VDYP_proj1 <- VDYP_proj %>%
  left_join(sample_data7 %>% select(FEATURE_ID_vegcomp, CLSTR_ID, BEC_ZONE, PROJ_AGE_ADJ),
            by = c("FEATURE_ID" = "FEATURE_ID_vegcomp"))

VDYP_proj2 <- VDYP_proj1 %>% 
  select(CLSTR_ID, BEC_ZONE, FEATURE_ID, LAYER_ID, SPECIES_1_CODE, 
         PROJ_AGE_ADJ, PRJ_BA, PRJ_TPH, PRJ_TOTAL_AGE, PRJ_DOM_HT, PRJ_VOL_DWB) %>%
  mutate_at(vars(PRJ_BA, PRJ_TPH, PRJ_DOM_HT, PRJ_VOL_DWB), ~replace(., is.na(.), 0))

vdyp_year <- VDYP_proj2 %>% 
  filter(LAYER_ID != "D") %>% 
  expand(nesting(FEATURE_ID, LAYER_ID), PRJ_TOTAL_AGE = full_seq(PRJ_TOTAL_AGE, 1))

VDYP_proj3 <- vdyp_year %>%
  left_join(VDYP_proj2, by = c("FEATURE_ID", "LAYER_ID", "PRJ_TOTAL_AGE")) 

VDYP_proj4 <- VDYP_proj3 %>% 
  group_by(FEATURE_ID, LAYER_ID) %>% 
  mutate(CLSTR_ID = zoo::na.locf(CLSTR_ID, na.rm = FALSE),
         BEC_ZONE = zoo::na.locf(BEC_ZONE, na.rm = FALSE),
         SPECIES_1_CODE = zoo::na.locf(SPECIES_1_CODE, na.rm = FALSE),
         PROJ_AGE_ADJ = zoo::na.locf(PROJ_AGE_ADJ, na.rm = FALSE),
         vdyp_ba = zoo::na.approx(PRJ_BA, PRJ_TOTAL_AGE, rule = 2),
         vdyp_tph = zoo::na.approx(PRJ_TPH, PRJ_TOTAL_AGE, rule = 2),
         vdyp_dom_ht = zoo::na.approx(PRJ_DOM_HT, PRJ_TOTAL_AGE, rule = 2),
         vdyp_vol_dwb = zoo::na.approx(PRJ_VOL_DWB, PRJ_TOTAL_AGE, rule = 2)) %>% 
  ungroup()

saveRDS(VDYP_proj4, "D:/R/mature_inventory/files/rds/VDYP_proj_all.rds")

#VDYP_proj5 <- VDYP_proj4 %>%
#  filter(!is.na(CLSTR_ID)) %>%
#  filter(ifelse(!is.na(PROJ_AGE_ADJ) & PROJ_AGE_ADJ < 526, PROJ_AGE_ADJ == PRJ_TOTAL_AGE, PRJ_TOTAL_AGE == 526))

VDYP_proj5 <- VDYP_proj4 %>%
  filter(!is.na(CLSTR_ID), PROJ_AGE_ADJ == PRJ_TOTAL_AGE) %>%
  select(-PRJ_BA, -PRJ_TPH, -PRJ_DOM_HT, -PRJ_VOL_DWB)

VDYP_proj6 <- VDYP_proj5 %>%
  group_by(CLSTR_ID, FEATURE_ID) %>%
  arrange(desc(LAYER_ID)) %>%
  slice(1)

VDYP_proj6 <- VDYP_proj6  %>%
  rowwise() %>%
  #*further adjustments, corrections to species codes based on bec zone & expected outcome;
  # *bec coast vs interior for species corrections;
  mutate(bec_i_c = ifelse(BEC_ZONE %in% c('CWH','CDF','MH','CMA'), "C", "I"), 
         SPECIES_INV = SPECIES_1_CODE,
         SPECIES_INV = case_when(SPECIES_1_CODE == "A" ~ "AT",
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
         SPC_GRP2 = ifelse(SPECIES_INV %in% decidspc, 'DE', SPECIES_INV)#,
         #LIVE_VOL_PER_HA = ifelse(SPECIES == "PL", LIVE_VOL_PER_HA_125, LIVE_VOL_PER_HA_175),
         #DEAD_VOL_PER_HA = ifelse(SPECIES == "PL", DEAD_VOL_PER_HA_125, DEAD_VOL_PER_HA_175),
         #source = "Inventory"
  ) %>%
  as.data.table()


saveRDS(VDYP_proj6, "D:/R/mature_inventory_rep/shiny_app/data/VDYP_proj.rds")



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


saveRDS(grd_by_sp4, "D:/R/mature_inventory_rep/shiny_app/data/VDYP_proj.rds")
saveRDS(grd_by_sp3, "D:/R/mature_inventory_rep/shiny_app/data/lead_vol.rds")


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

saveRDS(vol_comp, "D:/R/mature_inventory_rep/shiny_app/data/spc_vol.rds")



VDYP_grd <- VDYP_grd %>%
  filter(CLSTR_ID %in% sample_data7$CLSTR_ID)

saveRDS(VDYP_grd, "D:/R/mature_inventory_rep/shiny_app/data/VDYP_grd.rds")




grdspc_vol1 <- grdspc_vol %>%
  left_join(sample_data7 %>% select(MGMT_UNIT, CLSTR_ID, Design), by = "CLSTR_ID") %>%
  group_by(MGMT_UNIT, Design, SPECIES) %>%
  summarise(livevol = sum(VHA_NTWB_NVAF_LS, na.rm = T),
            deadvol = sum(VHA_NTWB_NVAF_DS, na.rm = T)) %>%
  ungroup() %>%
  group_by(MGMT_UNIT, Design) %>%
  reframe(#n = n(),
    SPECIES = SPECIES,
    livevolperc = livevol/sum(livevol, na.rm= T) * 100,
    deadvolperc = deadvol/sum(deadvol, na.rm= T) * 100,
    source = "Ground") %>%
  data.table

inv_vol1 <- inv_vol %>%
  group_by(MGMT_UNIT, Design, SPECIES) %>%
  summarise(livevol = sum(LIVE_VOL_PER_HA, na.rm = T),
            deadvol = sum(DEAD_VOL_PER_HA_175, na.rm = T)) %>%
  ungroup() %>%
  group_by(MGMT_UNIT, Design) %>%
  reframe(#n = n(),
    SPECIES = SPECIES,
    livevolperc = livevol/sum(livevol, na.rm= T) * 100,
    deadvolperc = deadvol/sum(deadvol, na.rm= T) * 100,
    source = "Inventory") %>%
  data.table


vol_comp <- rbind(grdspc_vol1, inv_vol1)
