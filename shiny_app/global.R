###############################################################################
#
# Global script ---- 
#
###############################################################################

# contains :- 

# 1. required packages
# 2. required datafiles
# 3. lists for dashboard filters
# 4. common chart themes
# 5. extra UI components that are not reactive (cookie box/guided tours/updates modal)
# 6. sourcing functions created for app (see functions folder) 


# 1. required packages ----------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(data.table)
library(ggplot2)
#library(plotly)
library(dplyr)
options(dplyr.summarize.inform = FALSE)
library(sf)
library(leaflet)
library(DT)
#library(rmarkdown)
#library(knitr)
library(flextable)
set_flextable_defaults(font.family = "Arial")
library(tibble)
library(tidyverse)
library(scales)
library(gridExtra)
library(waiter)
library(tinytex)
library(kableExtra)
library(pandoc)
library(pagedown)
library(htmltools)
library(webshot)


# 2. required datafiles ------------------------------------------------------------

# main datasets 
sample_data <- readRDS("data/sample_data.rds")
lead_vol <- readRDS("data/lead_vol.rds")
spc_vol <- readRDS("data/spc_vol.rds")
vdyp_grd <- readRDS("data/VDYP_grd.rds")

# shapefiles (for map) 
tsa_sp <- st_transform(st_read("data/tsa_sp.shp"),4326)

# Others
decidspc <- c('A','AC','ACT','ACB','AT',
              'DM','DR','E','EA','EP','EW',
              'MB','MV','KC','RA','V','VB','VP','VV',
              'W','WB','WP','WT','ZH','XH','XC')

lwr_limit = 0.9
upr_limit = 1.1


#3. lists for filter dropdowns ------------------------------------------------------
# for TSA selection
tsa_list <- sort(unique(sample_data %>% 
                          #filter(TSA_filter == "Y") %>% 
                          group_by(TSA_DESC) %>% 
                          filter(n() > 10) %>% 
                          pull(TSA_DESC)))


# 4. chart themes  ----------------------------------------------------------------
theme_set(theme_bw(15, base_family = 'Arial'))

# colour palettes for plots

# common parameters for plots



# 5. extra UI components  ----------------------------------------------------------



# 6. sourcing functions created for app (see functions folder) -------------------------------
pchIcons <- function(pch = 0:14, width = 30, height = 30, ...) {
  n <- length(pch)
  files <- character(n)
  # create a sequence of png images
  for (i in seq_len(n)) {
    f <- tempfile(fileext = ".png")
    png(f, width = width, height = height, bg = "transparent")
    par(mar = c(0, 0, 0, 0))
    plot.new()
    points(.5, .5, pch = pch[i], cex = min(width, height) / 8, ...)
    dev.off()
    files[i] <- f
  }
  files
}


