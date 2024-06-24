library(tidyverse)
library(foreach)
library(data.table)

# Read wetlands table
wetlands <- readr::read_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/project_wetlands_w_flow_class_water_regime.csv") 

# convert it from a tibble to a data.table
wetlands_dt <- wetlands %>% 
  as.data.table()

# group wetlands by whether or not they meet a water regime threshold, then take the minimum flow_class (i.e., hydrographic classification of intersecting stream or water body)
# if it meets the threshold or a value of '6' if it does not
wetlands_dt[, "wr_1_cut_flow_class" := ifelse(water_regime_1_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_1_cut_GROUP_ID)]
wetlands_dt[, "wr_2_cut_flow_class" := ifelse(water_regime_2_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_2_cut_GROUP_ID)]
wetlands_dt[, "wr_3_cut_flow_class" := ifelse(water_regime_3_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_3_cut_GROUP_ID)]
wetlands_dt[, "wr_4_cut_flow_class" := ifelse(water_regime_4_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_4_cut_GROUP_ID)]
wetlands_dt[, "wr_5_cut_flow_class" := ifelse(water_regime_5_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_5_cut_GROUP_ID)]
wetlands_dt[, "wr_6_cut_flow_class" := ifelse(water_regime_6_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_6_cut_GROUP_ID)]
wetlands_dt[, "wr_7_cut_flow_class" := ifelse(water_regime_7_cutoff == 1,  min(flow_class, na.rm=T), 6), by = .(wr_7_cut_GROUP_ID)]


# write propagated wetlands to CSV. After this, run `water_regime_join` tool in arcgis toolbox
wetlands_dt %>% select(wetland_index, 
                       wr_1_cut_flow_class,
                       wr_2_cut_flow_class,
                       wr_3_cut_flow_class,
                       wr_4_cut_flow_class,
                       wr_5_cut_flow_class,
                       wr_6_cut_flow_class,
                       wr_7_cut_flow_class) %>%  
  readr::write_csv("E:/EDF/wotus_wetlands/analysis/results/conus/Tables/project_wetlands_w_flow_class_water_regime_propagated.csv")