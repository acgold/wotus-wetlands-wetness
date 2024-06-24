library(sf)
library(tidyverse)

gdb <- "E:/EDF/wotus_wetlands/data/NWI/combined/nwi_clipped_mosaic.gdb"
output_location <- "E:/EDF/wotus_wetlands/analysis/Input/merged_wetlands_conus.gdb"

nwi_code_csv <- readr::read_csv("E:/EDF/wotus_wetlands/data/NWI/NWI-Code-Definitions-May-2023/NWI-Code-Definitions/NWI_Code_Definitions_small.csv")

layers = sf::st_layers(gdb)

for(i in 1:length(layers$name)){
  if(i == 1){
    lyr <- sf::st_read(gdb, layer = layers$name[i]) %>% 
      left_join(nwi_code_csv, by = c("ATTRIBUTE"))
    
    sf::st_write(lyr, output_location, append = F, layer = "merged_wetlands_conus", delete_layer = T)
  }
  if(i > 1){
    lyr <- sf::st_read(gdb, layer = layers$name[i]) %>% 
      left_join(nwi_code_csv, by = c("ATTRIBUTE"))
    
    sf::st_write(lyr, output_location, append = T, layer = "merged_wetlands_conus")
  }
}
