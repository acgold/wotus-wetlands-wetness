library(rvest)
library(tidyverse)
library(sf)
library(arcgisbinding)

############### Set links for NWI webpage #####################
url <- "https://www.fws.gov/program/national-wetlands-inventory/download-state-wetlands-data"
html <- read_html(url)
link_nodes <- html %>% html_nodes(xpath = "//table//a")  
link_urls  <- link_nodes %>% html_attr("href")
download_links <- link_urls[stringr::str_detect(link_urls, "geodatabase")]

#################  Download all NWI datasets available on webpage #########################
options(timeout=60*30)

for(i in 24:length(download_links)){
  download.file(download_links[i], file.path("E:/EDF/wotus_wetlands/data/NWI/states_zip",str_replace(download_links[i], "^.+State-Downloads/","")))
}

############## Extract #####################
directory <- "E:/EDF/wotus_wetlands/data/NWI/states_zip"

files <- list.files(directory)

for(i in 1:length(files)){
  unzip(paste0(directory,"/",files[i]),
                  exdir = "E:/EDF/wotus_wetlands/data/NWI/states")
}

############# Write metadata to single geodatabase #######################
arc.check_product()

directory <- "E:/EDF/wotus_wetlands/data/NWI/states"

files <- list.files(directory)

for(i in 1:length(files)){
  prefix <- str_replace(files[i],"//_.*","")
  print(prefix)
  if(nchar(prefix) == 2 & prefix != "AK" & prefix != "HI"){
    project_metadata <- arc.select(arc.open(file.path(directory,files[i],paste0(prefix,"_Wetlands_Project_Metadata"))))
    
    arc.write(file.path("E:/EDF/wotus_wetlands/data/NWI/combined/nwi_mosaic_metadata.gdb",paste0(prefix, "_Wetlands_Project_Metadata")), project_metadata)   
  }
}





