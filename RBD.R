#Code for allocating Scottish farms in the fertiliser survey to river basin districts.
#Do this from their CPH.

input_file <- "RESAS - DATA GOVERNANCE - BSFP 2022 - SAMPLE SENT  - May 2022.xls"
input_file_sheet <- "RESAS Main contacts"
Year <- 2022
Census_address_data_file <- "address_email_04nov21.sas7bdat"

library(sf)
library(tidyverse)
library(haven)
library(readxl)
source("GR Functions.R")
# system("unzip WFDRiverBasinDistrictsCycle2-SHP.zip")

RBDs <- st_read("data/WFD_River_Basin_Districts_Cycle_2.shp") %>% 
  st_set_crs("27700")
  
Solway_Tweed <- RBDs %>% 
  filter(rbd_name=="Solway Tweed")
GB_outline <- st_read("bdline_essh_gb/Data/GB/high_water_polyline.shp") %>% 
  st_set_crs("27700")

# within = lengths(st_intersects(Dummy_data, my_spdf)) > 0
# outwith = !within
# 
# plot(my_spdf$geometry)
# plot(Dummy_data[within], col="blue", pch=19, add=TRUE)
# plot(Dummy_data[outwith], col="red", pch=19, add=TRUE)

#Get the grid references for all census farms
agscensuspath <- '//s0177a/sasdata1/ags/census/agscens/'

Census_address_data <- tryCatch(
  {
    Holdings_data <- read_sas(Census_address_data_file)
  },
  error = function(e)
  {
    file.copy(paste0(agscensuspath, Census_address_data_file), getwd())
    return(read_sas(Census_address_data_file))
  }
)
names(Census_address_data) <- tolower(names(Census_address_data))
for (x in colnames(Census_address_data)){
  attr(Census_address_data[[deparse(as.name(x))]], "format.sas")=NULL
}

Census_address_data_process <- Census_address_data %>% 
  select(parish, holding, grid_reference) %>% 
  mutate(grid_reference = gsub(" ","",grid_reference)) %>% 
  filter(grid_reference != "",
         substr(grid_reference, 1, 1) %in% c("N","H"),
         substr(grid_reference,2,2) %in% LETTERS) %>% 
  mutate(letters=substr(grid_reference,1,2))
Census_address_data_process <- Census_address_data_process %>% 
  mutate(eastings=osg_parse(grid_reference)$easting,
         northings = osg_parse(grid_reference)$northing)


#Read in the CPH data for the farms in the survey sample
BSFP_sample_raw <- read_xls(input_file, sheet=input_file_sheet) %>% 
  mutate(parish=as.numeric(substr(cph,3,5)),
         holding=as.numeric(substr(cph,6,9)))
BSFP_sample <- BSFP_sample_raw %>% 
  select(parish, holding, cph) %>% 
  inner_join(Census_address_data_process, by=c("parish", "holding"))





#Check and plot holdings within/outiwth Solway/Tweed
BSFP_geometry <- st_as_sf(BSFP_sample, coords= c("eastings", "northings")) %>% 
  st_geometry() %>% 
  st_set_crs("27700")
Solway_Tweed <- st_set_crs(Solway_Tweed, "27700")

within = lengths(st_intersects(BSFP_geometry, Solway_Tweed)) > 0
outwith = !within
BSFP_Scotland_RBD <- BSFP_sample %>% 
  mutate(RBD = within)
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="TRUE"]="Solway-Tweed"
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="FALSE"]="Scotland"
BSFP_Scotland_RBD_crs <- BSFP_Scotland_RBD %>% 
  st_as_sf(coords = c("eastings", "northings")) %>% 
  st_geometry()

plot(GB_outline$geometry, ylim=c(525000,1220531))
plot(RBDs$geometry, add=TRUE)

plot(BSFP_Scotland_RBD_crs[within], col="blue", pch=19, add=TRUE)
plot(BSFP_Scotland_RBD_crs[outwith], col="red", pch=19, add=TRUE)


#Create final table for sending to Defra, and export as csv
BSFP_Scotland_RBD_Defra <- BSFP_Scotland_RBD %>% 
  select(cph, RBD)
write.csv(BSFP_Scotland_RBD_Defra, file=paste0("BSFP_Scotland_RBD_",Year,".csv"), row.names = FALSE)

Grid_ref_QA <- BSFP_sample %>% 
  filter(grid_reference=="NN000000")
Missing_data <- BSFP_sample_raw %>% 
  anti_join(BSFP_sample, by=c("parish", "holding")) %>% 
  bind_rows(Grid_ref_QA) %>% 
  left_join(Census_address_data, by=c("parish", "holding"))
  

if(nrow(Missing_data>0)){
  cat("Manually check the locations of farms in the Missing_data data frame.")
  cat("Either they are missing from the census address file, or the census says their grid reference is NN000000, which is somewhat unlikely!")
  cat("Check, and add/edit the output csv directly before sending to Defra")
}
