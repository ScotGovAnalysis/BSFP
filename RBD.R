#Code for allocating Scottish farms in the fertiliser survey to river basin districts.
#Do this from their CPH.


library(sf)
library(tidyverse)
library(haven)
library(readxl)
source("GR Functions.R")
system("unzip WFDRiverBasinDistrictsCycle2-SHP.zip")

my_spdf <- st_read("data/WFD_River_Basin_Districts_Cycle_2.shp") 
Solway_Tweed <- my_spdf %>% 
  filter(rbd_name=="Solway Tweed")
outline <- st_read("bdline_essh_gb/Data/GB/high_water_polyline.shp")

Dummy_data<- st_set_crs(Dummy_data, "27700")
my_spdf <- st_set_crs(my_spdf, "27700")
outline <- st_set_crs(outline, "27700")

within = lengths(st_intersects(Dummy_data, my_spdf)) > 0
outwith = !within

plot(my_spdf$geometry)
plot(Dummy_data[within], col="blue", pch=19, add=TRUE)
plot(Dummy_data[outwith], col="red", pch=19, add=TRUE)

#Get the grid references for all census farms
agscensuspath <- '//s0177a/sasdata1/ags/census/agscens/'
Census_address_data_file <- "address_email_30May23.sas7bdat"
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
  mutate(letters=substr(grid_reference,1,2)) %>% 
  # group_by(letters) %>% 
  # summarise(count=n())
  # filter(parish==891,
  #        holding==418) %>%
  # rowwise() %>% 
  mutate(eastings=osg_parse(grid_reference)$easting,
         northings = osg_parse(grid_reference)$northing)


#Read in the CPH data for the farms in the survey sample
BSFP_sample <- read_xls("2023 BSFP Scotland request for addresses (populated by RESAS).xls", sheet="MainContacts") %>% 
  mutate(parish=as.numeric(substr(cph,3,5)),
         holding=as.numeric(substr(cph,6,9))) %>% 
  select(cph, parish, holding)


#Cut down census dataset to only farms in the BSFP sample
Census_address_data_process <- Census_address_data_process %>% 
  inner_join(BSFP_sample, by=c("parish", "holding"))

Missing_data_check <- Census_address_data_process %>% 
  filter(grid_reference=="NN000000") %>% 
  left_join(Census_address_data, by=c("parish", "holding"))

check <- Census_address_data %>% 
  filter(parish==35, holding==20)

if(nrow(Missing_data_check>0)){
cat("Manually check the locations of farms in the Missing_data_check data frame. The census says their grid reference is NN000000, which is somewhat unlikely!")
}



#Check and plot holdings within/outiwth Solway/Tweed
Dummy_data <- st_as_sf(Census_address_data_process, coords= c("eastings", "northings"))
Dummy_data <- st_geometry(Dummy_data)

Dummy_data<- st_set_crs(Dummy_data, "27700")
Solway_Tweed <- st_set_crs(Solway_Tweed, "27700")

within = lengths(st_intersects(Dummy_data, Solway_Tweed)) > 0
outwith = !within
BSFP_Scotland_RBD <- Census_address_data_process %>% 
  mutate(RBD = within)
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="TRUE"]="Solway-Tweed"
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="FALSE"]="Scotland"
BSFP_Scotland_RBD_crs <- st_as_sf(BSFP_Scotland_RBD, coords = c("eastings", "northings"))
BSFP_Scotland_RBD_crs <- st_geometry(BSFP_Scotland_RBD_crs)

plot(outline$geometry)
plot(my_spdf$geometry)

plot(BSFP_Scotland_RBD_crs[within], col="blue", pch=19, add=TRUE)
plot(BSFP_Scotland_RBD_crs[outwith], col="red", pch=19, add=TRUE)


#Create final table for sending to Defra, and export as csv
BSFP_Scotland_RBD_Defra <- BSFP_Scotland_RBD %>% 
  select(cph, RBD)
write.csv(BSFP_Scotland_RBD, file="BSFP_Scotland_RBD.csv")
