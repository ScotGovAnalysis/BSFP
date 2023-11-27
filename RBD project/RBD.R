#Code for allocating Scottish farms in the fertiliser survey to river basin districts.
#Do this from their CPH.

#Manual inputs. 
#Specify the file, and tab, with the list of CPHs included in the survey.
input_file <- "RESAS - DATA GOVERNANCE - BSFP 2022 - SAMPLE SENT  - May 2022.xls"
input_file_sheet <- "RESAS Main contacts"
#Specify the year of the sample. This is used in naming the output CSV.
Year <- 2022
#Specify the census SAS dataset used to get the grid references.
agscensuspath <- '//s0177a/sasdata1/ags/census/agscens/'
Census_address_data_file <- "address_email_04nov21.sas7bdat"

##Packages used. sf is for GIS-style work.
library(sf)
library(tidyverse)
library(haven)
library(readxl)
#Call in the functions used for tidying grid references.
source("GR Functions.R")

#Read the shapefile with the boundaries of the river basin districts
RBDs <- st_read("data/WFD_River_Basin_Districts_Cycle_2.shp") %>% 
  #27700 is the coordinate reference system ID for the OS grid.
  st_set_crs("27700")

#Single out the Solway-Tweed RBD.  
Solway_Tweed <- RBDs %>% 
  filter(rbd_name=="Solway Tweed") %>% 
  st_set_crs("27700")

#For plotting the map at the end, read in an outline of Great Britain.
GB_outline <- st_read("bdline_essh_gb/Data/GB/high_water_polyline.shp") %>% 
  st_set_crs("27700")


##Get the grid references for all census farms
#Read in the census address file. First check if it's in the working directory, and then download it if it isn't.
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
#Cleaning of column names
names(Census_address_data) <- tolower(names(Census_address_data))
for (x in colnames(Census_address_data)){
  attr(Census_address_data[[deparse(as.name(x))]], "format.sas")=NULL
}

#Process the census address file. Fix formatting of grid references (remove spaces), and remove holdings with bad data.
Census_address_data_process <- Census_address_data %>% 
  select(parish, holding, grid_reference) %>% 
  mutate(grid_reference = gsub(" ","",grid_reference)) %>% 
  filter(grid_reference != "",
         substr(grid_reference, 1, 1) %in% c("N","H"),
         substr(grid_reference,2,2) %in% LETTERS)
#Calculate eastings and northings from the grid reference. This uses the function in the GR functions script.
Census_address_data_process <- Census_address_data_process %>% 
  mutate(eastings = osg_parse(grid_reference)$easting,
         northings = osg_parse(grid_reference)$northing)


#Read in the CPH data for the farms in the survey sample
BSFP_sample_raw <- read_xls(input_file, sheet=input_file_sheet) %>% 
  #Get parish and holding from the CPH number
  mutate(parish=as.numeric(substr(cph,3,5)),
         holding=as.numeric(substr(cph,6,9)))
#Join the BSFP sample with the census address. Inner, rather than left, join so that holdings with missing data aren't included.
#We'll need to check missing data holdings manually later.
BSFP_sample <- BSFP_sample_raw %>% 
  select(parish, holding, cph) %>% 
  inner_join(Census_address_data_process, by=c("parish", "holding"))





##Check and plot holdings within/outiwth Solway/Tweed
#Use functions from the st package to format the eastings and northings into the appropriate format.
BSFP_geometry <- st_as_sf(BSFP_sample, coords= c("eastings", "northings")) %>% 
  st_geometry() %>% 
  st_set_crs("27700")

#Find BSFP holdings which are inside the Solway-Tweed RBD boundary
within = lengths(st_intersects(BSFP_geometry, Solway_Tweed)) > 0
# Define all other farms as outside of the Solway-Tweed boundary
outwith = !within
#Add the RBD information as a column (either True or False)
BSFP_Scotland_RBD <- BSFP_sample %>% 
  mutate(RBD = within)
#Change the True/False to Solway-Tweed/Scotland
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="TRUE"]="Solway-Tweed"
BSFP_Scotland_RBD$RBD[BSFP_Scotland_RBD$RBD=="FALSE"]="Scotland"
#Create a new dataframe with a plottable format
BSFP_Scotland_RBD_crs <- BSFP_Scotland_RBD %>% 
  st_as_sf(coords = c("eastings", "northings")) %>% 
  st_geometry()

#Plot a map of Scotland, then add the RBD boundaries and BSFP holdings. Colour code holdings based on RBD.
plot(GB_outline$geometry, ylim=c(525000,1220531))
plot(RBDs$geometry, add=TRUE)
plot(BSFP_Scotland_RBD_crs[within], col="blue", pch=19, add=TRUE)
plot(BSFP_Scotland_RBD_crs[outwith], col="red", pch=19, add=TRUE)

#Create final table for sending to Defra, and export as csv
BSFP_Scotland_RBD_Defra <- BSFP_Scotland_RBD %>% 
  select(cph, RBD)
write.csv(BSFP_Scotland_RBD_Defra, file=paste0("BSFP_Scotland_RBD_",Year,".csv"), row.names = FALSE)

#For QA - find holdings with the grid reference NN000000, which seems to be a default option somewhere.
Grid_ref_QA <- BSFP_sample %>% 
  filter(grid_reference=="NN000000")
# Add in any farms with no grid reference, or badly formatted grid reference, in the census dataset
Missing_data <- BSFP_sample_raw %>% 
  anti_join(BSFP_sample, by=c("parish", "holding")) %>% 
  bind_rows(Grid_ref_QA) %>% 
  left_join(Census_address_data, by=c("parish", "holding"))
  
#Print a message if there are any manual checks needed
if(nrow(Missing_data>0)){
  message("Manually check the locations of farms in the Missing_data data frame.")
  message("Either they are missing from the census address file, or the census says their grid reference is NN000000, which is somewhat unlikely!")
  message("Check, and add/edit the output csv directly before sending to Defra")
}
