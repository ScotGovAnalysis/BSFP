#Code for allocating Scottish farms in the fertiliser survey to river basin districts.
#Do this from their CPH.
setwd("C:/Users/u449906/Documents/R/repos/BSFP/RBD project")

#Manual inputs. 
#Specify the file, and tabs, with the lists of CPHs included in the survey. Ask the census team for this file.
#Input file needs to be an xlsx. Manually re-save if this isn't the case.
input_file <- "Input data/RESAS - DATA GOVERNANCE - BSFP 2022 - SAMPLE SENT  - May 2022.xlsx"
output_file <- "Output data/RESAS - DATA GOVERNANCE - BSFP 2022 - SAMPLE SENT  - May 2022.xlsx"
input_file_sheets <- c("RESAS Main contacts", "RESAS Reserve 1","RESAS Reserve 2","RESAS Reserve 3")
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
#The file came from here: https://www.data.gov.uk/dataset/368ae5fb-65a1-4f19-98ff-a06a1b86b3fe/wfd-river-basin-districts-cycle-2
RBDs <- st_read("Input data/Shapefiles/WFD_River_Basin_Districts_Cycle_2.shp") %>% 
  #27700 is the coordinate reference system ID for the OS grid.
  st_set_crs("27700")

#Single out the Solway-Tweed RBD.  
Solway_Tweed <- RBDs %>% 
  filter(rbd_name=="Solway Tweed") %>% 
  st_set_crs("27700")

#For plotting the map at the end, read in an outline of Great Britain.
# GB_outline <- st_read("Input data/Shapefiles/CTRY_DEC_2022_GB_BFC_v2_c.shp")%>% 
#   st_set_crs("27700")
#Similarly, Scotland outline (file from:https://geoportal.statistics.gov.uk/datasets/ons::nuts1-jan-2018-ultra-generalised-clipped-boundaries-in-the-uk/explore) :
Scotland_outline <- st_read("Input data/Shapefiles/NUTS1_Jan_2018_UGCB_in_the_UK.shp") %>% 
  filter(nuts118nm=="Scotland") %>% 
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
#Do for each input sheet (main/reserves) in turn, and append to main list
BSFP_sample_raw <- NULL
for (input_sheet in input_file_sheets){
  BSFP_sample_raw_single <- read_xls(input_file, sheet=input_sheet)
  #Get parish and holding from the CPH number
  if("reservecph" %in% colnames(BSFP_sample_raw_single)){
    BSFP_sample_raw_single <- BSFP_sample_raw_single %>% 
    mutate(parish=as.numeric(substr(reservecph,3,5)),
           holding=as.numeric(substr(reservecph,6,9)),
           cph=reservecph,
           main_reserve="reserve")
  } else {
    BSFP_sample_raw_single <- BSFP_sample_raw_single %>% 
    mutate(parish=as.numeric(substr(cph,3,5)),
           holding=as.numeric(substr(cph,6,9)),
           main_reserve="main")
  }
  BSFP_sample_raw <- BSFP_sample_raw %>% 
    bind_rows(BSFP_sample_raw_single)
}

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


#Create final table for sending to Defra, and export as csv
BSFP_Scotland_RBD_Defra <- BSFP_Scotland_RBD %>% 
  select(cph, RBD)
#Add the farms with missing grid reference data, defaulting to "Scotland" RBD
Missing_data_add <- BSFP_sample_raw %>% 
  anti_join(BSFP_sample, by=c("parish", "holding")) %>% 
  select(cph) %>% 
  mutate(RBD="Scotland") 
BSFP_Scotland_RBD_Defra <- BSFP_Scotland_RBD_Defra %>% 
  bind_rows(Missing_data_add)
write.csv(BSFP_Scotland_RBD_Defra, file=paste0("Output data/BSFP_Scotland_RBD_",Year,".csv"), row.names = FALSE)


#For QA - find holdings with the grid reference NN000000, which seems to be a default option somewhere.
Grid_ref_QA <- BSFP_sample %>% 
  filter(grid_reference=="NN000000")
#Find farms not within the GB_outline boundary
in_Scotland = lengths(st_intersects(BSFP_geometry, Scotland_outline)) > 0
not_in_Scotland = !in_Scotland
outwith_Scotland_QA <- BSFP_sample %>% 
  mutate(outwith_Scotland=not_in_Scotland) %>% 
  filter(outwith_Scotland==TRUE)
# Add in any farms with no grid reference, or badly formatted grid reference, or which may be in the sea in the census dataset
Missing_data <- BSFP_sample_raw %>% 
  anti_join(BSFP_sample, by=c("parish", "holding")) %>% 
  bind_rows(Grid_ref_QA) %>%
  bind_rows(outwith_Scotland_QA) %>% 
  left_join(Census_address_data, by=c("parish", "holding"))

#Plot a map of Scotland, then add the RBD boundaries and BSFP holdings. Colour code holdings based on RBD.
plot(Scotland_outline$geometry)
plot(Solway_Tweed$geometry, type="l", col="green", add=TRUE)
plot(BSFP_Scotland_RBD_crs[within], col="blue", pch=19, add=TRUE)
plot(BSFP_Scotland_RBD_crs[outwith], col="orange", pch=19, add=TRUE)
plot(BSFP_Scotland_RBD_crs[not_in_Scotland], col="red", pch=19, add=TRUE)

#Print a message if there are any manual checks needed
if(nrow(Missing_data>0)){
  message("Manually check the locations of farms in the Missing_data data frame.")
  message("Either they are missing from the census address file, or the census says their grid reference is NN000000 or it is in the sea somewhere which is somewhat unlikely!")
  message("Check, and edit the output csv directly before sending to Defra")
}

#Create the actual output file by loading the input file, appending the RBD sheet and writing to the output folder
workbook <- loadWorkbook(input_file)
addWorksheet(wb=workbook, "RBDs")
writeData(workbook, "RBDs", BSFP_Scotland_RBD_Defra)
saveWorkbook(wb = workbook, file = output_file, overwrite = TRUE)
