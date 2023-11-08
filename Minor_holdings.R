##Import packages
library(tidyverse)
library(haven)
library(data.table)
library(writexl)

#Function to apply name formats for farm types
# apply_type_formats <- function(table_name) {
#   setkey(setDT(table_name),census_type)
#   table_name[setDT(census_type_tab),farmtype:=i.census_type_words]
#   return(table_name)
# }
#Variables for farmtype names and numbering
# fbs_type_numbers <- c(1:9)
# fbs_type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed","All farm types")
# fbs_type_tab <- data.frame(fbs_type_numbers, fbs_type_words)
# census_type_numbers <- c(1:16)
# census_type_words <- c("Cereals","General Cropping","Dairy","LFA Sheep","LFA Cattle","LFA Cattle and Sheep","Lowland Livestock","Mixed",
#                        "Specialist horticulture & permanent crops", "Specialist pigs", "Specialist poultry", "General cropping - forage", 
#                        "Unclassified", "All farm types", "FBS farm types only", "FBS farm types only, and meeting FBS thresholds")
# census_type_tab <- data.frame(census_type_numbers, census_type_words)

#Identify the locations and names of the datasets to be imported

# year = 2021
year_range = 2020:2021
# desired_variables <- c("item40")
census_directory_path <- '//s0177a/sasdata1/ags/census/agscens/'
# FBS_directory_path <- '//s0177a/sasdata1/ags/fas/'
# census_data_file <- paste0("june",year,".sas7bdat")
# FBS_data_file <- paste0("so_y", datayear, "_fa",".sas7bdat")

## Read in the census data for the relevant crop year. The code first looks in the project folder and reads in the data from there if the
## file has already been downloaded. If it's not found there, it copies the file from the SAS drive then reads it in.
read_sas2 <- function(directory, filename){
  census_data <- tryCatch(
    {
      census_data <- read_sas(filename)
    },
    error = function(e)
    {
      file.copy(paste0(directory, filename), getwd())
      return(read_sas(filename))
    }
  )
}
import_sas <-function(directory_path, filename) {
  dataset <- read_sas2(directory_path, filename)
  names(dataset) <- tolower(names(dataset))
  for (var in colnames(dataset)) {
    attr(dataset[[deparse(as.name(var))]],"format.sas")=NULL
  }
  return(dataset)
}
# check <- import_sas(census_directory_path, census_data_file)

all_years <- NULL
for (year in year_range){
    census_data_file <- paste0("june", year, ".sas7bdat")
    single_year <- import_sas(census_directory_path, census_data_file) %>% 
      mutate(total_fruit = item36 + item37,
             total_grass = item2321+item2322,
             cropyear = year) %>% 
      select(parish, holding, cropyear,
             total_crops_and_fallow = item40,
             total_fruit,
             total_ornamentals = item82,
             total_glasshouse = item1943,
             total_grass,
             rough_grazing = item47,
             cgarea) %>% 
      rowwise() %>% 
      mutate(total_relevant_area = sum(total_crops_and_fallow, total_fruit, total_glasshouse, total_grass, na.rm=T)) %>% 
      mutate(major_minor_flag = ifelse(total_relevant_area < 20, "minor", "major"))
    all_years <- all_years %>% 
      bind_rows(single_year)
}

all_years_summary <- all_years %>% 
  group_by(cropyear, major_minor_flag) %>% 
  summarise(count=n(),
            total_crop_area = sum(total_crops_and_fallow, na.rm=T),
            total_grass_area = sum(total_grass, na.rm=T),
            total_farmed_area = sum(total_relevant_area, na.rm=T),
            total_rough_grazing = sum(rough_grazing, na.rm=T))
