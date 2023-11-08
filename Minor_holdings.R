##Import packages
library(tidyverse)
library(haven)
library(data.table)
library(writexl)


year_range = 2020:2021
census_directory_path <- '//s0177a/sasdata1/ags/census/agscens/'

#Function to check if a dataset exists in the working directory, then either import it if so, or download it then import it.
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
# Function which calls above function, then does minor cleaning
import_sas <-function(directory_path, filename) {
  dataset <- read_sas2(directory_path, filename)
  names(dataset) <- tolower(names(dataset))
  for (var in colnames(dataset)) {
    attr(dataset[[deparse(as.name(var))]],"format.sas")=NULL
  }
  return(dataset)
}

#Create all_years variable, which will be appended to in the for loop.
all_years <- NULL
for (year in year_range){
    census_data_file <- paste0("june", year-2000, ".sas7bdat")
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
