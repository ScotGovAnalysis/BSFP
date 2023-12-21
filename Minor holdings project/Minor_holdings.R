setwd("Minor holdings project")
##Import packages
library(tidyverse)
library(haven)
library(data.table)
library(writexl)

#Years of interest. Would ideally extend to 2022 and 2023, but unsure of the right datasets to use.
year_range = 2020:2022
#Use an environment variable to specify the census data paths.
#See https://csgillespie.github.io/efficientR/set-up.html#renviron
census_directory_path <- Sys.getenv("Census_directory_path")
agstemp_path <- Sys.getenv("Agstemp_directory_path")

#Function to check if a dataset exists in the working directory, then either read it if so, or download it then read it.
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
#For loop to 
for (year_loop in year_range){
  #Specify the filename for the current year - format is, e.g, "June21"
  if(year_loop %in% 2020:2021){
    path <- census_directory_path
    census_data_file <- paste0("june", year_loop-2000, ".sas7bdat")
  }
  if(year_loop == 2022){
    path <- agstemp_path
    census_data_file <- paste0("JAC22_draft_22_11_23.sas7bdat")
  }
  if(year_loop == 2023){
    path <- agstemp_path
    census_data_file <- paste0("JAC23_27_10_draft.sas7bdat")
  }
  #Use the import_sas function defined above to read in a single year's census data
  single_year_raw <- import_sas(path, census_data_file)
  single_year <- single_year_raw %>% 
    mutate(
      #total_fruit = soft fruit + orchard fruit
      total_fruit = item36 + item70 + item71 + item72 + item75 + item2832,
      #total grass = grass under 5 years + grass over 5 years
      total_grass = item2321+item2322,
      total_glasshouse = item85 + item86,
      cropyear = year_loop) %>% 
    select(parish, holding, cropyear,
           total_fruit,
           total_grass,
           total_crops_and_fallow = item40,
           total_ornamentals = item82,
           total_glasshouse,
           rough_grazing = item47,
           land_rented_in = item7,
           item50) %>% 
    rowwise() %>% 
    # mutate(total_glasshouse = sum(item85, item86, na.rm=T)) %>% 
    #ornamentals, veg and fruit appear to be included in total_crops_and_fallow already, so don't need to be included in the total here
    mutate(total_croppable_area = sum(total_crops_and_fallow, total_glasshouse, total_grass, na.rm=T)) %>% 
    mutate(total_farmed_area = sum(total_croppable_area, land_rented_in, na.rm=T)) %>% 
    #Add a variable flagging whether total crop and grass area is above or below 20ha
    mutate(major_minor_flag = ifelse(total_croppable_area < 20, "minor", "major"))
  #Append the single year of data to the all_years dataset
  all_years <- all_years %>% 
    bind_rows(single_year)
}

#Summarise the key stats for each year and major/minor group
all_years_summary <- all_years %>% 
  group_by(cropyear, major_minor_flag) %>% 
  summarise(count=n(),
            total_crop_area = sum(total_crops_and_fallow, na.rm=T),
            total_grass_area = sum(total_grass, na.rm=T),
            total_croppable_area = sum(total_croppable_area, na.rm=T),
            total_rough_grazing = sum(rough_grazing, na.rm=T),
            total_glasshouse = sum(total_glasshouse, na.rm=T),
            item50 = sum(item50, na.rm=T),
            total_land_rented_in = sum(land_rented_in, na.rm=T),
            total_farmed_area = sum(total_farmed_area, na.rm=T))
write.csv(all_years_summary, "all_years_summary.csv")

totals <- all_years %>% 
  group_by(cropyear) %>% 
  summarise(count=n(),
            total_crop_area = sum(total_crops_and_fallow, na.rm=T),
            total_grass_area = sum(total_grass, na.rm=T),
            total_farmed_area = sum(total_croppable_area, na.rm=T),
            total_rough_grazing = sum(rough_grazing, na.rm=T),
            total_glass = sum(total_glasshouse, na.rm=T),
            tot_fruit = sum(total_fruit, na.rm=T),
            total_glasshouse = sum(total_glasshouse, na.rm=T),
            item50 = sum(item50, na.rm=T))
