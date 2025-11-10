############################################
###### Script to clean DURIN WP4 data ######
############################################

# --- Vegetation measurements in control and removal plots ---- #

# Install and load packages
packages <- c("tidyverse")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Set wd
wd <- getwd()
setwd(wd)

# File specifications
## Site names
sites_name <- c("LYGRA","SOGNDAL","SENJA","KAUTOKEINO")

## File paths for each site ##To be updated with download from OSF ----------------------------
file_paths <- sprintf(
  "raw_data/DURIN_WP4_raw_4Corners_%s_field_traits_dwarf_shrubs_forbs_gram_summer_2025.csv",
  sites_name
)

## Give the vector the same names as the sites
names(file_paths) <- sites_name

## Column types
col_spec <- cols(
  year = col_integer(),
  date = col_date(format = ""),
  date_2 = col_date(format = ""),
  site_name = col_character(),
  siteID = col_character(),
  habitat = col_character(),
  plotID = col_character(),
  recorder = col_character(),
  weather = col_character(),
  species = col_character(),
  speciesID = col_character(),
  plant_nr = col_integer(),
  control_top_height = col_double(),
  control_bottom_height = col_double(),
  control_stem_length = col_double(),
  control_stem_diameter = col_double(),
  comments_1 = col_character(),
  removal_top_height = col_double(),
  removal_bottom_height = col_double(),
  removal_stem_length = col_double(),
  removal_stem_diameter = col_double(),
  comments_2 = col_character()
)

#column_names <- c("year","date","date_2","site_name","siteID","habitat","plotID","recorder","weather","species","speciesID","plant_nr","control_top_height","control_bottom_height","control_stem_length","control_stem_diameter","comments_1","removal_top_height","removal_bottom_height","removal_stem_length","removal_stem_diameter","comments_2")

# Read the files into a named list of tibbles 
vegetation_data_list <- file_paths %>%
  map(~ read_csv(.x,col_types= col_spec)) #reads each CSV

# Inspect ---------------------------------------------------------------
str(vegetation_data_list)   # shows a list of tibbles, each named by site

#Quick check on database
View(vegetation_data_list$LYGRA)










#Deal with data format
#Deal with NA's --> think it's ok

#Add a column with flag (outlier data in HEIGHT/DIAM/LENGTH) for removal and control plots --> deal with 'JR encoding'

#Move comments columns and rename after "comment_control" and "comment_removal"

#Replace empty cell by NA cells --> should be ok

#Check typos
##Data format OK 
##Decimal separator OK


