######################################################
###### Script to analyse WP4 DURIN Biomass data ######
######################################################
# --- Biomass weighing measurements ---- #

# Install and load packages
packages <- c("tidyverse") #add any other packages that you may need to this list
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Set working directory
wd <- getwd()
setwd(wd)

# File specifications
## Path to the csv file #Check that it matches what you downloaded!
file_path <- "../raw_data/DURIN_WP_4_raw_4Corners_lab_biomass_weighing_dwarf_shrubs_2025.csv" #As you can see, I exported it as a csv file. You may want to work with Excel if you prefer. That would change some lines in the script.
## Define column names
col_names <- c('recorder_sep','date_sep','time_begin_sep','time_end_sep','site_name','siteID','habitat','PlotID','IndividualID','SpeciesID','Bag_code','IN_OUT_parts','brown_stems_in','brown_stems_out',
               'sep_status','dried_status','recorder_weighing','date_weighing','weighed_status','biomass_brown_stem_in_1','biomass_brown_stem_in_2','biomass_brown_stem_in_3','biomass_brown_stem_out',
               'biomass_green_stem_in_1','biomass_green_stem_in_2','biomass_green_stem_in_3','biomass_green_stem_out','biomass_alive_leaves_in_1','biomass_alive_leaves_in_2','biomass_alive_leaves_in_3',
               'biomass_alive_leaves_in_4','biomass_alive_leaves_in_5','biomass_alive_leaves_in_6','biomass_alive_leaves_out','biomass_dead_leaves_in','biomass_dead_leaves_out','biomass_berries_in',
               'biomass_berries_out','comment_sep','comment_weighing')
## Define column types 
col_spec <- cols(
  recorder_sep = col_character(),
  date_sep = col_date(format = ""),
  time_begin_sep = col_time(format = ""),
  time_end_sep = col_time(format = ""),
  site_name = col_character(),
  siteID = col_character(),
  habitat = col_character(),
  PlotID = col_character(),
  IndividualID = col_character(),
  SpeciesID = col_character(),
  Bag_code = col_character(),
  IN_OUT_parts = col_character(),
  brown_stems_in = col_integer(),
  brown_stems_out = col_integer(),
  sep_status =  col_integer(),
  dried_status = col_integer(),
  recorder_weighing = col_character(),
  date_weighing = col_date(format = ""),
  weighed_status = col_integer(),
  biomass_brown_stem_in_1 = col_double(),
  biomass_brown_stem_in_2 = col_double(),
  biomass_brown_stem_in_3 = col_double(),
  biomass_brown_stem_out = col_double(),
  biomass_green_stem_in_1 = col_double(),
  biomass_green_stem_in_2 = col_double(),
  biomass_green_stem_in_3 = col_double(),
  biomass_green_stem_out = col_double(),
  biomass_alive_leaves_in_1 = col_double(),
  biomass_alive_leaves_in_2 = col_double(),
  biomass_alive_leaves_in_3 = col_double(),
  biomass_alive_leaves_in_4 = col_double(),
  biomass_alive_leaves_in_5 = col_double(),
  biomass_alive_leaves_in_6 = col_double(),
  biomass_alive_leaves_out = col_double(),
  biomass_dead_leaves_in = col_double(),
  biomass_dead_leaves_out = col_double(),
  biomass_berries_in = col_double(),
  biomass_berries_out = col_double(),
  comment_sep = col_character(),
  comment_weighing = col_character()
)

# Read the file into a tibble
biomass_raw <- read_csv(file_path, col_types = col_spec) #I exported the file as a csv. If you want to export (from Drive) and read it as an excel file, use read_excel() from the readxl package

# Inspect data (optional)
#View(biomass_raw)
#str(biomass_raw)

####################################################################################
###################### MANDATORY DATA PROCESSING STEPS #############################
####################################################################################
# Calculate sum of all parts inside the bag (value 1, value 2, value 3, etc)
biomass_raw <- biomass_raw %>%
  mutate(
    biomass_brown_stem_in = if_else(
      if_all(starts_with("biomass_brown_stem_in_"), is.na),
      NA_real_,
      rowSums(select(., starts_with("biomass_brown_stem_in_")), na.rm = TRUE)
    ),
    biomass_green_stem_in = if_else(
      if_all(starts_with("biomass_green_stem_in_"), is.na),
      NA_real_,
      rowSums(select(., starts_with("biomass_green_stem_in_")), na.rm = TRUE)
    ),
    biomass_alive_leaves_in = if_else(
      if_all(starts_with("biomass_alive_leaves_in_"), is.na),
      NA_real_,
      rowSums(select(., starts_with("biomass_alive_leaves_in_")), na.rm = TRUE)
    )
  )

#Delete columns with individual part weights inside the bag
biomass_raw <- biomass_raw %>%
  select(-starts_with("biomass_brown_stem_in_"), -starts_with("biomass_green_stem_in_"), -starts_with("biomass_alive_leaves_in_"))  

# Calculate the sum of in and out part (necessary for individuals weight), and add a column for that
biomass_raw <- biomass_raw %>%
  mutate(
    biomass_brown_stem_total = ifelse(
      is.na(biomass_brown_stem_in) & is.na(biomass_brown_stem_out),
      NA,
      rowSums(cbind(biomass_brown_stem_in, biomass_brown_stem_out), na.rm = TRUE)
    ),
    biomass_green_stem_total = ifelse(
      is.na(biomass_green_stem_in) & is.na(biomass_green_stem_out),
      NA,
      rowSums(cbind(biomass_green_stem_in, biomass_green_stem_out), na.rm = TRUE)
    ),
    biomass_alive_leaves_total = ifelse(
      is.na(biomass_alive_leaves_in) & is.na(biomass_alive_leaves_out),
      NA,
      rowSums(cbind(biomass_alive_leaves_in, biomass_alive_leaves_out), na.rm = TRUE)
    ),
    biomass_dead_leaves_total = ifelse(
      is.na(biomass_dead_leaves_in) & is.na(biomass_dead_leaves_out),
      NA,
      rowSums(cbind(biomass_dead_leaves_in, biomass_dead_leaves_out), na.rm = TRUE)
    ),
    biomass_berries_total = ifelse(
      is.na(biomass_berries_in) & is.na(biomass_berries_out),
      NA,
      rowSums(cbind(biomass_berries_in, biomass_berries_out), na.rm = TRUE)
    )
  )

#Move columns before comments
biomass_raw <- biomass_raw %>%
  select(-comment_sep, -comment_weighing, everything(), comment_sep, comment_weighing)

#####################################################################################

# Example of filtering steps - modify as needed ####

## Filter data to only focus on VV species from bags that have been separated and weighed
## Comment line if you want to keep more data
biomass_vv_weighed <- biomass_raw %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1)

## If you want to filter on one site (KA or SE) or if Open or Forested, or both sites and habitat, change the lines below
biomass_vv_weighed_KA_F <- biomass_vv_weighed %>%
  filter(siteID == "KA") %>% #Kautokeino
  filter(habitat == "Forested") #Forested

# Example of data analysis - modify as needed ####

## Plot of leaf biomass VS stem biomass for individuals (VV_1, VV_2 or VV_3 only), for each site and by habitat
# Fit a regression line to the plot
biomass_vv_weighed %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3")) %>% # only individuals VV_1, VV_2 and VV_3
  ggplot(aes(x = biomass_brown_stem_total, y = biomass_alive_leaves_total, color = habitat)) + # total biomass in and out of individuals
  geom_point() +
  facet_wrap(~ siteID) + # plot by site
  #fit a regression line to the points
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Leaf Biomass vs Stem Biomass for Vaccinium vitis-idaea individuals",
    x = "Stem Biomass (g)",
    y = "Leaf Biomass (g)"
  ) +
  theme_minimal()

#Boxplot 


