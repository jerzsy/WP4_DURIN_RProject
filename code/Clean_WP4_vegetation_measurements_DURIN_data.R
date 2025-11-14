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

# Stack all data into one tibble
vegetation_data_stack <- bind_rows(vegetation_data_list)#, .id = "site_name")

#Quick plot to see vegetation height between sites, only for certain speciesID (column name) and habitat
ggplot(data=vegetation_data_stack %>% filter(speciesID=="EN" & habitat=="Forested")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, color=site_name)) +
  labs(title="Vegetation height (cm) in control plots for Vaccinium myrtillus in open habitat",
       x="Site", y="Height (cm)") +
  theme_minimal()

#Quick plot to see vegetation height VS stem length for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID=="CV")) +
  geom_point(aes(x=control_stem_length, y=control_top_height, color=habitat)) +
  #Add a 1:1 line
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_length, y=control_top_height, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title="Vegetation height (cm) VS stem length (cm) in control plots for Calluna vulgaris",
       x="Stem length (cm)", y="Height (cm)") +
  theme_minimal()

# Quick plot to see vegetation height VS stem diameter for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID=="EN")) +
  geom_point(aes(x=control_stem_diameter, y=control_top_height, color=habitat)) +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_diameter, y=control_top_height, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title="Vegetation height (cm) VS stem diameter (mm) in control plots for EN",
       x="Stem diameter (mm)", y="Height (cm)") +
  theme_minimal()

# Quick plot to see stem length VS stem diameter for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID=="EN")) +
  geom_point(aes(x=control_stem_diameter, y=control_stem_length, color=habitat)) +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_diameter, y=control_stem_length, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title="Stem length (cm) VS stem diameter (mm) in control plots for EN",
       x="Stem diameter (mm)", y="Stem length (cm)") +
  theme_minimal()

# Quick boxplot to see vegetation height in control VS removal plots for each site, for Open habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID=="VV" & habitat=="Open")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, fill="Control")) +
  geom_boxplot(aes(x=site_name, y=removal_top_height, fill="Removal"), alpha=0.5) +
  labs(title="Vegetation height (cm) in control VS removal plots for VV in open habitat",
       x="Site", y="Height (cm)") +
  scale_fill_manual(name="Plot type", values=c("Control"="lightblue", "Removal"="salmon")) +
  theme_minimal()

# Quick boxplot to see vegetation height in control VS removal plots for each site, for Forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID=="VV" & habitat=="Forested")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, fill="Control")) +
  geom_boxplot(aes(x=site_name, y=removal_top_height, fill="Removal"), alpha=0.5) +
  labs(title="Vegetation height (cm) in control VS removal plots for VV in forested habitat",
       x="Site", y="Height (cm)") +
  scale_fill_manual(name="Plot type", values=c("Control"="lightblue", "Removal"="salmon")) +
  theme_minimal()

# Data cleaning
## Add a column with flag for removal and control plots --> deal with 'JR encoding'
vegetation_data_clean <- vegetation_data_stack %>%
  mutate(
    flag_control = case_when(
      #quick check on strange values in heights and stem lengths/diameters
      (control_top_height < 0 | control_bottom_height < 0 | control_stem_length < 0 | control_stem_diameter < 0) ~ "negative_value",
      (control_bottom_height > control_top_height) ~ "bottom_higher_than_top",
      #compare stem length and top height - if top height > stem length by more than 5 cm, flag as "stem_length_lower_than_top"
      (control_top_height - control_stem_length > 2) ~ "stem_length_lower_than_top_by_2_cm",
      #check for very tall VV individuals (>25 cm) - flag as "very_tall_VV"
      (speciesID == "VV" & control_top_height > 25) ~ "very_tall_VV",
      #check for huge CV individuals (>70 cm) - flag as "very_tall_CV"
      (speciesID == "CV" & control_top_height > 70) ~ "very_tall_CV",
      TRUE ~ "OK"
    ),
    flag_removal = case_when(
      #quick check on strange values in heights and stem lengths/diameters
      (removal_top_height < 0 | removal_bottom_height < 0 | removal_stem_length < 0 | removal_stem_diameter < 0) ~ "negative_value",
      (removal_bottom_height > removal_top_height) ~ "bottom_higher_than_top",
      #compare stem length and top height - if top height > stem length by more than 5 cm, flag as "stem_length_lower_than_top"
      (removal_top_height - removal_stem_length > 2) ~ "stem_length_lower_than_top_by_2_cm",
      #check for very tall VV individuals (>25 cm) - flag as "very_tall_VV"
      (speciesID == "VV" & removal_top_height > 25) ~ "very_tall_VV",
      #check for huge CV individuals (>70 cm) - flag as "very_tall_CV"
      (speciesID == "CV" & removal_top_height > 70) ~ "very_tall_CV",
      TRUE ~ "OK"
    )
  )

## Rename after "comment_control" and "comment_removal"
vegetation_data_clean <- vegetation_data_clean %>%
  rename(
    comment_control = comments_1,
    comment_removal = comments_2
  )

## Reorganize the tibble so that flag_* columns come after their comment_* column

#Check typos
##Data format OK 
##Decimal separator OK
##NAs OK


