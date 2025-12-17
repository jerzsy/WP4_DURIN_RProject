############################################
###### Script to clean DURIN WP4 data ######
############################################

# --- Shrubs measurements in large DURIN plots ---- #
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
## File path
file_path<- "raw_data/DURIN_WP4_raw_4Corners_field_traits_shrubs_2025.csv"

## Column names
col_spec <- cols(
  year = col_integer(),
  date = col_date(format = ""),
  site_name = col_character(),
  siteID = col_character(),
  habitat = col_character(),
  plotID = col_character(),
  recorder = col_character(),
  weather = col_character(),
  plant_nr = col_integer(),
  species = col_character(),
  speciesID = col_character(),
  dist_to_center_m = col_double(),
  girth_cm_1 = col_double(),
  girth_cm_2 = col_double(),
  girth_cm_3 = col_double(),
  stems_nb = col_double(),
  canopy_top_height_cm = col_double(),
  canopy_bottom_height_cm = col_double(),
  crown_diameter_m = col_double(),
  comments = col_character()
)

# Read the file
raw_df <- read_csv(file_path, col_types = col_spec, na = c("NA",""))

# Quick check
str(raw_df)

# 1. Clean the data

## Check for typos
# Site names
raw_df <- raw_df %>%
  mutate(
    site_name = case_when(
      site_name %in% c("SENJA","Senja","senja") ~ "Senja",
      site_name %in% c("KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO") ~ "Kautokeino",
      TRUE ~ site_name
    )
  )
#Site ID
raw_df <- raw_df %>%
  mutate(
    siteID = case_when(
      siteID %in% c("SE","SENJA","Senja","senja","se","Se") ~ "SE",
      siteID %in% c("KA","KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO","ka","Ka") ~ "KA",
      TRUE ~ siteID
    )
  )

# Habitat
raw_df <- raw_df %>%
  mutate(
    habitat = case_when(
      habitat %in% c("Open","open","OPen","opne") ~ "Open",
      habitat %in% c("Forested","forested","FORESTED","forested") ~ "Forested",
      TRUE ~ habitat
    )
  )

## Make flags and put them in a dedicated 'flags' column

#--------------------------------------------------------------#
### Flag function for automatic check on measurements! ###

make_flags <- function(girth_cm_1,girth_cm_2,girth_cm_3,stems_nb,dist_to_center,canopy_top_height, canopy_bottom_height, crown_diameter) {
  flags <- c(
    # Check for negative values in girth, numbers, distance, height, diameter
    case_when(
      girth_cm_1 < 0 | girth_cm_2 < 0 | girth_cm_3 < 0 | stems_nb < 0 | dist_to_center < 0 | canopy_top_height < 0 | canopy_bottom_height < 0 | crown_diameter < 0 ~ "unexpected_negative_values",
      TRUE ~ NA_character_
    ),
    
    # Check for number of stems that have decimals that are not integers
    case_when(
      stems_nb %% 1 != 0 ~ "non_integer_stem_number",
      TRUE ~ NA_character_
    ),
    
    # Check for canopy bottom height being higher than canopy top height
    case_when(
      !is.na(canopy_bottom_height) & !is.na(canopy_top_height) & (canopy_bottom_height > canopy_top_height) ~ "canopy_bottom_higher_than_top",
      TRUE ~ NA_character_
    ),
    
    # Check for crown diameter being unreasonably high (>4 m)
    case_when(
      !is.na(crown_diameter) & (crown_diameter > 4) ~ "unreasonably_high_crown_diameter",
      TRUE ~ NA_character_
    )
  )
  
  # Remove NAs → keep only flags that triggered
  flags <- flags[!is.na(flags)]
  
  if (length(flags) == 0) "OK" else flags
  
}
#-------------------------------------------------------------------#

### Apply flag function to the dataset
clean_df <- raw_df %>%
  mutate(
    flags = pmap(
      list(girth_cm_1, girth_cm_2, girth_cm_3,
           stems_nb,
           dist_to_center_m,
           canopy_top_height_cm, canopy_bottom_height_cm,
           crown_diameter_m),
      make_flags
    )
  )

### Add some flags manually - some measurements were divided by 100 in crown diameter because the measurement was supposedly taken in cm and not m
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    flags = case_when(
      plotID == "L_KA_O_EN_2" & plant_nr == 2 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_EN_2" & plant_nr == 1 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_EN_3" & plant_nr == 1 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_VV_4" & plant_nr == 1 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_VV_4" & plant_nr == 2 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_VV_4" & plant_nr == 3 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_EN_5" & plant_nr == 1 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_EN_5" & plant_nr == 2 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_KA_F_EN_5" & plant_nr == 3 ~ list(c(flags, "crown_diameter_divided_by_100")),
      TRUE ~ list(flags)
    )
  ) %>%
  ungroup()

# 2. Export cleaned dataset
## Change flag column into something readable in Excel
final_df_export_csv <- clean_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export in CSV
write.csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_traits_shrubs_2025.csv", row.names = FALSE)

# 3. Quick plots on data

## Histogram of crown diameter
ggplot(clean_df, aes(x = crown_diameter_m)) +
  geom_histogram(binwidth = 0.1, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Crown Diameter", x = "Crown Diameter (m)", y = "Frequency") +
  theme_minimal()
## Histogram of canopy top height
ggplot(clean_df, aes(x = canopy_top_height_cm)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Canopy Top Height", x = "Canopy Top Height (cm)", y = "Frequency") +
  theme_minimal()
## Histogram of canopy bottom height
ggplot(clean_df, aes(x = canopy_bottom_height_cm)) +
  geom_histogram(binwidth = 10, fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Canopy Bottom Height", x = "Canopy Bottom Height (cm)", y = "Frequency") +
  theme_minimal()
##Plotting top height vs crown diameter
ggplot(clean_df, aes(x = crown_diameter_m, y = canopy_top_height_cm)) +
  geom_point(color = "purple") +
  # Fitting a regression line
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Crown Diameter vs Canopy Top Height", x = "Crown Diameter (m)", y = "Canopy Top Height (cm)") +
  theme_minimal()
