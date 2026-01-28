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

# Source function to calculate some shrub height
source('code/Function_Calculate_Tree_Height.R')

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

#-----------------------------------------------#
# 1. Clean the data

## 1.1 Check for typos
# Site names
raw_df <- raw_df %>%
  mutate(
    site_name = case_when(
      site_name %in% c("SENJA","Senja","senja") ~ "Senja",
      site_name %in% c("KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO") ~ "Kautokeino",
      site_name %in% c("LYGRA","Lygra","lygra","lyrga","Lyrga") ~ "Lygra",
      site_name %in% c("SOGNDAL","Sogndal","sogndal","Sogndl","Sogndla") ~ "Sogndal",
      TRUE ~ site_name
    )
  )
#Site ID
raw_df <- raw_df %>%
  mutate(
    siteID = case_when(
      siteID %in% c("SE","SENJA","Senja","senja","se","Se") ~ "SE",
      siteID %in% c("KA","KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO","ka","Ka") ~ "KA",
      siteID %in% c("LY","LYGRA","Lygra","lygra","lyrga","Lyrga","ly","Ly") ~ "LY",
      siteID %in% c("SO","SOGNDAL","Sogndal","sogndal","Sogndl","Sogndla","so","So") ~ "SO",
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

## 1.2 Remove full NA lines, from species to comments columns
raw_df <- raw_df %>%
  filter(!if_all(species:comments, is.na))

## 1.3 Make flags and put them in a dedicated 'flags' column

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

### Adding some flags manually - 
#### Dividing or multiplying values by 100 because obviously wrong units were used when entering data
#### some measurements were divided by 100 in crown diameter because the measurement was supposedly taken in cm and not m
#### some measurements were multiplied by 100 in top height because the measurement was supposedly taken in m and not cm
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
      plotID == "L_SO_F_VM_5" & plant_nr == 2 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_SO_F_VM_5" & plant_nr == 3 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_SO_F_VM_1" & plant_nr == 2 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_SO_F_VM_1" & plant_nr == 3 ~ list(c(flags, "crown_diameter_divided_by_100")),
      plotID == "L_SO_O_VM_1" & plant_nr == 1 ~ list(c(flags, "crown_diameter_divided_by_100")),
      
      plotID == "L_SO_F_VV_5" & plant_nr == 3 ~ list(c(flags, "crown_diameter_divided_by_100_but_not_sure")),
      
      plotID == "L_SO_F_VV_1" & plant_nr == 2 ~ list(c(flags, "top_height_multiplied_by_100")),
      plotID == "L_SO_F_VV_2" & plant_nr == 2 ~ list(c(flags, "top_height_multiplied_by_100")),
      plotID == "L_SO_F_EN_1" & plant_nr == 2 ~ list(c(flags, "top_height_multiplied_by_100")),
      plotID == "L_SO_O_EN_5" & plant_nr == 3 ~ list(c(flags, "top_height_multiplied_by_100")),
      plotID == "L_SO_O_CV_4" & plant_nr == 2 ~ list(c(flags, "top_height_multiplied_by_100")),
      TRUE ~ list(flags)
    )
  ) %>%
  ungroup()

### Not sure of the plot name given because already existing or mismatch between fieldsheets
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    flags = case_when(
      plotID == "L_LY_F_EN_3" ~ list(c(flags, "plot_name_doubtful_see_comments")),
      plotID == "L_LY_O_EN_1" ~ list(c(flags, "plot_name_doubtful_see_comments")),
      TRUE ~ list(flags)
    )
  ) %>%
  ungroup()

## 1.4 Calculate some heights with angles to canopy because shrub was to tall to measure directly
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    canopy_top_height_cm = case_when(
      #L_SO_F_VM_5 nr 2 top height with angle: dist: 2.5; level; top = 37; base = -30; low canopy
      plotID == "L_SO_F_VM_5" & plant_nr == 2 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 37,
        angle_base = -30,
        dist_to_tree = 250,#in cm for shrubs because we want a height in cm
        tree_position = "same",
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      #L_SO_F_VM_1 nr 2 top height with angle: dist: 2.5; level; top = 37; base = -30; low canopy
      plotID == "L_SO_F_VM_1" & plant_nr == 2 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 37,
        angle_base = -30,
        dist_to_tree = 250,#in cm for shrubs because we want a height in cm
        tree_position = "same",
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      #L_SO_F_VV_5 nr 1 top height measured from angle: dist = 3.7; top = 30; base = -13
      plotID == "L_SO_F_VV_5" & plant_nr == 1 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 30,
        angle_base = -13,
        dist_to_tree = 370,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      #L_SO_F_VV_5 nr 2 top height measured from angle: dist = 3.5; top = 38; base = -10
      plotID == "L_SO_F_VV_5" & plant_nr == 2 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 38,
        angle_base = -10,
        dist_to_tree = 350,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      #L_SO_F_VV_1 nr 1 Top and bottom heights measured from angle: dist = 2.5; upper angle = 36; bottom= bottom canopy = -35
      plotID == "L_SO_F_VV_1" & plant_nr == 1 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 36,
        angle_base = -35,
        dist_to_tree = 250,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,

      #L_SO_F_VV_2 nr 1 Top and bottom heights measured from angle: dist = 2.5; upper angle = 36; bottom= bottom canopy = -35
      plotID == "L_SO_F_VV_2" & plant_nr == 1 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 36,
        angle_base = -35,
        dist_to_tree = 250,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,

      #L_SO_F_EN_3 nr 1 top height measured from angle: dist = 2.4; top angle = 44 ; bottom angle = -24; canopy base = -24;
      plotID == "L_SO_F_EN_3" & plant_nr == 1 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 44,
        angle_base = -24,
        dist_to_tree = 240,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      #L_SO_F_VM_3 nr 2 top height measured with angle: dist = 3m, top = 39; base = -20; low canopy = -15
      plotID == "L_SO_F_VM_3" & plant_nr == 2 ~ calculate_tree_height(
        plotID = plotID,
        plant_nr = plant_nr,
        angle_canopy = 39,
        angle_base = -20,
        dist_to_tree = 300,#in cm for shrubs because we want a height in cm
        tree_position = NA,
        height_pos = "top",
        eye_height = NA,
        verbose = TRUE
      )$height_canopy,
      
      TRUE ~ canopy_top_height_cm
    )
  ) %>%
  ungroup()

### Limit number to 3 decimals for height measurements columns
clean_df <- clean_df %>%
  mutate(
    canopy_top_height_cm = round(canopy_top_height_cm, 3),
    canopy_bottom_height_cm = round(canopy_bottom_height_cm, 3),
  )

## 1.5 Adding a column that says if the individual from one plot is the same as another plot.

## This is of limited utility because we are not sure it was stated every time in the comments when it was the same individual.
## It is only stated starting from the second occurrence of the individual (not the first time it is measured).
## It is written in the comment column if an individual is the same as another plot but here we are applying another criteria.
## We are comparing data from all the measurement column.s to see if there are duplicates.
clean_df <- clean_df %>%
  mutate(
    same_as_other_individual =
      duplicated(select(., 
                        large_girth_cm_1, large_girth_cm_2, large_girth_cm_3,
                        #large_stems_nb, #commenting stem number because sometimes "0" is written, sometimes NA
                        small_girth_cm_1, small_girth_cm_2, small_girth_cm_3,
                        #small_stems_nb,
                        girth_cm_1, girth_cm_2, girth_cm_3,
                        #stems_nb,
                        canopy_top_height_cm,
                        canopy_bottom_height_cm,
                        crown_diameter_m
      ))
  )

#-----------------------------------------------#

# 2. Rearrange columns so that comments and flags are last
final_df <- clean_df %>%
  select(
    year, date, site_name, siteID, habitat, plotID, recorder, weather,
    plant_nr, species, speciesID,
    dist_to_center_m,
    large_girth_cm_1, large_girth_cm_2, large_girth_cm_3, large_stems_nb,
    small_girth_cm_1, small_girth_cm_2, small_girth_cm_3, small_stems_nb,
    girth_cm_1, girth_cm_2, girth_cm_3, stems_nb,
    canopy_top_height_cm,
    canopy_bottom_height_cm,
    crown_diameter_m,
    same_as_other_individual,
    comments,
    flags)

#-----------------------------------------------#

# 3. Export cleaned dataset
## Change flag column into something readable in Excel
final_df_export_csv <- final_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export in CSV
write.csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_traits_shrubs_2025.csv", row.names = FALSE)

#-----------------------------------------------#

# 4. Quick plots on data

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