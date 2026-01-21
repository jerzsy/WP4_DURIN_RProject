############################################
###### Script to clean DURIN WP4 data ######
############################################

# --- Trees measurements in large DURIN plots ---- #
# Install and load packages
packages <- c("tidyverse")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Set wd
wd <- getwd()
setwd(wd)

# Source function to calculate trees height
source('code/Function_Calculate_Tree_Height.R')

# File specifications
## File path
file_path<- "raw_data/DURIN_WP4_raw_4Corners_field_traits_trees_2025.csv"

## Column types
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
  girth_cm = col_double(),
  girth_large_cm_1 = col_double(),
  girth_large_cm_2 = col_double(),
  girth_large_cm_3 = col_double(),
  large_stems_nb = col_double(),
  girth_small_cm_1 = col_double(),
  girth_small_cm_2 = col_double(),
  girth_small_cm_3 = col_double(),
  small_stems_nb = col_double(),
  dist_to_tree_m = col_double(),
  eye_height_m = col_double(),
  tree_position = col_character(),
  angle_top_canopy = col_double(),
  angle_bottom_canopy = col_double(),
  angle_base = col_double(),
  crown_diameter_m = col_double(),
  comments = col_character()
)

# Read the file
raw_df <- read_csv(file_path, col_types = col_spec, na = c("NA",""))

# Quick check
str(raw_df)

#-------------------------------------------#

# 1. Clean the data

## 1.1 Check for typos
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

# Tree position
raw_df <- raw_df %>%
  mutate(
    tree_position = case_when(
      tree_position %in% c("up","uphill","Uphill","UPHILL","Up","UP") ~ "up",
      tree_position %in% c("down","downhill","Downhill","DOWNHILL","Down","DOWN","DWON","dwon","donw") ~ "down",
      tree_position %in% c("same","Same","SAME","at same level","At same level","AT SAME LEVEL") ~ "same",
      TRUE ~ tree_position
    )
  )

## 1.2 Remove full NA lines, from species to comments columns
raw_df <- raw_df %>%
  filter(!if_all(species:comments, is.na))

## 1.3 Make flags and put them in a dedicated 'flags' column

#--------------------------------------------------------------#
### Flag function for automatic check on measurements! ###
make_flags <- function(girth_cm,girth_l_1,girth_l_2,girth_l_3,girth_s_1,girth_s_2,girth_s_3,large_stems_nb,small_stems_nb,dist_to_center,dist_to_tree,eye_height,angle_top_canopy,angle_bottom_canopy,angle_base,crown_diameter,tree_position) {
  flags <- c(
    # Check for negative values in girth, numbers, distance, height, diameter
    case_when(
      girth_cm < 0 | girth_l_1 < 0 | girth_l_2 < 0 | girth_l_3 < 0 | girth_s_1 < 0 | girth_s_2 < 0 | girth_s_3 < 0 | large_stems_nb < 0 | small_stems_nb < 0 | dist_to_center < 0 | dist_to_tree < 0 | eye_height < 0 | crown_diameter < 0 ~ "unexpected_negative_values",
              TRUE ~ NA_character_
      ),
    
    # Check for number of stems that have decimals that are not integers
    case_when(
      large_stems_nb %% 1 != 0 | small_stems_nb %% 1 != 0 ~ "non_integer_stem_number",
      TRUE ~ NA_character_
    ),
    
    # Check for angle of top of canopy less than angle of bottom of canopy
    case_when(
      angle_top_canopy < angle_bottom_canopy ~ "angle_top_less_than_angle_bottom",
      TRUE ~ NA_character_
    ),
  
    # Check for angle of top of canopy lower than angle at base
    case_when(
      angle_top_canopy < angle_base ~ "angle_top_less_than_angle_base",
      TRUE ~ NA_character_
    ),
    
    # Check for angle of bottom of canopy lower than angle at base
    case_when(
      angle_bottom_canopy < angle_base ~ "angle_bottom_less_than_angle_base",
      TRUE ~ NA_character_
    ),
    
    # # Check for angle of base positive and angle of top negative # Same as previous condition
    # case_when(
    #   (angle_base > 0) & (angle_top_canopy < 0) ~ "angle_base_positive_while_angle_top_negative",
    #   TRUE ~ NA_character_
    # ),
    
    # # Check for angle of base positive and angle of bottom negative # Same as previous condition
    # case_when(
    #   (angle_base > 0) & (angle_bottom_canopy < 0) ~ "angle_base_positive_while_angle_bottom_negative",
    #   TRUE ~ NA_character_
    # ),
    
    # Check for angle of base equal to angle of top canopy
    case_when(
      (angle_base == angle_top_canopy) ~ "angle_base_equal_to_angle_top",
      TRUE ~ NA_character_
    ),
    
    # # Check for angle of base equal to angle of bottom canopy #Bottom could be zero in height!
    # case_when(
    #   (angle_base == angle_bottom_canopy) ~ "angle_base_equal_to_angle_bottom",
    #   TRUE ~ NA_character_
    # ),
    
    # Check for tree not being uphill with angle to base and to canopy top > 0
    case_when(
      (angle_base > 0) & (angle_top_canopy > 0) & (tree_position != 'uphill') ~ "angle_top_indicates_tree_uphill_but_tree_position_not_set_to_uphill",
      TRUE ~ NA_character_
    ),
    
    # Check for tree not being uphill with angle to base and to canopy bottom > 0
    case_when(
      (angle_base > 0) & (angle_bottom_canopy > 0) & (tree_position != 'uphill') ~ "angle_bottom_indicates_tree_uphill_but_tree_position_not_set_to_uphill",
      TRUE ~ NA_character_
    )
  )
  # Remove NAs → keep only flags that triggered
  flags <- flags[!is.na(flags)]
  
  if (length(flags) == 0) "OK" else flags
}
#--------------------------------------------------------------#

### Apply flag function to the dataset
clean_df <- raw_df %>%
  mutate(
    flags = pmap(
      list(girth_cm, girth_large_cm_1, girth_large_cm_2, girth_large_cm_3,
           girth_small_cm_1, girth_small_cm_2, girth_small_cm_3,
           large_stems_nb, small_stems_nb,
           dist_to_center_m, dist_to_tree_m,
           eye_height_m,
           angle_top_canopy, angle_bottom_canopy, angle_base,
           crown_diameter_m,
           tree_position),
      make_flags
    )
  )

### Manual flag
#### Not sure plot name
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    flags = case_when(
      plotID == "L_LY_F_EN_3" ~ list(c(flags, "plot_name_doubtful_see_comments")),
      TRUE ~ list(flags)
    )
  )%>%
  ungroup()
  
## 1.4 Update one plot name
## In the database, there are two L_SE_O_EN_2 plots: one of them is wrongly named and should be L_SE_O_VV_3 instead.
clean_df <- clean_df %>%
  mutate(
    plotID = case_when(
      plotID == "L_SE_O_EN_2" & 
        dist_to_center_m %in% c(1.85, 2.44, 2.37) ~ "L_SE_O_VV_3",
      TRUE ~ plotID
    )
  )


#-----------------------------------------------#

# 2. Add columns with heights of tree based on angle measurements

## 2.1 Apply height calculation function to the dataset and create a new columns for them
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    height_top = calculate_tree_height(plotID, plant_nr, dist_to_tree_m, eye_height_m, angle_top_canopy, angle_base, tree_position, 'top', verbose = TRUE)$height_canopy,
    height_bottom = calculate_tree_height(plotID, plant_nr, dist_to_tree_m, eye_height_m, angle_bottom_canopy, angle_base, tree_position, 'bottom', verbose = TRUE)$height_canopy,
    height_top_eye = calculate_tree_height(plotID, plant_nr, dist_to_tree_m, eye_height_m, angle_top_canopy, angle_base, tree_position, 'top', verbose = FALSE)$height_canopy_eye,
    height_bottom_eye = calculate_tree_height(plotID, plant_nr, dist_to_tree_m, eye_height_m, angle_bottom_canopy, angle_base, tree_position, 'bottom', verbose = FALSE)$height_canopy_eye
  ) %>%
  ungroup()

## 2.2 Check on height in meters?
###Print warning on negative heights?
negative_heights <- clean_df %>%
  filter(height_top < 0 | height_bottom < 0)
if (nrow(negative_heights) > 0) {
  print("Warning! Some calculated heights are negative! Check measurements!")
  print(negative_heights %>% select(plotID, plant_nr, height_top, height_bottom))
}

## 2.3 Limit number to 3 decimals for height measurements columns
clean_df <- clean_df %>%
  mutate(
    height_top = round(height_top, 3),
    height_bottom = round(height_bottom, 3),
    height_top_eye = round(height_top_eye, 3),
    height_bottom_eye = round(height_bottom_eye, 3)
  )

## 2.4 Enter manually some heights that have been measured directly in the field
clean_df <- clean_df %>%
  mutate(
    height_top = case_when(
      plotID == "L_SO_O_CV_4" & plant_nr == 3 ~ 2.15,
      TRUE ~ height_top
    )
  )

clean_df <- clean_df %>%
  mutate(
    height_bottom = case_when(
      plotID == "L_SO_O_CV_4" & plant_nr == 3 ~ 0,
      TRUE ~ height_bottom
    )
  )


#-----------------------------------------------#

# 3. Adding a column that says if the individual from one plot is the same as another plot.

## This is of limited utility because we are not sure it was stated every time in the comments when it was the same individual.
## It is only stated starting from the second occurrence of the individual (not the first time it is measured).
## It is written in the comment column if an individual is the same as another plot but here we are applying another criteria.
## We are comparing data from all the measurement column.s to see if there are duplicates.
clean_df <- clean_df %>%
  mutate(
    same_as_other_individual =
      duplicated(select(.,
                        girth_cm,
                        girth_large_cm_1, girth_large_cm_2, girth_large_cm_3,
                        #large_stems_nb, #commenting stem number because sometimes "0" is written, sometimes NA
                        girth_small_cm_1, girth_small_cm_2, girth_small_cm_3,
                        #small_stems_nb,
                        dist_to_tree_m,
                        eye_height_m,
                        tree_position,
                        angle_top_canopy, angle_bottom_canopy, angle_base,
                        crown_diameter_m
      ))
  )

# 4. Rearrange dataset
## Select and order columns
final_df <- clean_df %>%
  select(year, date, site_name, siteID, habitat, plotID, recorder, weather,
         plant_nr, species, speciesID,
         dist_to_center_m,
         girth_cm,
         girth_large_cm_1, girth_large_cm_2, girth_large_cm_3, large_stems_nb,
         girth_small_cm_1, girth_small_cm_2, girth_small_cm_3, small_stems_nb,
         dist_to_tree_m,
         eye_height_m,
         tree_position,
         angle_top_canopy, angle_bottom_canopy, angle_base,
         height_top, height_bottom, height_top_eye, height_bottom_eye,
         crown_diameter_m,
         same_as_other_individual,
         comments,
         flags)

#-------------------------------------------------------------#
# 5. Export cleaned dataset
## Change flag column into something readable in Excel
final_df_export_csv <- final_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export in CSV
write.csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_traits_trees_2025.csv", row.names = FALSE)

#------------------------------------------------------#
# 5. Quick plots 

## Quick plot to check height_top vs height_top_eye and height_bottom vs height_bottom_eye
ggplot(final_df, aes(x = height_top, y = height_top_eye)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Height Top: Angles vs Eye Height Method",
       x = "Calculated Height Top (m)",
       y = "Height Top from Eye Height (m)") +
  theme_minimal()

ggplot(final_df, aes(x = height_bottom, y = height_bottom_eye)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Height Bottom: Angles vs Eye Height Method",
       x = "Calculated Height Bottom (m)",
       y = "Height Bottom from Eye Height (m)") +
  theme_minimal()

## Quick plot of difference between height_top and height_top_eye and height_bottom and height_bottom_eye
final_df <- final_df %>%
  mutate(
    diff_height_top = height_top - height_top_eye,
    diff_height_bottom = height_bottom - height_bottom_eye
  ) 
ggplot(final_df, aes(x = diff_height_top)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Difference between Angles Height Top and Eye Height Method",
       x = "Difference in Height Top (m)",
       y = "Frequency") +
  theme_minimal()
ggplot(final_df, aes(x = diff_height_bottom)) +
  geom_histogram(binwidth = 0.05, fill = "green", color = "black") +
  labs(title = "Difference between Angles Height Bottom and Eye Height Method",
       x = "Difference in Height Bottom (m)",
       y = "Frequency") +
  theme_minimal()

## Quick plot of difference as a function of height_top and height_bottom
ggplot(final_df, aes(x = height_top, y = diff_height_top)) +
  geom_point() +
  labs(title = "Difference in Height Top vs Angles Height Top",
       x = "Angles Height Top (m)", 
       y = "Difference in Height Top (m)") +
  theme_minimal()
ggplot(final_df, aes(x = height_bottom, y = diff_height_bottom)) +
  geom_point() +
  labs(title = "Difference in Height Bottom vs Angles Height Bottom",
       x = "Angles Height Bottom (m)", 
       y = "Difference in Height Bottom (m)") +
  theme_minimal()

## Quick plot of Height top VS crown diameter
ggplot(final_df, aes(x = crown_diameter_m, y = height_top)) +
  geom_point(color = "purple") +
  # Fit a regression line
  geom_smooth(method = "lm", se = FALSE, color = "orange") +
  labs(title = "Crown Diameter vs Height Top",
       x = "Crown Diameter (m)",
       y = "Height Top (m)") +
  theme_minimal()

