############################################
###### Script to clean DURIN WP4 data ######
############################################

# --- Vegetation measurements on DWARF-SHRUBS in Extra WP4 biomass removal plots ---- #

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
file_path<- "raw_data/DURIN_WP4_raw_4Corners_field_biomass_trait_cover_dwarf_shrubs_2025_OFF_SE_KA_ONLY.csv" #UPDATE LATER WITH LYGRA AND SOGNDAL

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
  species = col_character(),
  speciesID = col_character(),
  plant_nr = col_integer(),
  plant_cover = col_double(),
  top_height_in_all = col_double(),
  top_height_in_without_flowers = col_double(),
  top_height_out_all = col_double(),
  top_height_in_out_all = col_double(),
  top_height_in_out_without_flowers = col_double(),
  stem_diam_in = col_double(),
  stem_diam_out = col_double(),
  stem_length_in = col_double(),
  stem_length_only_out = col_double(),
  stem_length_in_out = col_double(),
  number_harvested_indiv_without_3_rep = col_integer(),
  number_harvest_indiv = col_integer(),
  Pic_numbers = col_character(),
  comment = col_character()
)

# Read the file #Only for Senja and Kautokeino for now as measurements are still ongoing in the lab for Lygra and Sogndal.
raw_df <- read_csv(file_path, col_types = col_spec, na = c("NA",""))

# Quick check
str(raw_df)

#------------------------------------------------------------------------------#

# 1. Data check
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

### Habitat
raw_df <- raw_df %>%
  mutate(
    habitat = case_when(
      habitat %in% c("Open","open","OPen","opne") ~ "Open",
      habitat %in% c("Forested","forested","FORESTED","forested") ~ "Forested",
      TRUE ~ habitat
    )
  )

### SpeciesID
raw_df <- raw_df %>%
  mutate(
    speciesID = case_when(
      speciesID %in% c("EN","en","En") ~ "EN",
      speciesID %in% c("VV","vv","Vv") ~ "VV",
      speciesID %in% c("CV","cv","Cv") ~ "CV",
      speciesID %in% c("VM","vm","Vm") ~ "VM",
      TRUE ~ speciesID
    )
  )

# 2. Calculate Stem length IN + OUT of the plot (it's needed for North plot only) as the sum of stem length in and stem length out
# I need to calculate it now for the next 'flag column' creation step
raw_df <- raw_df %>%
  mutate(
    stem_length_in_out = case_when(
      !is.na(stem_length_in) & !is.na(stem_length_only_out) ~ stem_length_in + stem_length_only_out,
      TRUE ~ stem_length_in_out
    )
  )
## Limit to one decimal
raw_df <- raw_df %>%
  mutate(
    stem_length_in_out = round(stem_length_in_out, 1)
  )

# 3. Add columns with flags
#-------------------------------------------------------------------#
## flag function for automatic check on measurements! ###
make_flags <- function(cover, top_in, top_in_flowers, top_out, top_in_out, top_in_out_flowers, stem_len_in, stem_len_out, stem_len_in_out, stem_diam_in, stem_diam_out, species) {
  stem_len = case_when(
    !is.na(stem_len_in) & is.na(stem_len_in_out) ~ stem_len_in,
    !is.na(stem_len_in_out) ~ stem_len_in_out,
    TRUE ~ NA_real_
  )
  
  flags <- c(
    #Check for negative values
    case_when(
      cover < 0 | top_in < 0 | top_in_flowers < 0 | top_out < 0 | top_in_out < 0 | top_in_out_flowers < 0 | stem_len_in < 0 | stem_len_out < 0 | stem_len_in_out < 0 | stem_diam_in < 0 | stem_diam_out < 0 ~ "negative_value",
      TRUE ~ NA_character_
    ),

    #compare stem length and top height - if top height > stem length by more than 2 cm
    case_when(
      (top_in - stem_len) > 2  | (top_in_flowers - stem_len) > 2 | (top_out - stem_len) > 2 | (top_in_out - stem_len) > 2 | (top_in_out_flowers - stem_len) > 2 ~ "stem_length_lower_than_top_by_2_cm",
      TRUE ~ NA_character_
    ),
    
    #check for very tall VV individuals (>25 cm)
    case_when(
      (species == "VV" & top_in > 25) | (species == "VV" & top_in_flowers > 25) | (species == "VV" & top_out > 25) | (species == "VV" & top_in_out > 25) | (species == "VV" & top_in_out_flowers > 25) ~ "very_tall_VV",
      TRUE ~ NA_character_
    ),
    
    #check for huge CV individuals (>70 cm)
    case_when(
      (species == "CV" & top_in > 70) | (species == "CV" & top_in_flowers > 70) | (species == "CV" & top_out > 70) | (species == "CV" & top_in_out > 70) | (species == "CV" & top_in_out_flowers > 70) ~ "very_tall_CV",
      TRUE ~ NA_character_
    ),
    #check for huge stem diameter (>20 mm)
    case_when(
      (stem_diam_in > 20) | (stem_diam_out > 20) ~ "huge_stem_diameter",
      TRUE ~ NA_character_
    ),
    #check for NA values in EN, VV, CV or VM height/stem length/stem diameter - except if full line is NA
    case_when(
      species %in% c("EN","VV","CV","VM") &
        (is.na(top_in) | is.na(stem_len_in) | is.na(stem_diam_in)) &
        !(is.na(top_in) & is.na(stem_len_in) & is.na(stem_diam_in)) ~ "missing_measured_value",
      TRUE ~ NA_character_
    ),
    
    #check for NA values in EN, VV, CV or VM stem length/stem diameter when part of the individual is out
    case_when(
      species %in% c("EN","VV","CV","VM") &
        (is.na(stem_len_out) | is.na(stem_len_in_out)) &
        !(is.na(stem_diam_out)) ~ "missing_measured_value_out",
      TRUE ~ NA_character_
    ),
    case_when(
      species %in% c("EN","VV","CV","VM") &
        !(is.na(stem_len_out) | is.na(stem_len_in_out)) &
        (is.na(stem_diam_out)) ~ "missing_measured_value_out",
      TRUE ~ NA_character_
    ),
    
    # Check for top_height values equal to 0. It is up to the data user to decide what to do with these values. May be a bit suspicious for TOP height.
    case_when(
      (top_in == 0) | (top_in_flowers == 0) | (top_out == 0) | (top_in_out == 0) | (top_in_out_flowers == 0) ~ "top_height_equal_0",
      TRUE ~ NA_character_
    )
  )
  
  # Remove NAs → keep only flags that triggered
  flags <- flags[!is.na(flags)]
  
  if (length(flags) == 0) "OK" else flags
}
#-------------------------------------------------------------------#


## Apply the flagging function to the dataset
clean_df <- raw_df %>%
  mutate(
    flags = pmap(
      list(plant_cover, 
           top_height_in_all, top_height_in_without_flowers, top_height_out_all, top_height_in_out_all, top_height_in_out_without_flowers, 
           stem_length_in, stem_length_only_out, stem_length_in_out, 
           stem_diam_in, stem_diam_out,
           speciesID),
      make_flags
    )
  )

## Manual flags
#E_SE_F_CV_2 CV 3 probably no canopy in - delete?

# 4. Adding missing number of harvested individuals based on information found on the biomass bag
# For this, I checked every NA in number_harvested_indiv_without_3_rep - I replace with what I found on the bag
# in the lab, or, if there is no "all" bag, I put 0 instead as there is no individuals harvested in addition to
# the replicates. I also checked any 1, 2 and 3 values in number_harvested_indiv_without_3_rep as there may be a
# mistake too (encoding the number of replicates harvested if there is no additional individuals harvested) 
# and compared it with the lab information.
# To do for South.
# Recheck for CV.
# And maybe KA_F_VV_4 and KA_F_VV_5.
clean_df <- clean_df %>%
  mutate(
    number_harvested_indiv_without_3_rep = 
      case_when(
        plotID == "E_SE_O_EN_4" & speciesID == "VM"  ~ 0, #NA was written instead of 0 (3 replicates)
        plotID == "E_SE_O_EN_4" & speciesID == "EN"  ~ 32, #NA was written
        plotID == "E_SE_O_CV_5" & speciesID == "VM"  ~ 1, #0 was written
        plotID == "E_SE_O_VV_4" & speciesID == "VV" ~ 1,#was written 0 instead of 1 as there is an all bag with one individual.
        plotID == "E_SE_F_VV_2" & speciesID == "VM" ~ 1, #NA was written
        plotID == "E_KA_O_VM_1" & speciesID == "VV" ~ 3, #NA was written 
        plotID == "E_KA_O_VM_1" & speciesID == "EN" ~ 0, #NA was written instead of 0 (only 1 replicate)
        plotID == "E_KA_F_EN_2" & speciesID == "VM" ~ 0, #NA was written instead of 0 (3 replicates)
        plotID == "E_KA_F_VM_2" & speciesID == "VM" ~ 18, #NA was written
        plotID == "E_KA_F_VM_4" & speciesID == "VM" ~ 20, #NA was written
        plotID == "E_KA_F_VM_4" & speciesID == "VV" ~ 25, #NA was written
        plotID == "E_KA_F_VM_4" & speciesID == "EN" ~ 0, #NA was written instead of 0 (only 1 replicate)
        
        #plotID == "E_SE_F_CV_2" & speciesID == "CV" ~ ,#to count in the lab!
        #plotID == "E_KA_F_VM_2" & speciesID == "VV" ~ ,#to count in the lab!
        #plotID == "E_KA_F_VV_4" & speciesID == "EN" ~ ,#figuring out if this is actually VV_5_B!
        #plotID == "E_KA_F_VV_5" & speciesID == "VV" ~ ,#figuring out if this is actually VV_5_A!
        
        plotID == "E_SE_O_EN_2" & speciesID == "VV" ~ 0,#was written 3 for the 3 replicates instead of 0
        plotID == "E_KA_O_VV_5" & speciesID == "EN" ~ 0,#was written 3 for the 3 replicates instead of likely 0 as we could not find an "all" bag.
        plotID == "E_SE_F_CV_3" & speciesID == "VV" ~ 0,#was written 2 for the 2 replicates instead of likely 0 (no extra bags).
        
        TRUE ~ number_harvested_indiv_without_3_rep
      )
  )

## Now calculate the total number of harvested individuals
## If NA in the 'without replicates' number --> NA
## If 0 or more --> number of replicates (0 to 3), which depends on the number of lines without NA's values in the measurements for the species
## for only EN or CV or VV or VM species rows, for one plot
clean_df <- clean_df %>%
  group_by(plotID, speciesID) %>%
  mutate(
    number_replicates = case_when(
      speciesID %in% c("EN","CV","VV","VM") ~ sum(!is.na(top_height_in_all) | !is.na(stem_length_in) | !is.na(stem_diam_in)),
      TRUE ~ NA_integer_
    ),
    number_harvest_indiv = case_when(
      is.na(number_harvested_indiv_without_3_rep) ~ number_harvest_indiv,#number_harvest_indiv and not NA because for South sites (LY, SO), 
                                                                        #we directly calculated the total number of individuals on the field
      !is.na(number_harvested_indiv_without_3_rep) ~ number_harvested_indiv_without_3_rep + number_replicates,
      TRUE ~ number_harvest_indiv
    )
  ) %>%
  ungroup() %>%
  select(-number_replicates)

# 5. Create new columns that give the stem diameter and stem length of the full individual, whether it is laying only in the plot
# Or both in and outside of the plot.
# This means, if there is a stem length in + out, this is the full length of the individual; otherwise it's the stem length in.
# This means, if there is a stem diameter out, this is the full diameter of the individual; otherwise it's the stem diameter in. 
clean_df <- clean_df %>%
  mutate(
    full_indiv_stem_length = case_when(
      !is.na(stem_length_in_out) ~ stem_length_in_out,
      is.na(stem_length_in_out) & !is.na(stem_length_in) ~ stem_length_in,
      TRUE ~ NA_real_
    ),
    full_indiv_stem_diameter = case_when(
      !is.na(stem_diam_out) ~ stem_diam_out,
      is.na(stem_diam_out) & !is.na(stem_diam_in) ~ stem_diam_in,
      TRUE ~ NA_real_
    )
  )

# Do the same for vegetation height. However, for the moment, it is not possible for the North. Indeed, they measured the 
# vegetation height only outside the plot when partially out, and not on the full individual. 
# I am planning on measuring a full individual height based on the biomass weight (to scale the contribution of 
# in/out depending on the biomass in/out)
# In the South, vegetation height when the individual was both in and out was measured in the plot and on the full individual. 




# 6. Reorganize columns of the dataset so that everything comes before the comment/flags columns; 
# and full stem length and stem diameter columns come after stem_length_in_out
final_df <- clean_df %>%
  select(year:stem_length_in_out, full_indiv_stem_length, full_indiv_stem_diameter, number_harvested_indiv_without_3_rep:number_harvest_indiv, Pic_numbers, comment, flags)

# 7. Export dataset
## Change flags column into something readable outside R.
final_df_export_csv <- final_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export csv file
write_csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_biomass_trait_cover_dwarf_shrubs_2025_OFF_SE_KA_ONLY.csv")

# TO DO
## Replace comment column for the "lab" column by the comment column from the original dataset. NOT the comment in the lab column. And merge for the measurements 
#I think I'll have another script to merge both databases and that'll be my raw measurements data sheet.

# Quick ggplots
## Cover vs top height in
ggplot(clean_df, aes(x = plant_cover, y = top_height_in_all, color = speciesID)) +
  geom_point() +
  #Fit a regression line for each species
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Plant cover vs Top height IN", x = "Plant cover (%)", y = "Top height IN (cm)")

## Cover vs stem length in
ggplot(clean_df, aes(x = plant_cover, y = stem_length_in, color = speciesID)) +
  geom_point() +
  #Fit a regression line for each species
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Plant cover vs Stem length IN", x = "Plant cover (%)", y = "Stem length IN (cm)")

## Histogram of harvested number of individuals by species for Senja Open
ggplot(clean_df %>% filter(site_name == "Kautokeino" & habitat == "Open"), aes(x = number_harvest_indiv, fill = speciesID)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  theme_minimal() +
  labs(title = "Histogram of harvested number of individuals - Kautokeino Open", x = "Number of harvested individuals", y = "Count")
