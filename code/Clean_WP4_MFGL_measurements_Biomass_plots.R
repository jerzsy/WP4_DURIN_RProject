############################################
###### Script to clean DURIN WP4 data ######
############################################

# --- Vegetation measurements on MOSS, FORBS, GRAMINOIDS, LITTER AND LICHEN in Extra WP4 biomass removal plots ---- #

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
file_path<- "raw_data/DURIN_WP4_raw_4Corners_field_biomass_trait_cover_MFGL_2025.csv"

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
  vg_type = col_character(),
  vg_initials = col_character(),
  plant_cover = col_character(), #importing as character to avoid problems with "<1" plant cover.
  top_height_1 = col_double(),
  top_height_2 = col_double(),
  top_height_3 = col_double(),
  top_height_4 = col_double(),
  moss_reestimate = col_double(),
  Comment = col_character()
)

# Read the file #Only for Senja and Kautokeino for now as measurements are still ongoing in the lab for Lygra and Sogndal.
raw_df <- read_csv(file_path, col_types = col_spec, na = c("NA",""))

# Quick check
str(raw_df)

#Find "<1" entries in plant cover column and convert to 0.5, then convert column to numeric.
raw_df <- raw_df %>%
  mutate(plant_cover = ifelse((plant_cover == "<1" | plant_cover == " <1" | plant_cover == "< 1"), 0.5, plant_cover),
         plant_cover = as.numeric(plant_cover))

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

### Vegetation type
raw_df <- raw_df %>%
  mutate(
    vg_initials = case_when(
      vg_initials %in% c("M","m","M ","m ") ~ "M",
      vg_initials %in% c("F","f","F ","f ") ~ "F",
      vg_initials %in% c("CV","cv","Cv") ~ "CV",
      vg_initials %in% c("VM","vm","Vm") ~ "VM",
      TRUE ~ vg_initials
    )
  )

# 2. Moving one line - it was encoded on the wrong line
# Forbs (plant cover = 3) measurements were encoded on the Graminoids line (plant cover = 0) for this plot.
#E_KA_O_VV_4, move line from vg_initials G to F for column top_height_1 to top_height_4
#It is a very long piece of code but I could not figure out how to do it better.
raw_df <- raw_df %>%
  mutate(
    top_height_1 = case_when(
      plotID == "E_KA_O_VV_4" & vg_initials == "F" ~ raw_df$top_height_1[raw_df$plotID == "E_KA_O_VV_4" & raw_df$vg_initials == "G"][2],
      plotID == "E_KA_O_VV_4" & vg_initials == "G" ~ NA_real_,
      TRUE ~ top_height_1
    ),
    top_height_2 = case_when(
      plotID == "E_KA_O_VV_4" & vg_initials == "F" ~ raw_df$top_height_2[raw_df$plotID == "E_KA_O_VV_4" & raw_df$vg_initials == "G"][2],
      plotID == "E_KA_O_VV_4" & vg_initials == "G" ~ NA_real_,
      TRUE ~ top_height_2
    ),
    top_height_3 = case_when(
      plotID == "E_KA_O_VV_4" & vg_initials == "F" ~ raw_df$top_height_3[raw_df$plotID == "E_KA_O_VV_4" & raw_df$vg_initials == "G"][2],
      plotID == "E_KA_O_VV_4" & vg_initials == "G" ~ NA_real_,
      TRUE ~ top_height_3
    ),
    top_height_4 = case_when(
      plotID == "E_KA_O_VV_4" & vg_initials == "F" ~ raw_df$top_height_4[raw_df$plotID == "E_KA_O_VV_4" & raw_df$vg_initials == "G"][2],
      plotID == "E_KA_O_VV_4" & vg_initials == "G" ~ NA_real_,
      TRUE ~ top_height_4
    )
  )



# 3. Add columns with flags
make_flags <- function(cover, top_1, top_2, top_3, top_4, moss_reestimate, vg_initials) {

  flags <- c(
    #Check for negative values
    case_when(
      cover < 0 | top_1 < 0 | top_2 < 0 | top_3 < 0 | top_4 < 0 | moss_reestimate < 0 ~ "negative_value",
      TRUE ~ NA_character_
    ),
    
    # Check for full line of NA in vegetation height
    case_when(
      is.na(top_1) & is.na(top_2) & is.na(top_3) & is.na(top_4) & vg_initials == "VG" ~ "missing_vegetation_heights",
      TRUE ~ NA_character_
    ),
    
    # Check for plant cover > 0 in Moss/Forbs/Graminoids lines but no height measurements
    case_when(
      #filter for M, F and G vg_initials
      cover > 0 & (is.na(top_1) & is.na(top_2) & is.na(top_3) & is.na(top_4)) & (vg_initials == 'M' | vg_initials == 'F' | vg_initials == 'G') ~ "missing_height_measurements",
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
           top_height_1, top_height_2, top_height_3, top_height_4,
           moss_reestimate,
           vg_initials),
      make_flags
    )
  )

## Adding flags manually
#Nothing for now, not sure I should add something.

final_df <- clean_df
# 4. Export dataset
## Change flags column into something readable outside R.
final_df_export_csv <- final_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export csv file
write_csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_biomass_trait_cover_MFGL_2025.csv")