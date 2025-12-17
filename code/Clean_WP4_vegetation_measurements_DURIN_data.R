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

# Read the files into a named list of tibbles 
vegetation_data_list <- file_paths %>%
  map(~ read_csv(.x,col_types= col_spec)) #reads each CSV

# Inspect ---------------------------------------------------------------
str(vegetation_data_list)   # shows a list of tibbles, each named by site

# Stack all data into one tibble
vegetation_data_stack <- bind_rows(vegetation_data_list)#, .id = "site_name")

# Data cleaning
## 1. Check for typos
### Site names
vegetation_data_stack <- vegetation_data_stack %>%
  mutate(
    site_name = case_when(
      site_name %in% c("SENJA","Senja","senja") ~ "Senja",
      site_name %in% c("KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO") ~ "Kautokeino",
      site_name %in% c("LYGRA","Lygra","lygra","Lygr") ~ "Lygra",
      site_name %in% c("SOGNDAL","Sogndal","sogndal","Songdal") ~ "Sogndal",
      TRUE ~ site_name
    )
  )
  
### Habitat
vegetation_data_stack <- vegetation_data_stack %>%
  mutate(
    habitat = case_when(
      habitat %in% c("Open","open","OPen","opne") ~ "Open",
      habitat %in% c("Forested","forested","FORESTED","forested") ~ "Forested",
      TRUE ~ habitat
    )
  )

### SpeciesID
vegetation_data_stack <- vegetation_data_stack %>%
  mutate(
    speciesID = case_when(
      speciesID %in% c("EN","en","En") ~ "EN",
      speciesID %in% c("VV","vv","Vv") ~ "VV",
      speciesID %in% c("CV","cv","Cv") ~ "CV",
      speciesID %in% c("VM","vm","Vm") ~ "VM",
      speciesID %in% c("G","g") ~ "G",
      speciesID %in% c("F","f") ~ "F",
      TRUE ~ speciesID
    )
  )


## 2. Add columns with flags for removal and control plots
#-------------------------------------------------------------------#
### flag function for automatic check on measurements! ###
make_flags <- function(top, bottom, stem_len, stem_diam, species) {
  
  flags <- c(
    case_when(
      top < 0 | bottom < 0 | stem_len < 0 | stem_diam < 0 ~ "negative_value",
      TRUE ~ NA_character_
    ),
    case_when(
      bottom > top ~ "bottom_height_higher_than_top",
      TRUE ~ NA_character_
    ),
    #compare stem length and top height - if top height > stem length by more than 2 cm
    case_when(
      (top - stem_len) > 2 ~ "stem_length_lower_than_top_by_2_cm",
      TRUE ~ NA_character_
    ),
    #check for very tall VV individuals (>25 cm)
    case_when(
      species == "VV" & top > 25 ~ "very_tall_VV",
      TRUE ~ NA_character_
    ),
    #check for huge CV individuals (>70 cm)
    case_when(
      species == "CV" & top > 70 ~ "very_tall_CV",
      TRUE ~ NA_character_
    ),
    #check for huge stem diameter (>20 mm)
    case_when(
      stem_diam > 20 ~ "huge_stem_diameter",
      TRUE ~ NA_character_
    ),
    #check for NA values in EN, VV, CV or VM height/stem length/stem diameter - except if full line is NA (i.e, no control measurements taken)
    case_when(
      species %in% c("EN","VV","CV","VM") &
        (is.na(top) | is.na(bottom) | is.na(stem_len) | is.na(stem_diam)) &
        !(is.na(top) & is.na(bottom) & is.na(stem_len) & is.na(stem_diam)) ~ "missing_measured_value",
      TRUE ~ NA_character_
    ),
    
    #check for NA values in Graminoids or Forbs - except if full line is NA (i.e, no control measurements taken)
    case_when(
      species %in% c("G","F") &
        (is.na(top) | is.na(bottom)) &
        !(is.na(top) & is.na(bottom)) ~ "missing_measured_value",
      TRUE ~ NA_character_
    )
  )
  
  # Remove NAs → keep only flags that triggered
  flags <- flags[!is.na(flags)]
  
  if (length(flags) == 0) "OK" else flags
}
#-------------------------------------------------------------------#

### Apply the flagging function to control and removal data
vegetation_data_clean <- vegetation_data_stack %>%
  mutate(
    control_flags = pmap(
      list(control_top_height,
           control_bottom_height,
           control_stem_length,
           control_stem_diameter,
           speciesID),
      make_flags
    ),
    removal_flags = pmap(
      list(removal_top_height,
           removal_bottom_height,
           removal_stem_length,
           removal_stem_diameter,
           speciesID),
      make_flags
    )
  )

### Adding some warnings on data manually in the flag column

#### Configuration of replicates - canopy or root out? ####

            #### -- Control plots -- ####

##### Root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    control_flags = case_when(
      plotID == "LY_O_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "LY_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "VM" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "LY_F_VM_1" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "LY_F_VM_3" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_EN_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_EN_3" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_EN_5" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_CV_1" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_CV_3" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_CV_3" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_CV_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_VM_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_VM_1" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_VM_3" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_VM_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_O_VV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_EN_1" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_CV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_CV_5" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_VV_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_VV_5" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SO_F_VV_5" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_O_EN_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_O_EN_2" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_O_CV_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_O_VM_5" & speciesID == "VV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_O_VV_5" & speciesID == "VV" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_F_EN_5" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_F_CV_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_F_CV_2" & speciesID == "VM" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "SE_F_VM_5" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      plotID == "KA_O_VV_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "root_out")),
      
      TRUE ~ list(control_flags)
    )
  ) %>%
  ungroup()

##### Canopy out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    control_flags = case_when(
      plotID == "SO_O_CV_4" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 1 ~ 
        list(c(control_flags, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 2 ~ 
        list(c(control_flags, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 3 ~ 
        list(c(control_flags, "canopy_out")),
      
      plotID == "SE_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(control_flags, "canopy_out")),
      
      TRUE ~ list(control_flags)
    )
  ) %>%
  ungroup()

##### Canopy out and root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    control_flags = case_when(
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "canopy_root_out")),
      
      plotID == "SO_F_CV_4" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "canopy_root_out")),
      
      plotID == "SO_F_CV_4" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(control_flags, "canopy_root_out")),
      
      TRUE ~ list(control_flags)
    )
  ) %>%
  ungroup()


                #### -- Removal plots -- ####

##### Root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    removal_flags = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "LY_F_CV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "LY_F_VM_5" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SO_O_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SO_O_CV_4" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SO_F_EN_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SE_O_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SE_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SE_F_EN_3" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SE_F_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "SE_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(removal_flags, "root_out")),
      
      plotID == "KA_F_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(removal_flags, "root_out")),
      
      TRUE ~ list(removal_flags)
    )
  ) %>%
  ungroup()

##### Canopy out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    removal_flags = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "LY_F_CV_5" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(removal_flags, "canopy_out")),
      
      plotID == "SE_O_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(removal_flags, "canopy_out")),
      
      TRUE ~ list(removal_flags)
    )
  ) %>%
  ungroup()

##### Canopy out and root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    removal_flags = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(removal_flags, "canopy_root_out")),
      
      TRUE ~ list(removal_flags)
    )
  ) %>%
  ungroup()

#### Adding other specific warnings to some replicates in the flags column #### Note: Nothing in the removals!

                  #### -- Control plots -- ####
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    control_flags = case_when(
      #Graminoids with maybe a flower on top. Should consider delete it if this is a problem (see picture JR's sphone)
      plotID == "LY_O_VM_2" & speciesID == "G" & plant_nr == 1 ~ 
        list(c(control_flags, "maybe_flower_height_measured")),
      
      plotID == "LY_O_VM_2" & speciesID == "G" & plant_nr == 3 ~ 
        list(c(control_flags, "maybe_flower_height_measured")),
      
      #Measurements taken on the same individual for 3 replicates - should consider averaging them?
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(control_flags, "replicates_from_same_individual")),
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(control_flags, "replicates_from_same_individual")),
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(control_flags, "replicates_from_same_individual")),
      
      #Top height higher than stem length for EN - which is strange as ENs are prostrate
      plotID == "KA_O_VM_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(control_flags, "top_height_higher_than_stem_length")),
      
      TRUE ~ list(control_flags)
    )
  ) %>%
  ungroup()

# 3. Rearrange dataset
## Rename after "comment_control" and "comment_removal"
vegetation_data_clean <- vegetation_data_clean %>%
  rename(
    control_comment = comments_1,
    removal_comment = comments_2
  )

## Reorganize the tibble so that flags_* columns come after their comment_* column
vegetation_data_clean <- vegetation_data_clean %>%
  select(
    year, date, date_2, site_name, siteID, habitat, plotID, recorder, weather,
    species, speciesID, plant_nr,
    control_top_height, control_bottom_height,
    control_stem_length, control_stem_diameter,
    control_comment, control_flags,
    removal_top_height, removal_bottom_height,
    removal_stem_length, removal_stem_diameter,
    removal_comment, removal_flags
  )

##Convert to long type: 
###This means moving everything related to control and removal plots into a single column, with an additional column specifying whether the measurement is from a control or removal plot.
vegetation_data_clean_long <- vegetation_data_clean %>%
  pivot_longer(
    cols = c(control_top_height, control_bottom_height, control_stem_length, control_stem_diameter, control_comment, control_flags,
             removal_top_height, removal_bottom_height, removal_stem_length, removal_stem_diameter, removal_comment, removal_flags),
    names_to = c("removal_control", ".value"),
    names_pattern = "(control|removal)_(.*)"
  )

# 4. Export cleaned dataset
## Change flags column into something readable in Excel
vegetation_export_csv <- vegetation_data_clean_long
vegetation_export_csv$flags <- vapply(
  vegetation_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export in CSV
write.csv(vegetation_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_traits_dwarf_shrubs_forbs_gram_summer_2025.csv", row.names = FALSE)

#--------------------------------------------------------------------------------------#

# 5. Quick plots to visualize data

#What you want to see on your plots
species_plot = 'EN' #Species ID to plot
habitat_plot = 'Open' #Habitat to plot

#Quick plot to see vegetation height between sites, only for certain speciesID (column name) and habitat - change species and habitat
ggplot(data = vegetation_data_stack %>% filter(speciesID == species_plot & habitat == habitat_plot)) +
  geom_boxplot(aes(x = site_name, y = control_top_height, color = site_name)) +
  labs(title = sprintf("Vegetation height (cm) in control plots for %s in %s habitat",species_plot, habitat_plot),
    x = "Site", y = "Height (cm)") +
  theme_minimal()

#Boxplot of vegetation height in control plots for open VS forested habitat for 4 sites - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot)) +
  geom_boxplot(aes(x=habitat, y=control_top_height, fill=habitat)) +
  facet_wrap(~site_name) +
  labs(title=sprintf("Vegetation height (cm) in control plots for %s in open VS forested habitat",species_plot),
       x="Habitat", y="Height (cm)") +
  theme_minimal()

#Quick plot to see vegetation height VS stem length for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot)) +
  geom_point(aes(x=control_stem_length, y=control_top_height, color=habitat)) +
  #Add a 1:1 line
  geom_abline(slope=1, intercept=0, linetype="dashed", color="black") +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_length, y=control_top_height, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title=sprintf("Vegetation height (cm) VS stem length (cm) in control plots for %s",species_plot),
       x="Stem length (cm)", y="Height (cm)") +
  theme_minimal()

# Quick plot to see vegetation height VS stem diameter for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot)) +
  geom_point(aes(x=control_stem_diameter, y=control_top_height, color=habitat)) +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_diameter, y=control_top_height, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title=sprintf("Vegetation height (cm) VS stem diameter (mm) in control plots for %s",species_plot),
       x="Stem diameter (mm)", y="Height (cm)") +
  theme_minimal()

# Quick plot to see stem length VS stem diameter for each site, for open VS forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot)) +
  geom_point(aes(x=control_stem_diameter, y=control_stem_length, color=habitat)) +
  #Fit regression line on Open VS Forested data
  geom_smooth(aes(x=control_stem_diameter, y=control_stem_length, color=habitat), method="lm", se=FALSE) +
  facet_wrap(~site_name) +
  labs(title=sprintf("Stem length (cm) VS stem diameter (mm) in control plots for %s",species_plot),
       x="Stem diameter (mm)", y="Stem length (cm)") +
  theme_minimal()

# Quick boxplot to see vegetation height in control VS removal plots for each site, for Open habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot & habitat=="Open")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, fill="Control")) +
  geom_boxplot(aes(x=site_name, y=removal_top_height, fill="Removal"), alpha=0.5) +
  labs(title=sprintf("Vegetation height (cm) in control VS removal plots for %s in open habitat",species_plot),
       x="Site", y="Height (cm)") +
  scale_fill_manual(name="Plot type", values=c("Control"="lightblue", "Removal"="salmon")) +
  theme_minimal()

# Quick boxplot to see vegetation height in control VS removal plots for each site, for Forested habitat - change species
ggplot(data=vegetation_data_stack %>% filter(speciesID==species_plot & habitat=="Forested")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, fill="Control")) +
  geom_boxplot(aes(x=site_name, y=removal_top_height, fill="Removal"), alpha=0.5) +
  labs(title=sprintf("Vegetation height (cm) in control VS removal plots for %s in forested habitat",species_plot),
       x="Site", y="Height (cm)") +
  scale_fill_manual(name="Plot type", values=c("Control"="lightblue", "Removal"="salmon")) +
  theme_minimal()


