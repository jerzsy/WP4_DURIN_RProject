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

# Stack all data into one tibble
vegetation_data_stack <- bind_rows(vegetation_data_list)#, .id = "site_name")

# Data cleaning

## 1. Add columns with flag for removal and control plots
#-------------------------------------------------------------------#
### Flag function for automatic check on measurements! ###
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
    flag_control = pmap(
      list(control_top_height,
           control_bottom_height,
           control_stem_length,
           control_stem_diameter,
           speciesID),
      make_flags
    ),
    flag_removal = pmap(
      list(removal_top_height,
           removal_bottom_height,
           removal_stem_length,
           removal_stem_diameter,
           speciesID),
      make_flags
    )
  )

### Adding some warnings on data manually in the flag column

#### Configuration of replicates - canopy, root out? ####

            #### -- Control plots -- ####

##### Root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_control = case_when(
      plotID == "LY_O_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "LY_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "VM" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "LY_F_VM_1" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "LY_F_VM_3" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_EN_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_EN_3" & speciesID == "VM" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_EN_5" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_CV_1" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_CV_3" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_CV_3" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_CV_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_VM_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_VM_1" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_VM_3" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_VM_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_O_VV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_EN_1" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_EN_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_CV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_CV_5" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_VV_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_VV_5" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SO_F_VV_5" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_O_EN_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_O_EN_2" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_O_CV_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_O_VM_5" & speciesID == "VV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_O_VV_5" & speciesID == "VV" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_F_EN_5" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_F_CV_1" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_F_CV_2" & speciesID == "VM" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "SE_F_VM_5" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      plotID == "KA_O_VV_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "root_out")),
      
      TRUE ~ list(flag_control)
    )
  ) %>%
  ungroup()

##### Canopy out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_control = case_when(
      plotID == "SO_O_CV_4" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 1 ~ 
        list(c(flag_control, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 2 ~ 
        list(c(flag_control, "canopy_out")),
      
      plotID == "SO_F_VM_4" & speciesID == "G" & plant_nr == 3 ~ 
        list(c(flag_control, "canopy_out")),
      
      plotID == "SE_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_control, "canopy_out")),
      
      TRUE ~ list(flag_control)
    )
  ) %>%
  ungroup()

##### Canopy out; root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_control = case_when(
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "canopy_root_out")),
      
      plotID == "SO_F_CV_4" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "canopy_root_out")),
      
      plotID == "SO_F_CV_4" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_control, "canopy_root_out")),
      
      TRUE ~ list(flag_control)
    )
  ) %>%
  ungroup()


                #### -- Removal plots -- ####

##### Root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_removal = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "LY_F_CV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "LY_F_VM_5" & speciesID == "VM" & plant_nr == 2 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SO_O_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SO_O_CV_4" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SO_F_EN_4" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SE_O_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SE_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SE_F_EN_3" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SE_F_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "SE_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "KA_O_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_removal, "root_out")),
      
      plotID == "KA_F_EN_4" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_removal, "root_out")),
      
      TRUE ~ list(flag_removal)
    )
  ) %>%
  ungroup()

##### Canopy out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_removal = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "LY_F_CV_2" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "LY_F_CV_5" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 1 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "SO_O_EN_2" & speciesID == "EN" & plant_nr == 3 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "SO_O_CV_2" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_removal, "canopy_out")),
      
      plotID == "SE_O_CV_1" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_removal, "canopy_out")),
      
      TRUE ~ list(flag_removal)
    )
  ) %>%
  ungroup()

##### Canopy out; root out
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_removal = case_when(
      plotID == "LY_F_CV_1" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_removal, "canopy_root_out")),
      
      TRUE ~ list(flag_removal)
    )
  ) %>%
  ungroup()

#### Adding other specific warnings to some replicates in the flag column #### Nothing in the removals!

                  #### -- Control plots -- ####
vegetation_data_clean <- vegetation_data_clean %>%
  rowwise() %>%
  mutate(
    flag_control = case_when(
      #Graminoids with maybe a flower on top. Should consider delete it if this is a problem (see picture JR's sphone)
      plotID == "LY_O_VM_2" & speciesID == "G" & plant_nr == 1 ~ 
        list(c(flag_control, "maybe_flower_height_measured")),
      
      plotID == "LY_O_VM_2" & speciesID == "G" & plant_nr == 3 ~ 
        list(c(flag_control, "maybe_flower_height_measured")),
      
      #Measurements taken on the same individual for 3 replicates - should consider averaging them?
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 1 ~ 
        list(c(flag_control, "replicates_from_same_individual")),
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 2 ~ 
        list(c(flag_control, "replicates_from_same_individual")),
      plotID == "SO_O_VV_3" & speciesID == "CV" & plant_nr == 3 ~ 
        list(c(flag_control, "replicates_from_same_individual")),
      
      #Top height higher than stem length for EN - which is strange as ENs are prostrate
      plotID == "KA_O_VM_2" & speciesID == "EN" & plant_nr == 2 ~ 
        list(c(flag_control, "top_height_higher_than_stem_length")),
      
      TRUE ~ list(flag_control)
    )
  ) %>%
  ungroup()

## Rename after "comment_control" and "comment_removal"
vegetation_data_clean <- vegetation_data_clean %>%
  rename(
    comment_control = comments_1,
    comment_removal = comments_2
  )

## Reorganize the tibble so that flag_* columns come after their comment_* column
vegetation_data_clean <- vegetation_data_clean %>%
  select(
    year, date, date_2, site_name, siteID, habitat, plotID, recorder, weather,
    species, speciesID, plant_nr,
    control_top_height, control_bottom_height,
    control_stem_length, control_stem_diameter,
    comment_control, flag_control,
    removal_top_height, removal_bottom_height,
    removal_stem_length, removal_stem_diameter,
    comment_removal, flag_removal
  )

#Convert to long type: 
##removal and control data in one column
#vegetation_data_long <- vegetation_data_clean %>%
#  pivot_longer(
#    cols = c(control_top_height, control_bottom_height, control_stem_length, control_stem_diameter,
#             removal_top_height, removal_bottom_height, removal_stem_length, removal_stem_diameter),
#    names_to = c("type", ".value"),
#    names_pattern = "(control|removal)_(.*)"
#  )


##and add a column with "removal" or "control" type


# Export in csv
vegetation_export <- vegetation_data_clean %>%
  mutate(
    flag_control_csv = sapply(flag_control, paste, collapse = "; "),
    flag_removal_csv = sapply(flag_removal, paste, collapse = "; ")
  )

write.csv(vegetation_export, "vegetation_data_clean.csv", row.names = FALSE)

#Quick check on database
#View(vegetation_data_list$LYGRA)

#Quick plot to see vegetation height between sites, only for certain speciesID (column name) and habitat
ggplot(data=vegetation_data_stack %>% filter(speciesID=="EN" & habitat=="Forested")) +
  geom_boxplot(aes(x=site_name, y=control_top_height, color=site_name)) +
  labs(title="Vegetation height (cm) in control plots for EN in open habitat",
       x="Site", y="Height (cm)") +
  theme_minimal()

#Boxplot of vegetation height in control plots for open VS forested habitat for 4 sites
ggplot(data=vegetation_data_stack %>% filter(speciesID=="VV")) +
  geom_boxplot(aes(x=habitat, y=control_top_height, fill=habitat)) +
  facet_wrap(~site_name) +
  labs(title="Vegetation height (cm) in control plots for VV in open VS forested habitat",
       x="Habitat", y="Height (cm)") +
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


