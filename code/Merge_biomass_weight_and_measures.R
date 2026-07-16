############################################
###### Script to merge DURIN WP4 data ######
############################################

# --- Vegetation measurements on DWARF-SHRUBS in Extra WP4 biomass removal plots with biomass weights ---- #

# Install and load packages
packages <- c("tidyverse")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Set wd
wd <- getwd()
setwd(wd)

# File paths
file_path_weight <- "clean_data/DURIN_WP4_clean_4Corners_lab_biomass_weight_dwarf_shrubs_2025.csv" #As you can see, I exported it as a csv file. You may want to work with Excel if you prefer. That would change some lines in the script.
file_path_measure <- "clean_data/DURIN_WP4_clean_4Corners_field_biomass_structure_cover_dwarf_shrubs_2025.csv"

# Column specifications
col_spec_weight <- cols(
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
  
  sep_status =  col_integer(),
  dried_status = col_integer(),
  recorder_weighing = col_character(),
  date_weighing = col_date(format = ""),
  date_weighing_2 = col_date(format = ""),
  weighed_status = col_integer(),
  
  biomass_brown_stem_out = col_double(),
  
  biomass_dust_stem_in = col_double(),
  biomass_dust_stem_out = col_double(),
  #update with biomass_dust_stem_in and biomass_dust_stem_out

  biomass_green_stem_out = col_double(),
  
  biomass_likely_alive_leaves_in = col_double(),
  biomass_likely_alive_top_leaves_in = col_double(),
  biomass_maybe_alive_leaves_in = col_double(),
  
  biomass_alive_leaves_out = col_double(),
  biomass_maybe_alive_leaves_out = col_double(),
  
  biomass_dead_leaves_in = col_double(),
  biomass_likely_dead_leaves_in = col_double(),
  biomass_maybe_dead_top_leaves_in = col_double(),
  
  biomass_dead_leaves_out = col_double(),
  biomass_maybe_dead_leaves_out = col_double(),
  
  biomass_berries_flowers_in = col_double(),
  biomass_berries_flowers_out = col_double(),
  
  petiole = col_double(),
  
  biomass_dust_in = col_double(),
  biomass_dust_out = col_double(),
  root_in_out = col_character(),
  
  recorder_cut_VM = col_character(),
  date_cut_VM = col_date(format = ""),
  time_cut_VM = col_time(format = ""),
  status_cut_VM = col_integer(),
  comment_cut_VM = col_character(),
  pic_VM_from_June_23 = col_character(),
  
  biomass_brown_stem_in = col_double(),
  biomass_green_stem_in = col_double(),
  
  biomass_leaves_all_EN_CV = col_double(),
  
  biomass_alive_leaves_in = col_double(),
  biomass_maybe_dead_leaves_in = col_double(),
  
  biomass_brown_stem_total = col_double(),
  biomass_green_stem_total = col_double(),
  biomass_alive_leaves_total = col_double(),
  biomass_likely_alive_leaves_total = col_double(),
  biomass_likely_alive_top_leaves_total = col_double(),
  biomass_maybe_alive_leaves_total = col_double(),
  biomass_dead_leaves_total = col_double(),
  biomass_likely_dead_leaves_total = col_double(),
  biomass_maybe_dead_leaves_total = col_double(),
  biomass_maybe_dead_top_leaves_total = col_double(),
  biomass_berries_flowers_total = col_double(),
  biomass_dust_total = col_double(),
  
  biomass_leaves_total = col_double(),
  biomass_stem_total = col_double(),
  
  comment_sep = col_character(),
  comment_weighing = col_character(),
  DO_NOT_EDIT_comment_after = col_character(),
  
  flags_weight = col_character()
 
)

col_spec_measure <- cols(
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
  full_indiv_top_height = col_double(),
  full_indiv_stem_length = col_double(),              
  full_indiv_stem_diameter = col_double(), 
  number_harvested_indiv_without_3_rep = col_integer(),
  number_harvest_indiv = col_integer(),
  Pic_numbers = col_character(),
  comment = col_character(),
  recorder_lab = col_character(),
  date_lab = col_date(format = ""),
  comment_lab = col_character(),
  flags = col_character()
)

#Open cleaned database
db_weight <- read_csv(file_path_weight, col_types = col_spec_weight,na = c("NA",""))
db_measure <- read_csv(file_path_measure, col_types = col_spec_measure,na = c("NA",""))

# Merge the two databases by Bag_code
##Correct a few Bag_code and plotIDs based on observations in the lab ... in the measurements datasheet and in the biomass weight datasheet
## Maybe to be updated ... 
## Could be added to cleaning of weight and measure databases ... 

###Duplicate biomass bags
db_weight <- db_weight %>%
  mutate(PlotID = case_when(
    PlotID == "E_KA_F_VV_5_B" ~ "E_KA_F_VV_4", ##KA_F_VV_5_B in lab (weight) is very likely KA_F_VV_4 in the measurement database #98% sure :)
    PlotID == "E_KA_F_VV_5_A" ~ "E_KA_F_VV_5", ##KA_F_VV_5_A in lab (weight) is very likely KA_F_VV_5 in the measurement database #98% sure :)
    # Add mix in Lygra too: two LY_O_VV_2 bags (individual by individual because there is no way to differentiate the set of bags) 
    #.. a bit less sure about this one. We have new measurements in the lab for them, but not heights. 
    # The problem is only for VV and CV. Because there is no EN in the plots and VM is only in LY_O_VV_2
    # in the weight database, there is only VV_2_A and VV_2_B but in the measurements database, there is also VV_2 and VV_3 measurements.
    # TO BE CONTINUED FOR CV's in this plot
    Bag_code == "E_LY_O_VV_2_A_VV_ALL" ~ "E_LY_O_VV_3", # E_LY_O_VV_2_A_VV_ALL has 12+3 individuals so this may be E_LY_O_VV_3_VV_ALL that has 16 individuals in the fieldsheet
    Bag_code == "E_LY_O_VV_2_B_VV_ALL" ~ "E_LY_O_VV_2", # E_LY_O_VV_2_B_VV_ALL has 28 + 3 individuals so this should be E_LY_O_VV_2_VV_ALL that has 31 individuals in the fieldsheet
    Bag_code == "E_LY_O_VV_2_B_VV_2" ~ "E_LY_O_VV_2", #stem length for VV_2_B is very short; stem length cannot be much bigger than height so this should be VV_2. 
    Bag_code == "E_LY_O_VV_2_A_VV_2" ~ "E_LY_O_VV_3", #So VV_2_A should be VV_3
    Bag_code == "E_LY_O_VV_2_B_VV_3" ~ "E_LY_O_VV_2", #stem length very short so should be VV_2.
    Bag_code == "E_LY_O_VV_2_A_VV_3" ~ "E_LY_O_VV_3", #So VV_2_A should be VV_3
    #Bag_code == "E_LY_O_VV_2_A_VV_1" ~ "E_LY_O_VV_3", # Not sure about these two. 
    #Bag_code == "E_LY_O_VV_2_B_VV_1" ~ "E_LY_O_VV_2" # Not sure about these two. These ones (VV2_B_VV_1 and VV_2_VV_1) have the same height/stem length. Yet, the individual is a bit
    #twisted. Which means that the height should not be much bigger than the stem length, which is the case for VV_2_B_VV_1 versus VV_3_VV_1.
    TRUE ~ PlotID)
    )

###Wrong plotID on fieldsheets
#Done in Clean_WP4_dwarf-shrubs_measurements_Biomass_plots.R

## Then add Bag_code to the db_measure database based on the PlotID, speciesID and plant_nr columns
db_measure <- db_measure %>%
  mutate(Bag_code = paste0(plotID, "_", speciesID, "_", plant_nr))
##And update bag code in weight too
db_weight <- db_weight %>%
  mutate(Bag_code = paste0(PlotID, "_", IndividualID))

##Change PlotID column in db_weight to plotID
db_weight <- db_weight %>%
  rename(plotID = PlotID)

#Keep Bag_code rows that are common to the two databases
##Merge the two databases by Bag_code
merged_db <- inner_join(db_measure,db_weight)#, by="Bag_code")#only keeps observations from db_measure that have a matching key in db_weight

#HEIGHT ON FULL INDIV FOR NORTH SITE AS A FUNCTION OF BIOMASS OF LEAVES IN/OUT for North sites
# Where height outside of the plot was measured only on the part out
# In the South, it's the full individual height
## Calculate the proportion of leaves in and out
### Column names for leaves biomass in and out
cols_leaves_in <- c('biomass_likely_alive_leaves_in','biomass_likely_alive_top_leaves_in','biomass_maybe_alive_leaves_in','biomass_dead_leaves_in',
                    'biomass_likely_dead_leaves_in','biomass_maybe_dead_top_leaves_in',  'biomass_alive_leaves_in','biomass_maybe_dead_leaves_in')
cols_leaves_out <- c('biomass_alive_leaves_out','biomass_maybe_alive_leaves_out','biomass_dead_leaves_out','biomass_maybe_dead_leaves_out')

### Total biomass for leaves in and out based on cols_leaves_in and cols_leaves_out
merged_db <- merged_db %>%
  mutate(
    biomass_leaves_in = if_else(
      if_all(all_of(cols_leaves_in),is.na),
      NA_real_,
      rowSums(across(all_of(cols_leaves_in)),na.rm = TRUE)
      ),
    biomass_leaves_out = if_else(
      if_all(all_of(cols_leaves_out),is.na),
      NA_real_,
      rowSums(across(all_of(cols_leaves_out)),na.rm=TRUE)
    )
    )

### Weighed mean of height of full individual based on the biomass of leaves in and out
### The idea behind it is that it is mimicking the behavior on the field when measuring the height
### of the full individual, taking into account the in and out parts (i.e., what was done in the South):
### if less biomass is outside, this tends to be less taken into account in the height measurement.

merged_db <- merged_db %>%
  mutate(top_height_in_out_all = if_else(is.na(top_height_in_out_all) & !is.na(top_height_out_all) & !is.na(top_height_in_all) & !is.na(biomass_leaves_in) & !is.na(biomass_leaves_out),
                                         ((biomass_leaves_in*top_height_in_all) + (biomass_leaves_out*top_height_out_all))/(biomass_leaves_in + biomass_leaves_out),
                                         top_height_in_out_all)
         )

### Two decimal for top_height_in_out_all
merged_db <- merged_db %>%
  mutate(top_height_in_out_all = round(top_height_in_out_all,2))

### Full individual top height
merged_db <- merged_db %>%
  mutate(
    full_indiv_top_height = case_when(
      !is.na(top_height_in_out_all) ~ top_height_in_out_all,# now with in_out for North sites too. 
      is.na(top_height_in_out_all) & is.na(top_height_out_all) & !is.na(top_height_in_all) ~ top_height_in_all,
      TRUE ~ NA_real_
    )
  )#the remaining NA's are either individuals without measurements but with weights, or not weighed yet;

#-----------------------------------------------------------------------------------------------#
####### START ANALYSIS ########
#NOTE: it's only for the INDIVIDUALS THAT ARE IN COMMON BETWEEN WEIGHT AND MEASUREMENTS DATABASE
#IF you plot only measurements from the merged_db it's likely that you will also plot 
#measurements that do not have yet a weight measured, but whose bag_code is already included
#in the weight database.

#!! maybe should remove individuals with out_part_lost as flags and "no_bag_found" as flags_weight

# Plot biomass weight as a function of stem diameter
##biomass_leaves_total as a function of full_indiv_stem_diameter
ggplot(merged_db, aes(x = full_indiv_stem_diameter, y = biomass_leaves_total)) +
  geom_point() +
  labs(x = "Stem Diameter (mm)", y = "Total Leaf Biomass (g)") +
  theme_minimal()

##biomass_stem_total
ggplot(merged_db, aes(x = full_indiv_stem_diameter, y = biomass_stem_total)) +
  geom_point() +
  labs(x = "Stem Diameter (mm)", y = "Total Stem Biomass (g)") +
  theme_minimal()
