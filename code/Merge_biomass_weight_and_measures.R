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
  
  biomass_dust_stem_TOBECHECKED = col_double(),
  biomass_dust_stem_in = col_double(),
  biomass_dust_stem_out = col_double(),
  #update with biomass_dust_stem_in and biomass_dust_stem_out

  biomass_green_stem_out = col_double(),
  
  biomass_leaves_all_EN_CV = col_double(),
  
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
  plant_cover = col_character(), #importing as character to avoid problems with "<1" plant cover.
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
  comment = col_character(),
  recorder_lab = col_character(),
  date_lab = col_date(format = ""),
  comment_lab = col_character()
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
    # TO BE CONTINUED FOR CV's
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
####First, remove the empty KA_O_VM_2 plotID lines from db_measure
db_measure <- db_measure %>%
  filter(!(plotID == "E_KA_O_VM_2")) #Remove the empty KA_O_VM_2 plotID lines from db_measure
####Then change plotIDs
db_measure <- db_measure %>%
  mutate(plotID = case_when(
    plotID == "E_KA_O_" ~ "E_KA_O_VM_2", ##KA_O_ in measurements database is likely KA_O_VM_2 in the lab. There is no VM for this one though
    ## But no VM bags were found for KA_O_VM_2 in the lab too. There is a match in the number of VV
    ## but not in the dates (14/07 on bags and 15/07 in the database) # There is also only 1 EN.
    plotID == "E_KA_O_VM_3" ~ "E_KA_F_VM_3",##KA_O_VM_3 in the measurements database is likely KA_F_VM_3 in the lab (weight). It matches more or less
    ## with the number of individuals ... No EN too. 
    TRUE ~ plotID)
    )
###Update stem length  full_indiv_stem_length from LY_O_VV_2 and LY_O_VV_3 with values from LY_O_VV_2_A and LY_O_VV_2_B 
###because they were measured in the lab on the A and B bags
db_measure <- db_measure %>%
  mutate(stem_length_in = case_when(
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 2 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 2],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 2 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 2],
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 3 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 3],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 3 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 3],
    #plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 1 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 1]
    #plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 1 ~ db_measure$stem_length_in[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 1]
    TRUE ~ stem_length_in)
    )

db_measure <- db_measure %>%
  mutate(full_indiv_stem_length = case_when(
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 2 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 2],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 2 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 2],
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 3 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 3],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 3 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 3],
    #plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 1 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_A" & db_measure$speciesID == "VV" & db_measure$plant_nr == 1]
    #plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 1 ~ db_measure$full_indiv_stem_length[db_measure$plotID == "E_LY_O_VV_2_B" & db_measure$speciesID == "VV" & db_measure$plant_nr == 1]
    TRUE ~ full_indiv_stem_length)
  )

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
merged_db <- inner_join(db_measure,db_weight, by="Bag_code")#only keeps observations from db_measure that have a matching key in db_weight

#-----------------------------------------------------------------------------------------------#
####### START ANALYSIS ########
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

