######################################################
##### Script to clean WP4 DURIN Biomass weights ######
######################################################
# ---- Biomass weight measurements ---- #

# Install and load packages
packages <- c("tidyverse") #add any other packages that you may need to this list
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)


# Set working directory
wd <- getwd()
setwd(wd)

# File specifications
## Path to the csv file #Check that it matches what you downloaded!
file_path <- "raw_data/DURIN_WP_4_raw_4Corners_lab_biomass_weighing_dwarf_shrubs_2025.csv" #As you can see, I exported it as a csv file. You may want to work with Excel if you prefer. That would change some lines in the script.
#file_path <- "raw_data/DURIN_WP_4_raw_4Corners_lab_biomass_weighing_dwarf_shrubs_2025.csv" #As you can see, I exported it as a csv file. You may want to work with Excel if you prefer. That would change some lines in the script.

## Define column names
col_names <- c('recorder_sep','date_sep','time_begin_sep','time_end_sep','site_name','siteID','habitat','PlotID',
               'IndividualID','SpeciesID','Bag_code','IN_OUT_parts',
               'brown_stems_in','brown_stems_out',
               'date_red_leaves','recorder_red_leaves','pic_VM_red_leaves',
               'sep_status','dried_status','recorder_weighing','date_weighing','date_weighing_2','weighed_status',
               'biomass_brown_stem_in_1', 'biomass_brown_stem_in_2','biomass_brown_stem_in_3','biomass_brown_stem_in_4',
               'biomass_brown_stem_out',
               'biomass_dust_stem_in','biomass_dust_stem_out',
               'biomass_green_stem_in_1','biomass_green_stem_in_2','biomass_green_stem_in_3','biomass_green_stem_out',
               'biomass_leaves_all_EN_CV_1','biomass_leaves_all_EN_CV_2','biomass_leaves_all_EN_CV_3','biomass_leaves_all_EN_CV_4',
               'biomass_alive_leaves_in_1','biomass_alive_leaves_in_2','biomass_alive_leaves_in_3',
               'biomass_alive_leaves_in_4','biomass_alive_leaves_in_5','biomass_alive_leaves_in_6',
               'biomass_likely_alive_leaves_in','biomass_likely_alive_top_leaves_in','biomass_maybe_alive_leaves_in',
               'biomass_alive_leaves_out','biomass_maybe_alive_leaves_out',
               'biomass_dead_leaves_in','biomass_likely_dead_leaves_in',
               'biomass_maybe_dead_leaves_in_1','biomass_maybe_dead_leaves_in_2',
               'biomass_maybe_dead_top_leaves_in',
               'biomass_dead_leaves_out','biomass_maybe_dead_leaves_out',
               'biomass_berries_flowers_in',
               'biomass_berries_flowers_out',
               'petiole','biomass_dust_in','biomass_dust_out',
               'root_in_out',
               'comment_sep','comment_weighing','DO_NOT_EDIT_comment_after',
               'recorder_cut_VM','date_cut_VM','time_cut_VM','status_cut_VM','comment_cut_VM','pic_VM_from_June_23','check_JR_VM_Cut')

## Define column types #Update with new columns
col_spec <- cols(
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
  brown_stems_in = col_integer(),
  brown_stems_out = col_integer(),  
  date_red_leaves = col_date(format = ""),
  recorder_red_leaves = col_character(),
  pic_VM_red_leaves = col_character(),
  sep_status =  col_integer(),
  dried_status = col_integer(),
  recorder_weighing = col_character(),
  date_weighing = col_date(format = ""),
  date_weighing_2 = col_date(format = ""),
  weighed_status = col_integer(),

  biomass_brown_stem_in_1 = col_double(),
  biomass_brown_stem_in_2 = col_double(),
  biomass_brown_stem_in_3 = col_double(),
  biomass_brown_stem_in_4 = col_double(),
  
  biomass_brown_stem_out = col_double(),
  
  biomass_dust_stem_in = col_double(),
  biomass_dust_stem_out = col_double(),
  #update with biomass_dust_stem_in and biomass_dust_stem_out
  
  biomass_green_stem_in_1 = col_double(),
  biomass_green_stem_in_2 = col_double(),
  biomass_green_stem_in_3 = col_double(),
  
  biomass_green_stem_out = col_double(),
  
  biomass_leaves_all_EN_CV_1 = col_double(),
  biomass_leaves_all_EN_CV_2 = col_double(),
  biomass_leaves_all_EN_CV_3 = col_double(),
  biomass_leaves_all_EN_CV_4 = col_double(),
  
  biomass_alive_leaves_in_1 = col_double(),
  biomass_alive_leaves_in_2 = col_double(),
  biomass_alive_leaves_in_3 = col_double(),
  biomass_alive_leaves_in_4 = col_double(),
  biomass_alive_leaves_in_5 = col_double(),
  biomass_alive_leaves_in_6 = col_double(),
  
  biomass_likely_alive_leaves_in = col_double(),
  biomass_likely_alive_top_leaves_in = col_double(),
  biomass_maybe_alive_leaves_in = col_double(),
  
  biomass_alive_leaves_out = col_double(),
  biomass_maybe_alive_leaves_out = col_double(),
  
  biomass_dead_leaves_in = col_double(),
  biomass_likely_dead_leaves_in = col_double(),
  biomass_maybe_dead_leaves_in_1 = col_double(),
  biomass_maybe_dead_leaves_in_2 = col_double(),
  biomass_maybe_dead_top_leaves_in = col_double(),
  
  biomass_dead_leaves_out = col_double(),
  biomass_maybe_dead_leaves_out = col_double(),
  
  biomass_berries_flowers_in = col_double(),
  biomass_berries_flowers_out = col_double(),
  
  petiole = col_double(),
  
  biomass_dust_in = col_double(),
  biomass_dust_out = col_double(),
  
  root_in_out = col_character(),
  comment_sep = col_character(),
  comment_weighing = col_character(),
  DO_NOT_EDIT_comment_after = col_character(),
  recorder_cut_VM = col_character(),
  date_cut_VM = col_date(format = ""),
  time_cut_VM = col_time(format = ""),
  status_cut_VM = col_integer(),
  comment_cut_VM = col_character(),
  pic_VM_from_June_23 = col_character(),
  check_JR_VM_Cut = col_character()
)

# Read the file into a tibble
biomass_raw <- read_csv(file_path, col_types = col_spec, na = c("NA","")) #I exported the file as a csv. If you want to export (from Drive) and read it as an excel file, use read_excel() from the readxl package

####################################################################################
###################### MANDATORY DATA PROCESSING STEPS #############################
####################################################################################
# Calculate sum of all parts inside the bag (value 1, value 2, value 3, etc) #To update if more
biomass_raw <- biomass_raw %>%
  mutate(
    biomass_brown_stem_in = if_else(
      if_all(starts_with("biomass_brown_stem_in_"), is.na),
      NA_real_,
      rowSums(
        select(., starts_with("biomass_brown_stem_in_")),
        na.rm = TRUE)
      ),
    biomass_green_stem_in = if_else(
      if_all(starts_with("biomass_green_stem_in_"), is.na),
      NA_real_,
      rowSums(
      select(., starts_with("biomass_green_stem_in_")),
      na.rm = TRUE)
    ),
    biomass_leaves_all_EN_CV = if_else(
      if_all(starts_with("biomass_leaves_all_EN_CV_"),is.na),
      NA_real_,
      rowSums(
        select(.,starts_with('biomass_leaves_all_EN_CV_')),
        na.rm = TRUE)
      ),
    biomass_alive_leaves_in = if_else(
      if_all(starts_with("biomass_alive_leaves_in_"), is.na),
      NA_real_,
      rowSums(
        select(., starts_with("biomass_alive_leaves_in_")),
        na.rm = TRUE)
    ),
      biomass_maybe_dead_leaves_in = if_else(
        if_all(starts_with("biomass_maybe_dead_leaves_in_"), is.na),
        NA_real_,
        rowSums(
          select(., starts_with("biomass_maybe_dead_leaves_in_")),
          na.rm = TRUE)
      )
  )


# Delete columns with individual part weights inside the bag
biomass_raw <- biomass_raw %>%
  select(-starts_with("biomass_brown_stem_in_"), -starts_with("biomass_green_stem_in_"), -starts_with("biomass_leaves_all_EN_CV_"), -starts_with("biomass_alive_leaves_in_"), -starts_with("biomass_maybe_dead_leaves_in_"))  

# Calculate the sum of in and out part (necessary for individuals weight), and add a column for that
biomass_raw <- biomass_raw %>%
  mutate(
    biomass_brown_stem_total = ifelse(
      is.na(biomass_brown_stem_in) & is.na(biomass_brown_stem_out),
      NA,
      rowSums(cbind(biomass_brown_stem_in, biomass_brown_stem_out), na.rm = TRUE)
    ),
    biomass_green_stem_total = ifelse(
      is.na(biomass_green_stem_in) & is.na(biomass_green_stem_out),
      NA,
      rowSums(cbind(biomass_green_stem_in, biomass_green_stem_out), na.rm = TRUE)
    ),
    biomass_alive_leaves_total = ifelse(
      is.na(biomass_alive_leaves_in) & is.na(biomass_alive_leaves_out),
      NA,
      rowSums(cbind(biomass_alive_leaves_in, biomass_alive_leaves_out), na.rm = TRUE)
    ),
    biomass_likely_alive_leaves_total = biomass_likely_alive_leaves_in, # no OUT column 
    biomass_likely_alive_top_leaves_total = biomass_likely_alive_top_leaves_in, # no OUT column
    biomass_maybe_alive_leaves_total = ifelse(
      is.na(biomass_maybe_alive_leaves_in) & is.na(biomass_maybe_alive_leaves_out),
      NA,
      rowSums(cbind(biomass_maybe_alive_leaves_in, biomass_maybe_alive_leaves_out), na.rm = TRUE)
    ),
    biomass_dead_leaves_total = ifelse(
      is.na(biomass_dead_leaves_in) & is.na(biomass_dead_leaves_out),
      NA,
      rowSums(cbind(biomass_dead_leaves_in, biomass_dead_leaves_out), na.rm = TRUE)
    ),
    biomass_likely_dead_leaves_total = biomass_likely_dead_leaves_in,# no OUT column
    biomass_maybe_dead_leaves_total = ifelse(
      is.na(biomass_maybe_dead_leaves_in) & is.na(biomass_maybe_dead_leaves_out),
      NA,
      rowSums(cbind(biomass_maybe_dead_leaves_in, biomass_maybe_dead_leaves_out), na.rm = TRUE)
    ),
    biomass_maybe_dead_top_leaves_total = biomass_maybe_dead_top_leaves_in, # no OUT column
    biomass_berries_flowers_total = ifelse(
      is.na(biomass_berries_flowers_in) & is.na(biomass_berries_flowers_out),
      NA,
      rowSums(cbind(biomass_berries_flowers_in, biomass_berries_flowers_out), na.rm = TRUE)
    ),
    biomass_dust_total = ifelse(
      is.na(biomass_dust_in) & is.na(biomass_dust_out),
      NA,
      rowSums(cbind(biomass_dust_in, biomass_dust_out), na.rm = TRUE)
    ),
    biomass_dust_stem_total = ifelse(
      is.na(biomass_dust_stem_in) & is.na(biomass_dust_stem_out),
      NA,
      rowSums(cbind(biomass_dust_stem_in, biomass_dust_stem_out), na.rm = TRUE)
    )#update with biomass_dust_stem_in and biomass_dust_stem_out
  )

# Add a column for all the leaves (alive and dead) and all the stems (green and brown)
# Adding biomass_leaves_all_EN_CV for the total of leaves on all bag for EN and CV
cols_leaves <- c('biomass_leaves_all_EN_CV','biomass_alive_leaves_total','biomass_likely_alive_leaves_total','biomass_likely_alive_top_leaves_total',
                'biomass_maybe_alive_leaves_total','biomass_dead_leaves_total','biomass_likely_dead_leaves_total',
                'biomass_maybe_dead_leaves_total','biomass_maybe_dead_top_leaves_total','petiole')
cols_stem <- c('biomass_dust_stem_total','biomass_brown_stem_total','biomass_green_stem_total')
biomass_raw <- biomass_raw %>%
  mutate(
    biomass_leaves_total = if_else(
      if_all(all_of(cols_leaves), is.na),
      NA_real_,
      rowSums(across(all_of(cols_leaves)), na.rm = TRUE)
    ),
    biomass_stem_total = if_else(
      if_all(all_of(cols_stem), is.na),
      NA_real_,
      rowSums(across(all_of(cols_stem)), na.rm = TRUE)
    )
  )

# Add flag for "no bag found" and "two individuals" in replicate bags
# no bag found: E_SE_O_EN_2_VV_ALL; E_KA_F_EN_2_VM_ALL; E_SE_O_EN_4_VM_ALL; E_KA_O_VV_5_EN_ALL; E_LY_O_CV_2_VV_3 (CV found in bag instead)
# E_SO_F_CV_2_VM_2 - looks like two individuals, remove? 
# TO BE CONTINUED with CV and EN being processed. 

biomass_raw <- biomass_raw %>%
  mutate(
    flags_weight = case_when(
    Bag_code == "E_SE_O_EN_2_VV_ALL" ~ "is_there_an_all_bag", #maybe only 3 replicates and that's a mistake in the fieldsheet (3 was written)
    Bag_code == "E_KA_F_EN_2_VM_ALL" ~ "is_there_an_all_bag", #maybe only 3 replicates and that's a mistake in the fieldsheet (NA was written)
    Bag_code == "E_SE_O_EN_4_VM_ALL" ~ "is_there_an_all_bag", #maybe only 3 replicates and that's a mistake in the fieldsheet (NA was written)
    Bag_code == "E_KA_O_VV_5_EN_ALL" ~ "is_there_an_all_bag", #maybe only 3 replicates and that's a mistake in the fieldsheet (3 was written)
    Bag_code == "E_LY_O_CV_2_VV_3" ~ "no_bag_found",
    Bag_code == "E_SO_F_CV_2_VM_2" ~ "maybe_two_individuals",
    Bag_code == "E_SE_O_VM_3_VV_3" ~ "out_part_lost",
    Bag_code == "E_LY_F_EN_1_EN_2" ~ "out_part_lost"
  ))

#Delete unnecessary columns
clean_data <- biomass_raw %>%
  select(-"brown_stems_in",-"brown_stems_out",-"date_red_leaves",-"recorder_red_leaves",-"pic_VM_red_leaves")

#Move columns before comments
clean_data <- clean_data %>%
  select(-flags_weight,-comment_sep, -comment_weighing, -DO_NOT_EDIT_comment_after, everything(), comment_sep, comment_weighing, DO_NOT_EDIT_comment_after, flags_weight)

# Write as CSV
write_csv(clean_data,"clean_data/DURIN_WP4_clean_4Corners_lab_biomass_weight_dwarf_shrubs_2025.csv")

#####################################################################################
#Analysis
#Maybe should delete individual with no bag found, and out_part_lost + maybe_two_individuals as flags_weight

# First, compute the ratio of leaf to stem
clean_data_ratio <- clean_data %>%
  mutate(leaf_stem_ratio = biomass_leaves_total/biomass_stem_total)#biomass_alive_leaves_total / biomass_brown_stem_total) 

#For the coding of north south and inland coastal
clean_data_ratio <- clean_data_ratio %>%
  mutate(north_south = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                              labels = c("South", "North", "South", "North"))) %>%
  mutate(coast_inland = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                               labels = c("Coast", "Coast", "Inland", "Inland")))

# Organize levels
clean_data_ratio$north_south <- factor((clean_data_ratio$north_south),
                                       levels = c("North", "South"))

clean_data_ratio$coast_inland <- factor((clean_data_ratio$coast_inland),
                                        levels = c("Coast", "Inland"))


# Second, filter the data to only keep VV or EN  or specif site (North/South/VV_VV/etc)
# only for individuals bags
#Taking any VV that have been weighed
biomass_vv_weighed <- clean_data_ratio %>%
  filter(SpeciesID == "VV") %>%
  #filter(weighed_status == 1) %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3", "VV_4"))#sometimes, there are four replicates

# Define colour palette if using full name versions
Habitat <- c("Forested" = "#083508", "Open" = "#589758")
Species_Fruit <- c("CV" = "#DF697E",
                   "EN" = "#404040",
                   "VM" = "#323284",
                   "VV" = "#D93137")

#Plot leaf_stem_ratio for the 4 sites and habitat
#OUTLIERS ARE E_LY_O_EN_3 VV_3 and E_LY_O_EN_3 VV_4 TO BE CHECKED
ggplot(biomass_vv_weighed,
       aes(x = habitat,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Leaf to Stem ratio",
    color = "Habitat",
    fill = "Habitat"
  ) +
  
  theme_bw()

#Plot leaves total
ggplot(biomass_vv_weighed,
       aes(x = habitat,
           y = biomass_leaves_total,
           color = habitat,
           fill = habitat)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Biomass leaves total (g)",
    color = "Habitat",
    fill = "Habitat"
  ) +
  
  theme_bw()

#Plot stems total
ggplot(biomass_vv_weighed,
       aes(x = habitat,
           y = biomass_stem_total,
           color = habitat,
           fill = habitat)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Biomass stems total (g)",
    color = "Habitat",
    fill = "Habitat"
  ) +
  
  theme_bw()

#------------------------#
# Bas previous plots
# North sites, VV species
biomass_vv_north <- clean_data_ratio %>%
  filter(siteID == "KA" | siteID == "SE") %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1) %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3", "VV_4"))

# 4corners, VV species
# filter on plotID that contains VV
biomass_vv_4_corners<- clean_data_ratio %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1) %>%
filter(grepl("VV", PlotID)) %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3", "VV_4"))

#4 corners, VV_VV plots
biomass_vv_4corners <- clean_data_ratio %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1) %>%
  # filter on plotID that contains VV
  filter(grepl("VV", PlotID)) %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3", "VV_4"))

# Define colour palette if using full name versions
Habitat <- c("Forested" = "#083508", "Open" = "#589758")
Species_Fruit <- c("CV" = "#DF697E",
                   "EN" = "#404040",
                   "VM" = "#323284",
                   "VV" = "#D93137")



#Filter on plotID that have been weighed
biomass_weighed <- clean_data %>%
  filter(weighed_status == 1)


# #Sum of all the leaves insite the plot
# biomass_weighed <- biomass_weighed %>%
#   mutate(
#     biomass_alive_leave_total_in = rowSums(
#       select(., biomass_alive_leaves_in, biomass_likely_alive_leaves_in,
#              biomass_likely_alive_top_leaves_in, biomass_maybe_alive_leaves_in),
#       na.rm = TRUE
#     ),
#     biomass_stem_total_in = rowSums(
#       select(., biomass_brown_stem_in, biomass_green_stem_in),
#       na.rm = TRUE
#     ),
#     biomass_total_in = rowSums(
#       select(., biomass_alive_leaves_in, biomass_likely_alive_leaves_in,
#              biomass_likely_alive_top_leaves_in, biomass_maybe_alive_leaves_in,
#              biomass_brown_stem_in, biomass_green_stem_in),
#       na.rm = TRUE
#     )
#   )

#For the coding of north south and inland coastal
biomass_weighed <- biomass_weighed %>%
  mutate(north_south = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                              labels = c("South", "North", "South", "North"))) %>%
  mutate(coast_inland = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                               labels = c("Coast", "Coast", "Inland", "Inland")))


# Organize levels
biomass_weighed$north_south <- factor((biomass_weighed$north_south),
                                      levels = c("North", "South"))

biomass_weighed$coast_inland <- factor((biomass_weighed$coast_inland),
                                       levels = c("Coast", "Inland"))


# filter on individuals
biomass_weighed_individuals <- biomass_weighed %>%
  filter(IndividualID %in% c(
    "VV_1", "VV_2", "VV_3", "VV_4",
    "EN_1", "EN_2", "EN_3", "EN_4",
    "CV_1", "CV_2", "CV_3", "CV_4",
    "VM_1", "VM_2", "VM_3", "VM_4"
  ))

# #Sum of all the leaves insite the plot
# biomass_weighed_individuals <- biomass_weighed_individuals %>%
#   mutate(
#     biomass_alive_leave_total_in = rowSums(
#       select(., 
#              biomass_alive_leaves_in,
#              biomass_likely_alive_leaves_in,
#              biomass_likely_alive_top_leaves_in,
#              biomass_maybe_alive_leaves_in),
#       na.rm = TRUE
#     )
#   )

# #Sum of all the leaves + stems insite the plot
# biomass_weighed_individuals <- biomass_weighed_individuals %>%
#   mutate(
#     biomass_alive_leave_total_in = rowSums(
#       select(., biomass_alive_leaves_in, biomass_likely_alive_leaves_in,
#              biomass_likely_alive_top_leaves_in, biomass_maybe_alive_leaves_in),
#       na.rm = TRUE
#     ),
#     biomass_stem_total_in = rowSums(
#       select(., biomass_brown_stem_in, biomass_green_stem_in),
#       na.rm = TRUE
#     ),
#     biomass_total_in = rowSums(
#       select(., biomass_alive_leaves_in, biomass_likely_alive_leaves_in,
#              biomass_likely_alive_top_leaves_in, biomass_maybe_alive_leaves_in,
#              biomass_brown_stem_in, biomass_green_stem_in),
#       na.rm = TRUE
#     )
#   )

#filter EN species, only individuals, only weighed
biomass_EN_weighed <- biomass_weighed %>%
  filter(SpeciesID == "EN") %>%
  filter(weighed_status == 1) %>%
  filter(IndividualID %in% c("EN_1", "EN_2", "EN_3", "EN_4"))

#Calculate the ratio of leaf to stem for EN individuals
biomass_EN_weighed <- biomass_EN_weighed %>%
  mutate(leaf_stem_ratio = biomass_alive_leaves_total / biomass_brown_stem_total)


#Filter for EN + VV in the EN plots Kautokeino, individuals only
biomass_EN_VV_KA <- biomass_weighed %>%
  filter(siteID == "KA") %>%
  filter(grepl("EN", PlotID)) %>%
  filter(SpeciesID %in% c("EN", "VV")) %>%
  filter(weighed_status == 1) %>%
  filter(IndividualID %in% c("EN_1", "EN_2", "EN_3", "EN_4", "VV_1", "VV_2", "VV_3", "VV_4"))

#calculate the total of the leaves (alive + likely alive) for EN and VV individuals in the EN plots in Kautokeino
biomass_EN_VV_KA <- biomass_EN_VV_KA %>%
  mutate(biomass_alive_leaves_total_extra = rowSums(
    select(., biomass_alive_leaves_total, biomass_likely_alive_leaves_total),
    na.rm = TRUE
  ))

#calculate the ratio of leaf to stem for EN and VV individuals in the EN plots in Kautokeino
biomass_EN_VV_KA <- biomass_EN_VV_KA %>%
  mutate(leaf_stem_ratio = biomass_alive_leaves_total_extra / biomass_brown_stem_total)


#filter for VV in the north sites
biomass_VV_KA_SE <- biomass_weighed %>%
  filter(siteID == "KA" | siteID == "SE") %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1)

#calculate the total of the leaves (alive + likely alive) for EN and VV individuals in the EN plots in Kautokeino
biomass_VV_KA_SE <- biomass_VV_KA_SE %>%
  mutate(biomass_alive_leaves_total_extra = rowSums(
    select(., biomass_alive_leaves_total, biomass_likely_alive_leaves_total),
    na.rm = TRUE
  ))

# #calculate the ratio of leaf to stem for VV individuals in the north sites
# biomass_VV_KA_SE <- biomass_VV_KA_SE %>%
#   mutate(biomass_VV_KA_SE, leaf_stem_ratio = biomass_alive_leaves_total_extra / biomass_brown_stem_total)

#filter for VV in the north sites, only individuals
biomass_VV_KA_SE_individuals <- biomass_VV_KA_SE %>%
  filter(siteID == "KA" | siteID == "SE") %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1) %>%
  filter(IndividualID %in% c("VV_1", "VV_2", "VV_3", "VV_4"))

#filter for VV in the north sites
biomass_VV_KA_SE <- biomass_weighed %>%
  filter(siteID == "KA" | siteID == "SE") %>%
  filter(SpeciesID == "VV") %>%
  filter(weighed_status == 1)

# #calculate the total of the leaves (alive + likely alive) for EN and VV individuals in the EN plots in Kautokeino
# biomass_VV_KA_SE <- biomass_VV_KA_SE %>%
#   mutate(biomass_alive_leaves_total_extra = rowSums(
#     select(., biomass_alive_leaves_total, biomass_likely_alive_leaves_total),
#     na.rm = TRUE
#   ))

# #calculate the ratio of leaf to stem for VV individuals in the north sites
# biomass_VV_KA_SE <- biomass_VV_KA_SE %>%
#   mutate(biomass_VV_KA_SE, leaf_stem_ratio = biomass_alive_leaves_total_extra / biomass_brown_stem_total)
# 
# biomass_VV_KA_SE_individuals$Plot_species <- str_sub(biomass_VV_KA_SE_individuals$PlotID, start = -4, end = -3)
# 
# 


# Plot with Species along x-asix and coloured by habitat
ggplot(biomass_vv_4corners,
       aes(x = SpeciesID,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio",
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw()



# Plot with Habitat along x-asix and coloured by Species
ggplot(biomass_vv_4_corners,
       aes(x = habitat,
           y = leaf_stem_ratio,
           color = SpeciesID,
           fill = SpeciesID)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Leaf to Stem ratio",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()



#Plot with the weight of the parts only insite of the plot + only leaves
ggplot(biomass_weighed,
       aes(x = habitat,
           y = biomass_alive_leaves_in,
           color = SpeciesID,
           fill = SpeciesID)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.13,
    position = position_dodge(width = 0.5)
  ) +
  
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Mean of alive leaves insite the plot",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()


#plot with the weight of the parts only insite + only individual + only leaves
ggplot(biomass_weighed_individuals,
       aes(x = habitat,
           y = biomass_alive_leaves_in,
           color = SpeciesID,
           fill = SpeciesID)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.13,
    position = position_dodge(width = 0.5)
  ) +
  
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Mean of alive leaves insite the plot individuals level",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()



#plot with the weight of the plants only insite + only individual + leaves + stems
ggplot(biomass_weighed_individuals,
       aes(x = habitat,
           y = biomass_total_in,
           color = SpeciesID,
           fill = SpeciesID)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.13,
    position = position_dodge(width = 0.5)
  ) +
  
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Mean of alive leaves and stems insite the plot induviduals level",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()


#plot with the weight of the plants only insite + leaves + stems
ggplot(biomass_weighed,
       aes(x = habitat,
           y = biomass_total_in,
           color = SpeciesID,
           fill = SpeciesID)) +
  
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.4
  ) +
  
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.5)
  ) +
  
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.13,
    position = position_dodge(width = 0.5)
  ) +
  
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Mean of alive leaves and stems insite the plot",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()


#plot with the ratio of leaf to stem for EN individuals
ggplot(biomass_EN_weighed,
       aes(x = SpeciesID,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio EN individuals",
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw()


#plot with the ratio of leaf to stem for EN and VV individuals kautokeino
ggplot(biomass_EN_VV_KA,
       aes(x = SpeciesID,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio EN + VV individuals KA",
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw()


#plot with the ratio of leaf to stem for VV total in north sites
ggplot(biomass_VV_KA_SE,
       aes(x = SpeciesID,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio VV in north sites",
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw()


#plot with the ratio of leaf to stem for VV individual in north sites
ggplot(biomass_VV_KA_SE_individuals,
       aes(x = SpeciesID,
           y = leaf_stem_ratio,
           color = habitat,
           fill = habitat)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Habitat) +
  scale_fill_manual(values = Habitat) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio VV in north sites individual level",
    color = "Habitat",
    fill = "Habitat"
  ) +
  theme_bw()

##############################

#plot with the ratio of leaf to stem for VV individual in north sites
ggplot(biomass_VV_KA_SE_individuals,
       aes(x = habitat,
           y = leaf_stem_ratio,
           color = Plot_species,
           fill = Plot_species)) +
  # Add points for each plant nr and make them slightly offset from one another using jitter, and faint using alpha
  geom_point(
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width = 0.6
    ),
    alpha = 0.25
  ) +
  # Create summary statistics for mean point and error bar
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    position = position_dodge(width = 0.6)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15,
    position = position_dodge(width = 0.6)
  ) +
  # Use the manual colours we specified
  scale_color_manual(values = Species_Fruit) +
  scale_fill_manual(values = Species_Fruit) +
  facet_grid(north_south ~ coast_inland) +
  labs(
    x = "Species",
    y = "leave to stem ratio VV in north sites individual level",
    color = "Plot_species",
    fill = "Plot_species"
  ) +
  theme_bw()




