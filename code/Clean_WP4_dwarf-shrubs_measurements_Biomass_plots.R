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
file_path<- "raw_data/DURIN_WP4_raw_4Corners_field_biomass_trait_cover_dwarf_shrubs_2025_OFF_ALL.csv"#"raw_data/DURIN_WP4_raw_4Corners_field_biomass_structure_cover_dwarf_shrubs_2025_OFF_SE_KA_ONLY.csv" #UPDATE LATER WITH LYGRA AND SOGNDAL

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
# Make sure there is no blank space in the beginning or the end of the string in the character columns
raw_df <- raw_df %>%
  mutate(across(where(is.character), ~ trimws(.)))
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

# 4. Correct a few measurements with "0" as top height. I checked in the lab, they either don't have any canopy associated
# with the measurements, or all dead leaves. Updating them to NAs.
raw_df <- raw_df %>%
  mutate(
    top_height_out_all = case_when(
      plotID == "E_SE_O_EN_4" & speciesID == "EN" & plant_nr == 3 ~ NA_real_,#no canopy
      plotID == "E_SE_O_EN_3" & speciesID == "EN" & plant_nr == 2 ~ NA_real_,#no leaves alive
      plotID == "E_SE_O_CV_2" & speciesID == "CV" & plant_nr == 2 ~ NA_real_,#no canopy
      #plotID == "E_SE_O_VM_1" & speciesID == "EN" & plant_nr == 3 ~ NA_real_,#there is canopy so this is strange!
      plotID == "E_SE_O_VV_3" & speciesID == "EN" & plant_nr == 1 ~ NA_real_,#no canopy
      plotID == "E_SE_O_VV_3" & speciesID == "EN" & plant_nr == 2 ~ NA_real_,#no canopy
      TRUE ~ top_height_out_all
    )
  )

# 4. Add columns with flags
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
    case_when(# check for missing value inside the plot
      species %in% c("EN","VV","CV","VM") &
        (is.na(top_in) | is.na(stem_len_in) | is.na(stem_diam_in)) &
        !(is.na(top_in) & is.na(stem_len_in) & is.na(stem_diam_in)) ~ "missing_measured_value_in",
      TRUE ~ NA_character_
    ),
    
    #check for NA values in EN, VV, CV or VM stem length/stem diameter when part of the individual is out
    case_when(#no stem length out while stem diameter out. If stem diameter out, there is a stem out. if height in only, can be missing value (SO, HO) 
      # with maybe a mistake on diam out (on canopy)
      # Case HI SI DO, see below.
      species %in% c("EN","VV","CV","VM") &
        (is.na(stem_len_out) & is.na(stem_len_in_out)) & (is.na(top_out) & is.na(top_in_out) & is.na(top_in_out_flowers)) &
        !(is.na(stem_diam_out)) ~ "missing_measured_value_out_check_root", 
      TRUE ~ NA_character_
    ),
    case_when( # if stem out, can be either stem or canopy out or both so missing diameter out or height out.
      #Case HI SO DI, see below.
      species %in% c("EN","VV","CV","VM") &
        (!(is.na(stem_len_out)) | !(is.na(stem_len_in_out))) &
        (is.na(stem_diam_out) & is.na(top_out) & is.na(top_in_out) & is.na(top_in_out_flowers)) ~ "missing_measured_value_out",
      TRUE ~ NA_character_
    ),
    case_when( # if diam out and canopy (H) out but no stem out (to root): either missing value (SO) or measured diam on canopy
      #Case HO SI DO, see below.
      species %in% c("EN","VV","CV","VM") &
        !(is.na(stem_diam_out)) & !(is.na(top_out) & is.na(top_in_out) & is.na(top_in_out_flowers)) &
        (is.na(stem_len_out) & is.na(stem_len_in_out)) ~ "missing_measured_value_out_or_MISTAKE_ON_DIAM_OUT",
      TRUE ~ NA_character_
    ),
    # case_when( # if diam out and canopy (H) out and stem out (tip to root): maybe they measured diam on canopy
    #   #Case HO SO DO, see below. #CHECKED
    #   species %in% c("EN","VV","CV","VM") &
    #     !(is.na(stem_diam_out)) & !(is.na(top_out) & is.na(top_in_out) & is.na(top_in_out_flowers)) &
    #     (!(is.na(stem_len_out)) | !(is.na(stem_len_in_out))) ~ "MAYBE_MISTAKE_ON_DIAM_OUT_TO_CHECK",
    #   TRUE ~ NA_character_
    # ),
    
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
#E_SO_F_CV_2_VM_2 look like there are two different individuals - should this be removed? #in weight database cleaning
#E_SE_O_VM_3_VV_3 out part lost in the field + E_LY_F_EN_1_EN2 #here and in weight database cleaning
clean_df <- clean_df %>%
  rowwise() %>%
  mutate(
    flags = case_when(
      plotID == "E_SE_O_VM_3" & speciesID == "VV" & plant_nr == 3 ~ list(c(flags, "out_part_lost")),
      plotID == "E_LY_F_EN_1" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "out_part_lost")),
      TRUE ~ list(flags)
    )
  )%>%
  ungroup()

## Check for North: I think they measured out all the time (or at least sometimes), even when not rooted outside. At least for some.
# So (H/S/D= height/stem length/diameter; I/O: inside/inside+outside):
# HI SI DI - if no mistake in the field (no missing value), this case is fine.
# HI SI DO - this case should not exist - must be a missing value (logically SO, maybe HO in the North) - check root in/out + missing value in lab
# HI SO DI - this case should not exist - must be a missing value (either DO or HO) - check missing value
# HI SO DO - if no mistake in the field (no missing value), this case is fine.
# HO SI DI - if longest stem is in the plot, this case is fine.
# HO SI DO - this case should not exist - must be a missing value (SO) or they measured diameter on canopy out - check root in/out OR missing value in lab
# HO SO DI - if no mistake in the field (no missing value), this case is fine.
# HO SO DO - if individual rooted out, it's fine, if rooted in, no - check root in/out in lab.
# To be checked: HI SI DO; HI SO DI?; HO SI DO; HO SO DO

## Export a table with bagID for flags "missing_measured_value_out_check_root"; "missing_measured_value_out"; "missing_measured_value_out_or_MISTAKE_ON_DIAM_OUT";
# "MAYBE_MISTAKE_ON_DIAM_OUT_TO_CHECK"

# target_flags <- c(
#   "missing_measured_value_out_check_root",
#   "missing_measured_value_out",
#   "missing_measured_value_out_or_MISTAKE_ON_DIAM_OUT",
#   "MAYBE_MISTAKE_ON_DIAM_OUT_TO_CHECK"
# )
# 
# flag_export <- clean_df %>%
#   tidyr::unnest(flags) %>%
#   dplyr::filter(flags %in% target_flags) %>%
#   dplyr::select(plotID, speciesID, plant_nr, flags)
# 
# write.csv(flag_export, "clean_data/flag_report_bagID.csv", row.names = FALSE)

##Now, add manual flags for the root in/root out. With the flag_report_bagID, I came back to the lab with Maike and we checked every bags to identify
#if it was rooted in or out. So now, I will add a flag for the one that are actually rooted in or likely in, or likely out.
#I don't write the ones that were identified as out, as it is as it should be. See fieldsheet for more details.
#Note, when we checked, some where already separated for leaves and stem + some other already weighed (cut, without leaves)
# I will also manually select the diameter in for these ones that actually have a diameter in and out measured.   
#E_SE_O_EN_3_EN_2 root_likely_out #pic #thickest stem out, connects to stem from inside
#E_SE_O_CV_2_EN_1 root_in 
#E_SE_O_CV_2_EN_2 root_in
#E_SE_O_VM_5_EN_3 root_in #only roots inside
#E_SE_F_EN_2_VM_3 root_in #pic
#E_SE_F_EN_2_EN_1 root_in #pic #thickest stem in
#E_SE_F_EN_2_EN_3 root_likely_out #pic #hairy roots only on the outside, thickes stem on the outside
#E_SE_F_EN_3_EN_2 unclear_where_rooted #pic #hairy roots only out
#E_SE_F_EN_5_EN_1 root_likely_out #pic #thickest stem out, connects to stem from inside
#E_SE_F_CV_3_EN_1 root_likely_out #pic #hairroots only on the outside, thickest stem on the outside
#E_SE_F_CV_3_EN_2 root_likely_in #pic #hairroots only inside, thickest stem inside
#E_SE_F_VM_2_VM_1 root_likely_in #pic in VM_cut pics
#E_SE_F_VV_5_EN_2 root_likely_out #pic #hairroots only on the outside, thickest stem on the outside
#E_KA_O_EN_1_EN_2 unclear_where_rooted #pic #hairroots in and out, thickest stem in
#E_KA_O_EN_1_EN_3 root_likely_out #pic #thickest stem out, connects to stem in which seems close to canopy
#E_KA_O_EN_2_EN_2 root_likely_in #pic #thickest stem in, connects to stem out
#E_KA_O_VV_5_EN_2 root_likely_out #big stem out #pic
#E_KA_F_EN_2_VM_1 missing_diam_out
#E_KA_F_EN_3_EN_3 root_likely_out #pic #thickest stem out, connects to stem IN which seems closer to canopy
#E_KA_F_VV_4_EN_2 root_likely_out #VV_5_B in lab #pic
#E_KA_F_VV_5_EN_2 root_in #VV_5_A in lab #dead leaves IN only #pic
#E_KA_F_VV_5_EN_3 root in #VV_5_A in lab #no leaves in #pic
#E_KA_O_..._EN_1 root_out #KA_O_VM_2_EN_1 in lab

#TO BE CONTINUED FOR CV #DONE #ALL OUT

clean_df <- clean_df %>% #CHECK HERE #ROWWISE?
  rowwise() %>%
  mutate(
    flags = case_when(
      plotID == "E_SE_O_EN_3" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_out")),
      plotID == "E_SE_O_CV_2" & speciesID == "EN" & plant_nr == 1 ~ list(c(flags, "root_in")),
      plotID == "E_SE_O_CV_2" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_in")),
      plotID == "E_SE_O_VM_5" & speciesID == "EN" & plant_nr == 3 ~ list(c(flags, "root_in")),
      plotID == "E_SE_F_EN_2" & speciesID == "VM" & plant_nr == 3 ~ list(c(flags, "root_in")),
      plotID == "E_SE_F_EN_2" & speciesID == "EN" & plant_nr == 1 ~ list(c(flags, "root_in")),
      plotID == "E_SE_F_EN_2" & speciesID == "EN" & plant_nr == 3 ~ list(c(flags, "root_likely_out")),
      plotID == "E_SE_F_EN_3" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "unclear_where_rooted")),
      plotID == "E_SE_F_EN_5" & speciesID == "EN" & plant_nr == 1 ~ list(c(flags, "root_likely_out")),
      plotID == "E_SE_F_CV_3" & speciesID == "EN" & plant_nr == 1 ~ list(c(flags, "root_likely_out")),
      plotID == "E_SE_F_CV_3" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_in")),
      plotID == "E_SE_F_VM_2" & speciesID == "VM" & plant_nr == 1 ~ list(c(flags, "root_likely_in")),
      plotID == "E_SE_F_VV_5" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_out")),
      plotID == "E_KA_O_EN_1" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "unclear_where_rooted")),
      plotID == "E_KA_O_EN_1" & speciesID == "EN" & plant_nr == 3 ~ list(c(flags, "root_likely_out")),
      plotID == "E_KA_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_in")),
      plotID == "E_KA_O_VV_5" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_out")),
      plotID == "E_KA_F_EN_2" & speciesID == "VM" & plant_nr == 1 ~ list(c(flags, "missing_diam_out")),
      plotID == "E_KA_F_EN_3" & speciesID == "EN" & plant_nr == 3 ~ list(c(flags, "root_likely_out")),
      plotID == "E_KA_F_VV_4" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_likely_out")),
      plotID == "E_KA_F_VV_5" & speciesID == "EN" & plant_nr == 2 ~ list(c(flags, "root_in")),
      plotID == "E_KA_F_VV_5" & speciesID == "EN" & plant_nr == 3 ~ list(c(flags, "root_in")),
      TRUE ~ list(flags)
    )
  )%>%
  ungroup()

# 5. Adding missing number of harvested individuals based on information found on the biomass bag
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
        
        plotID == "E_SE_F_CV_2" & speciesID == "CV" ~ 2,#1 in all bag (or maybe two as there are two different branches but seemed to be from the same individiual) + 1 unknown bag ; to count in the lab!
        plotID == "E_KA_F_VM_2" & speciesID == "VV" ~ 30,#to count in the lab!#counted
        #plotID == "E_KA_F_VV_4" & speciesID == "EN" ~ ,#figuring out if this is actually VV_5_B!
        #plotID == "E_KA_F_VV_5" & speciesID == "VV" ~ ,#figuring out if this is actually VV_5_A!
        plotID == "E_SO_F_CV_2" & speciesID == "VM" ~ 22,#to count in the lab!#counted
        #plotID == "E_SO_F_CV_2" & speciesID == "CV" ~ ,#to count in the lab!
        
        plotID == "E_SE_O_EN_2" & speciesID == "VV" ~ 0,#was written 3 for the 3 replicates instead of 0
        plotID == "E_KA_O_VV_5" & speciesID == "EN" ~ 0,#was written 3 for the 3 replicates instead of likely 0 as we could not find an "all" bag.
        plotID == "E_SE_F_CV_3" & speciesID == "VV" ~ 0,#was written 2 for the 2 replicates instead of likely 0 (no extra all bags).
        
        TRUE ~ number_harvested_indiv_without_3_rep
      )
  )

## Now calculate the total number of harvested individuals
## If NA in the 'without replicates' number --> NA (Northern sites) or number of harvested individuals (for Southern sites where we directly 
## calculated the total number of individuals on the field and not the number of individuals harvested without the replicates)
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
      is.na(number_harvested_indiv_without_3_rep) ~ number_harvest_indiv,# = number_harvest_indiv and not NA because for South sites (LY, SO), 
                                                                        #we directly calculated the total number of individuals on the field
      !is.na(number_harvested_indiv_without_3_rep) ~ number_harvested_indiv_without_3_rep + number_replicates,
      TRUE ~ number_harvest_indiv
    )
  ) %>%
  ungroup() %>%
  select(-number_replicates)

# 6. Create new columns that give the stem diameter and stem length of the full individual, whether it is laying only in the plot
# Or both in and outside of the plot.
# This means, if there is a stem length in + out, this is the full length of the individual; otherwise it's the stem length in.
# This means, if there is a stem diameter out, this is the full diameter of the individual; otherwise it's the stem diameter in. 
# Except for the individuals from the North that have been identified in the lab as root_in
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

#Exceptions for individuals in the north rooted in with stem diameter out and in measured
#in and likely in 
# TO BE CONTINUED FOR CV #DONE ALL ROOTED OUT
clean_df <- clean_df %>% #CHECK HERE
  mutate(
    full_indiv_stem_diameter = case_when(
      plotID == "E_SE_O_CV_2" & speciesID == "EN" & plant_nr == 1 ~ stem_diam_in,
      plotID == "E_SE_O_CV_2" & speciesID == "EN" & plant_nr == 2 ~ stem_diam_in,
      plotID == "E_SE_O_VM_5" & speciesID == "EN" & plant_nr == 3 ~ stem_diam_in,
      plotID == "E_SE_F_EN_2" & speciesID == "VM" & plant_nr == 3 ~ stem_diam_in,
      plotID == "E_SE_F_EN_2" & speciesID == "EN" & plant_nr == 1 ~ stem_diam_in,
      plotID == "E_SE_F_CV_3" & speciesID == "EN" & plant_nr == 2 ~ stem_diam_in,
      plotID == "E_SE_F_VM_2" & speciesID == "VM" & plant_nr == 1 ~ stem_diam_in,
      plotID == "E_KA_O_EN_2" & speciesID == "EN" & plant_nr == 2 ~ stem_diam_in,
      plotID == "E_KA_F_VV_5" & speciesID == "EN" & plant_nr == 2 ~ stem_diam_in,
      plotID == "E_KA_F_VV_5" & speciesID == "EN" & plant_nr == 3 ~ stem_diam_in,
      TRUE ~ full_indiv_stem_diameter
    )
  )


# Do the same for vegetation height. However, for the moment, it is not possible for the North. Indeed, they measured the 
# vegetation height only outside the plot when partially out, and not on the full individual. 
# I am planning on measuring a full individual height based on the biomass weight (to scale the contribution of 
# in/out depending on the biomass in/out)
# In the South, vegetation height when the individual was both in and out was measured in the plot and on the full individual. 

clean_df <- clean_df %>%
  mutate(
    full_indiv_top_height = case_when(
      !is.na(top_height_in_out_all) ~ top_height_in_out_all,# only for South site then, as for North sites it's only out_all measured.
      is.na(top_height_in_out_all) & is.na(top_height_out_all) & !is.na(top_height_in_all) ~ top_height_in_all, # So for North sites, that will only tell when it's fully in. 
      TRUE ~ NA_real_
    )
  )

#7. Correct a few wrong plotID/Bag_code in the fieldsheet that I have now identified in the lab. 
##First, remove the empty KA_O_VM_2 plotID lines from clean_df
clean_df <- clean_df %>%
  filter(!(plotID == "E_KA_O_VM_2")) #Remove the empty KA_O_VM_2 plotID lines from clean_df
##Then change plotIDs
clean_df <- clean_df %>%
  mutate(plotID = case_when(
    plotID == "E_KA_O_" ~ "E_KA_O_VM_2", ##KA_O_ in measurements database is likely KA_O_VM_2 in the lab. There is no VM for this one though
    ## But no VM bags were found for KA_O_VM_2 in the lab too. There is a match in the number of VV
    ## but not in the dates (14/07 on bags and 15/07 in the database) # There is also only 1 EN.
    plotID == "E_KA_O_VM_3" ~ "E_KA_F_VM_3",##KA_O_VM_3 in the measurements database is likely KA_F_VM_3 in the lab (weight). It matches more or less
    ## with the number of individuals ... No EN too. 
    TRUE ~ plotID)
  )
clean_df <- clean_df %>%
  mutate(habitat = case_when(
    plotID == "E_KA_F_VM_3" ~ "Forested", 
    TRUE ~ habitat)
    )

##Update stem length + full_indiv_stem_length from LY_O_VV_2 and LY_O_VV_3 with values from LY_O_VV_2_A and LY_O_VV_2_B 
##because they were measured in the lab on the A and B bags
#Mix in Lygra: two LY_O_VV_2 bags (individual by individual because there is no way to differentiate the set of bags) 
#.. a bit less sure about this one. We have new measurements in the lab for them, but not heights. 
# The problem is only for VV and CV. Because there is no EN in the plots and VM is only in LY_O_VV_2
# in the weight database, there is only VV_2_A and VV_2_B but in the measurements database, there is also VV_2 and VV_3 measurements.
# TO BE CONTINUED FOR CV's
# E_LY_O_VV_2_A_VV_ALL has 12+3 individuals so this may be E_LY_O_VV_3_VV_ALL that has 16 individuals in the fieldsheet
# E_LY_O_VV_2_B_VV_ALL has 28 + 3 individuals so this should be E_LY_O_VV_2_VV_ALL that has 31 individuals in the fieldsheet
#stem length for E_LY_O_VV_2_B_VV_2 is very short; stem length cannot be much bigger than height so this should be E_LY_O_VV_2_VV_2 
#So E_LY_O_VV_2_A_VV_2 should be E_LY_O_VV_3_VV_2
#stem length very short for E_LY_O_VV_2_B_VV_3 so should be VV_2.
#So E_LY_O_VV_2_A_VV_3 should be VV_3
#Bag_code == "E_LY_O_VV_2_A_VV_1" ~ "E_LY_O_VV_3", # Not sure about these two. 
#Bag_code == "E_LY_O_VV_2_B_VV_1" ~ "E_LY_O_VV_2" # Not sure about these two. These ones (VV2_B_VV_1 and VV_2_VV_1) have the same height/stem length. Yet, the individual is a bit
#twisted. Which means that the height should not be much bigger than the stem length, which is the case for VV_2_B_VV_1 versus VV_3_VV_1.

clean_df <- clean_df %>%
  mutate(stem_length_in = case_when(
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 2 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 2],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 2 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 2],
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 3 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 3],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 3 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 3],
    #plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 1 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 1]
    #plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 1 ~ clean_df$stem_length_in[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 1]
    TRUE ~ stem_length_in)
  )

clean_df <- clean_df %>%
  mutate(full_indiv_stem_length = case_when(
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 2 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 2],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 2 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 2],
    plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 3 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 3],
    plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 3 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 3],
    #plotID == "E_LY_O_VV_3" & speciesID == "VV" & plant_nr == 1 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_A" & clean_df$speciesID == "VV" & clean_df$plant_nr == 1]
    #plotID == "E_LY_O_VV_2" & speciesID == "VV" & plant_nr == 1 ~ clean_df$full_indiv_stem_length[clean_df$plotID == "E_LY_O_VV_2_B" & clean_df$speciesID == "VV" & clean_df$plant_nr == 1]
    TRUE ~ full_indiv_stem_length)
  )

## Remove E_LY_O_VV_2_B and _A (full plot to avoid duplicates in measurements 
## even if I didn't figure out which one is which)
clean_df <- clean_df %>%
  filter(!(plotID == "E_LY_O_VV_2_B")) %>%
  filter(!(plotID == "E_LY_O_VV_2_A"))
  
# 7. Reorganize columns of the dataset so that everything comes before the comment/flags columns; 
# and full stem length and stem diameter columns come after stem_length_in_out
final_df <- clean_df %>%
  select(year:stem_length_in_out, full_indiv_top_height, full_indiv_stem_length, full_indiv_stem_diameter, number_harvested_indiv_without_3_rep:number_harvest_indiv, Pic_numbers, comment, recorder_lab, date_lab, comment_lab, flags)

# 8. Export dataset
## Change flags column into something readable outside R.
final_df_export_csv <- final_df
final_df_export_csv$flags <- vapply(
  final_df_export_csv$flags,
  function(x) paste(sort(unique(x)), collapse = "|"),
  character(1)
)

## Export csv file
write_csv(final_df_export_csv, "clean_data/DURIN_WP4_clean_4Corners_field_biomass_structure_cover_dwarf_shrubs_2025.csv")#_OFF_SE_KA_ONLY.csv")

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

#############
#For the coding of north south and inland coastal
clean_df_to_plot <- clean_df %>%
  filter(speciesID == "EN" | speciesID == "CV" | speciesID == "VV" | speciesID == "VM") %>%
  mutate(north_south = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                              labels = c("South", "North", "South", "North"))) %>%
  mutate(coast_inland = factor(siteID, levels = c("LY", "SE", "SO", "KA"),
                               labels = c("Coast", "Coast", "Inland", "Inland"))) %>%
  #remove CV's in KA site
  filter(!(siteID == "KA" & speciesID == "CV"))
  
# Organize levels
clean_df_to_plot$north_south <- factor((clean_df_to_plot$north_south),
                                levels = c("North", "South"))

clean_df_to_plot$coast_inland <- factor((clean_df_to_plot$coast_inland),
                                 levels = c("Coast", "Inland"))
# Define colour palette if using full name versions
Habitat <- c("Forested" = "#083508", "Open" = "#589758")
Species_Fruit <- c("CV" = "#DF697E",
                   "EN" = "#404040",
                   "VM" = "#323284",
                   "VV" = "#D93137")

#Plot full_indiv_stem_diameter for the 4 sites and habitat
ggplot(clean_df_to_plot,
       aes(x = habitat,
           y = full_indiv_stem_diameter,
           color = speciesID,
           fill = speciesID)) +
  
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
  scale_color_manual(values = Species_Fruit) +
  scale_fill_manual(values = Species_Fruit) +
  facet_grid(north_south ~ coast_inland) +
  
  labs(
    x = "Habitat",
    y = "Individual Stem Diameter",
    color = "Species",
    fill = "Species"
  ) +
  
  theme_bw()

