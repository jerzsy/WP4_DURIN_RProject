# Open and clean database for measurements in the lab in the South biomass plots and merge with North plots database (field only)
# --- Vegetation measurements on DWARF-SHRUBS in Extra WP4 biomass removal plots - North & South plots ---- #
# Install and load packages
packages <- c("readxl", "tidyverse", "janitor")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Set wd
wd <- getwd()
setwd(wd)

# File specifications
## File path
file_path_lab<- "raw_data/DURIN_WP4_raw_4Corners_field_biomass_trait_cover_dwarf_shrubs_2025_OFF_LABWORK_MEASURES.xlsx"
file_path_full_db <- "raw_data/DURIN_WP4_raw_4Corners_field_biomass_structure_cover_dwarf_shrubs_2025_OFF.csv"

## Column names for Lab database
col_names_lab_prior <- c('plotID', 'speciesID', 'plant_nr', 'individual',	'bag_code',	'recorder_lab',	'date_lab',	'top_height_in_all', 'top_height_in_without_flowers',	'top_height_in_out_all','top_height_in_out_without_flowers',	'stem_diam_in', 'stem_diam_out', 'stem_length_in', 'stem_length_only_out', 'stem_length_in_out',	'number_harvested_indiv_without_3_rep',	'number_harvest_indiv',	'comment', 'comment_lab')

## Column names for Full database (for now without measurements from the lab)
#col_names_full_db <- c('year','date','site_name','siteID','habitat','plotID','recorder','weather','species','speciesID','plant_nr','plant_cover','top_height_in_all','top_height_in_without_flowers','top_height_out_all','top_height_in_out_all','top_height_in_out_without_flowers','stem_diam_in','stem_diam_out','stem_length_in','stem_length_only_out','stem_length_in_out','number_harvested_indiv_without_3_rep','number_harvest_indiv','Pic_numbers','comment')
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
  comment = col_character()
)

# Import lab EXCEL file without first line and everything as a character
raw_df_lab <- read_excel(file_path_lab, sheet = "Sheet1", skip = 1, col_names = col_names_lab_prior, col_types = "text",na = c("NA",""))

# Import full database
raw_df_full_db <- read_csv(file_path_full_db, col_types = col_spec, na = c("NA",""))#read_csv(file_path_full_db, col_types = cols(.default = col_character()), na = c("NA",""))


#Find "<1" entries in plant cover column and convert to 0.5, then convert column to numeric.
raw_df_full_db <- raw_df_full_db %>%
  mutate(plant_cover = ifelse((plant_cover == "<1" | plant_cover == " <1" | plant_cover == "< 1"), 0.5, plant_cover),
         plant_cover = as.numeric(plant_cover))

# Target "to be measured" and "to be measured by JR" and change to NA
raw_df_lab <- raw_df_lab %>%
  mutate(across(everything(), ~na_if(., "to be measured"))) %>%
  mutate(across(everything(), ~na_if(., "to be measured by JR"))) %>%
  mutate(across(everything(), ~na_if(., "maybe measure?"))) %>%
  mutate(across(everything(), ~na_if(., "Maybe measure?"))) %>%
  mutate(across(everything(), ~na_if(., "IN")))


# Convert every column to the desired format
raw_df_lab <- raw_df_lab %>%
  mutate(plotID = as.character(plotID),
         speciesID = as.character(speciesID),
         plant_nr = as.integer(plant_nr),
         individual = as.character(individual),
         bag_code = as.character(bag_code),
         recorder_lab = as.character(recorder_lab),
         date_lab = as.Date(as.numeric(date_lab), format = "%Y-%m-%d", origin = "1899-12-30"),
         top_height_in_all = as.numeric(top_height_in_all),
         top_height_in_without_flowers = as.numeric(top_height_in_without_flowers),
         top_height_in_out_all = as.numeric(top_height_in_out_all),
         top_height_in_out_without_flowers = as.numeric(top_height_in_out_without_flowers),
         stem_diam_in = as.numeric(stem_diam_in),
         stem_diam_out = as.numeric(stem_diam_out),
         stem_length_in = as.numeric(stem_length_in),
         stem_length_only_out = as.numeric(stem_length_only_out),
         stem_length_in_out = as.numeric(stem_length_in_out),
         number_harvested_indiv_without_3_rep = as.numeric(number_harvested_indiv_without_3_rep),
         number_harvest_indiv = as.numeric(number_harvest_indiv),
         comment = as.character(comment),
         comment_lab = as.character(comment_lab))

# Remove individual and bag_code columns from raw_df_lab database
raw_df_lab <- raw_df_lab %>%
  select(-individual, -bag_code)

# Add missing columns in raw_df_lab from raw_df_full_db and vice versa
col_names_lab <- colnames(raw_df_lab)
col_names_full_db <- colnames(raw_df_full_db)
missing_cols_lab <- setdiff(col_names_full_db, col_names_lab)
missing_cols_full_db <- setdiff(col_names_lab, col_names_full_db)

# Add these missing cols to the raw_df_lab datasheet
for (col in missing_cols_lab) {
  raw_df_lab[[col]] <- raw_df_full_db[[col]][NA_integer_]
}
# Add these missing cols to the raw_df_full_db datasheet
for (col in missing_cols_full_db) {
  raw_df_full_db[[col]] <- raw_df_lab[[col]][NA_integer_]
}

#Reorder columns in raw_df_lab as in raw_df_full_db
raw_df_lab <- raw_df_lab %>%
  select(all_of(colnames(raw_df_full_db)))

# Take care of E_SO_F_EN_1, EN species ID, plant_nr 2: same individual.
## Take the average of both measurements. In the lab, stem diameter in will be remeasured on main stem.
avg_SO_F_EN_1_EN_2 <-
  raw_df_lab %>%
  filter(plotID == "E_SO_F_EN_1",
         speciesID == "EN",
         plant_nr == 2) %>%
  summarise(
    across(
      top_height_in_all:stem_length_in_out,
      ~ mean(.x, na.rm = TRUE)
    )
  )

## Update the values in raw_df_lab and raw_df_full_db for E_SO_F_EN_1, EN species ID, plant_nr 2 with the average values calculated above
## and remove one of the two lines
## Doing it now because it is needed for merging the two databases.
### remove one of the two entries
raw_df_lab <- raw_df_lab %>%
  filter(!(plotID == "E_SO_F_EN_1" & speciesID == "EN" & plant_nr == 2 & top_height_in_all == 21.1))
raw_df_full_db <- raw_df_full_db %>%
  filter(!(plotID == "E_SO_F_EN_1" & speciesID == "EN" & plant_nr == 2 & top_height_in_all == 21.1))
###Replace the remaining entries with the avg_SO_F_EN_1_EN_2 values in columns
raw_df_lab <- raw_df_lab %>%
  mutate(
    across(
      top_height_in_all:stem_length_in_out,
      ~ if_else(
        plotID == "E_SO_F_EN_1" &
          speciesID == "EN" &
          plant_nr == 2,
        avg_SO_F_EN_1_EN_2[[cur_column()]],
        .x
      )
    )
  )

raw_df_full_db <- raw_df_full_db %>%
  mutate(
    across(
      top_height_in_all:stem_length_in_out,
      ~ if_else(
        plotID == "E_SO_F_EN_1" &
          speciesID == "EN" &
          plant_nr == 2,
        avg_SO_F_EN_1_EN_2[[cur_column()]],
        .x
      )
    )
  )

#Filling in missing_cols_lab in raw_df_lab with data from same columns in raw_df_full_db depending on the plotID, speciesID and plant_nr
# Could have been done with a merge function instead, probably. 
for (col in missing_cols_lab){
  for (i in 1:nrow(raw_df_lab)) {
    plotID_i <- raw_df_lab$plotID[i]
    speciesID_i <- raw_df_lab$speciesID[i]
    plant_nr_i <- raw_df_lab$plant_nr[i]

    value_to_fill <- raw_df_full_db[[col]][which(raw_df_full_db$plotID == plotID_i & raw_df_full_db$speciesID == speciesID_i & raw_df_full_db$plant_nr == plant_nr_i)]
    if (length(value_to_fill) > 1){
      warning(paste("Multiple matches found for plotID:", plotID_i, "speciesID:", speciesID_i, "plant_nr:", plant_nr_i, "in column:", col))
      sys.exit()
    }
    if (length(value_to_fill) == 0){
      warning(paste("No match found for plotID:", plotID_i, "speciesID:", speciesID_i, "plant_nr:", plant_nr_i, "in column:", col))
    } else {
      raw_df_lab[[col]][i] <- value_to_fill[1] # Assuming there's only one match
    }
  }
}

## raw_df_lab <- raw_df_lab %>%
##   rows_update(
##     raw_df_full_db %>%
##       select(plotID, speciesID, plant_nr, all_of(missing_cols_lab)),
##     by = c("plotID", "speciesID", "plant_nr")
##   )


# # Change back E_SO_F_EN_1, EN species ID, plant_nr 2 and top_height_in_all == 21.1 to 2 as plant number
# raw_df_lab <- raw_df_lab %>%
#   mutate(plant_nr = ifelse(plotID == "E_SO_F_EN_1" & speciesID == "EN" & plant_nr == 4 & top_height_in_all == 21.1, 2, plant_nr))
# 
# raw_df_full_db <- raw_df_full_db %>%
#   mutate(plant_nr = ifelse(plotID == "E_SO_F_EN_1" & speciesID == "EN" & plant_nr == 4 & top_height_in_all == 21.1, 2, plant_nr))

# Now merge both datasheets - taking Senja and Kautokeino sites only and adding them at the end of the lab database.
## Check for typos in siteIDs
raw_df_full_db <- raw_df_full_db %>%
  mutate(across(where(is.character), ~ trimws(.)))

raw_df_full_db <- raw_df_full_db %>%
  mutate(
    siteID = case_when(
      siteID %in% c("SE","SENJA","Senja","senja","se","Se") ~ "SE",
      siteID %in% c("KA","KAUTOKEINO","Kautokeino","kautokeino","KAUTOKINO","ka","Ka") ~ "KA",
      siteID %in% c("LY","LYGRA","Lygra","lygra","lyrga","Lyrga","ly","Ly") ~ "LY",
      siteID %in% c("SO","SOGNDAL","Sogndal","sogndal","Sogndl","Sogndla","so","So") ~ "SO",
      TRUE ~ siteID
    )
  )

raw_df_full_db_north <- raw_df_full_db[which(raw_df_full_db$siteID %in% c("SE", "KA")),]

raw_df_full_db_south <- raw_df_full_db[which(raw_df_full_db$siteID %in% c("LY", "SO")),]

## Put first raw_df_lab then raw_df_full_db_north
clean_df <- bind_rows(raw_df_lab, raw_df_full_db_north) #equivalent to merge(raw_df_lab,raw_df_full_db_north,by=colnames(raw_df_lab),all=T,sort=F)

# Check the difference between the South database and the lab (online) database
# In case when entering data in the lab, some values were changed (which most of the time should not be the case)
col_to_check <- setdiff(colnames(raw_df_lab),missing_cols_full_db)
raw_df_lab_check <- raw_df_lab[which(raw_df_lab$plotID %in% raw_df_full_db_south$plotID),]  
for (col in col_to_check){
  #compare values in two columns with a function
  a <- raw_df_full_db_south[col]
  b <- raw_df_lab_check[col]
  if (!all(a == b, na.rm = TRUE)) {#remove NA values before comparison
    warning(paste("Values in column", col, "are different between the lab and the South database. Please check the values in this column."))
    #print the values that are different as well as the rows where they are different
    diff_rows <- which(a != b)
    print(paste("Values in column", col, "are different in the following rows: "))
    print(diff_rows)
    print(paste(raw_df_full_db_south$plotID[diff_rows],raw_df_full_db_south$speciesID[diff_rows],raw_df_full_db_south$plant_nr[diff_rows]))
    print(a[diff_rows,])
    print(paste(raw_df_lab_check$plotID[diff_rows],raw_df_lab_check$speciesID[diff_rows],raw_df_lab_check$plant_nr[diff_rows]))
    print(b[diff_rows,])
}
}
  
# Write as CSV
write_csv(clean_df,"raw_data/DURIN_WP4_raw_4Corners_field_biomass_trait_cover_dwarf_shrubs_2025_OFF_ALL.csv")