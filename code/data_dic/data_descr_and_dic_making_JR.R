# Script to assist with data description and data dictionary (JR)
# Install and load packages
packages <- c("tidyverse","dataDocumentation")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
# Load all packages in the vector
lapply(packages, library, character.only = TRUE)

# Input - dataset name 
data_name <- 'DURIN_WP4_clean_4Corners_field_traits_dwarf_shrubs_forbs_gram_summer_2025.csv'
  #'DURIN_WP4_clean_4Corners_field_traits_trees_2025.csv'
  #'DURIN_WP4_clean_4Corners_field_traits_shrubs_2025.csv'
  #'DURIN_WP4_clean_4Corners_field_traits_dwarf_shrubs_forbs_gram_summer_2025.csv'
table_ID <- 'traits_DURIN'
  #'trees' 
  #'shrubs'
  #'traits_DURIN'

#Download clean data from clean_data folder 
data_to_process <- read_csv("clean_data/" %>% paste0(data_name)) #or directly from OSF

# Make data description table - to run twice if description table needs to be updated with information
if(!file.exists("data_descr/description_table_" %>% paste0(table_ID,".csv"))){
  data_description <- make_description_table(data=data_to_process,table_ID=table_ID,path= "data_descr/")
  # Save data description table with new name (_table_ID)
  write_csv(data_description, "data_descr/description_table_" %>% paste0(table_ID,".csv"))
  #Then go update the data description table as it should be.
} else { # data description table already exists and has been manually modified.
  print("Data description table already exists, downloading it instead of making a new one")
  # Download data description table with new name (_table_ID)
  data_description <- read_csv("data_descr/description_table_" %>% paste0(table_ID,".csv"))%>%
    mutate(TableID = as.character(TableID))
}

#----------------------------------------------#
# Make data dictionary
data_dic <- make_data_dictionary(data=data_to_process,
                                 description_table=data_description,
                                 table_ID=table_ID,
                                 keep_table_ID = FALSE)
# Create a data_dictionary folder
if(!dir.exists("data_dic")){
  dir.create("data_dic")
}
# Write data dic
write_csv(data_dic, "data_dic/data_dictionary_" %>% paste0(table_ID,".csv"))

