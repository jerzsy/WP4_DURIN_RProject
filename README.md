# DURIN WP4 README file

This is a R project for cleaning DURIN data collected as part of Work Package 4 (but aimed for other WPs as well). It is using the DURIN project template from the associated GitHub repository. This repo is aimed at data sciences in ecology, but can also be used in other fields.

## R project structure

### Folder structure

The Durin repo has a specific folder structure that we recommend as best practice.
The data are kept in separate folders named *raw\_data* and *clean\_data*, while the code is stored in the folder *code*. In the folder *code*, you will also find a README file detailing the different R scripts and functions. 

- The raw data are the original and untouched files usually .csv or .txt formatted files.
- The clean data is often produced from the raw data using code and stored automatically.

JR also added separate folders for *data_descr* and *data_dic* that are meant for finalized data description tables and data dictionaries for each dataset that are cleaned as part of this R project. See the data documentation section below for more information.

In addition, there is a .gitignore text file, which tells git all the files that should be ignored.
This file can be edited and adapted to the users needs.

We also recommend to use a user licence.


### Data documentation

We strongly recommend to document the data, which is best practice.
The *code* folder contains a subfolder *data\_dic* with files scripts to produce a data dictionary.

The *data\_description.xlsx* file contains basic information about each variable: description, unit and how it was collected.
This file has to be done manually (until we find a solution to automate it).

The R file *make\_data\_dictionary.R* contains a function that makes the data dictionary from the data file and the description file.

The *data\_dic.R* file contains example code to make the data dictionary for each data set and to compile it to one file in the end.
Here each dataset needs to be entered.

The *download\_clean\_data.R* shows and example for how to download the clean data if it is stored externally on a data repository, for example on OSF.

The ***data\_descr\_and\_dic\_making\_JR.R*** file was added by JR. It creates the data description table and data dictionary based on the already existing functions in the data documentation. It is personalized for the datasets in this R project. 


