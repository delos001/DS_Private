
# TITLE: 270-301 Covance SAS datasets
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 12Nov2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#    since the SAS files from ACE environment are the same format, all can be
#       manipulated here in one file
#
## outputs:


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr')

# loop through required packages & if not already installed, load, then install
for(pkg in lpkgs) {
  
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

mywd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'

path1 = 'BMRN270\\270-301\\Covance_Raw\\'

file1 = 'bmn_270301_covance_lab_12oct20.sas7bdat'
file2 = 'bmn_270301_lb_blnd_fviii_12oct20.sas7bdat'

## read in files
covLab_raw = read_sas(data_file = paste(mywd, path1, file1, sep = ""),
                      .name_repair = 'check_unique')



blnd_fviii_raw = read_sas(data_file = paste(mywd, path1, file2, sep = ""),
                          .name_repair = 'check_unique')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## tag each df with its file source
covLab = covLab_raw %>%
  dplyr::mutate(FileSource = 'Covance_Lab_ACE')

blnd_fviii = blnd_fviii_raw %>%
  dplyr::mutate(FileSource = 'Blind_FVIII_ACE')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

## covLab and blnd_fviii are standard format so they are binded here
## bind dataframes together
Covance_ACE_final = covLab %>%
  dplyr::bind_rows(blnd_fviii) %>%
  dplyr::mutate('Results_Present' = ifelse(LBORRES == "" | LBSTAT == "NOT DONE",
                                           'No', 'Yes'))




