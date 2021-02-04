# TITLE: 270-301 Covance_ACEDataoper
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

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr')

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

## Ace files------
ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'

ACECovance1 = 'lbcovance.sas7bdat'
ACECovance2 = 'lbcovance_blinded.sas7bdat'

## read in files
lbcovance_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                           ACECovance1, sep = ""),
                         .name_repair = 'check_unique')

lbcovance_blinded_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                                   ACECovance2, sep = ""),
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
lbcovance = lbcovance_raw %>%
  dplyr::mutate(FileSource = 'lbcovance')

lbcovance_blinded = lbcovance_blinded_raw %>%
  dplyr::mutate(FileSource = 'lbcovance_blinded')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

## covLab and blnd_fviii are standard format so they are binded here
## bind dataframes together
Covance_ACE_final = lbcovance %>%
  dplyr::bind_rows(lbcovance_blinded) %>%
  dplyr::mutate('Results_Present' = ifelse(LBORRES == "" | LBSTAT == "NOT DONE",
                                           'No', 'Yes'))

