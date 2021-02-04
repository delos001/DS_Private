
# TITLE: 
# STUDY: 
# AUTHOR: 
# DATE: 

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
# 
#
## outputs:  This script uses the list of tests present in the DTS and identifies
##              if any tests are not present in the data transfer


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

source('./ExternalLaboratory/270-301_ExternalLab_Bind_All_Data.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA


## read in files


## Save path
outloc_bindall = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\Covance_Raw\\'


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE


## ADHOC ITEM: get unique list of tests performed

uniqueLabsPresent = LabBindAll_NoResults %>%
  dplyr::select(LBSPID, LBTESTCD, LBTEST) %>%
  unique()


write.csv(uniqueLabsPresent, file.path(outloc_bindall, 'uniqueLabsPresent.csv'),
                                      row.names = FALSE)


