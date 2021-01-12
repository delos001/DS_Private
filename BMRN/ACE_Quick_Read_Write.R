
# TITLE: 270-301 Read Write Script
# STUDY: NA
# AUTHOR: Jason Delosh
# DATE: Jan2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#    
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
ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'

fileName = 'lbcovance.sas7bdat'

file_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     ACECovance1, sep = ""),
                   .name_repair = 'check_unique')

##------------------------------------------------------------------------------
## FILTERING/WRANGLING IF NEEDED

myfile = file_raw

##  If any wrangling is needed, do it here:
# myfile = myfile %>%
#   dplyr::select('A', 'B', 'C') %>%
#   dplyr::filter(Site = 'n',
#                 Subject == 'n',
#                 Other == 'n'
#                 )


##------------------------------------------------------------------------------
##  WRITE OBJECT TO LOCAL LOCATION
localPath = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\'
writeName = myfile
fileName = 'BMRN270301_FileName'
fileExt = '.csv'

write.csv(writeName,
          file.path(localPath, paste0(fileName, fileExt)),
          row.names = FALSE)