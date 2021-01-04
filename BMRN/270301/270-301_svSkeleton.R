
# TITLE: 270-301_svSkeleton.R
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 13Oct2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
##  to get file1: Rave/Reporter/Study Configuration
##       chose study and hit Submit Report
##       chose most recent PROD CRF version
##       chose 'Folders'
##       select all rows and paste into MS excel
##       save as .csv  (udpate name of file 1 if you change name)
#
## outputs: subjVisits


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

source('270-301_svAnchorData.R')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA
localwd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'

path270301 = 'BMRN270\\270-301\\'

RRSCF = 'RAVE_Reporter_StudyConfiguration_Folders.csv'  ## see instructions

RRSCFRaw = read.csv(file.path(paste(localwd, path270301, sep = ''), 
                                     RRSCF), 
                            sep = ",", 
                            na.strings = c("", " ", "NA"))


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING
##  none

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE INTERMEDIATE TABLES--------------------------------------------


## create subset of forms: only required study visit forms
rqdForms = RRSCFRaw %>%
  ## SAS to csv output has hidden char ".' at end of visit names in Name col
  dplyr::mutate(Name = gsub("[^\x20-\x7E]", "", Name),
                id = 1) %>%  ## id used to join later
  dplyr::rename(FOLDERNAME = Name)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES--------------------------------------------

## subject-visit skeleton table: joined on id for each subject for each visit
svSkeleton = svRaw %>%
  select(SUBJECT) %>%
  unique() %>%  ## get unique subject list
  dplyr::mutate(id = 1) %>% ## id used to join later
  
  ## join required forms: yields a row for each visit for each subject
  dplyr::full_join(rqdForms, by = 'id') %>%
  dplyr::arrange(SUBJECT) %>%

  ## create lower and upper windows based on target
  dplyr::mutate(LWindow = ifelse(is.na(Target), NA,
                            ifelse(OID == "SCR" | OID == "SCRS", -42,
                              ifelse(OID == "BASE", -7, 
                                     End * -1)
                              )
                            ),
                UWindow = ifelse(is.na(Target), NA,
                            ifelse(OID == "SCR" | OID == "SCRS" | OID == "BASE",
                                   -1,
                                   End)
                            )
                ) %>%
  ## remove non-data entry folders from df
  dplyr::filter(!grepl("Year", FOLDERNAME)) %>%
  dplyr::filter(!grepl('W1P', OID)) %>%
  dplyr::select(SUBJECT, FOLDERNAME, OID, Parent.Folder, 
                End, Over, 
                LWindow, Target, UWindow)
