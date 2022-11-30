
# TITLE: 270-301 Covance SOW Analytes
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 04Mar2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
##   This script will produce an table of all Groupss and analytes in the 
##     current SOW
##   The input file is an aggregated file sent by covance which includes
##     some elements of the workbook form of the SOW and the test code list 
##     export
##
##
## *******IMPORTANT: -----------------------------------------------------------
##        essential to check/update global variables section of the 
##        source file to ensure specs are correct for variables and/or rows
##
## OUTPUTS: -------------------------------------------------------------------- 
##         SOWAnalytes


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

# specified in source file (see below)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS

source('./ExternalLaboratory/270-301_Covance_SOW_GlobalParams.R')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

# path and file name specified in source file (above)

## READ FILES
## files are read in via source script

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

# variables defined in source file (above)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING

# none

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

## Create df containing visit name from row1 as header 
VTSschedule = VTSTests %>%
  tidyr::pivot_longer(-c(GroupProjects, ProjectCode, GroupS), 
                      names_to = "VisitName", 
                      values_to = "Annotation",
                      values_drop_na = FALSE) %>%
  
  ## in SOW VTS xls, some labs have NA instead of -.  Change for consistency
  mutate_at('Annotation', ~replace(., is.na(.), '-')) %>%
  
  ## account for special chars (need to add standardize)
  dplyr::mutate(VisitName = gsub("[\r\n]", '', VisitName)) %>%
  
  ## join region codes
  dplyr::left_join(VTSLegendProjects, by = 'ProjectCode') %>%
  dplyr::mutate(ProjectNumber = ifelse(is.na(ProjectNumber), 
                                       "ALL", 
                                       ProjectNumber)) %>%
  
  ## split annotation column to extract only Code (used for join later)
  dplyr::mutate(CollectionCode = gsub('\\(.*%\\)', '', Annotation)) %>%
  
  ## join legend description
  dplyr::left_join(VTSLegend, by = 'CollectionCode') %>%
  
  ## create column that tells whether a test is required or not
  dplyr::mutate(PerformTest = 
                  ifelse(is.na(CollectionCode) | CollectionCode == '-', 'No',
                         gsub(' \\(.*\\)', 
                              '', 
                              Description))) %>%
  
  ## join analytes based on test group
  dplyr::left_join(SOWAnalytes, by = 'GroupS') %>%
  
  ## start here----------------
  ## join Covance Codes
  dplyr::left_join(TestsCodes_All, by = c('TestAnalytes' = 'TestDescription')) %>%
  

  ## join Covance visit name mapping
  dplyr::left_join(DTSVisitMapping, 
                   by = c('VisitName' = 'CovanceVisitName')) %>%
  
  dplyr::select(GroupS, ProjectNumber, VisitName, VisitDescriptionFoldername,
                Annotation, PerformTest, TestAnalytes, TestName, 
                TestCodes_Collapse)
  


## Extra Code used for qc
# dplyr::filter(is.na(TestName)) %>%
#   dplyr::select(TestAnalytes) %>%
#   unique()
 
  # runDate = as.character(Sys.Date())
  # outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\'
  # write.csv(VTSschedule, 
  #           file.path(outloc, 
  #                     paste0('270-301_VTSschedule_Mismatch_Analytes_', 
  #                            runDate,
  #                            '.csv')), row.names = FALSE) 