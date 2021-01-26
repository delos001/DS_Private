
# TITLE: 
# STUDY: 
# AUTHOR: 
# DATE: 

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
# 
#
## outputs:


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'readxl')

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

localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\'
localpath = 'Studies\\BMRN270\\270-301\\ToplineFreeze\\SnapshotCompareFiles\\'

file1 = 'snapshot_compare_270301_20210118.xlsx'
file2 = 'filename.ext'
file3 = 'filename.ext'


## read in files

LBCOVANCE_snap_raw = read_excel(paste0(localroot, localpath,
                                  file1), 
                            sheet = 'LBCOVANCE') %>%
  data.frame()



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

##  Create two tables here that are joined later
##  First table unests the baseline data set values (value.changes)
LBCOVANCEBase = LBCOVANCE_snap_raw %>%
  dplyr::select(Subject.ID, Visit.Name, Sample.Collection.Date,
                Specimen.identifier, Category.for.Lab.Test,
                Test.Short.Name, Lab.Test.Identifier,
                Test.Name, status,
                Value.changes) %>%
  ## first: turn string into a list where each element is split by the comma
  dplyr::mutate(tempsplit = strsplit(as.character(Value.changes), ',')) %>%
  tidyr::unnest(tempsplit) %>% ## new row per list item (opposite of collapse)
  ## split the tempsplit value into 2 cols: A and B
  tidyr::separate(tempsplit, c('A', 'B'), sep = '=') %>%
  dplyr::mutate(VARIABLE = trimws(A),
                VALUE = trimws(B)) %>%
  dplyr::select(-A, -B)

##  Second table unests the new data set values (new values)
LBCOVANCENew = LBCOVANCE_snap_raw %>%
  dplyr::select(Subject.ID, Visit.Name, Sample.Collection.Date,
                Specimen.identifier, Category.for.Lab.Test,
                Test.Short.Name, Lab.Test.Identifier,
                Test.Name, status,
                newvalues) %>%
  ## first: turn string into a list where each element is split by the comma
  dplyr::mutate(tempsplit = strsplit(as.character(newvalues), ',')) %>%
  tidyr::unnest(tempsplit) %>% ## new row per list item (opposite of collapse)
  ## split the tempsplit value into 2 cols: A and B
  tidyr::separate(tempsplit, c('A', 'B'), sep = '=') %>%
  dplyr::mutate(VARIABLE_new = trimws(A),
                VALUE_new = trimws(B)) %>%
  dplyr::select(-A, -B)

## join the columns adding new values for comparison with base values
LBCOVANCE_join = LBCOVANCEBase %>%
  dplyr::left_join(LBCOVANCENew[, c('Subject.ID', 'Visit.Name', 
                                    'Sample.Collection.Date', 
                                    'Specimen.identifier', 'Lab.Test.Identifier',
                                    'VARIABLE_new', 'VALUE_new')],
                   by = c('Subject.ID', 'Visit.Name', 
                          'Sample.Collection.Date', 
                          'Specimen.identifier', 'Lab.Test.Identifier',
                          'VARIABLE' = 'VARIABLE_new'))



## write output file

write.csv(LBCOVANCE_join,
          file.path(paste0(localroot, localpath), 
                    '270301_SnapShot_Compare_unroll.csv'), 
          row.names = FALSE)



