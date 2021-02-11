
# TITLE: 270-301 SnpshotCompare_Analysis_AllDATA
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 06-Feb-2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#  This script unrolls Clinical Programming Snapshot compare file to allow for 
#     analysis of the changed variables.  This differs from the Snapshot_Compare
#     Analysis file in that it contains EDC data changes (not just Covance)
#
## outputs:


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'readxl')

# loop through required packages & if not already installed, load, then install
for (pkg in lpkgs ) {
  
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS
source('270-301_svCompliance.R')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\'
localpath = 'Studies\\BMRN270\\270-301\\ToplineFreeze\\SnapshotCompareFiles\\'

file1 = 'snapshot_compare_270301_'
filedate1 = '20210201'   ## <------------------enter file date here YYYYMMDD
ext1 = '.xlsx'

snapshot_compare_file = paste0(localroot, localpath, file1, filedate1, ext1)


## READ IN FILES-------------------------------------

## this function will read in entire workbook where into a list of dataframes 
##      where each list will be named by the sheet
##      uses readxl package
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

## call function
mysheets <- read_excel_allsheets(snapshot_compare_file)


##------------------------------------------------------------------------------
## CREATE Global data sets and variables

lockList = c('0003', '1631', '1665', '1667', '1784')
stDat = as.Date('16-11-20', format = '%d-%m-%y')
enDat = as.Date('16-Nov-20', format = '%d-%b-%y')
decDat = as.Date('21-Dec-20', format = '%d-%b-%y')

## Max visit dates for all subjects
svComp_Nov = svCompliance %>%
  dplyr::filter(!is.na(Date_of_Visit))

svCompMax = svComp_Nov %>%
  dplyr::filter(Date_of_Visit <= enDat) %>%
  dplyr::select(Subject, Date_of_Visit) %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(MaxVisitDate = max(Date_of_Visit)) %>%
  dplyr::left_join(svComp_Nov[, c('Subject', 'Date_of_Visit', 'FolderName', 
                                  'Visit_Expected', 'FolderSeq')],
                   by = c('Subject', 'MaxVisitDate' = 'Date_of_Visit'))


##------------------------------------------------------------------------------
## AE DATASET

# rename columns for consistency
aesnap0 = mysheets$AE %>%
  dplyr::mutate(stDD_imp = ifelse(grepl("^UN ", 
                                        toupper(AESTDAT_RAW)), 
                                  1, AESTDAT_DD),
                stMM_imp = ifelse(grepl('UNK', 
                                        toupper(AESTDAT_RAW)), 
                                  1, AESTDAT_MM),
                
                enDD_imp = ifelse(grepl("^UN ", 
                                        toupper(AEENDAT_RAW)), 
                                  16, AEENDAT_DD),
                enMM_imp = ifelse(grepl("UNK", 
                                        toupper(AEENDAT_RAW)), 
                                  11, AEENDAT_MM),
                stDate_imp = as.Date(paste(stDD_imp, '-', 
                                           stMM_imp, '-', 
                                           AESTDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y'),
                enDate_imp = as.Date(paste(enDD_imp, '-', 
                                           enMM_imp, '-', 
                                           AEENDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y')) %>%
  dplyr::filter(AEENRTPT == 0) %>%  ## ongoing
  dplyr::filter(!is.na(enDate_imp)) %>%
  dplyr::filter(enDate_imp <= enDat)
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Adverse Event`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Start Date`,
                `EndDate` = `Stop Date`)

## original values---------------------------
aesnap1 = aesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
aesnap2 = aesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
aesnapjoin = aesnap1 %>%
  dplyr::left_join(aesnap2[, c('Subject', 'EventID', 'Event', 
                               'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'AE') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)

    

##------------------------------------------------------------------------------
## CEB DATASET

## rename columns for consistency
cebsnap0 = mysheets$CEB %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                #`StartDate` = `Start Date`,
                `EndDate` = `Week Ending Date`)

## original values---------------------------
cebsnap1 = cebsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
cebsnap2 = cebsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
cebsnapjoin = cebsnap1 %>%
  dplyr::left_join(cebsnap2[, c('Subject', 'EventID', 'Event', 
                               'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Bleeding Log',
                StartDate = `EndDate`) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## CM DATASET

## rename columns for consistency
cmsnap0 = mysheets$CM %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Medication`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Start Date`,
                `EndDate` = `End Date`)

## original values---------------------------
cmsnap1 = cmsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
cmsnap2 = cmsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
cmsnapjoin = cmsnap1 %>%
  dplyr::left_join(cmsnap2[, c('Subject', 'EventID', 'Event', 
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Con Meds') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## CMF DATASET

## rename columns for consistency
cmfsnap0 = mysheets$CMF %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Medication`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Start Date`,
                `EndDate` = `End Date`)

## original values---------------------------
cmfsnap1 = cmfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
cmfsnap2 = cmfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
cmfsnapjoin = cmfsnap1 %>%
  dplyr::left_join(cmfsnap2[, c('Subject', 'EventID', 'Event', 
                               'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'FVIII Infusion') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LAB DATASET

## rename columns for consistency
labsnap0 = mysheets$LAB %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `AnalyteName`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Clinical date of record (ex: visit date)`) %>%
                #`EndDate` = `End Date`)
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
labsnap1 = labsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
labsnap2 = labsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
labsnapjoin = labsnap1 %>%
  dplyr::left_join(labsnap2[, c('Subject', 'EventID', 'FieldId', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate', 'FieldId',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab',
                EndDate = `StartDate`) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE) %>%
  unique()

##------------------------------------------------------------------------------
## LBC DATASET

## rename columns for consistency
lbcsnap0 = mysheets$LBC %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) %>%
  #`EndDate` = `End Date`)
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
lbcsnap1 = lbcsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbcsnap2 = lbcsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbcsnapjoin = lbcsnap1 %>%
  dplyr::left_join(lbcsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab Chemistry',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBCG DATASET

## rename columns for consistency
lbcgsnap0 = mysheets$LBCG %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) %>%
  #`EndDate` = `End Date`)
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))


## original values---------------------------
lbcgsnap1 = lbcgsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbcgsnap2 = lbcgsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbcgsnapjoin = lbcgsnap1 %>%
  dplyr::left_join(lbcgsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab Chemistry',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBCOVANCE DATASET

## rename columns for consistency
lbcovancesnap0 = mysheets$LBCOVANCE %>%
  dplyr::rename(`Subject` = `Subject ID`,
                `Event` = `Test Name`,
                `EventID` = `Specimen identifier`,
                `StartDate` = `Sample Collection Date`)
  #`EndDate` = `End Date`)

## original values---------------------------
lbcovancesnap1 = lbcovancesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE)) %>% unique()

## new values---------------------------
lbcovancesnap2 = lbcovancesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

lbcovancesnapjoin = lbcovancesnap1 %>%
  dplyr::left_join(lbcovancesnap2[, c('Subject', 'Visit Name', 'EventID', 'Event', 
                                      'StartDate', 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'Visit Name',  'EventID', 'Event',
                          'StartDate', 'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Covance Labs',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE) %>%
  unique()


##------------------------------------------------------------------------------
## LBF DATASET

## rename columns for consistency
lbfsnap0 = mysheets$LBF %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) %>%
  #`EndDate` = `End Date`)
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))


## original values---------------------------
lbfsnap1 = lbfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbfsnap2 = lbfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbfsnapjoin = lbfsnap1 %>%
  dplyr::left_join(lbfsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local FVIII La',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBH DATASET

## rename columns for consistency
lbhsnap0 = mysheets$LBH %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) %>%
  #`EndDate` = `End Date`)
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
lbhsnap1 = lbhsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbhsnap2 = lbhsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbhsnapjoin = lbhsnap1 %>%
  dplyr::left_join(lbhsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab Hematology',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBSAMP_MAPPING DATASET

## rename columns for consistency
lbsamp_mappingsnap0 = mysheets$LBSAMP_MAPPING %>%
  dplyr::rename(`Subject` = `Subject ID`,
                `Event` = `Sample Description`,
                `EventID` = `Specimen identifier`,
                `StartDate` = `Sample Collection Date`)

## original values---------------------------
lbsamp_mappingsnap1 = lbsamp_mappingsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbsamp_mappingsnap2 = lbsamp_mappingsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbsamp_mappingsnapjoin = lbsamp_mappingsnap1 %>%
  dplyr::left_join(lbsamp_mappingsnap2[, c('Subject', 'EventID', 'Event', 
                                           'StartDate', 'Visit Name',
                                           'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate', 'Visit Name',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab SAMP',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBU DATASET

## rename columns for consistency
lbusnap0 = mysheets$LBU %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) 

## original values---------------------------
lbusnap1 = lbusnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
lbusnap2 = lbusnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbusnapjoin = lbusnap1 %>%
  dplyr::left_join(lbusnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab Urine',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## LBV DATASET

## rename columns for consistency
lbvsnap0 = mysheets$LBV %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Draw/Collection Date`) 

## original values---------------------------
lbvsnap1 = lbvsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))


## new values---------------------------
lbvsnap2 = lbvsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
lbvsnapjoin = lbvsnap1 %>%
  dplyr::left_join(lbvsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Local Lab Viral_AI Hepatitis',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## MHT DATASET

## rename columns for consistency
mhtsnap0 = mysheets$MHT %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Clinical date of record (ex: visit date)`) 

## original values---------------------------
mhtsnap1 = mhtsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
mhtsnap2 = mhtsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
mhtsnapjoin = mhtsnap1 %>%
  dplyr::left_join(mhtsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Targeted Medical Hx',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## MOU DATASET  (Liver ultrasound)

## rename columns for consistency
mousnap0 = mysheets$MOU %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
mousnap1 = mousnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))


## new values---------------------------
mousnap2 = mousnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
mousnapjoin = mousnap1 %>%
  dplyr::left_join(mousnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Liver Ultrasound',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## PE DATASET 

## rename columns for consistency
pesnap0 = mysheets$PE %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
pesnap1 = pesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
pesnap2 = pesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
pesnapjoin = pesnap1 %>%
  dplyr::left_join(pesnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Physical Exam',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## PR DATASET (concom procedures)

## rename columns for consistency
prsnap0 = mysheets$PR %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Type of Procedure/Surgery`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Start Date of Procedure /surgery:`,
                `EndDate` = `Stop Date of Procedure /surgery:`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
prsnap1 = prsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
prsnap2 = prsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
prsnapjoin = prsnap1 %>%
  dplyr::left_join(prsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                               'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Concomitant Procedure') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)

##------------------------------------------------------------------------------
## QSEM DATASET (work/school)

## rename columns for consistency
qsemsnap0 = mysheets$QSEM %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date subject completed the Diary`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
qsemsnap1 = qsemsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qsemsnap2 = qsemsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qsemsnapjoin = qsemsnap1 %>%
  dplyr::left_join(qsemsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                               'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Work/School Quest') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSEQ DATASET (EQ-5D-5L)

## rename columns for consistency
qseqsnap0 = mysheets$QSEQ %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))


## original values---------------------------
qseqsnap1 = qseqsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qseqsnap2 = qseqsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qseqsnapjoin = qseqsnap1 %>%
  dplyr::left_join(qseqsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'EQ-5D-5L Quest') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)



##------------------------------------------------------------------------------
## QSHAL DATASET (Hemophilia Activities List)

## rename columns for consistency
qshalsnap0 = mysheets$QSHAL %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
qshalsnap1 = qshalsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qshalsnap2 = qshalsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qshalsnapjoin = qshalsnap1 %>%
  dplyr::left_join(qshalsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Hemophilia Activities List') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSHAL DATASET (Hemophilia Activities List)

## rename columns for consistency
qshal2snap0 = mysheets$QSHAL2 %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Clinical date of record (ex: visit date)`) %>%
                #`EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))
                #EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
qshal2snap1 = qshal2snap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qshal2snap2 = qshal2snap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qshal2snapjoin = qshal2snap1 %>%
  dplyr::left_join(qshal2snap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Hemophilia Activities List',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSPROBE DATASET (Patient Reported Outcomes Burdens and Experiences (PROBE))

## rename columns for consistency
qsprobesnap0 = mysheets$QSPROBE %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
qsprobesnap1 = qsprobesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qsprobesnap2 = qsprobesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qsprobesnapjoin = qsprobesnap1 %>%
  dplyr::left_join(qsprobesnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                   'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'PROBE Quest') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSPROBE2 DATASET (Patient Reported Outcomes Burdens and Experiences (PROBE))

## rename columns for consistency
qsprobe2snap0 = mysheets$QSPROBE2 %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Clinical date of record (ex: visit date)`) %>%
                #`EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))
                #EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
qsprobe2snap1 = qsprobe2snap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qsprobe2snap2 = qsprobe2snap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qsprobe2snapjoin = qsprobe2snap1 %>%
  dplyr::left_join(qsprobe2snap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                    'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'PROBE Quest',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSQOL DATASET (Haemo-QoL-A Quality of Life)

## rename columns for consistency
qsqolsnap0 = mysheets$QSQOL %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))


## original values---------------------------
qsqolsnap1 = qsqolsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qsqolsnap2 = qsqolsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qsqolsnapjoin = qsqolsnap1 %>%
  dplyr::left_join(qsqolsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                     'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Haemo QOL Quest') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## QSWPAI DATASET (Work Productivity and Activity Impairment (WPAI+CIQ:HS))

## rename columns for consistency
qswpaisnap0 = mysheets$QSWPAI %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `eCRF Form Name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`,
                `EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))


## original values---------------------------
qswpaisnap1 = qswpaisnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
qswpaisnap2 = qswpaisnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
qswpaisnapjoin = qswpaisnap1 %>%
  dplyr::left_join(qswpaisnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                  'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Work Prod Quest') %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## SV DATASET (Study Visits))

## rename columns for consistency
svsnap0 = mysheets$SV %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of visit`) %>%
                #`EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))
                #EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
svsnap1 = svsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
svsnap2 = svsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
svsnapjoin = svsnap1 %>%
  dplyr::left_join(svsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                   'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Study Visits',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## UCRF DATASET (Unscheduled Study Visits))

## rename columns for consistency
ucrfsnap0 = mysheets$UCRF %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Clinical date of record (ex: visit date)`) %>%
  #`EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))
#EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
ucrfsnap1 = ucrfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
ucrfsnap2 = ucrfsnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
ucrfsnapjoin = ucrfsnap1 %>%
  dplyr::left_join(ucrfsnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                   'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Unscheduled Study Visits',
                EndDate = as.Date(StartDate, format = '%d%b%Y')) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)


##------------------------------------------------------------------------------
## VS DATASET (Vital Signs))

## rename columns for consistency
vssnap0 = mysheets$VS %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Folder instance name`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Date of assessment`) %>%
  #`EndDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))
#EndDate = as.Date(EndDate, format = '%d%b%Y'))

## original values---------------------------
vssnap1 = vssnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value changes`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit), 
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('VARIABLE', 'VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(VARIABLE = trimws(VARIABLE), 
                VALUE = trimws(VALUE))

## new values---------------------------
vssnap2 = vssnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`newvalues`), ',')) %>%
  tidyr::unnest(tempsplit) %>%
  
  ## 3 lines to deal with free text terms that contain commas
  dplyr::mutate(remove = ifelse(status == 'Updated' & !grepl('=', tempsplit),
                                1, 0)) %>%
  dplyr::filter(remove != 1) %>% dplyr::select(-remove) %>%
  
  ## split columns by =
  tidyr::separate(tempsplit, c('NEW_VARIABLE', 'NEW_VALUE'), sep = '=') %>%
  ## trim any whitespace
  dplyr::mutate(NEW_VARIABLE = trimws(NEW_VARIABLE), 
                NEW_VALUE = trimws(NEW_VALUE))

## join two data sets to re-align old and new values
vssnapjoin = vssnap1 %>%
  dplyr::left_join(vssnap2[, c('Subject', 'EventID', 'Event', 'StartDate',
                                 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'EventID', 'Event', 'StartDate',
                          'VARIABLE' = 'NEW_VARIABLE')) %>%
  dplyr::mutate(Description = 'Vital Signs',
                EndDate = `StartDate`) %>%
  dplyr::select(dataset, Description, Subject, EventID, Event, 
                StartDate, EndDate,
                status, VARIABLE, VALUE, NEW_VALUE)

## Bind all tables together-------------

snapBindAllChanges = bind_rows(aesnapjoin, cebsnapjoin, cmsnapjoin, cmfsnapjoin, 
                               labsnapjoin, lbcsnapjoin, lbcgsnapjoin, 
                               lbcovancesnapjoin, lbfsnapjoin, lbhsnapjoin, 
                               lbsamp_mappingsnapjoin, lbusnapjoin, lbusnapjoin,  
                               mhtsnapjoin, mousnapjoin, pesnapjoin, prsnapjoin, 
                               qsemsnapjoin, qseqsnapjoin, qshalsnapjoin, 
                               qshal2snapjoin, qsprobesnapjoin, qsprobe2snapjoin, 
                               qsqolsnapjoin, qswpaisnapjoin, svsnapjoin, 
                               ucrfsnapjoin, vssnapjoin) 
