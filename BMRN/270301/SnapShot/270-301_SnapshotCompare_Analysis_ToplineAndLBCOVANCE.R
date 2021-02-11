# TITLE: 270-301 SnpshotCompare_Analysis_ToplineANDLBCovance
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


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE PATHS AND FILE NAMES


localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\'
localpath = 'Studies\\BMRN270\\270-301\\ToplineFreeze\\SnapshotCompareFiles\\'


## define file variables for snaphsot_compare_topline file
sctl_file1 = 'snapshot_compare_topline_270301_'
sctl_filedate1 = '20210205'   ## <------------------enter file date here YYYYMMDD
sctl_ext1 = '.xlsx'

snapshot_compare_topline_file = paste0(localroot, localpath, 
                                       sctl_file1, sctl_filedate1, sctl_ext1)


## define file variables for snaphsot_compare file
scfile1 = 'snapshot_compare_270301_'
scfiledate1 = '20210205'   ## <------------------enter file date here YYYYMMDD
scext1 = '.xlsx'

snapshot_compare_file = paste0(localroot, localpath, 
                               scfile1, scfiledate1, scext1)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## READ IN FILES

##----------------------------------------------
## Read LBCOVANCE from the snapshot_compare_topline file
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
mysheets <- read_excel_allsheets(snapshot_compare_topline_file)

##----------------------------------------------
## Read LBCOVANCE from the snapshot_compare file
LBCOVANCE_snap_raw = read_excel(snapshot_compare_file, 
                                sheet = 'LBCOVANCE') %>%
  data.frame()



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES
stDat = as.Date('16-11-20', format = '%d-%m-%y')


summarySheet = mysheets$Summary[,2:ncol(mysheets$Summary)] %>% 
  dplyr::filter(!is.na(Form)) %>% 
  dplyr::mutate_all(~replace(., is.na(.), 0)) %>%
  dplyr::rename('21Dec20 Snapshot Topline' = '21Dec Snapshot Topline (20 datasets)',
                'CSR Topline' = 'CSR Topline (20 datasets)')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE INPUT TABLES

##------------------------------------------------------------------------------
## AE DATASET

# rename columns for consistency
aesnap0 = mysheets$AE %>%
dplyr::rename(`Subject` = `Subject name or identifier`,
              `Event` = `Adverse Event`,
              `EventID` = `Internal id for the record`,
              `StartDate` = `Start Date`,
              `EndDate` = `Stop Date`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

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
## CM DATASET

## rename columns for consistency
cmsnap0 = mysheets$CM %>%
  dplyr::rename(`Subject` = `Subject name or identifier`,
                `Event` = `Medication`,
                `EventID` = `Internal id for the record`,
                `StartDate` = `Start Date`,
                `EndDate` = `End Date`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'),
                EndDate = as.Date(EndDate, format = '%d%b%Y'))

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
## LBCOVANCE DATASET

## rename columns for consistency
lbcovancesnap0 = LBCOVANCE_snap_raw %>%
  dplyr::filter(Sample.Collection.Date <= stDat) %>%
  dplyr::rename(`Subject` = `Subject.ID`,
                `Event` = `Test.Name`,
                `EventID` = `Specimen.identifier`,
                `StartDate` = `Sample.Collection.Date`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

## original values---------------------------
lbcovancesnap1 = lbcovancesnap0 %>%
  ## unpack single cell contents into new rows for each varible, 
  dplyr::mutate(tempsplit = strsplit(as.character(`Value.changes`), ',')) %>%
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
  dplyr::left_join(lbcovancesnap2[, c('Subject', 'Visit.Name', 'EventID', 'Event', 
                                      'StartDate', 'NEW_VARIABLE', 'NEW_VALUE')],
                   by = c('Subject', 'Visit.Name',  'EventID', 'Event',
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
  dplyr::mutate(Description = 'Local FVIII Lab',
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
                `StartDate` = `Clinical date of record (ex: visit date)`) %>%
  dplyr::mutate(StartDate = as.Date(StartDate, format = '%d%b%Y'))

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
##------------------------------------------------------------------------------
## Bind all tables together-------------
tlAndLBCOVANCE_bind = dplyr::bind_rows(aesnapjoin, cmsnapjoin, lbcovancesnapjoin, 
                                lbfsnapjoin, mhtsnapjoin) 

tlAndLBCOVANCE_bind_nores = tlAndLBCOVANCE_bind %>%
  dplyr::select(-VALUE, -NEW_VALUE)
## write output file

write.csv(tlAndLBCOVANCE_bind_nores,
          file.path(paste0(localroot, localpath),
                    paste0('270301_SnapShot_Compare_Topline_unroll', 
                           sctl_filedate1, '.csv')),
          row.names = FALSE)




