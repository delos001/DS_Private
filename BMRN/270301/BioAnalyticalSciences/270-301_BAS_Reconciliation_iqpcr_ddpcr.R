
# TITLE: 270-301 BAS QC Check

# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 22Dec2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#  This script will perform header recon on data tested by BioAnalytical
#      Sciences and transferred to Clinical Data Management.  These include
#      the following:
#            ddPCR: matrics PBMC and whole blood
#            iqPCR: matrices semen and plasma
#  A subset of the study population was tested for the above assays as defined
#      in the 270-301 protocol
#
# 
## outputs:


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

## standard packages loaded with parent script 
## c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr')

library(data.table)
#library(datapasta)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS

source('270-301_svCompliance.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## Define paths--------------------------------------
ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301csrub = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'


## Define Files--------------------------------------

## CDM Dataset = LBDDPCR; Biological Matrices = PBMC and Whole Blood
lbiqpcr_file = 'lbiqpcr.sas7bdat'
lbddpcr_file = 'lbddpcr.sas7bdat'


## Read files----------------------------------------

lbiqpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub, 
                                         lbiqpcr_file, sep = ""),
                              .name_repair = 'check_unique') %>%
  dplyr::mutate(File_Name = 'lbiqpcr')

lbddpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub,
                                         lbddpcr_file, sep = ""),
                              .name_repair = 'check_unique') %>%
  dplyr::mutate(File_Name = 'lbddpcr')

lbBAS_bind = dplyr::bind_rows(lbiqpcr_raw, lbddpcr_raw)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
##  CREATE CHECKS
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------


##------------------------
##------------------------
## HEADER CHECK

## function to get column names
cf_col_num = function(t) {
  out = data.frame(
    ## get column names from the file
    'colName' = colnames(t %>% dplyr::select(-File_Name)),
    ## get the SAS labels from the file
    'colLabel' = map_chr(t %>% dplyr::select(-File_Name), attr_getter("label")),
    row.names = NULL)
  ## rename the columns based on the file name
  colnames(out) = c(paste0(gsub('\\_.*', '', ## get text before the _
                                deparse(substitute(t))), ## turn tbl into text
                           '_Col_Names'),
                    paste0(gsub('\\_.*', '', ## get text before the _
                                deparse(substitute(t))), ## turn tbl into text
                           '_Col_Label'))
  return(out)
}

## Apply function to both data sets, drop derived column for comparison purposes
lbiqpcr_cols = cf_col_num(lbiqpcr_raw)
lbddpcr_cols = cf_col_num(lbddpcr_raw)


## Show labels from DTS
##    Uses library(datapasta): datapast::dt_paste()  
##        (need to copy excel to clipboard first)
DTStbl = data.table::data.table(
  Index = c(1L,2L,3L,4L,5L,6L,7L,
            8L,9L,10L,11L,12L),
  DTSlabels = c("Study ID","Subject",
                "Visit","Actual Sampling Date",
                "Actual Sampling Time","Custom ID","Analyte",
                "Result","Concentration Units",
                "Biological Matrix","Result Comment",
                "Run ID"),
  Type = c("Any","Any","Any","Any",
           "Num","Num","Char","Char","Char",
           "Char","Char","Any"),
  Length = c(7L,9L,40L,9L,8L,17L,
             60L,60L,50L,20L,160L,12L),
  DTS_Col_Names = c("PROJECT","SUBJECT",
                    "FOLDERNAME","LBSTDTN","LBSTTM",
                    "LBREFID","LBTEST","LBORRES","LBORRESU",
                    "LBSPEC","LBREASND","RUNID"))

DTStbl_cols = DTStbl %>%
  dplyr::select(DTS_Col_Names, DTSlabels)

##  Outer join on column names to identify mismatchcolumn names
HeaderCheck = DTStbl_cols %>%
  dplyr::full_join(lbiqpcr_cols, 
                   by = c('DTS_Col_Names' = 'lbiqpcr_Col_Names')) %>%
  dplyr::full_join(lbddpcr_cols, 
                   by = c('DTS_Col_Names' = 'lbddpcr_Col_Names')) %>%
  
  ## rules to identify mismatch label names
  dplyr::mutate(
    Finding = 
      ifelse(is.na(DTS_Col_Names) | 
               is.na(lbiqpcr_Col_Label) | 
               is.na(lbddpcr_Col_Label),
             'Mismatch between dataset column name and DTS Biometrics Field Name',
        ifelse(DTSlabels != lbiqpcr_Col_Label | 
                 DTSlabels != lbddpcr_Col_Label,
               'Mismatch between dataset labels and DTS Watson LIMS Field Name',
               "OK")))

           
outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\BAS\\'
write.csv(HeaderCheck, 
          file.path(outloc, '270-301_BAS_Recon_columNames.csv'), 
          row.names = FALSE)


##------------------------
##------------------------
## DUPLICATE RECORDS

DupCheck = lbBAS_bind %>%
  dplyr::group_by(SUBJECT, FOLDERNAME, LBSTDTN, LBREFID, LBTEST) %>%
  dplyr::mutate(count = n()) %>%
  dplyr::filter(count > 1) %>%
  dplyr::arrange(SUBJECT, FOLDERNAME, LBSTDTN, LBREFID, LBTEST) %>%
  dplyr::mutate(Finding = 
                  ifelse(LBORRES == lag(LBORRES), 
                         paste0(count, 
                                ' duplicate test results identified'),
                         paste0(count, 
                                ' duplicate tests performed on same day'))) %>%
  dplyr::select(-count) %>%
  dplyr::filter(!is.na(Finding))


##------------------------
##------------------------
## MISSING DATA POINTS
MissingCheck = lbBAS_bind %>%
  dplyr::mutate(LBSTDTN = as.character(LBSTDTN)) %>%
  dplyr::mutate(Finding = 
                  ifelse(is.na(PROJECT) | PROJECT == "" | PROJECT == " " |
                           is.na(SUBJECT) | SUBJECT == "" | SUBJECT == " " |
                           is.na(FOLDERNAME) | FOLDERNAME == "" | FOLDERNAME == " " |
                           is.na(LBSTDTN) | LBSTDTN == "" | LBSTDTN == " " |
                           is.na(LBSTTM) | LBSTTM == "" | LBSTTM == " " |
                           is.na(LBREFID) | LBREFID == "" | LBREFID == " " |
                           is.na(LBTEST) | LBTEST == "" | LBTEST == " " |
                           is.na(LBORRES) | LBORRES == "" | LBORRES == " " |
                           is.na(LBORRESU) | LBORRESU == "" | LBORRESU == " " |
                           is.na(LBSPEC) | LBSPEC == "" | LBSPEC == " ", 
                         "Missing one or more expected values", 'OK')) %>%
  dplyr::mutate(LBSTDTN = as.Date(LBSTDTN)) %>%
  dplyr::filter(Finding != "OK")

              
##------------------------
##------------------------                                    
## CLIPPED STRINGS
ClippedCheck = lbBAS_bind %>%
  dplyr::mutate(Finding = 
                  ifelse(nchar(LBREASND) >= 160, 
                         "Manually review comment field for clipped sting", 
                         "OK")) %>%
  dplyr::filter(Finding != 'OK')



##------------------------
##------------------------
##  MISSING RUN ID Missing RunID, Reason Not Done and Results recon
RunIDCheck = lbBAS_bind %>%
  dplyr::mutate(Finding = 
                  ifelse((is.na(RUNID) | RUNID == "" | RUNID == " ") &
                         (!is.na(LBORRES) | LBORRES != "N.R."), 
                         "RunID is missing, but Results field does not indicate NA or N.R.", 
                         "OK")) %>%
  dplyr::filter(Finding != "OK")

##------------------------
##------------------------
##  RESULTS vs REASND RECON
##    flags when there are no results and reason not done is not specified
ResultCheck = lbBAS_bind %>%
  dplyr::mutate(Finding = 
                  ifelse(LBORRES == "N.R." & 
                           (is.na(LBREASND) | 
                              LBREASND == "" | 
                              LBREASND == " " |
                              LBREASND == "Not Applicable"),
                         "Results = N.R but a reason not done is not specified", 
                           "OK")) %>%
  dplyr::filter(Finding != 'OK')


##------------------------
##------------------------
##  VISIT LABEL MATCH
##    reconciliation between visit label and sample date

## From SVCompliance script, get visit names, dates, window
svSubs = svCompliance %>%
  dplyr::filter(Visit_Expected == 'Yes') %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FolderName,OID, INSTANCENAME,
                Date_of_Visit, LWindow_Date, Target_Date, UWindow_Date, Over,
                Visit_Expected)

## rename similar columns between lb data and svSubs
lbBAS_bind_m = lbBAS_bind %>% dplyr::rename(SUBJECT_bas = SUBJECT)



## OUTER JOIN WITH THREE STEPS
##  first perform R join
JoinR =  setDT(lbBAS_bind_m)[setDT(svSubs),
                         .(SUBJECT, FOLDERSEQ, FolderName,
                           LWindow_Date, UWindow_Date,
                           x.SUBJECT_bas, x.FOLDERNAME, x.LBSTDTN, x.LBSTTM,
                           x.LBREFID, x.LBTEST, x.LBORRES, x.LBSPEC),
                         all = TRUE,
                         on = .(SUBJECT_bas == SUBJECT,
                                LBSTDTN >= LWindow_Date,
                                LBSTDTN <= UWindow_Date)]
## remove the x prefix from column names
names(JoinR) = gsub('x.', '', names(JoinR), fixed = TRUE)

## second perform L join
JoinL =  setDT(svSubs)[setDT(lbBAS_bind_m),
                       .(x.SUBJECT, x.FOLDERSEQ, x.FolderName,
                         x.LWindow_Date, x.UWindow_Date,
                         SUBJECT_bas, FOLDERNAME, LBSTDTN, LBSTTM, LBREFID, 
                         LBTEST, LBORRES, LBSPEC),
                       all = TRUE,
                       on = .(SUBJECT == SUBJECT_bas,
                              LWindow_Date <= LBSTDTN,
                              UWindow_Date >= LBSTDTN)]
## remove the x prefix from column names
names(JoinL) = gsub('x.', '', names(JoinL), fixed = TRUE)

## third: row bind both R and L join df's and remove duplicates
lbBAS_FJoin = unique(rbind(JoinR, JoinL, use.names = FALSE))

## Find foldername mismatches and samples that do not fall in any window

lbBAS_folderCompare = lbBAS_FJoin %>%
  dplyr::mutate(
    Finding = 
      ifelse(is.na(SUBJECT_bas),
             'OK',
        ifelse(FOLDERNAME == 'Baseline' & 
                 (as.Date(LBSTDTN) >= as.Date(LWindow_Date) & 
                    as.Date(LBSTDTN) <= as.Date(UWindow_Date)),
               'OK',
          ifelse(is.na(SUBJECT), 
                 'Lab sample date does not fall within a protocol specified visit window',
            ifelse(FOLDERNAME != FolderName,
                   paste0('Folder name = ', FOLDERNAME, 
                          ' but lab date is within ', FolderName, 
                          ' protocol window'),
                   'OK')))))

FolderCheck = lbBAS_bind %>%
  dplyr::left_join(lbBAS_folderCompare[,c('SUBJECT_bas', 'FOLDERNAME',
                                          'LBSTDTN', 'LBSTTM', 'LBREFID',
                                          'LBTEST', 'LBORRES',
                                          'Finding')],
                   by = c('SUBJECT' = 'SUBJECT_bas', 'FOLDERNAME', 'LBSTDTN', 
                          'LBSTTM', 'LBREFID', 'LBTEST', 'LBORRES')) %>%
  dplyr::filter(Finding != 'OK')



##------------------------
##------------------------
## FINAL LISTING

lbBAS_BindChecks = dplyr::bind_rows(DupCheck, MissingCheck, ClippedCheck, 
                                    RunIDCheck, ResultCheck, FolderCheck)

runDate = as.character(Sys.Date())
outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\BAS\\'
write.csv(lbBAS_BindChecks, 
          file.path(outloc, 
                    paste0('270-301_BAS_Recon_', 
                    runDate,
                    '.csv')), row.names = FALSE)


