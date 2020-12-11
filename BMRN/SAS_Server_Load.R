

# TITLE: 270-301_SVAnchorDAta.R
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 24Sep2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------

# This script uses the SV EDC table to get key visits: scr, rescrn, bl, D1 and
#     protocol completion status for each subject assigned a screening number
# 
# RAVE/Reporter/SAS On Demand
#     Complete the following entries:
#           File Type = CSV
#           Zip File Target = Email
#           Extract Type = Cumulative
#           View Type = chose "SV" for this script
#           Enter and confirm a password to open the zip file
#     Leave all other fields default
#     The file will be emailed to you
#     Open file, enter password, save as <name>.csv in desired location

## outputs:
# svkey, svAnchorData


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

## update based on save location and file name

SASwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'

path1 = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'

file1 = 'sv.sas7bdat'     ##subject visit data
file2 = 'ds.sas7bdat'     ## subject disposition data
file3 = 'dsic.sas7bdat'   ## informed consent data
file4 = 'dssh.sas7bdat'   ## previous study history data
file5 = 'dsss.sas7bdat'   ## screening status
file6 = 'ie.sas7bdat'     ## eligibility data
testfile = 'lbf.sas7bdat'


## read data
svRaw = read_sas(data_file = paste(SASwd, path1, file1, sep = ""), 
                 .name_repair = 'check_unique')

dsRaw = read_sas(data_file = paste(SASwd, path1, file2, sep = ""), 
                 .name_repair = 'check_unique')

dsicRaw = read_sas(data_file = paste(SASwd, path1, file3, sep = ""), 
                   .name_repair = 'check_unique')

dsshRaw = read_sas(data_file = paste(SASwd, path1, file4, sep = ""), 
                   .name_repair = 'check_unique')

dsssRaw = read_sas(data_file = paste(SASwd, path1, file5, sep = ""), 
                   .name_repair = 'check_unique')


lbfTestRaw = read_sas(data_file = paste(SASwd, path1, testfile, sep = ""), 
                   .name_repair = 'check_unique')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE INTERMEDIATE TABLES--------------------------------------------

## list folderSeq number of visits of interest
svKeyFolders = c(1, 3, 4, 6, 139)


## Table: SVkey
## summary table: key visits(screening, smrt rscrn, d1, ET)
svKey = svRaw %>%
  dplyr::mutate(SVSTDAT_date = as.Date(SVSTDAT, format = '%d-%b-%y')) %>%
  dplyr::filter(FolderSeq %in% svKeyFolders) %>%
  dplyr::select(project, Subject, FolderName, SVSTDAT) %>%
  tidyr::spread(.,FolderName, SVSTDAT)  %>%
  dplyr::select('project', 'Subject', 'Screening', 
                'Smart Re-Screening', 'Baseline', 'Day 1', 
                'Early Termination') %>%
  dplyr::rename('Smart_Re_Screening' = 'Smart Re-Screening',
                'Day_1' = 'Day 1',
                'Early_Termination' = 'Early Termination')  %>%
  
  ## join date subject ended study and reason
  dplyr::left_join(dsRaw[,c('Subject', 'DSSTDAT', 
                            'DSTERM_COD', 'DSTERMSP')],
                   by = "Subject") %>%
  dplyr::rename('Study_Exit_Date' = 'DSSTDAT',
                'Reason_Subject_Exited_coded' = 'DSTERM_COD',
                'Reason_Subject_Exited_other' = 'DSTERMSP') %>%
  ## join ICF data
  dplyr::left_join(dsicRaw[, c('Subject', 'DSSTDAT', 
                               'ICRESIDU_COD', 'ICGENTST_COD', 'ICGENEXP_COD',
                               'DSSCAT', 'DSSCAT_COD')],
                   by = 'Subject') %>%
  dplyr::rename('Date_of_ICF' = 'DSSTDAT') %>%
  ## join previous study data
  dplyr::left_join(dsshRaw[, c('Subject', 'PSHSTAT', 'PSTUDY', 'PSUBJID')],
                   by = 'Subject') %>%
  ## join screening status data
  dplyr::left_join(dsssRaw[, c('Subject', 'SCRNFAIL_COD', 
                               'DSTERM', 'DSTERM_COD',
                               'ENRDAT')],
                   by = 'Subject') %>%
  dplyr::rename('Screen_Fail_Reason' = 'DSTERM',
                'Screen_Fail_Reason_coded' = 'DSTERM_COD')




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## SVAnchorData: all key data at beginning and end of study 
##    counts, min, max visit dates
svAnchorData = svRaw %>%
  dplyr::mutate(SVSTDAT_date = as.Date(SVSTDAT, format = '%d-%b-%y')) %>%
  dplyr::select(Subject, FolderName, SVSTDAT_date, FolderSeq) %>%
  dplyr::group_by(Subject) %>%
  
  ## key visit dates
  dplyr::summarize(
    ## most recent visit date (includes unsch)
    Visit_Date_Latest = max(na.omit(SVSTDAT_date)), 
    ## visit name for most recent visit (includes unsch visits)
    Visit_Name_Latest = 
      na.omit(FolderName[SVSTDAT_date == Visit_Date_Latest])[1],
    
    ## visit date for most recent required study visit
    Visit_Date_Latest_reqd = max(na.omit(SVSTDAT_date[FolderSeq < 139])),
    ## visit name for most recent required study visit
    Visit_Name_Latest_reqd = 
      na.omit(FolderName[FolderSeq < 139 & 
                           SVSTDAT_date == Visit_Date_Latest_reqd]),
    .groups = 'keep'
  ) %>%
  
  dplyr::ungroup() %>%
  
  ## join key dates table
  dplyr::left_join(svKey, by = "Subject") %>%
  
  dplyr::arrange(Subject, Screening) %>%
  dplyr::select(project, Subject, PSHSTAT, PSTUDY, PSUBJID,
                DSSCAT, DSSCAT_COD, Date_of_ICF,
                ICRESIDU_COD, ICGENTST_COD, ICGENEXP_COD,
                Screening, Smart_Re_Screening, 
                SCRNFAIL_COD, Screen_Fail_Reason, Screen_Fail_Reason_coded,
                Baseline, Day_1, ENRDAT,
                Early_Termination, Study_Exit_Date,
                Reason_Subject_Exited_coded, Reason_Subject_Exited_other,
                Visit_Date_Latest, Visit_Name_Latest, 
                Visit_Date_Latest_reqd, Visit_Name_Latest_reqd)