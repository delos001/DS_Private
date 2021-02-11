
# TITLE: 270-301 CSR UAT of Manual Conmeds and AES Script
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 28Jan2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
# 
#
## outputs:


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

source('270-301_svCompliance.R')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## Path to snapshot data from 21Dec
ACEcdmwd21Dec = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded21Dec= 'bmn270\\hemoa\\270301\\csrintunbl202011a\\dataoper\\'


## Path to dataoper data (current date of 28Jan)
ACEcdmwd28Jan = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded28Jan = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'



## read in files-----------------------------------
ae21DecRaw <- read_sas(data_file = paste(ACEcdmwd21Dec, ACE270301unblinded21Dec, 
                                         'ae.sas7bdat', sep = ""), 
                       .name_repair = 'check_unique')


cm21DecRaw <- read_sas(data_file = paste(ACEcdmwd21Dec, ACE270301unblinded21Dec, 
                                         'cm.sas7bdat', sep = ""), 
                       .name_repair = 'check_unique')


ae28JanRaw <- read_sas(data_file = paste(ACEcdmwd28Jan, ACE270301unblinded28Jan, 
                                     'ae.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')


cm28JanRaw <- read_sas(data_file = paste(ACEcdmwd28Jan, ACE270301unblinded28Jan, 
                                   'cm.sas7bdat', sep = ""), 
                 .name_repair = 'check_unique')



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## WRITE DATA

rtpth = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\'
chpth = 'Desktop\\Studies\\BMRN270\\270-301\\CSRFreeze\\writeRfiles\\'
outloc = paste0(rtpth, chpth)

# 
# write.csv(svComp_Nov,
#           file.path(outloc, 'svComp_Nov.csv'), row.names = FALSE)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

lockList = c('0003', '1631', '1665', '1667', '1784')

## The static cutoff is 16Nov2020.  Create variable to represent this date
cutoffDate = as.Date('16-Nov-20', format = '%d-%b-%y')
cutoffDate21Dec = as.Date('21-Dec-20', format = '%d-%b-%y')

## Create table with max visit data on or before 16Nov2020, for each subject
svComp_Nov = svCompliance %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, Date_of_Visit) %>%
  dplyr::filter(Data_Entry_Expected == 'Yes' & Visit_Missing == 'No') %>%
  dplyr::filter(Date_of_Visit <= cutoffDate) %>%
  dplyr::group_by(SUBJECT) %>%
  dplyr::slice(which.max(Date_of_Visit)) %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FolderName, OID, INSTANCENAME, RECORDID,
                Screening, Smart_Re_Screening, SCRNFAIL_COD, Baseline, Day_1,
                Study_Exit_or_Today, Date_of_Visit, Visit_Expected, 
                Data_Entry_Expected, Visit_Missing)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## AE Listing

## Select columns of interest from the 21Dec snapshot data
ae21Dec = ae21DecRaw %>%
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
  dplyr::rename(AESTDAT_21Dec = stDate_imp,
                AEENDAT_21Dec = enDate_imp,
                AEENRTPT_21Dec = AEENRTPT) %>%
  dplyr::select(SUBJECT, RECORDID, AETERM, 
                AESTDAT_21Dec, AEENDAT_21Dec, 
                AEENRTPT_21Dec)


## join the 21Dec snapshot data to the 28Jan data
ae28Jan = ae28JanRaw %>%
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
  dplyr::rename(AESTDAT_28Jan = stDate_imp,
                AEENDAT_28Jan = enDate_imp,
                AEENRTPT_28Jan = AEENRTPT) %>%

  ## pull in last visit completed prior to 16Nov2020
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  ## select columns of interest
  dplyr::select(SUBJECT, RECORDID, 
                AETERM, 
                AESTDAT_28Jan, AEENDAT_28Jan, AEENRTPT_28Jan, 
                Date_of_Visit, LASTDATAENTRY) %>%
  
  dplyr::left_join(ae21Dec[, c('SUBJECT', 'RECORDID', 'AETERM', 
                               'AESTDAT_21Dec', 'AEENDAT_21Dec', 
                               'AEENRTPT_21Dec')],
                   by = c('SUBJECT', 'RECORDID', 'AETERM')) %>%
    
  dplyr::filter(AESTDAT_28Jan <= Date_of_Visit) %>%
  dplyr::mutate(Keep = 
                  ifelse((is.na(AESTDAT_21Dec) & AEENDAT_28Jan <= Date_of_Visit) | 
                           (AEENRTPT_21Dec == 1 & 
                              AEENRTPT_28Jan == 0 & 
                              is.na(AEENDAT_21Dec) & 
                              !is.na(AEENDAT_28Jan)), 
                            'Keep', 'Delete')) %>%
  dplyr::filter(Keep == "Keep") %>%
  dplyr::arrange(SUBJECT, AESTDAT_28Jan)


write.csv(ae28Jan, 
          file.path(outloc, 'AEManual.csv'), row.names = FALSE)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CM Listing
cm21Dec = cm21DecRaw %>%
  dplyr::mutate(stDD_imp = ifelse(grepl("^UN ", 
                                        toupper(CMSTDAT_RAW)), 
                                  1, CMSTDAT_DD),
                stMM_imp = ifelse(grepl('UNK', 
                                        toupper(CMSTDAT_RAW)), 
                                  1, CMSTDAT_MM),
                
                enDD_imp = ifelse(grepl("^UN ", 
                                        toupper(CMENDAT_RAW)), 
                                  16, CMENDAT_DD),
                enMM_imp = ifelse(grepl("UNK", 
                                        toupper(CMENDAT_RAW)), 
                                  11, CMENDAT_MM),
                stDate_imp = as.Date(paste(stDD_imp, '-', 
                                           stMM_imp, '-', 
                                           CMSTDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y'),
                enDate_imp = as.Date(paste(enDD_imp, '-', 
                                           enMM_imp, '-', 
                                           CMENDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y')) %>%
  dplyr::rename(CMSTDAT_21Dec = stDate_imp,
                CMENDAT_21Dec = enDate_imp,
                CMENRTPT_21Dec = CMENRTPT) %>%
  dplyr::select(SUBJECT, RECORDID, CMTRT, 
                CMSTDAT_21Dec, CMENDAT_21Dec, 
                CMENRTPT_21Dec)


## join the 21Dec snapshot data to the 28Jan data
cm28Jan = cm28JanRaw %>%
  dplyr::mutate(stDD_imp = ifelse(grepl("^UN ", 
                                        toupper(CMSTDAT_RAW)), 
                                  1, CMSTDAT_DD),
                stMM_imp = ifelse(grepl('UNK', 
                                        toupper(CMSTDAT_RAW)), 
                                  1, CMSTDAT_MM),
                
                enDD_imp = ifelse(grepl("^UN ", 
                                        toupper(CMENDAT_RAW)), 
                                  16, CMENDAT_DD),
                enMM_imp = ifelse(grepl("UNK", 
                                        toupper(CMENDAT_RAW)), 
                                  11, CMENDAT_MM),
                stDate_imp = as.Date(paste(stDD_imp, '-', 
                                           stMM_imp, '-', 
                                           CMSTDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y'),
                enDate_imp = as.Date(paste(enDD_imp, '-', 
                                           enMM_imp, '-', 
                                           CMENDAT_YY,  
                                           sep = ""), 
                                     format = '%d-%m-%Y')) %>%
  dplyr::rename(CMSTDAT_28Jan = stDate_imp,
                CMENDAT_28Jan = enDate_imp,
                CMENRTPT_28Jan = CMENRTPT) %>%
  
  ## pull in last visit completed prior to 16Nov2020
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  ## select columns of interest
  dplyr::select(SUBJECT, RECORDID, 
                CMTRT, 
                CMSTDAT_28Jan, CMENDAT_28Jan, CMENRTPT_28Jan, 
                Date_of_Visit, LASTDATAENTRY) %>%
  
  dplyr::left_join(cm21Dec[, c('SUBJECT', 'RECORDID', 'CMTRT', 
                               'CMSTDAT_21Dec', 'CMENDAT_21Dec', 
                               'CMENRTPT_21Dec')],
                   by = c('SUBJECT', 'RECORDID', 'CMTRT')) %>%
  
  dplyr::filter(CMSTDAT_28Jan <= Date_of_Visit) %>%
  dplyr::mutate(Keep = 
                  ifelse((is.na(CMSTDAT_21Dec) & CMENDAT_28Jan <= Date_of_Visit) | 
                           (CMENRTPT_21Dec == 1 & 
                              CMENRTPT_28Jan == 0 & 
                              is.na(CMENDAT_21Dec) & 
                              !is.na(CMENDAT_28Jan)), 
                         'Keep', 'Delete')) %>%
  dplyr::filter(Keep == "Keep") %>%
  dplyr::arrange(SUBJECT, CMSTDAT_28Jan)

write.csv(cm28Jan, 
          file.path(outloc, 'CMManual.csv'), row.names = FALSE)
##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE





