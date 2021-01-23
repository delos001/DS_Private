
# TITLE: 270-301 Data Transfer Specification for Data With BioAnalytical Sciences

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

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS

source('270-301_svCompliance.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## Define paths--------------------------------------
ACEcdmwda = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblindeda = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'


## Define Files--------------------------------------



##------------------------------------------------------------------------------
## temporary path for LBDDPCR and LBIQPCR header test transfer files
bas_root = '\\\\sassysprd.bmrn.com\\cdm\\cdmdev\\'
bas_folder = 'bmn270\\hemoa\\270301\\csrunblinded\\output\\clin\\datatemp\\'

## CDM Dataset = LBDDPCR; Biological Matrices = PBMC and Whole Blood
lbiqpcr_header_file = 'lbiqpcr_header.sas7bdat'
lbddpcr_header_file = 'lbddpcr_header.sas7bdat'




## Read files----------------------------------------

lbiqpcr_header_raw = read_sas(data_file = paste(bas_root, bas_folder, 
                                                lbiqpcr_header_file, sep = ""),
                              .name_repair = 'check_unique')

lbddpcr_header_raw = read_sas(data_file = paste(bas_root, bas_folder,
                                                lbddpcr_header_file, sep = ""),
                              .name_repair = 'check_unique')





##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING

lbiqpcr_header = lbiqpcr_header_raw %>%
  dplyr::mutate(FileSource = 'LBIQPCR_HEADER')

lbddpcr_header = lbddpcr_header_raw %>%
  dplyr::mutate(FileSource = 'LBDDPCR_HEADER')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## get visit names, dates, windows
svSubs = svCompliance %>%
  dplyr::filter(Visit_Expected == 'Yes') %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FolderName,OID, INSTANCENAME,
                Date_of_Visit, LWindow_Date, Target_Date, UWindow_Date, Over,
                Visit_Expected)



## Bind lbiqpcr and lbddpcr files into one file
bas_Bind = dplyr::bind_rows(lbiqpcr_header, lbddpcr_header) %>%
  dplyr::rename(SUBJECT_bas = SUBJECT)



## perform outer join 
JoinR =  setDT(bas_Bind)[setDT(svSubs), 
                         .(SUBJECT, FOLDERSEQ, FolderName,
                           LWindow_Date, UWindow_Date,
                           x.SUBJECT_bas, x.FOLDERNAME, x.LBSTDTN, x.LBREFID, x.LBSPEC),
                         all = TRUE,
                         on = .(SUBJECT_bas == SUBJECT, 
                                LBSTDTN >= LWindow_Date, 
                                LBSTDTN <= UWindow_Date)]
names(JoinR) = gsub('x.', '', names(JoinR), fixed = TRUE)

JoinL =  setDT(svSubs)[setDT(bas_Bind),                                 
                       .(x.SUBJECT, x.FOLDERSEQ, x.FolderName,
                         x.LWindow_Date, x.UWindow_Date,
                         SUBJECT_bas, FOLDERNAME, LBSTDTN, LBREFID, LBSPEC),
                       all = TRUE,
                       on = .(SUBJECT == SUBJECT_bas, 
                              LWindow_Date <= LBSTDTN, 
                              UWindow_Date >= LBSTDTN)]
names(JoinL) = gsub('x.', '', names(JoinL), fixed = TRUE)



BASJoinFull = unique(rbind(JoinR, JoinL, use.names = FALSE))



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE








