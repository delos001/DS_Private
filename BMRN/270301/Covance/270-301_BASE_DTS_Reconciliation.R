
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


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS

source('270-301_svCompliance.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'

## Files:
## CDM Dataset = LBDDPCR; Biological Matrices = PBMC and Whole Blood
lbddpcr_ace = 'lbddpcr.sas7bdat'

## CDM Dataset = LBIQPCR; Bioligical Matrices = Semen and Plasma
lbiqpcr_ace = 'lbiqpcr.sas7bdat'

## CDM Dataset = LBIQPCR; Bioligical Matrices = PBMC
lb_x_ace = 'x.sas7bdat'


## Read files
lbddpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                         lbddpcr_ace, sep = ""),
                         .name_repair = 'check_unique')

## not available yet: pending build and test transfer
lbiqpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                         lbiqpcr_ace, sep = ""),
                       .name_repair = 'check_unique')


lb_x_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                      lb_x_ace, sep = ""),
                    .name_repair = 'check_unique')



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

svSubs = svCompliance %>%
  dplyr::filter(Visit_Expected == 'Yes') %>%
  dplyr::select(SUBJECT, Parent.Folder, FOLDERSEQ, FolderName,OID, INSTANCENAME,
                Date_of_Visit, LWindow_Date, Target_Date, UWindow_Date, Over,
                Visit_Expected)  %>%
  
  dplyr::full_join(lbddpcr_raw, by = c('SUBJECT', 'FolderName' = 'FOLDERNAME'))



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE





