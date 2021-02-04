
# TITLE: 270-301 BAS Header Reconciliation

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
library(tabulizer)

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
lbiqpcr_header_file = 'lbiqpcr_header.sas7bdat'
lbddpcr_header_file = 'lbddpcr_header.sas7bdat'


## Specify the pdf location
localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'
localpth = 'BMRN270\\270-301\\BAS\\'
file1 = 'BMN270-301_DTA_BioAnalytical_Sciences_22DEC2020.pdf'
mypdf = paste0(localroot, localpth, file1)

## extract the tables
pdfout = extract_tables(mypdf)




## Read files----------------------------------------

lbiqpcr_header_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub, 
                                                lbiqpcr_header_file, sep = ""),
                              .name_repair = 'check_unique')

lbddpcr_header_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub,
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

## Bind lbiqpcr and lbddpcr files into one file
bas_Bind = dplyr::bind_rows(lbiqpcr_header, lbddpcr_header) %>%
  dplyr::rename(SUBJECT_bas = SUBJECT)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE INPUT TABLES

## PDF Document------------------------------------------
## Get tables of interest from PDF controlled document
## bind tables, convert to data frame
tblA3 = as.data.frame(do.call(rbind, pdfout[15:16])) %>%
  ## remove extraneous rows: convert blanks to NA, then omit rows that are NA
  na_if("") %>%
  na.omit
colnames(tblA3) = c('Biological_Matrix(LBSPEC)', 'Analyte_Name(LBTESTCD)',
                    'Lab_Test_Category(LBCAT)', 'Lab_Test_Identifier(LBSPID',
                    'Lab_Test_Short_Name(LBTEST)')
tblA3 = tblA3 %>% dplyr::mutate(TableName = "Attachment 3")


## Anchor Data from SVCompliance Table--------------------
## get visit names, dates, windows
svSubs = svCompliance %>%
  dplyr::filter(Visit_Expected == 'Yes') %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FolderName,OID, INSTANCENAME,
                Date_of_Visit, LWindow_Date, Target_Date, UWindow_Date, Over,
                Visit_Expected)




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE


## perform outer join 
# JoinR =  setDT(bas_Bind)[setDT(svSubs), 
#                          .(SUBJECT, FOLDERSEQ, FolderName,
#                            LWindow_Date, UWindow_Date,
#                            x.SUBJECT_bas, x.FOLDERNAME, x.LBSTDTN, 
#                            x.LBREFID, x.LBSPEC),
#                          all = TRUE,
#                          on = .(SUBJECT_bas == SUBJECT, 
#                                 LBSTDTN >= LWindow_Date, 
#                                 LBSTDTN <= UWindow_Date)]
# names(JoinR) = gsub('x.', '', names(JoinR), fixed = TRUE)
# 
# JoinL =  setDT(svSubs)[setDT(bas_Bind),                                 
#                        .(x.SUBJECT, x.FOLDERSEQ, x.FolderName,
#                          x.LWindow_Date, x.UWindow_Date,
#                          SUBJECT_bas, FOLDERNAME, LBSTDTN, LBREFID, LBSPEC),
#                        all = TRUE,
#                        on = .(SUBJECT == SUBJECT_bas, 
#                               LWindow_Date <= LBSTDTN, 
#                               UWindow_Date >= LBSTDTN)]
# names(JoinL) = gsub('x.', '', names(JoinL), fixed = TRUE)
# 
# 
# 
# BASJoinFull = unique(rbind(JoinR, JoinL, use.names = FALSE))





