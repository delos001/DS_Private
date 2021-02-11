
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
library(datapasta)

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


## Specify the pdf location
localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'
localpth = 'BMRN270\\270-301\\BAS\\'
file1 = 'BMN270-301_DTA_BioAnalytical_Sciences_22DEC2020.pdf'
mypdf = paste0(localroot, localpth, file1)


## Read files----------------------------------------

lbiqpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub, 
                                         lbiqpcr_file, sep = ""),
                              .name_repair = 'check_unique')

lbddpcr_raw = read_sas(data_file = paste(ACEcdmwd, ACE270301csrub,
                                         lbddpcr_file, sep = ""),
                              .name_repair = 'check_unique')

lbBAS_bind = dplyr::bind_rows(lbiqpcr_raw, lbddpcr_raw)


##------------------------------------------------------------------------------
## Column header match DTS

## Get labels from data file
lbiqpcr_label_lookup_map <- data.frame(
  col_name = lbiqpcr_raw %>% names(),
  iqpcrlabels = lbiqpcr_raw %>% map_chr(attr_getter("label"))
)

lbddpcr_label_lookup_map <- data.frame(
  col_name = lbddpcr_raw %>% names(),
  ddpcrlabels = lbddpcr_raw %>% map_chr(attr_getter("label"))
)

## Show labels from DTS
## Uses library(datapast): datapast::dt_paste()  (need to copy excel to clipboard first)
specTbl = data.table::data.table(
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
  col_name = c("PROJECT","SUBJECT",
               "FOLDERNAME","LBSTDTN","LBSTTM",
               "LBREFID","LBTEST","LBORRES","LBORRESU",
               "LBSPEC","LBREASND","RUNID"))

specTbl2 = specTbl %>%
  dplyr::select(col_name, DTSlabels)

HeaderCheck = specTbl2 %>%
  dplyr::full_join(lbiqpcr_label_lookup_map, by = 'col_name') %>%
  dplyr::full_join(lbddpcr_label_lookup_map, by = 'col_name') %>%
  dplyr::mutate(Finding = 
                  ifelse(DTSlabels != iqpcrlabels | DTSlabels != ddpcrlabels, 
                           "Dataset labels do not match DTS",
                    ifelse(is.na(col_name), "Header name does not match DTS Biometrics Field Name",
                           "OK")))

           
outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\BAS\\'
write.csv(HeaderCheck, 
          file.path(outloc, '270-301_BAS_Recon_columNames.csv'), row.names = FALSE)

##------------------------------------------------------------------------------
## Duplicate records

DupCheck = lbBAS_bind[duplicated(lbBAS_bind[,c('SUBJECT', 'FOLDERNAME', 
                                               'LBSTDTN', 'LBSTTM', 'LBREFID',
                                               'LBTEST', 'LBORRES')]),]

DupCheck2 = lbBAS_bind %>%
  dplyr::group_by(SUBJECT, FOLDERNAME, LBSTDTN, LBSTTM, LBREFID, LBTEST, LBORRES) %>%
  dplyr::tally() %>%
  dplyr::filter(n > 1)


## OK



##------------------------------------------------------------------------------
## Missing Data
MissingCheck = lbBAS_bind %>%
  dplyr::mutate(LBSTDTN = as.character(LBSTDTN)) %>%
  dplyr::mutate(FindMissing = 
                  ifelse(is.na(PROJECT) | PROJECT == "" | PROJECT == " " |
                           is.na(SUBJECT) | SUBJECT == "" | SUBJECT == " " |
                           is.na(FOLDERNAME) | FOLDERNAME == "" | FOLDERNAME == " " |
                           is.na(LBSTDTN) | LBSTDTN == "" | LBSTDTN == " " |
                           is.na(LBSTTM) | LBSTTM == "" | LBSTTM == " " |
                           is.na(LBREFID) | LBREFID == "" | LBREFID == " " |
                           is.na(LBTEST) | LBTEST == "" | LBTEST == " " |
                           is.na(LBORRES) | LBORRES == "" | LBORRES == " " |
                           is.na(LBORRESU) | LBORRESU == "" | LBORRESU == " " |
                           is.na(LBSPEC) | LBSPEC == ""| LBSPEC == " ", 
                         "Missing one or more expected values", 'OK')) %>%
  dplyr::filter(FindMissing != "OK")
                  
##------------------------------------------------------------------------------                                      
## Clipped Strings
ClippedCheck = lbBAS_bind %>%
  dplyr::mutate(FindClipped = ifelse(nchar(LBREASND) >= 160, 
                                "Manual Review for Clipped String", "OK")) %>%
  dplyr::filter(FindClipped != 'OK')


##------------------------------------------------------------------------------                                      
## Missing RunID, Reason Not Done and Results recon
ResReasRunCheck = lbBAS_bind %>%
  dplyr::mutate(FindResReasRun1 = 
                  ifelse((is.na(RUNID) | RUNID == "" | RUNID == " ") &
                         (!is.na(LBORRES) | LBORRES != "N.R."), 
                         "1: RunID is missing, but Results field does not indicate NA or N.R.", 
                         ""),
                FindResReasRun2 = ifelse(LBORRES == "N.R." & 
                                           (is.na(LBREASND) | 
                                              LBREASND == "" | 
                                              LBREASND == " " |
                                              LBREASND == "Not Applicable"),
                           "2: Results reflect N.R but a reason not done is not specified", 
                           ""),
                FindResReasRun3 = ifelse(LBREASND != "Not Applicable" & LBREASND != 'QNS' & nchar(LBREASND) > 1 & LBORRES != 'N.R.', 
                                         "3: A reason not done is specified, but Results field does not reflect N.R",
                                         "OK")) %>%
  dplyr::mutate(FindResReasRun = trimws(paste(FindResReasRun1, '\n',
                                       FindResReasRun2, '\n',
                                       FindResReasRun3,  '\n',
                                       sep = "")),
                FindResReasRunFinal = ifelse(FindResReasRun == "", 
                                             "OK", 
                                             FindResReasRun)) %>%
  dplyr::select(-FindResReasRun1, -FindResReasRun2, -FindResReasRun3, 
                -FindResReasRun) %>%
                
  dplyr::filter(FindResReasRunFinal != "OK")


## meeting notes: check with Chunzi about adding new comments since data is tx non-cummulative

## possible new checks:  sometimes dates and visits:
      ##  visit and date don't match:, ie: lab date associated with D1, but lab date is actually in W4 protocol window

      ## 



outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\BAS\\'
write.csv(ResReasRunCheck, 
          file.path(outloc, '270-301_BAS_Recon.csv'), row.names = FALSE)


