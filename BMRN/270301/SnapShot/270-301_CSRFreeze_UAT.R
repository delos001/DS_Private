

# TITLE: 270-301 CSR UAT Script
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 08Jan2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
# 
#
## outputs: see UAT document


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


## Define paths--------------------------------------
ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301unblinded = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'

## update path
##\\sassysprd.bmrn.com\cdm\cdmdev\bmn270\hemoa\270301\csrunblinded\output\clin\archive\csr_data

## read files----------------------------------------
##    *note: the following files are read in with parent scripts 
##            dssh, sv

dssfRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'dssf.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
egRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     'eg.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')
fafsRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'fafs.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
falbRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     'falb.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')
falbaRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                      'falba.sas7bdat', sep = ""), 
                    .name_repair = 'check_unique')
falmRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'falm.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
ieRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'ie.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
lbcRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'lbc.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
lbcgRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'lbcg.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
lbhRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'lbh.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
lbuRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'lbu.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
lbvRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'lbv.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
mnynRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                    'mnyn.sas7bdat', sep = ""), 
                  .name_repair = 'check_unique')
mouRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'mou.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
peRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                   'pe.sas7bdat', sep = ""), 
                 .name_repair = 'check_unique')
prRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'pr.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
pregfuRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     'pregfu.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')
prgrptRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'prgrpt.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qsemRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qsem.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qseqRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qseq.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qshalRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qshal.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qshal2Raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qshal2.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qsprobeRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qsprobe.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qsprobe2Raw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qsprobe2.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qsqolRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qsqol.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
qswpaiRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'qswpai.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
srsRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'srs.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
svRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'sv.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
vsRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                   'vs.sas7bdat', sep = ""), 
                 .name_repair = 'check_unique')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## WRITE DATA

rtpth = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\'
chpth = '270-301\\CSRFreeze\\writeRfiles\\'
outloc = paste0(rtpth, chpth)


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

# The closed/locked sites in EDC are:
# 0003 - Vanderbilt Children's Hospital
# 1631 - Michigan State University
# 1665 - Indiana Hemophilia and Thrombosis Center
# 1667 - Morvan Hospital
# 1784 - Cliniques Universitaires Saint Luc

lockList = c('0003', '1631', '1665', '1667', '1784')

## The static cutoff is 16Nov2020.  Create variable to represent this date
cutoffDate = as.Date('16-Nov-20', format = '%d-%b-%y')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE INPUT/REFERENCE TABLES

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

# write.csv(svComp_Nov, 
#           file.path(outloc, 'svComp_Nov.csv'), row.names = FALSE)


## Create completed study visit list, for each subject, in collapsed format
svComp_coll = svCompliance %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, Date_of_Visit) %>%
  dplyr::filter(Data_Entry_Expected == 'Yes' & Visit_Missing == 'No') %>%
  dplyr::filter(Date_of_Visit <= cutoffDate) %>%
  dplyr::select(SUBJECT, OID) %>%
  unique() %>%
  dplyr::group_by(SUBJECT) %>%
  dplyr::summarise(OIDList = paste0(OID, collapse = ', '))


# write.csv(svComp_coll, 
#           file.path(outloc, 'svComp_coll.csv'), row.names = FALSE)


##-------------------------------------------------------------------
##-------------------------------------------------------------------
## CREATE TABLES PER BMRN270-301 Topline Data Freeze specification doc


## Prior Screening Information
dssf = dssfRaw %>%
  ## there is no data, no need to program
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::select(SUBJECT) %>% unique() %>%
  dplyr::arrange(SUBJECT)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Previous Study History
dssh = dsshRaw %>%
  ## there is no data, no need to program
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::select(SUBJECT) %>% unique() %>%
  dplyr::arrange(SUBJECT)
  
##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Fibroscan
fafs = fafsRaw %>%  ## there is no data, no need to program
  ## filter out locked sites
  # dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  # dplyr::filter(!SiteNumber %in% lockList)
    

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Liver biopsy
falb = falbRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(FADAT <= Date_of_Visit) %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER, FADAT) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, FADAT)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Liver MRI
falm = falmRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(FADAT <= Date_of_Visit) %>%
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER, FADAT) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, FADAT)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Elgibility Criteria
ie = ieRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER) %>% unique() %>%
  dplyr::arrange(SUBJECT)
  
  ## ********Note that subject 1733-3924 has two lines for screening***********
  # dplyr::select(SUBJECT, FOLDER) %>%
  # dplyr::group_by(SUBJECT) %>%
  # dplyr::filter(n() > 1)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Chemistry
lbc = lbcRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(LBDAT <= Date_of_Visit) %>%
  
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER, LBDAT) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, LBDAT)
  
  
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Coagulation
lbcg = lbcgRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(LBDAT <= Date_of_Visit) %>%
  
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER, LBDAT) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, LBDAT)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hematology
lbh = lbhRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(LBDAT <= Date_of_Visit) %>%
  
  dplyr::select(SUBJECT, FOLDERSEQ, FOLDER, LBDAT) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, LBDAT)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Urinalysis
lbu = lbuRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Viral And Autoimmune Hepatitis
lbv = lbvRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Visit Type
mnyn = mnynRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Liver Ultrasound
mou = mouRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Concomitant Procedures
pr = prRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Drug Exposure During Pregnancy Follow Up
pregfu = pregfuRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Drug Exposure During Pregnancy Report
prgrpt = prgrptRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Missed Work/School
qsem = qsemRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## EQ-5D-5L
qseq = qseqRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hemophilia Activities List (HAL) - Part 1
qshal = qshalRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hemophilia Activities List (HAL) - Part 2
qshal2 = qshal2Raw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Patient Reported Outcomes Burdens and Experiences (PROBE) - Page 1
qsprobe = qsprobeRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Patient Reported Outcomes Burdens and Experiences (PROBE) - Page 2
qsprobe2 = qsprobe2Raw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Haemo-QoL-A Quality of Life
qsqol = qsqolRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Work Productivity and Activity Impairment (WPAI+CIQ:HS)
qswpai = qswpaiRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Smart Re-Screening Question
srs = srsRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Date of Visit
sv = svRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  
  
vs = vsRaw %>%
  


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## Document session info
sess = sessionInfo()
toLatex(sess)
