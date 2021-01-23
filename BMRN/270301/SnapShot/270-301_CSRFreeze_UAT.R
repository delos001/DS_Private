

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




## read files----------------------------------------
##    *note: the following files are read in with parent scripts 
##            dssh, sv so need to re-run here then reren parent script without
##            loading data from that script

dsshRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     'dssh.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')
dssfRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                 'dssf.sas7bdat', sep = ""), 
               .name_repair = 'check_unique')
egRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
                                     'eg.sas7bdat', sep = ""), 
                   .name_repair = 'check_unique')
# fafsRaw = read_sas(data_file = paste(ACEcdmwd, ACE270301unblinded, 
#                                  'fafs.sas7bdat', sep = ""), 
#                .name_repair = 'check_unique')
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


rtpth = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\'
chpth = 'Desktop\\Studies\\BMRN270\\270-301\\CSRFreeze\\writeRfiles\\'
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
svComp_coll = svRaw %>%
  dplyr::left_join(svCompliance[, c('SUBJECT', 'OID', 'UWindow_Date')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>%
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  dplyr::arrange(SUBJECT, FOLDER) %>%
  dplyr::filter((is.na(SVSTDAT) & SVND == 1 & UWindow_Date <= cutoffDate) | SVSTDAT <= cutoffDate) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique() %>%
  dplyr::group_by(SUBJECT) %>%
  dplyr::summarise(FOLDERlist = paste0(FOLDER, collapse = ','))


write.csv(svComp_coll,
          file.path(outloc, 'svComp_coll.csv'), row.names = FALSE)


##-------------------------------------------------------------------
##-------------------------------------------------------------------
## CREATE TABLES PER BMRN270-301 Topline Data Freeze specification doc


## Prior Screening Information
dssf = dssfRaw %>%
  ## there is no data, no need to program
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::select(SUBJECT, FOLDER) %>% unique() %>%
  dplyr::arrange(SUBJECT)

write.csv(dssf,
            file.path(outloc, 'dssf.csv'), row.names = FALSE)
write.csv(dssfRaw,
          file.path(outloc, 'dssfRaw.csv'), row.names = FALSE)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Previous Study History
dssh = dsshRaw %>%
  ## there is no data, no need to program
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::select(SUBJECT, FOLDER) %>% unique() %>%
  dplyr::arrange(SUBJECT)

write.csv(dsshRaw,
          file.path(outloc, 'dsshRaw.csv'), row.names = FALSE)
write.csv(dssh,
          file.path(outloc, 'dssh.csv'), row.names = FALSE)


##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Previous Study History
eg = egRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::filter(FOLDER == 'UNS') %>%
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(EGDAT <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, EGDAT) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(egRaw,
          file.path(outloc, 'egRaw.csv'), row.names = FALSE)
write.csv(eg,
          file.path(outloc, 'eg.csv'), row.names = FALSE)
  
  
##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Fibroscan
fafs = fafsRaw  ## there is no data, no need to program
  ## filter out locked sites
  # dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  # dplyr::filter(!SiteNumber %in% lockList)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Liver biopsy
falba = falbaRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  #dplyr::filter(FADAT <= Date_of_Visit) %>%   ## dates not entered for this form
  dplyr::arrange(SUBJECT, FOLDERSEQ, FADAT) %>%
  dplyr::select(SUBJECT, FOLDER)

write.csv(falbaRaw,
          file.path(outloc, 'falbaRaw.csv'), row.names = FALSE)
write.csv(falba,
          file.path(outloc, 'falba.csv'), row.names = FALSE)
    

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
  dplyr::arrange(SUBJECT, FOLDERSEQ, FADAT) %>%
  dplyr::select(SUBJECT, FOLDER)

write.csv(falbRaw,
          file.path(outloc, 'falbRaw.csv'), row.names = FALSE)
write.csv(falb,
          file.path(outloc, 'falb.csv'), row.names = FALSE)
  

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
  dplyr::arrange(SUBJECT, FOLDER) %>%
  dplyr::select(SUBJECT, FOLDER)
  

write.csv(falmRaw,
          file.path(outloc, 'falmRaw.csv'), row.names = FALSE)
write.csv(falm,
          file.path(outloc, 'falm.csv'), row.names = FALSE)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Elgibility Criteria
ie = ieRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::arrange(SUBJECT, FOLDERSEQ) %>%
  dplyr::select(SUBJECT, FOLDER) %>% 
  unique()
  
  # dplyr::select(SUBJECT, FOLDER) %>%
  # dplyr::group_by(SUBJECT) %>%
  # dplyr::filter(n() > 1)

write.csv(ieRaw,
          file.path(outloc, 'ieRaw.csv'), row.names = FALSE)
write.csv(ie,
          file.path(outloc, 'ie.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Chemistry
lbc = lbcRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## some entries are missing dates: this pulls date from Instance name
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBSTAT == 0 & is.na(LBDAT), 1, 0)) %>%
  ## filter where lbdat is not entered AND not done is not checked
  dplyr::filter(ND_exception == 0) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()
  
write.csv(lbcRaw,
          file.path(outloc, 'lbcRaw.csv'), row.names = FALSE)
write.csv(lbc,
          file.path(outloc, 'lbc.csv'), row.names = FALSE)
  
  
##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Coagulation
lbcg = lbcgRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## some entries are missing dates: this pulls date from Instance name
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBND == 0 & is.na(LBDAT), 1, 0)) %>%
  ## filter where lbdat is not entered AND not done is not checked
  dplyr::filter(ND_exception == 0) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(lbgRaw,
          file.path(outloc, 'lbgRaw.csv'), row.names = FALSE)
write.csv(lbg,
          file.path(outloc, 'lbg.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hematology
lbh = lbhRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## some entries are missing dates: this pulls date from Instance name
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBSTAT == 0 & is.na(LBDAT), 1, 0)) %>%
  ## filter where lbdat is not entered AND not done is not checked
  dplyr::filter(ND_exception == 0) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(lbhRaw,
          file.path(outloc, 'lbhRaw.csv'), row.names = FALSE)
write.csv(lbh,
          file.path(outloc, 'lbh.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Urinalysis
lbu = lbuRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## some entries are missing dates: this pulls date from Instance name
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBSTAT == 0 & is.na(LBDAT), 1, 0)) %>%
  ## filter where lbdat is not entered AND not done is not checked
  dplyr::filter(ND_exception == 0) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(lbuRaw,
          file.path(outloc, 'lbuRaw.csv'), row.names = FALSE)
write.csv(lbu,
          file.path(outloc, 'lbu.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Viral And Autoimmune Hepatitis
lbv = lbvRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## some entries are missing dates: this pulls date from Instance name
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBSTAT == 0 & is.na(LBDAT), 1, 0)) %>%
  
  ## filter where lbdat is not entered AND not done is not checked
  dplyr::filter(ND_exception == 0) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()
  
write.csv(lbvRaw,
          file.path(outloc, 'lbvRaw.csv'), row.names = FALSE)
write.csv(lbv,
          file.path(outloc, 'lbv.csv'), row.names = FALSE)

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Visit Type
mnyn = mnynRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::arrange(SUBJECT, FOLDERSEQ) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(mnynRaw,
          file.path(outloc, 'mnynRaw.csv'), row.names = FALSE)
write.csv(mnyn,
          file.path(outloc, 'mnyn.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Liver Ultrasound
mou = mouRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::mutate(StartDateFix = as.Date(ifelse(is.na(MODAT), 
                                              as.Date(RECORDDATE), 
                                              as.Date(MODAT)),
                                       origin = '1970-01-01')) %>%
  dplyr::filter(StartDateFix <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, StartDateFix) %>%
  dplyr::select(SUBJECT, FOLDER)

write.csv(mouRaw,
          file.path(outloc, 'mouRaw.csv'), row.names = FALSE)
write.csv(mou,
          file.path(outloc, 'mou.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Concomitant Procedures
pr = prRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(!is.na(PRENDAT)) %>%
  dplyr::filter(PRENDAT <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, PRENDAT) %>%
  dplyr::select(SUBJECT, FOLDER, PRENDAT) %>%
  unique()

write.csv(prRaw,
          file.path(outloc, 'prRaw.csv'), row.names = FALSE)
write.csv(pr,
          file.path(outloc, 'pr.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Drug Exposure During Pregnancy Follow Up
pregfu = pregfuRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::filter(PREGTERM == 'Yes' | !is.na(DELIVDAT)) %>%
  dplyr::arrange(SUBJECT, FOLDER) %>%
  dplyr::select(SUBJECT, FOLDER)

write.csv(pregfuRaw,
          file.path(outloc, 'pregfuRaw.csv'), row.names = FALSE)
write.csv(pregfu,
          file.path(outloc, 'pregfu.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Drug Exposure During Pregnancy Report
prgrpt = prgrptRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::arrange(SUBJECT, FOLDER) %>%
  dplyr::select(SUBJECT, FOLDER)

write.csv(prgrptRaw,
          file.path(outloc, 'prgrptRaw.csv'), row.names = FALSE)
write.csv(prgrpt,
          file.path(outloc, 'prgrpt.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Missed Work/School
qsem = qsemRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::filter(!is.na(WSDAT)) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(WSDAT <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, WSDAT) %>%
  dplyr::select(SUBJECT, FOLDER)  %>%
  unique()
  
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## EQ-5D-5L
qseq = qseqRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::filter(!is.na(QSDAT)) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(QSDAT <= Date_of_Visit) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT) %>%
  dplyr::select(SUBJECT, FOLDER)  %>%
  unique()
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hemophilia Activities List (HAL) - Part 1
qshal = qshalRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  ## acount for missing dates
  dplyr::mutate(QSDAT_fix = as.Date(ifelse(is.na(QSDAT), 
                                           Date_of_Visit, QSDAT),
                                    origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT) %>%
  dplyr::select(SUBJECT, FOLDER)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Hemophilia Activities List (HAL) - Part 2
qshal2 = qshal2Raw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  dplyr::rename(QSDAT_fix = Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%

  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Patient Reported Outcomes Burdens and Experiences (PROBE) - Page 1
qsprobe = qsprobeRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 

  ## acount for missing dates
  dplyr::mutate(QSDAT_fix = as.Date(ifelse(is.na(QSDAT), 
                                           Date_of_Visit, QSDAT),
                                    origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'OID')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT) %>%
  dplyr::select(SUBJECT, FOLDER)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Patient Reported Outcomes Burdens and Experiences (PROBE) - Page 2
qsprobe2 = qsprobe2Raw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  dplyr::rename(QSDAT_fix = Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER)
  
  
 
##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Haemo-QoL-A Quality of Life
qsqol = qsqolRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  dplyr::mutate(QSDAT_fix = as.Date(ifelse(is.na(QSDAT), as.Date(Date_of_Visit),
                                           as.Date(QSDAT)), 
                                    origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Work Productivity and Activity Impairment (WPAI+CIQ:HS)
qswpai = qswpaiRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  dplyr::mutate(QSDAT_fix = as.Date(ifelse(FOLDER == 'ET', 
                                           as.Date(Study_Exit_or_Today),
                                      ifelse(is.na(QSDAT), 
                                             as.Date(Date_of_Visit),
                                             as.Date(QSDAT))), 
                                    origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(((FOLDER == 'ET' & 
                    Study_Exit_or_Today <= cutoffDate) | 
                   QSDAT_fix <= Date_of_Visit) & !is.na(QSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, QSDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()
  
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Smart Re-Screening Question
srs = srsRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  dplyr::filter(!is.na(SRSYN)) %>%
  dplyr::arrange(SUBJECT, FOLDERSEQ, SRSYN) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(srsRaw,
          file.path(outloc, 'srsRaw.csv'), row.names = FALSE)
write.csv(srs,
          file.path(outloc, 'srs.csv'), row.names = FALSE)
  

##-------------------------------------------------------------------
##-------------------------------------------------------------------
## Date of Visit
sv = svRaw %>%
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID', 
                                    'UWindow_Date')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  dplyr::mutate(INSTANCENAMEDATE = as.Date(gsub('.*-', '', 
                                                INSTANCENAME), 
                                           format = '%d %b %Y')) %>%
  
  dplyr::mutate(SVSTDAT_fix = as.Date(ifelse(FOLDER == 'UNS' & is.na(SVSTDAT), 
                                             as.Date(INSTANCENAMEDATE), 
                                         ifelse(is.na(SVSTDAT), as.Date(UWindow_Date),
                                                as.Date(SVSTDAT))), 
                                    origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(SVSTDAT_fix <= Date_of_Visit) %>%
  
  dplyr::arrange(SUBJECT, FOLDERSEQ, SVSTDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER) %>%
  unique()

write.csv(svRaw,
          file.path(outloc, 'svRaw.csv'), row.names = FALSE)
write.csv(sv,
          file.path(outloc, 'sv.csv'), row.names = FALSE)
  
  
##-------------------------------------------------------------------
##-------------------------------------------------------------------
##  Vital Signs   
vs = vsRaw %>% 
  ## filter out locked sites
  dplyr::mutate(SiteNumber = sub('-.*', '', SUBJECT)) %>%
  dplyr::filter(!SiteNumber %in% lockList) %>%
  
  ## join visit date to account for missing dates (when questionnaire not done)
  dplyr::left_join(svCompliance[, c('SUBJECT', 'Date_of_Visit',
                                    'Study_Exit_or_Today', 'OID', 
                                    'UWindow_Date')],
                   by = c('SUBJECT', 'FOLDER' = 'OID')) %>% 
  
  dplyr::mutate(INSTANCENAMEDATE = as.Date(gsub('.*-', '', 
                                                INSTANCENAME), 
                                           format = '%d %b %Y')) %>%
  
  dplyr::mutate(VSDAT_fix = as.Date(ifelse(FOLDER == 'UNS' & is.na(VSDAT), 
                                             as.Date(INSTANCENAMEDATE), 
                                             ifelse(is.na(VSDAT), as.Date(UWindow_Date),
                                                    as.Date(VSDAT))), 
                                      origin = '1970-01-01')) %>%
  dplyr::select(-Date_of_Visit) %>%
  
  ## join last visit date <= 16Nov20
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit')],
                   by = 'SUBJECT') %>%
  
  dplyr::filter(VSDAT_fix <= Date_of_Visit) %>%
  
  dplyr::arrange(SUBJECT, FOLDERSEQ, VSDAT_fix) %>%
  dplyr::select(SUBJECT, FOLDER)
  


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## Document session info
sess = sessionInfo()
toLatex(sess)
