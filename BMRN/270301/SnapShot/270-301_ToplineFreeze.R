
# TITLE: 270-301 Topline Freeze UAT Script
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 21Nov2020

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

source('270-301_svAnchorData.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

mywd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'

path1 = 'BMRN270\\270-301\\EDC_SASOnDemand\\'

## these will be loaded with svAnchorData.R script so blocked out here
# file1 = 'sv.sas7bdat'
# file2 = 'dsic.sas7bdat'
# file3 = 'dssh.sas7bdat'
# file4 = 'ie.sas7bdat'

file5 = 'cmf.sas7bdat'
file6 = 'ae.sas7bdat'
file7 = 'ceb.sas7bdat'
file8 = 'cer.sas7bdat'
file9 = 'cm.sas7bdat'
file10 = 'dm.sas7bdat'
file11 = 'ds.sas7bdat'
file12 = 'dsss.sas7bdat'
file13 = 'ecif.sas7bdat'
file14 = 'eg.sas7bdat'
file15 = 'enr.sas7bdat'
file16 = 'lbf.sas7bdat'
file17 = 'mh.sas7bdat'
file18 = 'mhb.sas7bdat'
file19 = 'mhh.sas7bdat'
file20 = 'mht.sas7bdat'
file21 = 'pe.sas7bdat'
file22 = 'peh.sas7bdat'
file23 = 'vs.sas7bdat'
file24 = 'vst.sas7bdat'

## read in files
cmfRaw = read_sas(data_file = paste(mywd, path1, file5, sep = ""), 
                 .name_repair = 'check_unique')
aeRaw = read_sas(data_file = paste(mywd, path1, file6, sep = ""), 
                 .name_repair = 'check_unique')
cebRaw = read_sas(data_file = paste(mywd, path1, file7, sep = ""), 
                   .name_repair = 'check_unique')
cerRaw = read_sas(data_file = paste(mywd, path1, file8, sep = ""), 
                  .name_repair = 'check_unique')
cmRaw = read_sas(data_file = paste(mywd, path1, file9, sep = ""), 
                  .name_repair = 'check_unique')
dmRaw = read_sas(data_file = paste(mywd, path1, file10, sep = ""), 
                 .name_repair = 'check_unique')
dsRaw = read_sas(data_file = paste(mywd, path1, file11, sep = ""), 
                 .name_repair = 'check_unique')
dsssRaw = read_sas(data_file = paste(mywd, path1, file12, sep = ""), 
                 .name_repair = 'check_unique')
ecifRaw = read_sas(data_file = paste(mywd, path1, file13, sep = ""), 
                   .name_repair = 'check_unique')
egRaw = read_sas(data_file = paste(mywd, path1, file14, sep = ""), 
                   .name_repair = 'check_unique')
enrRaw = read_sas(data_file = paste(mywd, path1, file15, sep = ""), 
                 .name_repair = 'check_unique')
lbfRaw = read_sas(data_file = paste(mywd, path1, file16, sep = ""), 
                 .name_repair = 'check_unique')
mhRaw = read_sas(data_file = paste(mywd, path1, file17, sep = ""), 
                 .name_repair = 'check_unique')
mhbRaw = read_sas(data_file = paste(mywd, path1, file18, sep = ""), 
                 .name_repair = 'check_unique')
mhhRaw = read_sas(data_file = paste(mywd, path1, file19, sep = ""), 
                 .name_repair = 'check_unique')
mhtRaw = read_sas(data_file = paste(mywd, path1, file20, sep = ""), 
                 .name_repair = 'check_unique')
peRaw = read_sas(data_file = paste(mywd, path1, file21, sep = ""), 
                 .name_repair = 'check_unique')
pehRaw = read_sas(data_file = paste(mywd, path1, file22, sep = ""), 
                 .name_repair = 'check_unique')
vsRaw = read_sas(data_file = paste(mywd, path1, file23, sep = ""), 
                 .name_repair = 'check_unique')
vstRaw = read_sas(data_file = paste(mywd, path1, file24, sep = ""), 
                 .name_repair = 'check_unique')


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

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES
stDat = as.Date('16-11-20', format = '%d-%m-%y')
enDat = as.Date('16-Nov-20', format = '%d-%b-%y')

## Max visit dates for all subjects
svComp_Nov = svCompliance %>%
  dplyr::filter(!is.na(Date_of_Visit))

svCompMax = svComp_Nov %>%
  dplyr::filter(Date_of_Visit <= enDat) %>%
  dplyr::select(Subject, Date_of_Visit) %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(MaxVisitDate = max(Date_of_Visit)) %>%
  dplyr::left_join(svComp_Nov[, c('Subject', 'Date_of_Visit', 'FolderName', 
                                  'Visit_Expected', 'FolderSeq')],
                   by = c('Subject', 'MaxVisitDate' = 'Date_of_Visit'))


## AE AE files------------------------------------------------------------------

ae = aeRaw %>%
  dplyr::select(Folder, Subject, AESPID, 
                AESTDAT, AESTDAT_RAW,
                AESTDAT_YY, AESTDAT_MM, AESTDAT_DD,
                AEENDAT, AEENDAT_RAW,
                AEENDAT_YY, AEENDAT_MM, AEENDAT_DD,
                AEENRTPT) %>%
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
  dplyr::filter(AEENRTPT == 0) %>%  ## ongoing
  dplyr::filter(!is.na(enDate_imp)) %>%
  dplyr::filter(enDate_imp <= enDat)

aeCollapse = ae %>%
  dplyr::select(Subject, AESPID) %>%
  dplyr::arrange(Subject, AESPID) %>%
  dplyr::group_by(Subject) %>%
  dplyr::summarise(AESPID_collapse = paste0(AESPID, collapse = ','))


## CEB Blood log infusion file--------------------------------------------------
ceb = cebRaw %>%
  dplyr::select(Subject) %>%
  unique()

length(ceb$Subject)

## CER Infusion related reaction file-------------------------------------------
cer = cerRaw %>%
  dplyr::select(Subject) %>%
  dplyr::arrange(Subject) %>%
  unique()

length(cer$Subject)

## CM Conmed file---------------------------------------------------------------

cm = cmRaw %>%
  dplyr::select(Folder, Subject, RecordPosition, 
                CMSTDAT, CMSTDAT_RAW,
                CMSTDAT_YY, CMSTDAT_MM, CMSTDAT_DD,
                CMENDAT, CMENDAT_RAW,
                CMENDAT_YY, CMENDAT_MM, CMENDAT_DD,
                CMENRTPT) %>%
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
  dplyr::filter(CMENRTPT == 0) %>%  ## ongoing
  dplyr::filter(!is.na(enDate_imp)) %>%
  dplyr::left_join(svCompMax[, c('Subject', 'MaxVisitDate')],
                   by = 'Subject') %>%
  dplyr::filter(enDate_imp <= MaxVisitDate)


cmCollapse = cm %>%
  dplyr::select(Subject, RecordPosition) %>%
  unique() %>%
  dplyr::arrange(Subject, RecordPosition) %>%
  dplyr::group_by(Subject) %>%

  dplyr::summarise(CMRecordPosition_collapse = paste0(RecordPosition, 
                                                      collapse = ','))
  

cm2 = cmRaw %>%
  dplyr::select(Folder, Subject, RecordPosition, 
                CMSTDAT, CMSTDAT_RAW,
                CMENDAT, CMENDAT_RAW,
                CMENRTPT)
  
## CMF FVIII Infusion file------------------------------------------------------
cmf = cmfRaw %>%
  dplyr::select(Folder, Subject, RecordPosition, 
                CMSTDAT, CMSTDAT_RAW,
                CMSTDAT_YY, CMSTDAT_MM, CMSTDAT_DD,
                CMENDAT, CMENDAT_RAW,
                CMENDAT_YY, CMENDAT_MM, CMENDAT_DD,
                CMENRTPT) %>%
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
  #dplyr::filter(CMENRTPT == 0) %>%  ## ongoing
  #dplyr::filter(!is.na(enDate_imp)) %>%
  dplyr::left_join(svCompMax[, c('Subject', 'MaxVisitDate')],
                   by = 'Subject') %>%
  dplyr::filter(stDate_imp <= MaxVisitDate)

length(unique(cmf$Subject))

cmfCollapse = cmf %>%
  dplyr::select(Subject, RecordPosition) %>%
  unique() %>%
  dplyr::arrange(Subject, RecordPosition) %>%
  dplyr::group_by(Subject) %>%
  
  dplyr::summarise(CMFRecordPosition_collapse = paste0(RecordPosition, 
                                                       collapse = ','))


## DM emographics file----------------------------------------------------------
dm = dmRaw %>%
  dplyr::select(Subject)
  
dmCollapse = dm %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectDM_collapse = paste0(Subject, collapse = ','))


## DS Disposition file----------------------------------------------------------

unique(dsRaw$DSTERM_COD)
ds = dsRaw %>%
  dplyr::select(Subject)
  #dplyr::filter(DSTERM_COD != "OTHER") ## uncomment to exclude SF

dsCollapse = ds %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectDS_collapse = paste0(Subject, collapse = ','))


## DSSS Screening Status file---------------------------------------------------
dsss = dsssRaw %>%
  dplyr::select(Subject)
#dplyr::filter(DSTERM_COD != "OTHER") ## uncomment to exclude SF

dsssCollapse = dsss %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectDSSS_collapse = paste0(Subject, collapse = ','))



## ECIF Study Drug Infusion file------------------------------------------------
ecif = ecifRaw %>%
  dplyr::select(Subject)

ecifCollapse = ecif %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectECIF_collapse = paste0(Subject, collapse = ','))


## EG 12Lead ECG file-----------------------------------------------------------
eg = egRaw %>%
  dplyr::filter(Folder != "UNS") %>%
  dplyr::select(Subject)

egCollapse = eg %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectEG_collapse = paste0(Subject, collapse = ','))


## ENR 12Lead ECG file----------------------------------------------------------
enr = enrRaw %>%
  #dplyr::filter(Folder != "UNS") %>%
  dplyr::select(Subject)

enrCollapse = enr %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectENR_collapse = paste0(Subject, collapse = ','))

## checking for subjects without SF or enrollment date
enr2 = svAnchorData %>%
  dplyr::filter(SCRNFAIL_COD == 'N' & is.na(ENRDAT))


## LBF Local FVII file----------------------------------------------------------
lbf = lbfRaw %>%
  #dplyr::filter(Folder != "UNS") %>%
  dplyr::select(SUBJECT, LBDAT, LBSTAT, INSTANCENAME) %>%
  dplyr::mutate(InstanceDate = as.Date(gsub(".*-", "", INSTANCENAME), 
                                       format = '%d %b %Y'),
                StartDateFix = as.Date(ifelse(is.na(LBDAT), 
                                              as.Date(InstanceDate), 
                                              as.Date(LBDAT)), 
                                       origin = "1970-01-01"),
                ND_exception = ifelse(LBSTAT == 0 & is.na(LBDAT), 1, 0)) %>%
  dplyr::filter(ND_exception == 0) %>%
  dplyr::left_join(svCompMax[, c('Subject', 'MaxVisitDate')],
                   by = c('SUBJECT' = 'Subject')) %>%
  dplyr::filter(MaxVisitDate <= enDat) %>%
  dplyr::filter(StartDateFix <= MaxVisitDate) %>%
  dplyr::select(SUBJECT, MaxVisitDate) %>%
  dplyr::arrange(SUBJECT) %>%
  unique()



## MH Medical History file------------------------------------------------------
mh = mhRaw %>%
  dplyr::select(Subject)

mhCollapse = mh %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectMH_collapse = paste0(Subject, collapse = ','))


## MHB Medical History Bleed file-----------------------------------------------
mhb = mhbRaw %>%
  dplyr::select(Subject)

mhbCollapse = mhb %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectMB_collapse = paste0(Subject, collapse = ','))


## MHH Hemophilia History Bleed file--------------------------------------------
mhh = mhhRaw %>%
  dplyr::select(Subject)

mhhCollapse = mhh %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectMHH_collapse = paste0(Subject, collapse = ','))


## MHT Targeted Medical History file--------------------------------------------
mht = mhtRaw %>%
  dplyr::select(Subject)

mhtCollapse = mht %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectMHT_collapse = paste0(Subject, collapse = ','))


## PE Physical Exam file--------------------------------------------------------
pe = peRaw %>%
  dplyr::filter(Folder == 'SCR') %>%
  dplyr::select(Subject)

peCollapse = pe %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectPE_collapse = paste0(Subject, collapse = ','))


## PE Physical Exam Hypersensitivity file---------------------------------------
peh = pehRaw %>%
  dplyr::select(Subject)

pehCollapse = peh %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectPEH_collapse = paste0(Subject, collapse = ','))



## Vital Signs file-------------------------------------------------------------
vs = vsRaw %>%
  dplyr::filter(Folder == "SCR") %>%
  dplyr::select(Subject)


vsCollapse = vs %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectVS_collapse = paste0(Subject, collapse = ','))


## Vital Signs timed file-------------------------------------------------------------
vst = vstRaw %>%
  dplyr::select(Subject)


vstCollapse = vst %>%
  unique() %>%
  dplyr::arrange(Subject) %>%
  dplyr::summarise(SubjectVST_collapse = paste0(Subject, collapse = ','))

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE



## write output csvs
outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\ToplineFreeze\\'
write.csv(aeCollapse, 
          file.path(outloc, 'AEcollapse.csv'), row.names = FALSE)

write.csv(svCompMax,
          file.path(outloc, 'svCompMax.csv'), row.names = FALSE)

write.csv(cmCollapse, 
          file.path(outloc, 'CMcollapse.csv'), row.names = FALSE)

write.csv(cmfCollapse, 
          file.path(outloc, 'CMFcollapse.csv'), row.names = FALSE)

write.csv(dmCollapse, 
          file.path(outloc, 'dmcollapse.csv'), row.names = FALSE)

write.csv(dsCollapse, 
          file.path(outloc, 'dscollapse.csv'), row.names = FALSE)

write.csv(dsssCollapse, 
          file.path(outloc, 'dssscollapse.csv'), row.names = FALSE)

write.csv(ecifCollapse, 
          file.path(outloc, 'ecifscollapse.csv'), row.names = FALSE)

write.csv(egCollapse, 
          file.path(outloc, 'egcollapse.csv'), row.names = FALSE)

write.csv(enrCollapse, 
          file.path(outloc, 'enrcollapse.csv'), row.names = FALSE)

write.csv(lbf, 
          file.path(outloc, 'lbfunique.csv'), row.names = FALSE)

write.csv(mhCollapse, 
          file.path(outloc, 'mhcollapse.csv'), row.names = FALSE)

write.csv(mhbCollapse, 
          file.path(outloc, 'mhbcollapse.csv'), row.names = FALSE)

write.csv(vsCollapse, 
          file.path(outloc, 'vscollapse.csv'), row.names = FALSE)

write.csv(mhhCollapse, 
          file.path(outloc, 'mhhcollapse.csv'), row.names = FALSE)


write.csv(mhtCollapse, 
          file.path(outloc, 'mhtcollapse.csv'), row.names = FALSE)

write.csv(peCollapse, 
          file.path(outloc, 'pecollapse.csv'), row.names = FALSE)

write.csv(pehCollapse, 
          file.path(outloc, 'pehcollapse.csv'), row.names = FALSE)

write.csv(vsCollapse, 
          file.path(outloc, 'vscollapse.csv'), row.names = FALSE)

write.csv(vstCollapse, 
          file.path(outloc, 'vstcollapse.csv'), row.names = FALSE)
