# TITLE: 270-301_BloodWorks_mutation
# STUDY: 370-301
# AUTHOR: Jaosn Delosh
# DATE: 12Nov2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#    The blood works files are not standard and need manipulation to be able 
#        to aggregate with Covance files found in ACE.  This script does that.
#
## outputs:  bwMutation


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS
source('./ExternalLaboratory/270-301_Covance_ACE_Dataoper.R')  ## needed to fix incorrect subject IDs
source('270-301_svAnchorData.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## Local files-----
localwd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'
path270301covance = 'BMRN270\\270-301\\Covance_Raw\\'

bwMutationsfile = 'BMN 270-301_bloodworks_mutation_11Feb2020.csv'


## read in files

bwMutations_raw = read.csv(file.path(paste(localwd, path270301covance, sep = ''), 
                                     bwMutationsfile), 
                           sep = ",", skip = 2, header = TRUE,
                           na.strings = c("", " ", "NA"))


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES
## bwMutation_raw doesn't contain LBSPID so hard coded from DTS specs here

bwMutations_LBSPID = data.frame(LBTEST = c('FVIII Genotyping', 
                                           'FVIII Genotyping Inversion Intron 22',
                                           'FVIII Genotyping HGVS Transcript', 
                                           'FVIII Genotyping HGVS Protein'),
                                CovanceName = c('Inversion - Intron 1', 
                                                'Inversion - Intron 22', 
                                                'HGVS Transcript', 
                                                'HGVS Protein'),
                                LBSPID = c(760, 871, 872, 873))

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

bwMutation = bwMutations_raw %>%
  ## fviiiGeno is not raw and has blank rows that need to be deleted
  subset(!is.na(Sample.ID)) %>%
  
  ## columns don't align with DVS so must be renamed
  dplyr::rename('FVIII Genotyping' = contains('Intron') & contains('1'),
                'FVIII Genotyping Inversion Intron 22' = contains('Intron') & 
                  contains('22'),
                'FVIII Genotyping HGVS Transcript' = contains('HGVS') & 
                  contains('Trans'),
                'FVIII Genotyping HGVS Protein' = contains('HGVS') & 
                  contains('Protein')
  ) %>%
  
  ## wide to long so each subject-test is on its own row
  tidyr::pivot_longer(c('FVIII Genotyping',
                        'FVIII Genotyping Inversion Intron 22',
                        'FVIII Genotyping HGVS Transcript',
                        'FVIII Genotyping HGVS Protein'), 
                      names_to = 'LBTEST', values_to = 'LBORRES') %>%
  
  ## join LBSPID created above in global variable section
  dplyr::left_join(bwMutations_LBSPID,
                   by = 'LBTEST') %>%
  dplyr::mutate(LBSPID = as.character(LBSPID),
                Results_Present = ifelse(X == 'Tested', 'Yes', 'No'),
                PROJECT = '270301',
                FileSource = 'bwMutations',
                LBDAT = as.Date('Date.of.Collection', format = '%d-%b-%y')) %>%
  
  ## select and rename columns to match covLab and blnd_fviii
  dplyr::select('PROJECT',
                'SUBJECT' = 'Sample.ID', 
                'FOLDERNAME' = 'Visit', 
                'LBDAT',
                'LBREFID' = 'Accn..',
                'LBSPID',
                'LBTEST',
                'LBORRES',
                'LBSTAT' = 'X',
                'Results_Present',
                'FileSource',
                'COMMENTS' = 'Comment')



## incorrect subject IDs appear to be present in bloodworks data.
## first get list of incorrect subjects and respective LBREFIDs
subjCompareMut = bwMutation %>% 
  dplyr::select(SUBJECT, LBREFID) %>% 
  unique() %>%
  dplyr::anti_join(svAnchorData[, c('SUBJECT')], 
                   by = c('SUBJECT' = 'SUBJECT')) %>%
  dplyr::mutate(FileSource = 'bwMutations') %>%
  
  ## then look up subject number in Covance data set to get 'correct' subject ID
  dplyr::left_join(Covance_ACE_final[, c('SUBJECT', 'LBREFID')], 
                   by = c('LBREFID')) %>%
  dplyr::rename(SUBJECT = SUBJECT.x,
                SUBJECT_fix = SUBJECT.y) %>%
  dplyr::distinct(SUBJECT, SUBJECT_fix)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

## join fix column to fviiiGeno
bwMutation_final = bwMutation %>%
  dplyr::left_join(subjCompareMut[,c('SUBJECT', 'SUBJECT_fix')], by = 'SUBJECT')
