
# TITLE: 270-301_Covance_bloodworks_mutation
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 12Nov2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#    The blood works files are not standard and need manipulation to be able 
#        to aggregate with Covance files found in ACE.  This script does that.
#
## outputs:  bwHLA

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


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

localwd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'
path270301covance = 'BMRN270\\270-301\\Covance_Raw\\'

bwHLAfile = 'bmn_270301_bloodworks_hla_11Feb2020.csv'


## read in files
bwHLA_raw = read.csv(file.path(paste(localwd, path270301covance, sep = ''), 
                               bwHLAfile), 
                     sep = ",", skip = 0, header = TRUE,                       
                     na.strings = c("", " ", "NA"))



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

bwHLA_LBSPID = c('HLA-A Allele 1' = 805, 
                 'HLA-A Allele 2' = 878, 
                 'HLA-B Allele 1' = 806, 
                 'HLA-B Allele 2' = 879, 
                 'HLA-C Allele 1' = 807, 
                 'HLA-C Allele 2' = 880, 
                 'HLA-DPB1 Allele 1' = 810, 
                 'HLA-DPB1 Allele 2' = 883, 
                 'HLA-DQB1 Allele 1' = 809, 
                 'HLA-DQB1 Allele 2' = 882, 
                 'HLA-DRB1 Allele 1' = 808, 
                 'HLA-DRB1 Allele 2' = 881,
                 'HLA Typing DRB1 Non-excluded alleles' = 868,
                 'HLA Typing DQB1 Non-excluded alleles' = 869,
                 'HLA Typing DPB1 Non-excluded alleles' = 870,
                 ## manually added because missing from DTS: 
                 'HLA-DQA1 Allele 1' = 1261,
                 'HLA Typing DQA1 Non-excluded alleles' = 1300,
                 'HLA-DQA1 Allele 2' = 1262)





## create dataframe from bwHLA_LBSPID list
bwHLA_LBSPIDdf = purrr::map2_df(bwHLA_LBSPID, names(bwHLA_LBSPID), 
                                function(x, y){
                                  data.frame(
                                    LBTEST = y, 
                                    LBSPID = as.character(x))
                                  })  


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

bwHLA = bwHLA_raw %>%
  ## fviiiGeno is not raw and has blank rows that need to be deleted
  subset(!is.na(Subject.ID)) %>%
  ## fill values to get rid of the stupid stupid outline data structure here
  tidyr::fill(Accession.Number, Test.Name, Visit.Name, Date.of.Collection) %>%
  dplyr::mutate(Locus_mod = ifelse(is.na(Locus), substr(Allele, 1, 4), 
                                   str_trim(Locus, side = 'both'))) %>%
  
  ## tests are listed vertically and horizontally (pvt to make tidy)
  tidyr::pivot_longer(c(Allele, 
                        Non.excluded.DRB1.alleles,
                        Non.excluded.DQB1.alleles,
                        Non.excluded.DPB1.alleles,
                        Non.excluded.DQA1.alleles),
                      names_to = 'Alleles', values_to = 'LBORRES') %>%
  
  ## pvt creates unwanted rows: this creates column to tag for removal
  dplyr::mutate(Allele_Locus = ifelse(Alleles == 'Allele', Locus_mod,
                                      substr(Alleles, 14, 17))) %>%
  
  ## remove unwanted rows (where allele locus doesn't match locus_mod)
  dplyr::filter(Locus_mod == Allele_Locus) %>%
  
  ## use existing data to contruct test name per DTS
  dplyr::mutate(LBTEST = ifelse(Alleles == 'Allele',
                                paste('HLA-', Locus_mod, ' Allele', " ", 
                                      Chromosome, 
                                      sep = ""),
                                paste('HLA Typing ', Locus_mod, 
                                      ' Non-excluded alleles', 
                                      sep = ""))) %>%
  
  ## add LBSPID
  dplyr::left_join(bwHLA_LBSPIDdf, by = 'LBTEST') %>%
  mutate(LBSTAT = "",
         Results_Present = ifelse(is.na(LBORRES), 'No', 'Yes'),
         PROJECT = '270301',
         FileSource = 'BloodWorks_HLA',
         LBDAT = as.Date(Date.of.Collection, format = '%d-%b-%y')) %>%
  dplyr::select('PROJECT',
                'SUBJECT' = 'Subject.ID', 
                'FOLDERNAME' = 'Visit.Name', 
                'LBREFID' = 'Accession.Number',
                'LBDAT',
                'LBSCAT' = 'Test.Name',
                'LBSPID',
                'LBTEST',
                'Class', 'Locus_mod', 'Chromosome', 'Alleles',
                'LBORRES',
                'LBSTAT',
                'Results_Present',
                'FileSource')

## incorrect subject IDs appear to be present in bloodworks data.
## first get list of incorrect subjects and respective LBREFIDs
subjCompareHLA = bwHLA %>% 
  dplyr::select(SUBJECT, LBREFID) %>% 
  unique() %>%
  dplyr::anti_join(svAnchorData[, c('Subject')], 
                   by = c('SUBJECT' = 'Subject')) %>%
  dplyr::mutate(FileSource = 'BloodWords_Mutation Works') %>%
  
  ## then look up subject number in Covance data set to get 'correct' subject ID
  dplyr::left_join(Covance_ACE_final[, c('SUBJECT', 'LBREFID')], 
                   by = c('LBREFID')) %>%
  dplyr::rename(SUBJECT = SUBJECT.x,
                SUBJECT_fix = SUBJECT.y) %>%
  dplyr::distinct(SUBJECT, SUBJECT_fix)


bwHLA_final = bwHLA %>%
  dplyr::left_join(subjCompareHLA[,c('SUBJECT', 'SUBJECT_fix')], by = 'SUBJECT')
