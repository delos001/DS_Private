
# TITLE: Covance All Labs No Results
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 15Oct2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
# 
#
## outputs:


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr')

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

mywd = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'

path1 = 'BMRN270\\270-301\\Covance_Raw\\'

file1 = 'bmn_270301_covance_lab_12oct20.sas7bdat'
file2 = 'BMN 270-301_bloodworks_mutation_11Feb2020.csv'
file3 = 'bmn_270301_lb_blnd_fviii_12oct20.sas7bdat'

covLab_raw = read_sas(data_file = paste(mywd, path1, file1, sep = ""),
                      .name_repair = 'check_unique')

fviiiGeno_raw = read.csv(file.path(paste(mywd, path1, sep = ''), file2), 
                         sep = ",", skip = 2, header = TRUE,
                         na.strings = c("", " ", "NA"))

blnd_fviii_raw = read_sas(data_file = paste(mywd, path1, file3, sep = ""),
                          .name_repair = 'check_unique')

## this is file without results
#CovHead = read_sas(data_file = paste(mywd, path1, file4, sep = ""),
#                   .name_repair = 'check_unique')

## write file to csv for qc if needed
#write.csv(covLab_raw, file.path(outloc, 'covLab_raw.csv', row.names = FALSE))
#write.csv(fviiiGeno_raw, file.path(outloc, 'fviiiGeno_raw.csv', row.names = FALSE))
#write.csv(blnd_fviii_raw, file.path(outloc, 'blnd_fviii_raw.csv', row.names = FALSE))

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

## fviiiGeno_raw doesn't contain LBSPID so it is hard coded from DTS specs here
fviiiGeno_LBSPID = data.frame(LBTEST = c('FVIII Genotyping', 
                                         'FVIII Genotyping Inversion Intron 22',
                                         'FVIII Genotyping HGVS Transcript', 
                                         'FVIII Genotyping HGVS Protein'),
                              CovanceName = c('Inversion - Intron 1', 
                                              'Inversion - Intron 22', 
                                              'HGVS Transcript', 
                                              'HGVS Protein'),
                              LBSPID = c(760, 871, 872, 873))

## define columns contains results that MUST BE REMOVED for blinded data
resCols = c('LBORRES', 'LBSTRESC', 'LBSTRESN', 'LBSIRES')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


## tag each df with its file source
covLab = covLab_raw %>%
  dplyr::mutate(FileSource = 'Cov_ub')

blnd_fviii = blnd_fviii_raw %>%
  dplyr::mutate(FileSource = 'FVII_bl')

## tag fviiiGeno_raw with project name
fviiiGeno = fviiiGeno_raw %>%
  dplyr::mutate(PROJECT = '270301',
                FileSource = 'FVIIIGeno')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## covLab and blnd_fviii are standard format so they are binded here
## bind dataframes together
covBind = covLab %>%
  dplyr::bind_rows(blnd_fviii) %>%
  dplyr::mutate('Results_Present' = ifelse(LBORRES == "" | LBSTAT == "NOT DONE",
                                           'No', 'Yes'))


## fviiiGeno------------------------
## blookwords file CSV: doesn't align with SAS files (CovLab and blnd_fviii)
fviiiGeno = fviiiGeno %>%
  ## fviiiGeno is not raw and has blank rows that need to be deleted
  subset(!is.na(Sample.ID)) %>%
  
  ## columns don't align with DVS so must be renamed
  dplyr::rename('FVIII Genotyping' = contains('Intron') & contains('1'),
                'FVIII Genotyping Inversion Intron 22' = contains('Intron') & contains('22'),
                'FVIII Genotyping HGVS Transcript' = contains('HGVS') & contains('Trans'),
                'FVIII Genotyping HGVS Protein' = contains('HGVS') & contains('Protein')
  ) %>%
  
  ## wide to long so each subject-test is on its own row
  tidyr::pivot_longer(c('FVIII Genotyping',
                        'FVIII Genotyping Inversion Intron 22',
                        'FVIII Genotyping HGVS Transcript',
                        'FVIII Genotyping HGVS Protein'), 
                      names_to = 'LBTEST', values_to = 'LBORRES') %>%
  
  ## join LBSPID created above in global variable section
  dplyr::left_join(fviiiGeno_LBSPID,
                   by = 'LBTEST') %>%
  dplyr::mutate(LBSPID = as.character(LBSPID)) %>%
  
  ## select and rename columns to match covLab and blnd_fviii
  dplyr::select('PROJECT',
                'SUBJECT' = 'Sample.ID', 
                'FOLDERNAME' = 'Visit', 
                'LBDAT' = 'Date.of.Collection',
                'LBREFID' = 'Accn..',
                'LBSPID',
                'LBTEST',
                'LBORRES',
                'LBSTAT' = 'X',
                'FileSource',
                'COMMENTS' = 'Comment') %>%
  dplyr::mutate('Results_Present' = ifelse(LBSTAT == 'Tested', 'Yes', 'No'),
                LBDAT = as.Date(LBDAT, format = '%d-%b-%y'))

## incorrect subject IDs appear to be present in bloodworks data.
## first get list of incorrect subjects and respective LBREFIDs
subjCompare = fviiiGeno %>% dplyr::select(SUBJECT, LBREFID) %>% unique() %>%
  dplyr::anti_join(svAnchorData[, c('Subject')], by = c('SUBJECT' = 'Subject')) %>%
  dplyr::mutate(Source = 'Blood Works') %>%
  
  ## then look up subject number in Covance data set to get 'correct' subject ID
  dplyr::left_join(covBind[, c('SUBJECT', 'LBREFID')], by = c('LBREFID')) %>%
  dplyr::rename(SUBJECT = SUBJECT.x,
                SUBJECT_fix = SUBJECT.y) %>%
  dplyr::distinct(SUBJECT, SUBJECT_fix)

## join fix column to fviiiGeno
fviiiGeno = fviiiGeno %>%
  dplyr::left_join(subjCompare[,c('SUBJECT', 'SUBJECT_fix')], by = 'SUBJECT')



##---------------------------------
## PAY ATTENTION: blinded vs unblinded data

## bind dataframes together and remove results columns to blind data
## *****CONTAINS RESULTS*********
covAll_Results = covBind %>%
  dplyr::bind_rows(fviiiGeno)

## does NOT contain results
covAll_NoResults = covBind %>%
  dplyr::bind_rows(fviiiGeno) %>%
  dplyr::select(-one_of(resCols))


#write.csv(covAll_Results, file.path(outloc, 'covAll_Results.csv', row.names = FALSE))
#write.csv(covAll_NoResults, file.path(outloc, 'covAll_NoResults.csv', row.names = FALSE))



