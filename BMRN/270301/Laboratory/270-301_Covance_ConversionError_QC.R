


# TITLE: 270-301 Covance SAS datasets
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 12Nov2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#    since the SAS files from ACE environment are the same format, all can be
#       manipulated here in one file
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
source('270-301_svCompliance.R')



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## Ace files------

ACEcdmwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'
ACE270301stagehold = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\stagecdm\\stagedata\\stagehold\\covance\\'

ACEdec08 = 'bmn_270301_lb_blnd_fviii_08dec20.sas7bdat'
ACEdec18 = 'bmn_270301_lb_blnd_fviii_19dec20.sas7bdat'


## read in files
dec08raw = read_sas(data_file = paste(ACEcdmwd, ACE270301stagehold, 
                                      ACEdec08, sep = ""),
                    .name_repair = 'check_unique')

dec18raw = read_sas(data_file = paste(ACEcdmwd, ACE270301stagehold, 
                                      ACEdec18, sep = ""),
                    .name_repair = 'check_unique')



##  All blinded test lbspid's:  for reference only
fviiiLBPSID = c('623', '625', '898', '899', '913', '900', '912', '914', '901', 
                '902', '1209', '1302', '1210', '864')

lbtestcd_fviiiActivity = c('F8ASE', 'F8ACE')



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## Create the file for 08Dec2020 Data
dec08 = dec08raw %>%
  dplyr::filter(LBTESTCD %in% lbtestcd_fviiiActivity) %>%
  dplyr::select(PROJECT, SUBJECT, FOLDERNAME, LBDAT, LBTIM, LBREFID, LBTESTCD,
                LBSPID, LBTEST, LBSIRES, LBSIRESU, LBORRES, LBORRESU, LBRESDAT) %>%
  
  dplyr::mutate(LBSIRES_num = 
                  ifelse(LBSIRES == '', '', 
                    as.numeric(ifelse(LBSIRES == 'No valid result obtained', '',
                                      str_remove(LBSIRES, '[<>]')))),
                Operator = ifelse(grepl('<', LBSIRES), '<', 
                             ifelse(grepl('>', LBSIRES), '>', '')),
                ResUnits = paste(LBSIRES, LBSIRESU, sep = " ")) %>%
  dplyr::rename(LBSIRES08 = LBSIRES,
                LBSIRESU08 = LBSIRESU,
                LBSIRES_num08 = LBSIRES_num,
                Operator08 = Operator,
                ResUnits08 = ResUnits,
                LBORRES08 = LBORRES, 
                LBORRESU08 = LBORRESU)


##------------------------------------------------------------------------------
## Create the file for 18Dec2020 Data
dec18 = dec18raw %>%
  dplyr::filter(LBTESTCD %in% lbtestcd_fviiiActivity) %>%
  dplyr::select(PROJECT, SUBJECT, FOLDERNAME, LBDAT, LBTIM, LBREFID, LBTESTCD,
                LBSPID, LBTEST, LBSIRES, LBSIRESU, LBORRES, LBORRESU, LBRESDAT) %>%
  
  #dplyr::filter(LBSIRES != 'No valid result obtained') %>%
  dplyr::mutate(LBSIRES_num = 
                  ifelse(LBSIRES == '', '', 
                    as.numeric(ifelse(LBSIRES == 'No valid result obtained', '',
                                      str_remove(LBSIRES, '[<>]')))),
                    Operator = ifelse(grepl('<', LBSIRES), '<', 
                                  ifelse(grepl('>', LBSIRES), '>', '')),
                ResUnits = paste(LBSIRES, LBSIRESU, sep = " ")) %>%
  
  dplyr::rename(LBSIRES18 = LBSIRES,
                LBSIRESU18 = LBSIRESU,
                LBSIRES_num18 = LBSIRES_num,
                Operator18 = Operator,
                ResUnits18 = ResUnits,
                LBORRES18 = LBORRES, 
                LBORRESU18 = LBORRESU)


##------------------------------------------------------------------------------
## Get last visit data on or before 16Nov2020

enDat = as.Date('16-Nov-20', format = '%d-%b-%y')

svComp_Nov = svCompliance %>%
  dplyr::filter(!is.na(Date_of_Visit))

svCompMax = svComp_Nov %>%
  dplyr::filter(Date_of_Visit <= enDat) %>%
  dplyr::select(SUBJECT, Date_of_Visit) %>%
  dplyr::group_by(SUBJECT) %>%
  dplyr::summarise(MaxVisitDate = max(Date_of_Visit)) %>%
  dplyr::left_join(svComp_Nov[, c('SUBJECT', 'Date_of_Visit', 'FolderName', 
                                  'Visit_Expected', 'FOLDERSEQ')],
                   by = c('SUBJECT', 'MaxVisitDate' = 'Date_of_Visit')) %>%
  dplyr::group_by(SUBJECT, MaxVisitDate) %>%
  dplyr::mutate(id = seq(n())) %>%
  dplyr::arrange(SUBJECT, MaxVisitDate, FOLDERSEQ) %>%
  dplyr::filter(id == 1)

##------------------------------------------------------------------------------
## Identify subjects in new file that aren't in Dec08 file
decAnti = dec18 %>%
  dplyr::anti_join(dec08[, c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 
                             'LBREFID', 'LBSPID',
                             'LBSIRES08', 'LBSIRESU08', "LBSIRES_num08",
                             'Operator08', 'ResUnits08')],
                   by = c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 'LBREFID',                    
                          'LBSPID')) %>%
  dplyr::mutate('NewData' = 'Yes') %>%
  
  ## join the last visit date prior to Nov16, 2020
  dplyr::left_join(svCompMax[, c('SUBJECT', 'MaxVisitDate')], 
                   by = 'SUBJECT') %>%
  
  ## calculate if the lab will be in topline
  dplyr::mutate(TL_Included = ifelse(LBDAT < MaxVisitDate, "Yes", "No"))



##------------------------------------------------------------------------------
## Join Files to compare results and units
decComp = dec18 %>%
  dplyr::left_join(dec08[, c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 
                             'LBREFID', 'LBSPID',
                             'LBSIRES08', 'LBSIRESU08', "LBSIRES_num08",
                             'Operator08', 'ResUnits08',
                             'LBORRES08', 'LBORRESU08')],
                   by = c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 'LBREFID',                    
                          'LBSPID')) %>%
  dplyr::mutate(LBSIRES_num18 = as.numeric(LBSIRES_num18),
                LBSIRES_num08 = as.numeric(LBSIRES_num08)) %>%
  dplyr::mutate(Results_Ratio = round(as.numeric(LBSIRES_num08)/as.numeric(LBSIRES_num18), 4)) %>%
  dplyr::mutate(Filter = ifelse(LBSIRESU18 == 'IU/mL', 'Incorrect units of IU/mL',
                           ifelse(LBSIRES18 != LBSIRES08 & 
                                    LBSIRESU18 == LBSIRESU08, 
                                  'Units match but results do not', 
                            ifelse(LBSIRES18 == LBSIRES08 & 
                                     LBSIRESU18 != LBSIRESU08, 
                                   'Results match with different units',
                             ifelse(LBSIRES18 == LBSIRES08 & 
                                      LBSIRESU18 == LBSIRESU08, 
                                    'Both results and units match',
                             'Other'))))) %>%
  
  dplyr::left_join(decAnti[, c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 
                               'LBREFID', 'LBSPID', 'NewData', 'MaxVisitDate',
                               'TL_Included')],
                   by = c('SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBTIM', 
                          'LBREFID', 'LBSPID')) %>%
                                
  dplyr::select(PROJECT, SUBJECT, FOLDERNAME, LBDAT, LBREFID, LBTESTCD, 
                LBSIRES08,  ## COMMENT OUT FOR BLINDED PPL
                LBSIRESU08,
                LBORRES08, ## COMMENT OUT FOR BLINDED PPL
                LBORRESU08,
                LBSIRES18,  ## COMMENT OUT FOR BLINDED PPL
                LBSIRESU18,
                LBORRES18, ## COMMENT OUT FOR BLINDED PPL
                LBORRESU18, 
                Results_Ratio, NewData, MaxVisitDate, TL_Included, Filter) %>%
  dplyr::filter(Filter != 'Both results and units match')







## Identify rows present in 18Dec that aren't in 08Dec



