# TITLE: Covance Top Line Metrics
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 15Oct2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#  this scrip uses covAll_NoResults file to identify missing labs specified 
#      in the CentralLabTopLine Metrics specs

## outputs: covTLM shows only the missing labs for subjects and specified visits


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS

## NOTE: should not contain results. uses 270-301_CovanceAll_noResults file only
source('270-301_svCompliance.R')
source('270-301_Covance_BIND_ALL.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

## output path save location
outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\CentralLabTopLine\\'

## list to select order and columns of interest for final data frame
covCols = c('PROJECT', 'SUBJECT', 'FOLDERNAME', 'LBDAT', 'LBREFID', 'LBTESTCD', 
            'LBSPID', 'LBTEST', 'LBCAT', 'LBSCAT', 'FileSource', 
            'Results_Present')

## Topline Covance Metrics specifications: analytes and LBSPID's of interest
tlmSpecs = list(CPK = 144,
                ALT = 103,
                von_Willebrand_factor_levels = 745,
                ## **NOTE: FVIII Act LBSIDS 899, 912, 913, 914 = back up' and 
                ##      'comment' samples so are excluded from list here
                FVIII_Activity = c(623, 625, 898, 900),
                FVIII_Genotype = c(760, 871, 872, 873),
                HLA_Genotype = c(805, 806, 807, 808, 809, 810, 868, 869, 870, 
                                 878, 879, 880, 881, 882, 883, 9991, 9992, 9993))

## create dataframe from tlmSpecs list of lists
tlmSpecsdf = purrr::map2_df(tlmSpecs, names(tlmSpecs), function(x, y){
  data.frame(
    Covance_Lab_Analyte = y, 
    LBSPID = as.character(x))
  })  


## list for sample-visit day specs
tlmSpecs2 = list(CPK = c('Screening & Baseline'),
                 ALT = c('Screening & Baseline'),
                 von_Willebrand_factor_levels = ('Baseline'),
                 FVIII_Activity = c('Baseline'),
                 FVIII_Genotype = c('Screening'),
                 HLA_Genotype = c('Screening')
                 )

## create dataframe from tlmSpecs list of lists
tlmSpecsdf2 = purrr::map2_df(tlmSpecs2, names(tlmSpecs), function(x, y){
  data.frame(
    Covance_Lab_Analyte = y, 
    SpecFolder = as.character(x))
})  
                 

## visit filter for Covance top line metrics: visits of interest
visitFilter = c('Screening', 'Baseline', 'Smart Re-Screening')

## Lab-visit combinations needed per specs (used to filter correct observations)
specList = c('ALT Baseline', 'ALT Screening',
             'CPK Baseline', 'CPK Screening', 
             'von_Willebrand_factor_levels Baseline',
             'FVIII_Activity Baseline', 
             'FVIII_Genotype Screening',
             'HLA_Genotype Screening')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE SECONDARY TABLES

## filter '270-301_Covance_BIND_ALL.R' output to spec
covFilter = LabBindAll_NoResults %>%
  ## rename subject column to use fix subject column with correct subID
  dplyr::rename(SUBJECT_orig = SUBJECT) %>%
  dplyr::mutate(SUBJECT = ifelse(is.na(SUBJECT_fix), SUBJECT_orig, 
                                 SUBJECT_fix)) %>%
  dplyr::filter(FOLDERNAME %in% visitFilter) %>%
  ## to deal with >1 sample on same date that has been cx, results = yes & no
  dplyr::filter(Results_Present != 'No') %>%
  
  ## add LBSPID and lab analyte grouping (note acts as filter for LBSPID)
  dplyr::inner_join(tlmSpecsdf, by = 'LBSPID') %>%
  dplyr::arrange(SUBJECT, LBDAT, LBSPID)

## create skeleton with unique visit-labTest pair
labSkeleton = covFilter %>%
  dplyr::select(LBSPID, LBTEST) %>%
  dplyr::distinct(LBSPID, LBTEST) %>%
  
  ## some test have diff codes based on freq but are same test.  fix here
  dplyr::arrange(LBTEST) %>%
  dplyr::mutate(LBTEST_eqv = 
                  ifelse(LBSPID == 898, 'Factor VIII Activity One Stage',
                    ifelse(LBSPID == 900, 'Factor VIII Activity Chromogenic',
                      LBTEST))) %>%
  dplyr::mutate(id = 1)  ## id used as join column later



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------

## from '270-301_svCompliance.R': filter for visits, per spec
covTLM = svCompliance %>%
  dplyr::filter(FolderName %in% visitFilter) %>%
  dplyr::mutate_at(vars(Baseline), na_if, NA) %>%
  
  ## Filter out visits that didn't occur:  
  dplyr::filter(Visit_Expected == 'Yes') %>%
  dplyr::filter(!is.na(Day_1)) %>%
  dplyr::select(Subject, FolderName, OID, FolderSeq, 
                Screening, Smart_Re_Screening, SCRNFAIL_COD,
                Baseline, Day_1, Date_of_Visit) %>%
  dplyr::mutate(id = 1) %>%

  ## join covBindFilter to StudyVisits.R output file
  dplyr::left_join(labSkeleton, by = 'id') %>% 
  dplyr::left_join(covFilter[,c('SUBJECT', 'FOLDERNAME','LBDAT', 'LBSPID', 
                                'LBTEST', 'Results_Present')], 
                   by = c('Subject' = 'SUBJECT',
                          'FolderName' = 'FOLDERNAME',
                          'LBSPID',
                          'LBTEST')) %>%
  
  ## NAs as result of join mean sample not present in covance raw data
  dplyr::mutate(Results_Present = ifelse(is.na(Results_Present), 'No', 
                                         Results_Present)) %>%
  
  ## Protocl Section 12.2: if scr occur within 30 days of the drug infusion,  
  ##    PE, VS, Chem, LTS, Hem, Urine, coag don't need to be repeated at BL
  dplyr::mutate(Scr_D1_Gap = ifelse(is.na(Day_1), 0, 
                              ifelse(!is.na(Smart_Re_Screening), 
                                     difftime(Day_1, Smart_Re_Screening, 
                                              units = 'days'), 
                                     difftime(Day_1, Screening, 
                                              units = 'days')))) %>%
  
  ## join lab analyte group names and paste folder-analyte name for filtering
  dplyr::left_join(tlmSpecsdf, by = 'LBSPID') %>%
  
  ## create new foldername column to combine Scrn and SmartRescreen
  dplyr::mutate(FolderName2 = ifelse(FolderName == 'Smart Re-Screening',
                                     'Screening', FolderName)) %>%
  
  ## create new variable that will be used for filtering
  dplyr::mutate('FolderAnalyte' = paste(Covance_Lab_Analyte, FolderName2, 
                                        sep = ' ')) %>%

  ## group so can later count when >1 line has same visit and test
  dplyr::group_by(Subject, FolderName2, LBTEST_eqv) %>%
  
  ## because of grouping counts when there are duplicate visit-lbtest lines
  ## then count yes's when >1 line has same visit and test
  dplyr::mutate(YesCnt = sum(Results_Present == 'Yes')) %>%  
  dplyr::arrange(Subject, LBTEST_eqv, Date_of_Visit) %>%
  dplyr::ungroup() %>%
  
  ## create group to use lag
  dplyr::group_by(Subject, LBTEST) %>%
  ## get the group results from screening visit on the BL
  dplyr::mutate(YesCnt_lag = lag(YesCnt)
                ) %>%
  dplyr::ungroup() %>%
  
  ## spec rules for test-visit combination requirements
  dplyr::mutate(
    SpecFilter = ifelse((FolderAnalyte == 'ALT Baseline' |
                           FolderAnalyte == 'CPK Baseline' |
                           FolderAnalyte == 'FVIII_Activity Baseline') &
                          Scr_D1_Gap < 30 &
                          ((YesCnt > 0 |
                              YesCnt_lag > 0)),
                        'Del',
                        ifelse(FolderAnalyte %in% specList &
                                 YesCnt < 1,
                               'Keep', 'Del')))

covTLM_filter = covTLM %>%
  dplyr::filter(SpecFilter == "Keep") %>%
  dplyr::filter(OID != 'SCRS')

covTLM_colapse = covTLM_filter %>%
  dplyr::select(FolderAnalyte, Subject) %>%
  unique() %>%
  dplyr::group_by(FolderAnalyte) %>%
  dplyr::summarise(Subject2 = paste0(Subject, collapse = ', ')) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Subject_Count = stringr::str_count(Subject2, ',') + 1) %>%
  dplyr::rename('Covance Lab Analyte' = FolderAnalyte,
                'Subjects with Missing Data' = Subject2)
  


#write.csv(covTLM_final, 
#          file.path(outloc, '270301_CentralLabTopLine.csv'), row.names = FALSE)
