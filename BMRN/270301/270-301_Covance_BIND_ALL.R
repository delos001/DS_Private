
# TITLE: Covance All Labs No Results
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 15Oct2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#   This script combines all external lab files together. It runs each source
#       specified in the LOAD PARENT SCRIPTS section (update as needed)


#  IMPORTANT: Using source('270-301_Covance_bloodworks_HLA.R') will create 
#     duplicate results for HLA Genotyping sub-category.  It was added when
#     an error in DTS mapping was found causing the HLA Genotyping resuls in
#     the ACE files to be incorrect.  
#     Therefore, bwHLA_final from source('270-301_Covance_bloodworks_HLA.R') 
#        uses a raw data set and adds correct results but the HLA Geotpying 
#        must then be removed from the ACE file so there are no duplicates
#
#     If DTS is corrected and the ACE files are subsequently corrected, you 
#        can comment out the code that filters out HLA Genotyping of 
#        Covance_ACE_datasets BUT YOU MUST also remove 
#        source('270-301_Covance_bloodworks_HLA.R')
#
#
## outputs:  Two possible outputs depending on whether you want want results
##               or no results due to blinding needs
##          1) LabBindAll_Results
##          2)LabBindAll_NoResults


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PARENT SCRIPTS
source('270-301_Covance_ACE_datasets.R')
source('270-301_Covance_bloodworks_mutation.R')
source('270-301_Covance_bloodworks_HLA.R')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

## define columns contains results that MUST BE REMOVED for blinded data
resCols = c('LBORRES', 'LBSTRESC', 'LBSTRESN', 'LBSIRES')

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## Exclude HLA Genotyping subcategory from ACE data sets (see instructions)
Covance_ACE_final = Covance_ACE_final %>%
  dplyr::filter(!LBSPID %in% c(805, 806, 807, 808, 809, 810, 868, 869, 870, 
                              878, 879, 880, 881, 882, 883))


##---------------------------------
## PAY ATTENTION: blinded vs unblinded data


## *****CONTAINS RESULTS*********
LabBindAll_Results = Covance_ACE_final %>%
  dplyr::bind_rows(bwMutation_final,
                   bwHLA_final)

## does NOT contain results
LabBindAll_NoResults = Covance_ACE_final %>%
  dplyr::bind_rows(bwMutation_final,
                   bwHLA_final) %>%
  dplyr::select(-one_of(resCols))


## Used only if writing to local is needed-------------------------------------

# outloc = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\CentralLabTopLine\\'
# write.csv(LabBindAll_NoResults, file.path(outloc, 'LabBindAll_Results.csv', 
#                                     row.names = FALSE))
# 
# write.csv(LabBindAll_NoResults, file.path(outloc, 'LabBindAll_NoResults.csv', 
#                                       row.names = FALSE))



