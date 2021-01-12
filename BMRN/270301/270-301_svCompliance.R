
# TITLE: Study Visit Compliance
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 24Sep2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
#
# This scripts gives visit information for all subjects and all visits and joins
#     data from 270-301_KeyVisitDates: scr, rescr, bl, D1, last visit, etc
# 
## outputs:
# svPrim: study visit completion and window compliance


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
source('270-301_svSkeleton.R')

##------------------------------------------------------------------------------
## LOAD DATA


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING
# none


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE TABLES

svCompliance = svSkeleton %>%
  
  ## join anchor data to calculate whether visit is expected, late, etc
  dplyr::left_join(svAnchorData[, c('SUBJECT', 'Screening', 'SCRNFAIL_COD', 
                                    'Smart_Re_Screening', 
                                    'Baseline', 'Day_1', 
                                    'Study_Exit_Date')], 
                   by = 'SUBJECT') %>%
  
  ## derive max date to compare study visit dates (max = exit date or today)
  dplyr::mutate(Study_Exit_or_Today = 
                  as.Date(ifelse(is.na(Study_Exit_Date), 
                                 Sys.Date(), 
                                 as.Date(Study_Exit_Date,
                                         origin = "1970-01-01")),
                          origin = "1970-01-01")) %>%
  
  ## calculate lower, target, upper window DATES
  dplyr::mutate(
    ## calculate lower window date
    LWindow_Date = 
      as.Date(
        ## if SF or no D1 yet, use actual date as target date
        ifelse(OID == 'SCR' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
               as.Date(Screening),
          ## if D1 present, scr date is based on D1 and Lwindow
          ifelse(OID == 'SCR' & !is.na(Day_1), as.Date(Day_1) + LWindow,
            ## if smrt rscr is na, put na
            ifelse(OID == 'SCRS' & is.na(Smart_Re_Screening), NA,
              ## if smrt rscr !na but is sf or no day1, use smrt rscr date
              ifelse(OID == 'SCRS' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
                     as.Date(Smart_Re_Screening),
                ifelse(OID == 'SCRS' & !is.na(Day_1), as.Date(Day_1) + LWindow,
                  ## if BL is na and its SF, put na
                  ifelse(OID == 'BASE' & is.na(Baseline) & SCRNFAIL_COD == 'Y',
                         NA,
                    ## if BL or D1 is na, use screening plus 1 day
                    ifelse(OID == 'BASE' & (is.na(Baseline) | is.na(Day_1)),
                           pmax(as.Date(Screening),
                                as.Date(Smart_Re_Screening), na.rm = TRUE) + 1, ## hard coded
                      ## all other cases, BL is based on D1 and window
                      ifelse(OID == 'BASE', as.Date(Day_1) + LWindow,
                        ## if D1 is na but is not SF, use screening plus 1 day
                        ifelse(OID == 'D1' & is.na(Day_1) & SCRNFAIL_COD != 'Y', 
                               as.Date(Screening) + 1,  ## hard coded
                          ## exclude non-rqd visits & SF, then D1+target+window
                          ifelse(is.na(Target) | SCRNFAIL_COD == 'Y', NA,
                               as.Date(Day_1) + Target + LWindow)))))))))),
        origin = "1970-01-01"),
    
    ## target date  (same logic as above).  Pre D1 visit target will be LWindow
    Target_Date = 
      as.Date(
        ifelse(OID == 'SCR' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
               as.Date(Screening),
          ifelse(OID == 'SCR' & !is.na(Day_1), as.Date(Day_1) + LWindow,
            ifelse(OID == 'SCRS' & is.na(Smart_Re_Screening), NA,
              ifelse(OID == 'SCRS' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
                     as.Date(Smart_Re_Screening),
                ifelse(OID == 'SCRS' & !is.na(Day_1), as.Date(Day_1) + LWindow,
                  ifelse(OID == 'BASE' & is.na(Baseline) & SCRNFAIL_COD == 'Y',
                         NA,
                    ifelse(OID == 'BASE' & (is.na(Baseline) | is.na(Day_1)), 
                           pmax(as.Date(Screening), 
                                as.Date(Smart_Re_Screening), na.rm = TRUE) + 1,
                      ifelse(OID == 'BASE', as.Date(Day_1) + LWindow,
                        ifelse(OID == 'D1' & is.na(Day_1) & SCRNFAIL_COD != 'Y',
                               as.Date(Screening) + 1, 
                          ifelse(is.na(Target) | SCRNFAIL_COD == 'Y', NA,
                                 as.Date(Day_1) + Target)))))))))),
        origin = "1970-01-01"),
    
    ## calculate upper window dates (same logic as above)
    UWindow_Date = 
      as.Date(
        ifelse(OID == 'SCR' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
               as.Date(Screening),
          ifelse(OID == 'SCR' & !is.na(Day_1), as.Date(Day_1) + UWindow,
            ifelse(OID == 'SCRS' & is.na(Smart_Re_Screening), NA,
              ifelse(OID == 'SCRS' & (SCRNFAIL_COD == 'Y' | is.na(Day_1)), 
                     as.Date(Smart_Re_Screening),
                ifelse(OID == 'SCRS' & !is.na(Day_1), as.Date(Day_1) + UWindow,
                  ifelse(OID == 'BASE' & is.na(Baseline) & SCRNFAIL_COD == 'Y',
                         NA,
                    ifelse(OID == 'BASE' & (is.na(Baseline) | is.na(Day_1)), 
                           pmax(as.Date(Screening),
                                as.Date(Smart_Re_Screening), na.rm = TRUE) + 41,
                      ifelse(OID == 'BASE', as.Date(Day_1) + UWindow,
                        ifelse(OID == 'D1' & is.na(Day_1) & SCRNFAIL_COD != 'Y', 
                               as.Date(Screening) + 42, 
                          ifelse(is.na(Target) | SCRNFAIL_COD == 'Y', NA,
                                 as.Date(Day_1) + Target + UWindow)))))))))),
        origin = "1970-01-01"),
  ) %>%
  
  ## data entry window allows additional days for data entry
  dplyr::mutate(
    Over_DataEntry_Date = 
      as.Date(ifelse(OID == 'SCR', as.Date(Screening) + Over,
                     ifelse(OID == 'SCRS' & !is.na(Smart_Re_Screening), 
                            as.Date(Smart_Re_Screening) + Over,
                            ifelse(OID == 'BASE' & !is.na(Baseline),
                                   as.Date(Baseline) + Over, 
                                   UWindow_Date + Over))),
              origin = '1970-01-01')
  ) %>%
  

  ## join sv data to get actual visit date data
  dplyr::left_join(svRaw[, c('SUBJECT',
                             'INSTANCENAME', 'RECORDID', 'FOLDER', 'FOLDERSEQ',
                             'SVSTDAT')],
                   by = c('SUBJECT', 'OID' = 'FOLDER')) %>%
  
  ## compare expected to actual visit dates
  dplyr::mutate(
    SVSTDAT_date = as.Date(SVSTDAT, format = '%d-%b-%y'),
    Days_Since_D1 = difftime(SVSTDAT_date, Day_1, units = 'days'),
    Days_Off_Target = ifelse(OID == 'SCRS' & is.na(Smart_Re_Screening), NA,
                        ifelse(OID == 'SCR' | OID == 'SCRS' | OID == 'BASE', 
                               as.numeric(LWindow - Days_Since_D1),
                               as.numeric(Days_Since_D1) - Target)),
    Out_of_Window = ifelse(Days_Off_Target >= LWindow & 
                             Days_Off_Target <= UWindow, 'Ok', 'OOW')
  ) %>%
    
  ## if date of visit or data entry is before study exit date or today
  dplyr::mutate(
    Visit_Expected = ifelse(!is.na(SVSTDAT), 'Yes',
                       ifelse(!is.na(UWindow_Date) &
                                UWindow_Date <= Study_Exit_or_Today, 
                              'Yes', 'No')),
    Data_Entry_Expected = ifelse(!is.na(SVSTDAT), 'Yes',
                            ifelse(!is.na(UWindow_Date) & 
                                     Over_DataEntry_Date <= Study_Exit_or_Today, 
                                     'Yes', 'No')),
    Visit_Missing = ifelse(Data_Entry_Expected == 'Yes' & is.na(SVSTDAT_date), 
                           'Yes', 'No')
  ) %>%
  
  ## select columns to keep in output
  dplyr::select(SUBJECT, 
                Parent.Folder, FOLDERSEQ, 
                FolderName = FOLDERNAME, 
                OID, INSTANCENAME, RECORDID,
                Screening, Smart_Re_Screening, SCRNFAIL_COD,
                Baseline, Day_1, Study_Exit_or_Today,
                Date_of_Visit = SVSTDAT_date,
                Days_Since_D1,
                LWindow, LWindow_Date, 
                Target, Target_Date,
                UWindow, UWindow_Date, 
                Days_Off_Target, Out_of_Window,
                Over, Over_DataEntry_Date,
                Visit_Expected, Data_Entry_Expected,
                Visit_Missing)


## write to local if needed
# write.csv(svCompliance,
#           file.path('C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\',
#                                   'svCompliance.csv'), row.names = FALSE)


