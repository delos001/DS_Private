
# TITLE: 
# STUDY: 
# AUTHOR: 
# DATE: 

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

path = 'C:\\path\\path1\\'
file1 = 'filename.ext'
file2 = 'filename.ext'
file3 = 'filename.ext'


## read in files




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

## set anchor date variables and format them
## fpi = first patient in, lpo = last patient out, tdy = today
fpiDate = as.Date('2017-Jan-01', format = '%Y-%b-%d')
lpoDate = as.Date('2022-Dec-31', format = '%Y-%b-%d')
tdyDate = as.Date(Sys.time(), format = '%Y-%b-%d')


## create sequence from fpi to lpo by day, month, year
fpilpoSeqDd = seq.Date(from = fpiDate, to = lpoDate, by = 'day')

## create sequence from fpi to todays date
fpitdySeqDd = seq.Date(from = fpiDate, to = tdyDate, by = 'day')

## get durations between fpi and lpo
fpilpoDurDd = lubridate::time_length(lubridate::interval(fpiDate, 
                                                         lpoDate), 
                                     unit = 'days')

## get durations between fpi and today
fpitdyoDurDd = lubridate::time_length(lubridate::interval(fpiDate, 
                                                          tdyDate), 
                                      unit = 'days')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## Calculate weeks:
##    wk_FirstFullMon: starts number on first full week starting with Monday
##    wk_FirstFullSun: starts number on first full week starting with Sunday
##    wk_JanOne: starts numbering using week in which Jan01 occurs

daySkeleton = data.frame(fpilpoSeqDd) %>%
  dplyr::mutate(
    YearDay = lubridate::yday(fpilpoSeqDd),
    Wk_JanOne = lubridate::week(fpilpoSeqDd),
    Wk_FirstFullMon = lubridate::isoweek(fpilpoSeqDd),
    Wk_Start_Mon = lubridate::floor_date(fpilpoSeqDd, week_start = 7),
    Wk_End_Mon = as.Date(lubridate::ceiling_date(fpilpoSeqDd, week_start = 7)),
    Wk_FirstFullSun = lubridate::epiweek(fpilpoSeqDd),
    Wk_Start_Sun = lubridate::floor_date(fpilpoSeqDd, week_start = -1),
    Wk_End_Sun = as.Date(lubridate::ceiling_date(fpilpoSeqDd, week_start = -1)))
                
                
                
                
                

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

