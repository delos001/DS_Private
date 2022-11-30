
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

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'readxl')

# loop through required packages & if not already installed, load, then install
for (pkg in lpkgs ) {
  
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

## Define paths and file names------------------------




##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES




## Checks

## 1----------------------------------------------------
## Missing Codes in Current SOW Analyte List
##   filter SOWAnalytes table for is.na(TestCode)


## 2---------------------------------------------------
## Check for discrepancy between Covance visit names and protocol
##   map/join VTSHeader with spec file containing the Covance-EDC visit name map
##   evaluate use of DTSVisitMapping table or if new process is needed


## 3---------------------------------------------------
## Compare SOW list of test to those in protocol
##   might need to be manual check: tests aren't mapped to protocol test names
##   Use VTSTests legend as input to review


## 4---------------------------------------------------
## Check testing type in Legend
##   might have to be manual review.  Need to ID SME to do this
##   Use VTSLegend table as input to review



## 5---------------------------------------------------
## Check project codes are assigned to correct country/region
##   need to define mapping stategy to map project codes to region
##   use VTSLegendProjects as input to review


