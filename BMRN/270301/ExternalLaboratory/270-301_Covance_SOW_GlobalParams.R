
# TITLE: 270-301 Covance SOW Global Parameters
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: Feb2021

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------
## This script will produce a series of tables that are inputs to SOW and DTS
##   checks as well as DQRL check inputs
##
## INPUTS:
##   207-301_SOW_Data_and_Test_Codes
##      Current SOW Analyte Page: Tests present on current SOW (current only)
##      Current SOW VTS: Lab test study schedule
##      Current SOW S&R Management: Vendor-TestGroup pairing
##      MasterTestList: all tests ever performed on the study
##
## OUTPUTS:
##   TestsCodes_All
##   SOWAnalytes
##   VTSHeader
##   VTSTests
##   VTSLegend
##   DTSVisitMapping


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES

lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'readxl',
          'janitor')


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

# none

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD DATA

## BioMarin Lab Tests SDTM: \\bmvsa02a.bmrn.com\cdm\cdmapp\metadata 

## Define paths and file names------------------------
ghroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\GitLabRepo\\'
ghpath = 'BMRN_270-301\\SpecificationInputs\\'

SOWFileName = '270-301_SOW_Data_and_Test_Codes'
ext1 = '.xlsx'

SOWpath = paste0(ghroot, ghpath, SOWFileName, ext1)


## Read workbook---------------------------------------
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, 
              function(X) 
                janitor::clean_names(readxl::read_excel(filename, 
                                                        sheet = X), 
                                     'upper_camel'))
  if (!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
## execute function
SOWSheets = read_excel_allsheets(SOWpath)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## DEFINE GLOBAL VARIABLES

## Set page names
SOWAnalyte_raw = SOWSheets$`Current SOW Analyte Page`
SOWVTS_raw = SOWSheets$`Current SOW VTS`
SOWSR_raw = SOWSheets$`Current SOW S&R Management`
MasterTestList_raw = SOWSheets$`MasterTestList`
VisitNameMap_raw = SOWSheets$DTS_VisitNames

## Current SOW Analyte Page sheet:-----------
## there are some rows that aren't tests: create variable list to remove
SOWAnalyteExc = c('Reflex Group(s)', 'Calculation Group(s)', 'Services(s)', 
                  'Test/Kit Build Only', 'INTERNAL GROUPS')



## Current SOW VTS sheet:----------------------
## Headers:
##   This is multi-header page.  
##   Specify rows to split into primary and secondary header
VTSprimHeader_rows = 1
VTSsecHeader_rows = c(2:10)

## Define non-test rows
VTSadim_rows1 = c(148:166)  
VTSadim_rows2 = c(172:nrow(SOWVTS_raw))
VTSlegend_rows = c(175:184)
VTSlegendProjects_rows = c(185:186)

## Define Group sub-headers
VTSgrpnames = c('Group Name(s)', 'Reflex Group(s)', 'Calculation Group(s)',
             'Service(s)', 'Test/Kit Build Only', 'Internal Groups')

## Identify Divider Columns
## Empty columns are present and must be remvoved
##    defined empty when row2 is na
##    transpose rows 1 and 2 and filter out na
VTSempty_cols = rownames(data.frame(t(SOWVTS_raw[1:2,])) %>% 
                        dplyr::filter(is.na(X2)))



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INITIAL DATA FORMATTING


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE PRIMARY TABLES

## TESTCODES_ALL-----------------------------------
## All study tests and associated test codes
TestsCodes_All = MasterTestList_raw %>%
  dplyr::group_by(GroupDescription, TestDescription) %>%
  dplyr::summarize(TestCodes_Collapse = paste0(TestCode, collapse = ', ')) %>%
  dplyr::ungroup()


## SOW ANALYTES------------------------------------
SOWAnalytes = SOWAnalyte_raw %>%
  ## remove blank rows (those that are all na)
  dplyr::filter_all(any_vars(complete.cases(.))) %>%
  tidyr::fill(GroupS) %>%
  dplyr::filter(!is.na(TestAnalytes)) %>%
  dplyr::filter(!GroupS %in% SOWAnalyteExc) %>%
  dplyr::select(GroupS, TestAnalytes, TestCode)



## VTS Header--------------------------------------------]
## Input file has multiple headers.  These must be removed from the test table
##   so VTSHeader separates this into its own data frame that can be used later
VTSHeader = 
  setNames(  ## use values in column 1 as header
    ## [-1,] drops row one after renaming header with row1 values
    data.frame(
      t(SOWVTS_raw[c(VTSprimHeader_rows, VTSsecHeader_rows), ])), ## transpose
    ## specify column 1 of the header rows to rename headers; remove fist row
    SOWVTS_raw[c(VTSprimHeader_rows, VTSsecHeader_rows), 1])[-1,] %>%
  ## empty columns have been transposed to rows: remove these 
  dplyr::filter(!is.na(`VISIT NAME`)) %>%
  `rownames<-`(NULL) ## reset index col


##  VTS Test Group Table---------------------------------
VTSTests = 
  ##  Excl rows that don't contain test group names (keep Covance visit names)
  SOWVTS_raw[-c(VTSsecHeader_rows,  ## secondary header rows
                VTSadim_rows1,  ## dministraive rows
                VTSadim_rows2), ## legend
             ## exclude empty columns
             !names(SOWVTS_raw) %in% VTSempty_cols] %>%
  ## Excl rows that are sub-groups
  dplyr::filter(!X1 %in% VTSgrpnames) %>%
  dplyr::filter(!is.na(X1)) %>%

  ## Row1 contains Covance visit names.  Replace header with Row1
  janitor::row_to_names(., row_number = 1) %>%
  
  dplyr::rename('GroupProjects' = `VISIT NAME`) %>%
  
  ## some tests have subscripts to identify region:
  dplyr::mutate(
    ## replace superscript to align test name with analyte group
    GroupS = ifelse(grepl('^FVI', GroupProjects), 
                   gsub('[12]$', '', GroupProjects),
                   GroupProjects),
    ## get superscript to identify region from legend
    ProjectCode = str_extract(GroupProjects, '[12]$')) %>%
  dplyr::select(GroupProjects, GroupS, ProjectCode, everything())


## VTS LEGEND------------------------------------------
## Create legend df to define symbols for tests in VTS
VTSLegend = setNames(data.frame(SOWVTS_raw[VTSlegend_rows, 1]), 'All') %>%
  ## split column into code and code description
  tidyr::separate(All, c('CollectionCodeFull', 'Description'), ' : ') %>%
  ## in VTS, percent varies so it not useful here: remove
  dplyr::mutate(CollectionCode = gsub('\\(%\\)', '', CollectionCodeFull),
                Description = gsub('\\(.*\\)', '', Description))


## VTS LEGEND REGIONS-----------------------------------
## Some group names have superscript to specify region
VTSLegendProjects = 
  setNames(data.frame(SOWVTS_raw[VTSlegendProjects_rows, 1]), 'All') %>%
  ## split by period, space, 'Project' or 'Projects', space
  tidyr::separate(All, c('ProjectCode', 'ProjectNumber'), '\\.\\sProjects*\\s')


## COVANCE - EDC VISIT NAME MAPPING--------------------
DTSVisitMapping = VisitNameMap_raw %>%
  dplyr::filter(!is.na(LabVisitNameLabvisit)) %>%
  tidyr::fill(CovanceVisitName)

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## CREATE FINAL TABLE

# none







