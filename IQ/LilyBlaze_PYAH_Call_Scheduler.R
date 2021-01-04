###############################################################################
###############################################################################
#
#
# TITLE: PYAH
#
# This script will convert data from wide to long format
# Date: Nov2020
#     Update: 
###############################################################################
###############################################################################
###############################################################################

#----------------------------------------------------------------------
# Load Packages
#----------------------------------------------------------------------

lpkgs = c('dplyr', 'data.table', 'readxl')

for(pkg in lpkgs){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

#----------------------------------------------------------------------
# Specify file parameters  ### Check These Parameters Before Running
#----------------------------------------------------------------------
#-------------------------------------------------------------------------------
# OPEN PARAMETERS---------------------------------------------------------------

rootpath = 'C:/Users/q713174/Desktop/LilyBlaze/'
trialpath = 'PYAH/ReceivedFiles/'

## Visit Data file 
vdName = "_Current_PYAH_Patient_Visit_Date"                                     #this is the visit data file name
vdExt = '.xlsx'                                                                 #this is the visit data extension
vdSheet = 'Patient Status Report_EXCEL'

## Zip Code file-------------------------------------------------------
##    shared with pyab study
ContactsName = "_Current_Site Contacts and Site Ops Assignments"                #this is the zip code data file name
ContactsExt = '.xlsx'                                                           #this is the zip code file extension
ContactsSheet = 'SC Assignments'                                                #this is the name of the sheet that has the zip code data


## Language file
LangName = '_Current_PYAH_Site_Language'
LangExt = '.xlsx'
LangSheet = 'Sheet1'


## Amendment file
amendName = "_Current_PYAH_Amendment"                                           
amendExt = '.xlsx'                                                              
amendSheet = 'Sheet1'                                                           


#SAVE PARAMETERS-------------------------------------------------------
## Final output save parameters----------------------------------------
trialsave <- 'PYAH/FixedVisitDateFiles/'
saveName <- "e-CTS_PYAH_Patient_Visit_Date_fixed"                          #this is what you want to name the file after running
saveExt <- '.csv'                                                               #this is what file type you want to save (csv is best practice)



#----------------------------------------------------------------------
# Read Files Into R Environment
#----------------------------------------------------------------------
## read visit data file---------------------------------
VisitDate_df = read_excel(paste(rootpath, trialpath,
                          vdName, 
                          vdExt, sep = ""), 
                    sheet = vdSheet,
                    na = "*!*") %>%
  data.frame()


## read zip code file----------------------------------
Contacts_df = read_excel(paste(rootpath,
                                ContactsName, 
                                ContactsExt, sep = ""), 
                         sheet = ContactsSheet) %>%
  data.frame()


## read language file----------------------------------
Language_df = read_excel(paste(rootpath, trialpath,
                                LangName,
                                LangExt, sep = ""),
                          sheet = LangSheet) %>%
  data.frame()

## read amendment file---------------------------------
Amendment_df = read_excel(paste(rootpath, trialpath,
                             amendName, 
                             amendExt, sep = "")) %>%
  data.frame()
#----------------------------------------------------------------------
# Format Columns
#----------------------------------------------------------------------

## visit date tibble:---------------
## specify columns that need to be manipulated in the visitdate file
charCols = c('Site', 'Patient', 'Gender')
dateCols = c('Patient.Visit.Processed.Date')
dashCols = c('Investigator')

# Convert columns that have numbers but should be factors
VisitDate_df[charCols] = lapply(VisitDate_df[charCols], as.character)

# Convert date columns
VisitDate_df[dateCols] = lapply(VisitDate_df[dateCols], 
                                as.Date, origin="1970-01-01")

# Strip dashes (except when they are between names, ie: hyphenated name)
VisitDate_df[dashCols] = lapply(VisitDate_df[dashCols], 
                                function(y) gsub('( |^)-+|-+( |$)', '\\1', 
                                                 gsub("[^ [:alnum:]'-]", 
                                                      '', y)))


## zipcode file:---------------
Contacts_df$Site = as.character(Contacts_df$'Site.Reference..')

## Language file:---------------
Language_df$Subject = as.character(Language_df$Subject)

## Amend file:---------------
Amendment_df$Patient = as.character(Amendment_df$Patient)

#----------------------------------------------------------------------
# PIVOT Long to wide for Visit 0 and 1 and rename to corresponding visit name
#----------------------------------------------------------------------


## converted to data.table and cast long to wide on visit column
VisitDate_wide = dcast(setDT(VisitDate_df), ...~ Visit,  ## ... gets vars not specified
                       value.var = "Patient.Visit.Processed.Date")

names(VisitDate_wide)[names(VisitDate_wide)=="0"] = 'Visit_0' 
names(VisitDate_wide)[names(VisitDate_wide)=="1"] = 'Visit_1' 

## Add column to identify Missing D1 dates
VisitDate_widedf = data.frame(VisitDate_wide) %>%
  dplyr::mutate(D1.Date.Present = ifelse(is.na(Visit_1) | is.null(Visit_1),
                                 "Missing", "Present"))

#----------------------------------------------------------------------
# Calculate Future Visits
#----------------------------------------------------------------------

VisDay = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
           20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 60, 85)

for (i in VisDay){
  VisitDate_widedf[paste('Visit', 
                         as.character(i), 
                         sep = "_")] = VisitDate_widedf$Visit_1 + i - 1
}


#----------------------------------------------------------------------
# PIVOT Wide to long
#----------------------------------------------------------------------


## list the columns to keep in the df (those that aren't being pivoted)
statcols = c('Site', 'Site.Name', 'Investigator', 'Patient', 'Gender', 
             'Patient.Status', 'D1.Date.Present')

## convert wide to long to each visit is a row for each subject
VisitDate_melt = melt(setDT(VisitDate_widedf), 
                      id.var = statcols,
                      variable.name = 'Visit',
                      measure.vars = patterns('Visit_'), ## pattern gets all visit cols
                      value.name = "Visit.Date")



## CREATE FINAL DATAFRAME FOR EXPORT-------------------------------------------
VisitDate_final = data.frame(VisitDate_melt) %>%
  ## add reference data (past, today, future) based on each visit date
  dplyr::mutate(Visit.Number = as.numeric(sub(".*_", "", Visit)),
                Reference.Day.Flag = 
                  ifelse(Visit.Date == as.Date(Sys.time()), 
                         "Today",
                    ifelse(Visit.Date == as.Date(Sys.time())+1, 
                           "Tomorrow",
                      ifelse(Visit.Date < as.Date(Sys.time()), 
                             "Past", "Future"))),
                
                ## these columsn allow aggregation by week and month
                Day.of.Week = weekdays(Visit.Date),
                Week.Number = week(Visit.Date),
                Month.Number = format(Visit.Date, '%m')) %>%
  
  
  ## join site location data
  dplyr::left_join(Contacts_df[,c('Site',
                                  'Location.State.Province', 
                                  'Location.City')],
                   by = c('Site' = 'Site')) %>%
  
  ## join subject language data
  dplyr::left_join(Language_df[, c('Subject', 'Language')],
                   by = c('Patient' = 'Subject')) %>%
  
  ## default to english if no language provided
  dplyr::mutate(Language = ifelse(is.na(Language), 'English', Language)) %>%
  
  ## joing protocol amendment data
  dplyr::left_join(Amendment_df[, c('Patient', 'Amd')], by = 'Patient') %>%
  
  ## add a space here so it doesn't show as blank in the output pivot table
  dplyr::mutate(Amendment = ifelse(is.na(Amd), " ", Amd)) %>%
  
  dplyr::arrange(Site, Patient, Visit.Date) %>%
  
  dplyr::select(Site, Site.Name, Investigator, 
                Location.State.Province, Location.City, 
                Patient, Patient.Status, 
                Gender, Language, Amendment,
                D1.Date.Present, Visit, Visit.Number, Visit.Date, 
                Day.of.Week, Week.Number, Month.Number, 
                Reference.Day.Flag)


#----------------------------------------------------------------------
# Write Melted Data to File
#----------------------------------------------------------------------

write.csv(VisitDate_final, file.path(paste(rootpath, trialsave, 
                                           saveName, saveExt, 
                                           sep = "")),
          row.names = FALSE)
