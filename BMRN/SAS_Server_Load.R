

# TITLE: 270-301_SVAnchorDAta.R
# STUDY: 270-301
# AUTHOR: Jason Delosh
# DATE: 24Sep2020

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## INSTRUCTIONS: ---------------------------------------------------------------

# This script uses the SV EDC table to get key visits: scr, rescrn, bl, D1 and
#     protocol completion status for each subject assigned a screening number
# 
# RAVE/Reporter/SAS On Demand
#     Complete the following entries:
#           File Type = CSV
#           Zip File Target = Email
#           Extract Type = Cumulative
#           View Type = chose "SV" for this script
#           Enter and confirm a password to open the zip file
#     Leave all other fields default
#     The file will be emailed to you
#     Open file, enter password, save as <name>.csv in desired location

## outputs:
# svkey, svAnchorData


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## LOAD PACKAGES
lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr', 'fs', 'readr')

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


## Function 
    ##  environment: production vs development, vs. quality assurance
    ##  data folder: string matching folder of interest (DM)
    ##  study:  list of one or more studies

    ##  blinding:  access blinded or unblinded folders
    ##  sas labels: yes or no
    ##  tables: list of tables to load, or "" = all .sas7bdat tables

env = 'PRd'
envFolder = 'cDm'
protocol = c('270301', '307201', '331201')
protocolFolder = 'csr'
protocolTable = c('dd')

envPath = fs::path(paste0('\\\\sassys', tolower(env), '.bmrn.com'), 
                   tolower(envFolder),
                   paste0(tolower(envFolder), tolower(env)))


applytest = fs::dir_ls(path = envPath,
                      regexp = protocol,
                      type = 'directory',
                      recurse = 2)

## Specify Paths----------------------------------------------------------------
## note: pathList1 and pathList2 are same, and no speed benefit noted
## note: add error handling: if protocol folder = FALSE 
##            1)print message, 
##            2) move to next protocol folder

## use lapply to get list of paths
pathList1 = lapply(protocol, 
                   function(p) 
                     fs::path(fs::dir_ls(path = envPath,
                                         regexp = p,
                                         type = 'directory',
                                         recurse = 2),
                              tolower(protocolFolder), 
                              'dataoper'))


## use loop to get list of paths
pathList2 = character()
for (p in protocol) {
  aPath = fs::dir_ls(path = envPath, 
                     regexp = p, 
                     type = 'directory', 
                     recurse = 2)
  pathList[p] = fs::path(aPath, tolower(protocolFolder), 'dataoper')
}


## Create File Names------------------------------------------------------------

## example to create list of paths and files to read:  https://stats.idre.ucla.edu/r/codefragments/read_multiple/
(f = file.path("https://stats.idre.ucla.edu/stat/data", c("auto.dta",
                                                           "cancer.dta", "efa_cfa.dta", "hsbmar.dta")))


lapply(mget(paste0(protocol, protocolTable)), function(x) read_sas(pathlist, paste0(protocolTable, '.sas7bdat')))


for (i in 1:length(pathList)) {
  for (j in protocolTable) {
    pathTable = fs::path(pathList[i], paste0(protocolTable[j], '.sas7bdat'))
    paste(names(pathList[i]), protocolTable[j], sep = "_") = read_sas(pathTable)
  }
}

dsRaw = read_sas(data_file = paste(mywd, path1, file2, sep = ""), 
                 .name_repair = 'check_unique')
fs::dir_ls(envPath)

paste0(pathList[1], paste0(protocolTable[1], '.sas7bdat'))
paste0(names(pathList[1]), protocolTable[1])

#dir_create(path_norm('C:\Users\ja903976\OneDrive - BioMarin\Desktop\'))
pwd
dir_create("C:/Users/ja903976/OneDrive - BioMarin/Desktop/TestFolder")
file_create("C:/Users/ja903976/OneDrive - BioMarin/Desktop/TestFolder/b.csv")
link_create(path_abs("C:/Users/ja903976/OneDrive - BioMarin/Desktop/TestFolder"), "c")

path_real("c/b")
parts = path_split("C:/Users/ja903976/OneDrive - BioMarin/Desktop/TestFolder/b")
path_abs("..")
path_split("..")
## read data



lbfTestRaw = read_sas(data_file = paste0(SASwd, path1, testfile), 
                   .name_repair = 'check_unique')


##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
## Get list of files with .sas7bdat pattern


## error handling: if function variables are incorrect, notify with error message
