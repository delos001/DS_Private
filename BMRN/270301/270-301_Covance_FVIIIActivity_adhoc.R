
## LOAD PACKAGES
lpkgs = c('haven', 'stringr', 'dplyr', 'tidyr', 'lubridate', 'purrr')

# loop through required packages & if not already installed, load, then install
for(pkg in lpkgs) {
  
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

SASwd = '\\\\sassysprd.bmrn.com\\cdm\\cdmprd\\'

path1 = 'bmn270\\hemoa\\270301\\csrunblinded\\dataoper\\'
path2 = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\Covance_qc\\adhocChecks\\'

file1 = 'lbcovance_blinded.sas7bdat'
file2 = 'MissingFVIIIActivity.csv'


lb_bl_Raw = read_sas(data_file = paste(SASwd, path1, file1, sep = ""), 
                 .name_repair = 'check_unique')

missing_Raw = read.csv(file.path(path2, file2)) %>%
  dplyr::mutate(Specimen.identifier..lbrefid. = as.character(Specimen.identifier..lbrefid.))



output = lb_bl_Raw %>%
  dplyr::select(SUBJECT, FOLDERNAME, LBDAT, LBREFID, LBTESTCD, LBSPID, LBTEST,
                LBORRES) %>%
  dplyr::filter(grepl("Activity", LBTEST)) %>%
  dplyr::mutate("ResultPresent" = ifelse(is.null(LBORRES) | LBORRES == "", "No", "Yes")) %>%
  dplyr::select(-LBORRES)

FVIII_out = missing_Raw %>%
  dplyr::left_join(output[, c("SUBJECT", "FOLDERNAME", "LBREFID", "LBTEST", 
                              "ResultPresent")],
                   by = c('subject_edc' = "SUBJECT",
                          "Specimen.identifier..lbrefid." = "LBREFID")) %>%
  tidyr::pivot_wider(names_from = LBTEST, values_from = "ResultPresent")

  
write.csv(FVIII_out, 
          file.path('C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\BMRN270\\270-301\\Covance_qc\\adhocChecks\\', 
                            'FVIII_out.csv'), row.names = FALSE)



  

