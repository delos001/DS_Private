


## Specify the pdf location
localroot = 'C:\\Users\\ja903976\\OneDrive - BioMarin\\Desktop\\Studies\\'
localpth = 'BMRN270\\270-301\\BAS\\'
file1 = 'BMN270-301_DTA_BioAnalytical_Sciences_22DEC2020.pdf'
mypdf = paste0(localroot, localpth, file1)

## extract the tables
pdfoutscrape <- extract_tables(file = mypdf,
                              method = 'decide',
                              output = "data.frame")


testpdfout = pdfoutscrape[4] %>%
  pluck(1)



