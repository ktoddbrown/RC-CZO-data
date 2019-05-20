read_PattonData <- function(dataDir = '~/Documents/Professional/Datasets/Renyolds-CZO/data',
                                 verbose=TRUE){
  #22-January-2019 1pm - 1:20pm
  datafile <- file.path(dataDir, 'Parton_DOI_ReynoldsCreek_07072018KAL.xlsx')
  meta.df <- readxl::read_excel(path=file.path(dataDir, 'Patton_DOI_ReynoldsCreek_07072018KAL.xlsx'),
                                sheet='MetaDATA', range = 'A2:B33') %>% mutate(tab='Soil Pits') %>%
    bind_rows(readxl::read_excel(path=file.path(dataDir, 'Patton_DOI_ReynoldsCreek_07072018KAL.xlsx'),
                                 sheet='MetaDATA', range = 'A36:B83') %>% mutate(tab='Soil Samples'))
  
  site.df <- readxl::read_excel(path=file.path(dataDir, 'Patton_DOI_ReynoldsCreek_07072018KAL.xlsx'),
                                    sheet='ReynoldsCreek_Soil Pits', na = c('NoData', 'BDL'))
  
  soilSamples.df <- readxl::read_excel(path=file.path(dataDir, 'Patton_DOI_ReynoldsCreek_07072018KAL.xlsx'),
                                       sheet='ReynoldsCreek_Soil Samples', na = c('NoData', 'BDL'))
  
  return(list(meta = meta.df,
              site = site.df,
              soilSamples = soilSamples.df))
}