read_RyanData <- function(dataDir = '~/Documents/Professional/Datasets/Renyolds-CZO/data',
                                 verbose=TRUE){
  
  meta <-  readxl::read_excel(path=file.path(dataDir, 
                            'Reynolds_Creek_Idaho_Critical_Zone_Observatory_Soil_Sample_Data_Ryan_Will (1).xlsx'),
                            sheet='metadata')
  temp <- readxl::read_excel(path=file.path(dataDir, 
                            'Reynolds_Creek_Idaho_Critical_Zone_Observatory_Soil_Sample_Data_Ryan_Will (1).xlsx'),
                            sheet='raw data', na=c('XXXXXXX','N/A', 'NA'))
  
  return(list(meta=meta, samples=temp))
}