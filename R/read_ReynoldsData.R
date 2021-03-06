#' Read in primary inorganic carbon data
#'
#' This function reads in the primary inorganic carbon data from _Reynolds_data (1).xlsx_
#'
#' @param dataDir string indentifying the data directory
#' @param verbose boolean flag for verbose outputs
#'
#' @return list of data tables including: site, pit, mass, and inorganic carbon data
#' @export
#'
read_ReynoldsData <- function(dataDir = '~/Documents/Professional/Datasets/Renyolds-CZO/data',
                              verbose=TRUE){
  
  ICData <- file.path(dataDir, 'Reynolds_data (1).xlsx')
  
  if(!file.exists(ICData)){
    stop('Invalid file path to [Reynolds_data (1).xlsx]')
  }
  
### Big long list identifing tables #####
  readRanges <- list(DH1=list(sheet_name='DH1', site_name='A1:B1', 
                              dates='D1:E1',
                              site='A3:G4', bulk_density='I19',
                              pit_description='A7:E15',
                              fine_fraction='A20:G50',
                              gravel_fraction='A53:D83'),
                     WH1=list(sheet_name='WH1',
                              site_name='A2:B2', dates='E2:F2', site='A4:G5', bulk_density='E17',
                              pit_description='A8:G13',
                              gravel_fraction='A18:E29'),
                     DRY1=list(sheet_name='DRY1',
                               site_name='A1:B1', dates='D1:E1', 
                               site='A3:G4',
                               bulk_density='E13',
                               pit_description='A7:G11',
                               fine_fraction='A14:E44',
                               gravel_fraction='A47:B77'),
                     OTH1=list(sheet_name='OTH1',
                               site_name='A1:B1', dates='D1:E1', 
                               site='A3:G4',
                               #bulk_density='E13',
                               pit_description='A7:G11'),
                     #fine_fraction='A14:E44',
                     #gravel_fraction='A47:B77')
                     Jun1=list(sheet_name='Jun1',
                               site_name='A1:B1', dates='D1:E1', 
                               site='A3:G4',
                               #bulk_density='E13',
                               pit_description='A7:D11'),
                     #fine_fraction='A14:E44',
                     #gravel_fraction='A47:B77')
                     PAR1=list(sheet_name='PAR1',
                               site_name='A1:B1', 
                               dates = 'H1:I1', #'H1:I2', #Specal formatting
                               site_PAR1='A2:F3', #Specal formatting
                               bulk_density='F13',
                               pit_description='A5:J11',
                               fine_fraction='A14:F47',
                               gravel_fraction='A91:E124'),
                     Flat1=list(sheet_name='Flat1',
                                site_name='A1:B1', 
                                dates ='E2:F2', #'E1:F2', #Specal formatting
                                site_Flat1='A2:G4', #Specal formatting
                                pit_description='A7:I11',
                                bulk_density='E15',
                                fine_fraction='A16:D35',
                                gravel_fraction='A41:L60'),
                     Off1=list(sheet_name='Off1',
                               site_name='A1:B1', 
                               dates = 'E1:F1', #'E1:G1', #Specal formatting
                               site='A3:G4',
                               pit_description='A8:J12',
                               bulk_density='E16',
                               fine_fraction='A17:F44',
                               gravel_fraction='A50:D77'),
                     Hoot1=list(sheet_name='Hoot1',
                                site_name='A1:B1', 
                                dates='E1:F1',
                                site='A3:G4',
                                pit_description='A8:I12',
                                bulk_density='E15',
                                fine_fraction='A16:F49',
                                gravel_fraction='A52:D85'),
                     Hoot2=list(sheet_name='Hoot2',
                                site_name='A1:B1', 
                                dates='E1:F1',
                                site='A3:G4',
                                pit_description='A8:I12',
                                bulk_density='E15',
                                fine_fraction='A16:F49',
                                gravel_fraction='A52:B85'),
                     Off2=list(sheet_name='Off2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:I11',
                               bulk_density='E15',
                               fine_fraction='A16:F46',
                               gravel_fraction='A50:D81'),
                     LWS1=list(sheet_name='LWS1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:I13',
                               bulk_density='E16',
                               fine_fraction='A17:F47',
                               gravel_fraction='A50:B80'),
                     NG1=list(sheet_name='NG1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:I13',
                              bulk_density='F15',
                              fine_fraction='A16:F43', #TODO check mislabled table
                              gravel_fraction='A49:D76'),
                     Gate1=list(sheet_name='Gate1',
                                site_name='A1:B1', 
                                dates='E1:F1',
                                site='A3:G4',
                                pit_description='A8:J13',
                                bulk_density='E16',
                                fine_fraction='A17:F24',
                                gravel_fraction='A29:B36'),
                     FRE1=list(sheet_name='FRE1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J13',
                               bulk_density='E15',
                               fine_fraction='A16:F43',
                               gravel_fraction='A48:B75'),
                     RDC1=list(sheet_name='RDC1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J12'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     RDC2=list(sheet_name='RDC2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J12'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     RDC3=list(sheet_name='RDC3',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J12'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     RDH1=list(sheet_name='RDH1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J11'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     RDH2=list(sheet_name='RDH2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J9'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     Chan1=list(sheet_name='Chan1',
                                site_name='A1:B1', 
                                dates='E1:F1',
                                site='A3:G4',
                                pit_description='A8:J11'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR1=list(sheet_name='WHR1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J12'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR2=list(sheet_name='WHR2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J12'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR3=list(sheet_name='WHR3',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J11'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR4=list(sheet_name='WHR4',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J11'),#,
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR5=list(sheet_name='WHR5',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4'),
                     #pit_description='A8:J11', #not recorded
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     WHR6=list(sheet_name='WHR6',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4'),
                     #pit_description='A8:J11', #not recorded
                     #bulk_density='E16', # not recorded
                     #fine_fraction='A17:F47', #not recorded
                     #gravel_fraction='A50:B80') #not recorded
                     FLAT2=list(sheet_name='FLAT2',
                                site_name='A1:B1', 
                                dates='E1:F1',
                                site='A3:G4',
                                pit_description='A8:J9',
                                bulk_density='E13',
                                fine_fraction='A14:E19',
                                gravel_fraction='A23:D28'),
                     QST1=list(sheet_name='QST1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J14',
                               bulk_density='H19',
                               fine_fraction='A20:F58',
                               gravel_fraction='A62:B100'),
                     QST2=list(sheet_name='QST2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J15',
                               bulk_density='F18',
                               fine_fraction='A19:F32',
                               gravel_fraction='A35:D48'),
                     SCJ1=list(sheet_name='SCJ1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J16',
                               bulk_density='E19',
                               fine_fraction='A20:F48',
                               gravel_fraction='A53:B81'),
                     RR1=list(sheet_name='RR1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J15',
                              bulk_density='E17',
                              fine_fraction='A18:F32',
                              gravel_fraction='A37:B51'),
                     SRB1=list(sheet_name='SRB1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J14',
                               bulk_density='E18',
                               fine_fraction='A19:F34',
                               gravel_fraction='A37:B52'),
                     DHA1=list(sheet_name='DHA1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J16',
                               bulk_density='G19',
                               fine_fraction='A20:F38',
                               gravel_fraction='A41:B59'),
                     WT1=list(sheet_name='WT1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J13',
                              bulk_density='E16',
                              fine_fraction='A17:F26',
                              gravel_fraction='A29:B38'),
                     RR2=list(sheet_name='RR2',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J13',
                              bulk_density='E16',
                              fine_fraction='A17:F29',
                              gravel_fraction='A33:B45'),
                     MDR1=list(sheet_name='MDR1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J11',
                               bulk_density='E14',
                               fine_fraction='A15:F25',
                               gravel_fraction='A28:B38'),
                     CB1=list(sheet_name='CB1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J13',
                              bulk_density='E16',
                              fine_fraction='A17:F27',
                              gravel_fraction='A31:B41'),
                     WCA1=list(sheet_name='WCA1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J9'),
                     #bulk_density='E19', #not recorded
                     #fine_fraction='A20:F48', #not recorded
                     #gravel_fraction='A53:B81') #not recorded
                     FTR1=list(sheet_name='FTR1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J13',
                               bulk_density='E16',
                               fine_fraction='A17:F30',
                               gravel_fraction='A34:B47'),
                     MDR2=list(sheet_name='MDR2',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J14',
                               bulk_density='E17',
                               fine_fraction='A18:F32',
                               gravel_fraction='A36:B50'),
                     TG1=list(sheet_name='TG1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J9'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     WTH1=list(sheet_name='WTH1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J9'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     MCR1=list(sheet_name='MCR1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J9'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     MC1=list(sheet_name='MC1',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J9'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     TG2=list(sheet_name='TG2',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4',
                              pit_description='A8:J11'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     TG3=list(sheet_name='TG3',
                              site_name='A1:B1', 
                              dates='E1:F1',
                              site='A3:G4'),
                     #pit_description='A8:J9'), TODO not recorded in format
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     WHU1=list(sheet_name='WHU1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J11'),
                     #bulk_density='E16', #not recorded
                     #fine_fraction='A17:F30', #not recorded
                     #gravel_fraction='A34:B47'), #not recorded
                     OWL1=list(sheet_name='OWL1',
                               site_name='A1:B1', 
                               dates='E1:F1',
                               site='A3:G4',
                               pit_description='A8:J13',
                               bulk_density='D20',
                               fine_fraction='A21:E26')
                     #gravel_fraction='A34:B47'), #not recorded
  )
  
  ## Check the sheet names ####
  if(verbose){
    print(paste('Bad names:', paste(setdiff(names(readRanges), readxl::excel_sheets(ICData)), collapse=', ')))
    print(paste('Sheets left out: [', length(setdiff(readxl::excel_sheets(ICData), names(readRanges))),']',  
                paste(setdiff(readxl::excel_sheets(ICData), names(readRanges)), collapse=', ')))
    print(paste('Sheets done: [', length(names(readRanges)), ']'))
  }
  
  allTableNames <- unique(unlist(lapply(readRanges, names)))
  
  ## Read in data ###
  rawData <- plyr::llply(readRanges, function(xx){
    plyr::llply(xx[-1], 
                function(yy){
                  readxl::read_excel(ICData, sheet=xx$sheet_name, 
                                     range=unlist(yy), col_types = 'text') %>%
                    dplyr::mutate(sheet=xx$sheet_name)})
  })
  
  longNames <- c('site_name', 'dates', 'bulk_density')
  longTables <- plyr::llply(setNames(longNames, longNames), function(xx){
    #print(xx)
    return(plyr::ldply(rawData, function(yy){
      #print(paste(yy$site_name, names(yy[[xx]])))
      return(names(yy[[xx]]))
    }, .id='core')) #pull long table info
  })
  longTables$site_name <- longTables$site_name %>%
    mutate(V2 = if_else(V1 == 'site 7: low sage 3/Parking 1', 'low sage 3/Parking 1',
                        if_else(V1 == 'site 9: low granite/Flat 1', 'low granite/Flat 1', V2)))
  
  longTables$dates <- longTables$dates %>%
    mutate(V2 = if_else(V1 == 'pit started: 7/1', '7/1', V2)) %>%
    separate(V2, into=c('startDate', 'endDate'), sep=' ?- ?', remove=FALSE) %>%
    mutate(sample_date = if_else(grepl('^\\d+$', startDate), 
                                 as.Date(as.numeric(startDate), origin = "1899-12-30"),
                                 if_else(grepl('^\\d+/\\d+/\\d+$', startDate),
                                         as.Date(startDate, "%m/%d/%Y"), 
                                         as.Date(paste(startDate, '/2014', sep=''), "%m/%d/%Y"))))
  
  tableNames <- setdiff(allTableNames, c('sheet_name', longNames))
  wideTables <- plyr::llply(setNames(tableNames, tableNames), function(xx){
    plyr::ldply(rawData, function(yy){yy[[xx]]}, .id='core') #pull wide table info
  })
  
  ## Site and pit tables ####
  #Deal with site_PAR1 and site_Flat1 which are special formats
  temp1 <- tibble(core = c('PAR1', 'Flat1'),
                  sheet = c('PAR1', 'Flat1'),
                  E = c(names(wideTables$site_PAR1)[3], names(wideTables$site_Flat1)[3]),
                  N = c(names(wideTables$site_PAR1)[4], names(wideTables$site_Flat1)[4]),
                  Elevation_m = c(names(wideTables$site_PAR1)[7], wideTables$site_Flat1[1,8] ),
                  'Parent material' = c(wideTables$site_PAR1[1,7], wideTables$site_Flat1[2,3]),
                  Slope_degrees = c(wideTables$site_Flat1[1,3], wideTables$site_PAR1[1, 3]),
                  Aspect_degrees = c(wideTables$site_Flat1[1,5], wideTables$site_PAR1[1, 5]))
  
  temp2 <- wideTables$site %>% group_by(core) %>%
    #merge all elevations
    mutate(Elevation_m = case_when(!is.na(`Elevation (m)`) ~ `Elevation (m)`,
                                   !is.na(`Elevation(m)`) ~ `Elevation(m)`,
                                   !is.na(`Elevation`) ~ `Elevation`,
                                   TRUE ~ as.character(NA)),
           Slope_degrees = case_when(!is.na(`Slope(degrees)`) ~ `Slope(degrees)`,
                                     !is.na(Slope) ~ Slope,
                                     TRUE ~ as.character(NA)),
           Aspect_degrees = case_when(!is.na(`Aspect (degrees)`) ~ `Aspect (degrees)`,
                                      !is.na(`Aspect(degrees)`) ~ `Aspect(degrees)`,
                                      !is.na(Aspect) ~ Aspect,
                                      TRUE ~ as.character(NA))) 
  
  cleanNames <- longTables$site_name %>% select(core, V2) %>% dplyr::rename(site_name = V2) 
  cleanBD <- longTables$bulk_density %>% select(core, V1) %>% dplyr::rename(bulk_density = V1)
  cleanDates <- longTables$dates %>% select(core, sample_date)
  cleanSite <- bind_rows(temp1, temp2) %>%
    select(core, E, N, Elevation_m, Slope_degrees, Aspect_degrees, `Parent material`, Vegetation)
  
  site.df <- full_join(cleanNames, cleanBD, by='core') %>%
    full_join(cleanDates, by='core') %>%
    full_join(cleanSite, by='core')
  
  pit.df <- wideTables$pit_description %>%
    mutate(Effervesence = if_else(is.na(Effervesence), Eff, Effervesence),
           Depth_cm = case_when(!is.na(Depth) ~ paste0("'", Depth),
                                !is.na(`Depth (cm)`) ~ paste0("'", `Depth (cm)`),
                                !is.na(`Pit Description`) ~ paste0("'", `Pit Description`),
                                TRUE ~ as.character(NA)),
           Structure = case_when(!is.na(Structure) ~ Structure,
                                 !is.na(Structure__1) ~ Structure__1,
                                 TRUE ~ as.character(NA)),
           Texture = case_when(!is.na(Texture) ~ Texture,
                               !is.na(Text) ~ Text,
                               TRUE ~ as.character(NA))) %>%
    rename('Gravel_percent' = 'Gravel(%)') %>%
    select(core, Depth_cm, Roots, Effervesence, Texture, 
           Color, Structure, Horizon, Bound, Gravel_percent, Notes)
  
  ## Mass and Inorganic C tables
  
  temp <- bind_rows(wideTables$fine_fraction, 
                    wideTables$gravel_fraction %>% mutate(ID=gsub('Flat2_', 'Flat2_G_', ID))) %>% #fix bad sample id
    mutate(ID = case_when(!is.na(ID) ~ ID,
                          !is.na(`Sample ID`) ~ `Sample ID`,
                          core=='Flat1' ~ X__1,
                          TRUE ~ as.character(NA)),
           Depth_cm = case_when(!is.na(Depth) ~ Depth,
                                !is.na(`Depth(cm)`) ~ `Depth(cm)`,
                                !is.na(`Depth (cm)`) ~ `Depth (cm)`,
                                TRUE ~ as.character(NA)),
           Fine_mass_g = case_when(!is.na(`Fine mass`) ~ `Fine mass`,
                                   !is.na(`Soil mass(g)`) ~ `Soil mass(g)`,
                                   !is.na(`Soil mass`) ~ `Soil mass`,
                                   !is.na(Soil) ~ Soil,
                                   TRUE ~ as.character(NA)),
           Gravel_mass_g = case_when(!is.na(Gravel) ~ Gravel,
                                     !is.na(`Gravel mass`) ~ `Gravel mass`,
                                     !is.na(`Gravel mass(g)`) ~ `Gravel mass(g)`,
                                     TRUE ~ as.character(NA)),
           IC_percent = case_when(!is.na(`% IC`) ~ `% IC`,
                                  !is.na(`%IC`) ~ `%IC`,
                                  !is.na(`Calculated value`) ~ `Calculated value`,
                                  !is.na(`Remove below limit of detection`) ~ `Remove below limit of detection`,
                                  core %in% c('PAR1') ~ X__1,
                                  core %in% c('Flat1', 'QST1', 'SCJ1', 
                                              'SRB1', 'DHA1', 'Hoot1', 'RR2', 
                                              'CB1', 'Off2') ~ as.character('missing'), #confirmed missing
                                  TRUE~as.character(NA))) %>%
    select(sheet, core, ID, Depth_cm, Gravel_mass_g, Fine_mass_g, IC_percent) %>%
    mutate(ID = 
             str_replace( str_replace( str_replace( str_replace(ID, 
                                                                'Flat_1', 'Flat1'),
                                                    'SRB_', 'SRB1_'),
                                       'CB_', 'CB1_'),
                          'FTR_', 'FTR1_')) %>% #fix formating
    separate(ID, into=c('ID1', 'ID2', 'ID3'), sep='_', remove=FALSE) %>%
    mutate(ID_sample = str_replace(toupper(ID1), toupper(core), '')) %>%
    mutate(ID_Depth_cm = case_when(core == 'PAR1' ~ ID2,
                                   is.na(ID3) ~ ID2,
                                   TRUE ~ ID3)) %>% ##TODO confirm with KL
    mutate(ID_fraction = case_when(core == 'PAR1' ~ ID3,
                                   is.na(ID3) ~ as.character(NA),
                                   TRUE ~ ID2)) ##TODO talk with KL and rekey
  
  mass.df <- temp %>% 
    select(core, ID_sample, ID_Depth_cm, contains('mass')) %>%
    gather(key='mass type', value='mass_g', contains('mass'), na.rm=TRUE) %>% unique %>%
    spread(key='mass type', value='mass_g')
  
  inorgnicC.df <- temp %>%
    select(core, ID_sample, ID_Depth_cm, ID_fraction, IC_percent) %>% unique
  
  ##Return site, pit, mass, and inroganic tables
  return(list(site=site.df, pit=pit.df, mass=mass.df, inorganicC=inorgnicC.df))
} 