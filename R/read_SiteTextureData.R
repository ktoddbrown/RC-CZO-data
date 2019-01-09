read_SiteTextureData <- function(dataDir = '~/Documents/Professional/Datasets/Renyolds-CZO/data',
                                 verbose=TRUE){
  #Jan-8-2019 12PM
  ###Identify the tables #####
  tableIDs <- bind_rows( tibble(Hydro7Hr = 'A4:J14', HydroID='B18:C18', Textures='A23:E33', #skip blanks
                                sheet_name = c('OTH1C', 'WHIL', 'Site 3', 'Jun1C', 'DH1L', 'LWS', 'DHIR',
                                          'PAR1C', 'MDR1', 'RR2', 'NG1C', 'QST2', 'FRE1L', 'CB1', 
                                          'OFF1C', 'SCJ1L', 'Hoot1', 'DHA1', 'FTR1', 'SRB', 'OFF2L',
                                          'QST1', 'WT1', 'RR1')), #regular sheets
                         tibble(sheet_name = 'Dry1C', Hydro7Hr = 'A4:J10', HydroID='B14:C14', Textures='A19:E25'),
                         tibble(sheet_name = 'Flat1', Hydro7Hr = 'A4:J23', HydroID='B27:C27', Textures='A32:E51'),
                         tibble(sheet_name ='MDR2', Hydro7Hr = 'A2:J12', HydroID='B16:C16', Textures='A21:E31'))
  
  ###Read in raw data #####
  Hydro7Hr_raw <- plyr::ddply(tableIDs, 'sheet_name', function(xx){
    ans <- readxl::read_excel(file.path(dataDir, 'Site_texture (1).xlsx'), sheet=xx$sheet_name, 
                       range=xx$Hydro7Hr, col_types = 'text') #%>%
      #mutate(rowID=1:length(ID))# Deal with bad identifiers
     
    return(ans)
  })
  
  #Jan-8-2019 1PM
  #Jan-8-2019 235PM
  #Trim the rows with no data
  #texture.df <- Texture_raw %>%
  #  filter(`Sand(g)` != '0' | `Silt(g)` != '0' | `Clay(g)` != '0' | !is.na(`Depth (cm)`)) %>%
  #  full_join()
  
  texture.df <- Hydro7Hr_raw %>%
    filter(!is.na(`Depth (cm)`)    | !is.na(`Mass Dried (g)`) |
           !is.na(`Sand Mass (g)`) | !is.na(`Ashed Mass (g)`)) %>%
    mutate_at(vars(contains('(g)'), contains("Reading")), as.numeric) %>%
    rename(`Wet Sieved + Oven dry mass (g)` = 'Sand Mass (g)', 
           `Temperature (C)` = `Temp ©`) %>%
    mutate(`Depth (cm)` = gsub(' ', '', `Depth (cm)`)) #Strip the spaces
    
  return(texture.df)
  
}
