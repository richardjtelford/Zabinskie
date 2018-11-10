## ---- read_silvaplana_Holocene_reconstruction
#Load 540 yr reconstruction - Holocene 2009

silva_load_recon_holocene <- function(f){
  #f <- "data/silvaplana/silvaplana2009.txt"
  recon_holocene <- read_table(f, skip = 91, n_max = 134) %>% 
    mutate(Year = as.numeric(Year))# '...' values to NA
  return(recon_holocene)
}

## ---- read_silvaplana_Holocene_fossil
#Load fossil data from Holocene 2009
silva_load_fos_holocene <- function(f){
  
  #f <- "data/silvaplana/silvaplana2009.txt"
  
  f2 <- readLines(f)
  YearAD <- grep("^YearAD", f2)
  
  #fix spaces in some taxon names
  f2[YearAD[4]] <- c("YearAD   Paramect   Paraph   Psec_b   Psec_c  Pseodok    Pseuo  Pseudos    Rheocr  Smitti   Sympos    Synor  Thienne   Trisso  Zalu_zal Zalu_lin  Pentan  Tanypus  Proclad   Pottha   Undiff   Unknown")
  
  fos_holocene <- map(YearAD, function(skip) {
    d <- read_table2(file = f, col_names = FALSE, skip  = skip, n_max = 134)
    h <- read.table(text = f2[skip], stringsAsFactors = FALSE, comment.char = "")
    h <- gsub("#", "_", h)
    names(d) <- h
    d
  })
  
  fos_holocene <- bind_cols(fos_holocene)
  # fos_holocene %>% 
  #   select(starts_with("YearAD")) %>%
  #   bind_cols(recon_holocene) %>% pn # all identical
  
  fos_holocene <- fos_holocene %>% select(-matches("^YearAD\\d"))
  return(fos_holocene)
}

## ---- read_silvaplana_2008_JoPL
#Load 140 yr reconstruction - JoPL 2009
silva_load_recon_jopl <- function(f){
  #f <- "silvaplana/data/silvaplana2008.txt"
  recon_jopl <- read_table(f, skip = 78) %>% 
    filter(!is.na(Year))
  return(recon_jopl)
}
## ---- read_silvaplana_2010_QSR
#Load 1000 yr reconstruction - QSR 2010
silva_load_recon_qsr <- function(f){
  f <- "data/silvaplana/silvaplana2010.txt"
  recon_qsr <- read_table(f, skip = 98) %>% 
    filter(!is.na(Year)) %>%   
    mutate(JulyT = `3-year`, 
           JulyT = ifelse(is.na(JulyT), `10-year`, JulyT)) %>% #use 10-yr smooth if needed
    select(Year, JulyT) %>% 
    filter(!is.na(JulyT)) %>% 
    arrange(desc(Year))
  return(recon_qsr)
}

# ## --- read_2010_QSR_unsmoothed
# #not archived
# 
# recon_qsr2 <- read_table("data/chironomids_Silvaplana.txt", skip = 1, col_names = FALSE) %>% 
#   rename(Year = X1, JulyT = X2) %>% 
#   filter(!is.na(JulyT)) %>% 
#   arrange(desc(Year))


silva_load_climate <- function(f){
    #"data/silvaplana/homog_mo_SIA.txt"
  sil_inst <- read.table(file = f, skip = 27, header = TRUE) %>%
    filter(Month == 7) %>% 
    select(year = Year, temperature = Temperature) %>% 
    filter(year <= 2001) #last year in paper
  return(sil_inst)
}

# digitised instrumental data for calibration in time
silva_load_digitised_climate <- function(){
  sil_cit_temp <- read.table("data/silvaplana/calibration_in_time.txt") %>% 
    select(1:2) %>% 
    set_names(c("year", "temperature")) %>% 
    mutate(year = round(year))
  
  sil_space_temp <- read.table("data/silvaplana/calibration_in_space.txt") %>% 
    select(1:2) %>% 
    set_names(c("year", "temperature")) %>% 
    bind_rows(#overplotted points
      data_frame(year = 1933, temperature = 11.3144),
      data_frame(year = 1941, temperature = 11.1127),
      data_frame(year = 1944, temperature = 11.5991)) %>% 
    arrange(year)
  
  bind_rows(cis = sil_space_temp, cit = sil_cit_temp, .id = "what")
}

