## ---- read_silvaplana_Holocene_reconstruction
#Load 540 yr reconstruction - Holocene 2009
recon_holocene <- read_table("silvaplana/data/silvaplana2009.txt", skip = 91, n_max = 134) %>% 
  mutate(Year = as.numeric(Year))# '...' values to NA


## ---- read_silvaplana_Holocene_fossil
#Load fossil data from Holocene 2009

f <- "silvaplana/data/silvaplana2009.txt"

f2 <- readLines(f)
YearAD <- grep("^YearAD", f2)

#fix spaces in some taxon names
f2[YearAD[4]] <- c("YearAD   Paramect   Paraph   Psec_b   Psec_c  Pseodok    Pseuo  Pseudos    Rheocr  Smitti   Sympos    Synor  Thienne   Trisso  Zalu_zal Zalu_lin  Pentan  Tanypus  Proclad   Pottha   Undiff   Unknown")

fos_holocene <- purrr::map(YearAD, function(skip) {
  d <- read_table2(file = f, col_names = FALSE, skip  = skip, n_max = 134)
  h <- read.table(text = f2[skip], stringsAsFactors = FALSE, comment.char = "")
  h <- gsub("#", "_", h)
  names(d) <- h
  d
})

fos_holocene <- do.call(bind_cols, fos_holocene)
# fos_holocene %>% 
#   select(starts_with("YearAD")) %>%
#   bind_cols(recon_holocene) %>% pn # all identical

holocene_year <- fos_holocene %>% select(YearAD) 

fos_holocene <- fos_holocene %>% select(-matches("^YearAD"))

#tidy up
rm(f, f2, YearAD)

## ---- read_silvaplana_2008_JoPL
#Load 140 yr reconstruction - JoPL 2009
recon_jopl <- read_table("silvaplana/data/silvaplana2008.txt", skip = 78) %>% 
  filter(!is.na(Year))

## ---- read_silvaplana_2010_QSR
#Load 1000 yr reconstruction - QSR 2010
recon_qsr <- read_table("data/silvaplana2010.txt", skip = 98) %>% 
  filter(!is.na(Year)) %>%   
  mutate(JulyT = `3-year`, 
         JulyT = ifelse(is.na(JulyT), `10-year`, JulyT)) %>% #use 10-yr smooth if needed
  select(Year, JulyT) %>% 
  filter(!is.na(JulyT)) %>% 
  arrange(desc(Year))

## --- read_2010_QSR_unsmoothed
#not archived

recon_qsr2 <- read_table("data/chironomids_Silvaplana.txt", skip = 1, col_names = FALSE) %>% 
  rename(Year = X1, JulyT = X2) %>% 
  filter(!is.na(JulyT)) %>% 
  arrange(desc(Year))
