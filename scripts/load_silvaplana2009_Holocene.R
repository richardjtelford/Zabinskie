## ---- read_silvaplana2009

f <- "data/silvaplana2009.txt"

f2 <- readLines(f)
grep("^Year ", f2)
grep("2a.", f2)

#reconstruction
recon_silv2009 <- read_table(f, skip = 91, n_max = 134)
recon_silv2009 <- recon_silv2009 %>% mutate(Year = as.numeric(Year))# '...' values to NA

ggplot(recon_silv2009, aes(x = Year, y = JulyT)) +
  geom_point() + 
  geom_line()

#fossil data
YearAD <- grep("^YearAD", f2)

#fix spaces in some taxon names
f2[YearAD[4]]
f2[YearAD[4]] <- c("YearAD   Paramect   Paraph   Psec_b   Psec_c  Pseodok    Pseuo  Pseudos    Rheocr  Smitti   Sympos    Synor  Thienne   Trisso  Zalu_zal Zalu_lin  Pentan  Tanypus  Proclad   Pottha   Undiff   Unknown")

silv2009 <- map(YearAD , function(skip) {
  d <- read_table2(file = f, col_names = FALSE, skip  = skip, n_max = 134)
  h <- read.table(text = f2[skip], stringsAsFactors = FALSE, comment.char = "")
  h <- gsub("#", "_", h)
  names(d) <- h
  d
})

silv2009 <- do.call(bind_cols, silv2009)
silv2009 %>% 
  select(starts_with("YearAD")) %>%
  bind_cols(recon_silv) %>% pn # all identical

silv2009 <- silv2009 %>% select(-matches("^YearAD"))

#tidy up
rm(f, f2)

## ---- read_silvaplana_2008_JoPL
recon_silv2008 <- read_table("data/silvaplana2008.txt", skip = 78) %>% 
  filter(!is.na(Year))

## ---- compare_2008_2009_recon

recon_silv2008 %>% 
  full_join(
    recon_silv2009 %>% filter(Year > min(recon_silv2008$Year)),
    by = "Year", suffix = c(".2008", ".2009")
    ) %>% 
  arrange(Year)

recon_silv2008 %>% mutate(source = "JOPL") %>% 
  bind_rows(
    recon_silv2009 %>% 
      filter(Year > min(recon_silv2008$Year)) %>% 
      mutate(source = "Holocene"),
    silva_met %>% filter(Month == 7, Year <= 2001) %>% 
      select(Year, JulyT = Temperature) %>% 
      mutate(source = "Instrumental")
    ) %>% 
  ggplot(aes(x = Year, y = JulyT, colour = source)) +
    geom_point() +
    geom_line()
  