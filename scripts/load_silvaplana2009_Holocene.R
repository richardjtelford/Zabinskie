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
