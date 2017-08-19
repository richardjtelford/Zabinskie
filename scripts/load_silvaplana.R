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
  bind_cols(recon_silv2009) %>% pn # all identical

silv2009 <- silv2009 %>% select(-matches("^YearAD"))

#tidy up
rm(f, f2)

## ---- read_silvaplana_2008_JoPL
recon_silv2008 <- read_table("data/silvaplana2008.txt", skip = 78) %>% 
  filter(!is.na(Year))

## ---- read_silvaplana_2010_QSR
recon_qsr <- read_table("data/silvaplana2010.txt", skip = 98) %>% 
  filter(!is.na(Year)) %>%   
  select(Year, JulyT = `3-year`) %>% 
  filter(!is.na(JulyT)) %>% 
  arrange(desc(Year))

## ---- compare_2008_2009_recon

recon_silv2008 %>% 
  full_join(
    recon_silv2009 %>% filter(Year > min(recon_silv2008$Year)),
    by = "Year", suffix = c(".2008", ".2009")
    ) %>% 
  arrange(desc(Year))

recon_silv2008 %>% mutate(source = "JoPL") %>% 
  bind_rows(
    recon_silv2009 %>% 
      filter(Year > min(recon_silv2008$Year)) %>% 
      mutate(source = "Holocene"), 
    recon_qsr %>% 
      filter(Year > min(recon_silv2008$Year)) %>% 
      mutate(source = "QSR") %>% 
      mutate(JulyT  = scale(JulyT, scale = FALSE) + mean(recon_silv2008$JulyT))#,
    # silva_met %>% filter(Month == 7, Year <= 2001) %>% 
    #   select(Year, JulyT = Temperature) %>% 
    #   mutate(source = "Instrumental")
    ) %>% 
  ggplot(aes(x = Year, y = JulyT, colour = source)) +
    geom_point() +
    geom_line() +
    labs(x = "Year CE", y = "Reconstructed July Temperature °C", colour = "") +
    theme(legend.position = c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())
  
#check for identical reconstructions
recon_silv2008 %>% filter(duplicated(JulyT))#none
recon_silv2009 %>% filter(duplicated(JulyT))#none

#join by reconstruction
tt <- recon_silv2008 %>% 
  full_join(recon_silv2009 %>% filter(Year >= 1834), 
            by = "JulyT", suffix = c(".2008", ".2009")) %>% 
  mutate(match = !(is.na(Year.2008)|is.na(Year.2009))) 

tt %>% filter(!match) %>% pn


ggplot(tt, aes(x = Year.2008, y = Year.2009, colour = match)) + geom_point() + geom_rug() +
  geom_abline(intercept = 0, slope = 1) +
  labs(colour = "Match")+
  theme(legend.position = c(0.1,.95), legend.justification  = c(0, 1))

side_by_side <- recon_silv2008 %>% 
  bind_cols(recon_silv2009 %>% 
              slice(1:nrow(recon_silv2008)) %>% 
              rename_all(paste0, ".Holocene")) 

side_by_side %>% 
  ggplot(aes(x = JulyT, y = JulyT.Holocene)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = c(-5:5))

side_by_side %>% filter((JulyT - JulyT.Holocene) %% 1 != 0)

side_by_side %>% gather(key = source, value = JulyT, -Year, -Year.Holocene) %>% mutate(source = recode(source, "JulyT" = "JoPL", "JulyT.Holocene" = "Holocene"), source = factor(source, levels = c("JoPL", "Holocene"))) %>%  
  ggplot(aes(x = Year, y = JulyT, colour = source)) + 
  geom_line() +
  labs(x = "Year CE", y = "Reconstructed temperature °C", colour = "Source") +
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1))


## 
library("png")
library("grid")
fig6a <- readPNG("data/JoPL_6a.png")


makefig<-function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}

jopl <- ggplot(recon_silv2008, aes(x = Year, y = JulyT)) +
  annotation_custom(rasterGrob(fig6a, width=unit(1,"npc"), height=unit(1,"npc")), xmin = 1816, xmax = 2011, ymin = 4.2, ymax = 16.25) +
  geom_point(colour = "red", size = 1) +
  geom_line(colour = "red") + 
  theme_classic() +
  theme(plot.margin = margin(t = 20, r = 20, b = 35, l = 25))

makefig(jopl)# ta
