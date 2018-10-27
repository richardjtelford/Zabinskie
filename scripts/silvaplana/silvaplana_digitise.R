#silvaplana digitise

#extract page 6 from pdf
system("qpdf data/silvaplana/s10933-008-9228-0.pdf  --pages silvaplana/s10933-008-9228-0.pdf 6 -- data/silvaplana/jopl_p6.pdf")

#uncompress
system("qpdf data/silvaplana/jopl_p6.pdf --stream-data=uncompress  data/silvaplana/jopl_p6_uc.pdf")

#import pdf

page6 <- readLines("data/silvaplana/jopl_p6_uc.pdf")

#find figure
start <- grep("/Document /MC5 BDC", page6)
end <- grep("\\[\\(Fig.\\)-330\\(3\\)\\]TJ", page6)
jopl_fig3 <- page6[start:end]

jopl_fig3 <- jopl_fig3[grepl("re$", jopl_fig3)] %>%
  read.table(text = .) %>% 
  set_names(c("x", "y", "width", "height", "re"))

taxa <- c("H_brund", "Smitti", "Paraph", "Paraten", "Sticto", "TanyC", "Tany.p", "Paracl", "Chir_Anth", "Dicrot", "Pseudos", "Parat", "H_Marc", "Limnoph", "Sergent", "Chiro_Plu", "Eukief", "Microt", "Polyped", "Micro.b", "Proclad", "Cory.o", "Parakieff_", "Ortho", "Tany_L", "Diamesa", "Micro.r", "Nanocl", "Tany.sp", "Monodi", "H_grim", "Cricoto", "Thienne", "Corynoneura", "Pentan", "TanyB", "Tany.c", "Synor", "Micro_I", "Psectrocladius sordidellus", "Rheocr", "Paracladopelma",  "H_sub", "Chaet",  "Protany", "Oliver", "Allops", "Zalu_zal", "Prodi", "Heterota")

#"Brilli"  "Cladop"  "Cladot"  "Coryno1"  "Einfeld"  "Georth"  "Glypto"  "Heterot"  "Lauter"  "Mesocr"  "Metrio"  "Pagast"  "Parach"  "Parach1"  "Paracl1" "Paracr"  "Paramect"  "Pottha"  "Psec_b"  "Psec_c"  "Pseodok"  "Pseuo"  "Stempel"  "Sympos"  "Tany1"  "Tanypus"  "Trisso"  "Undiff"  "Unknown"  "Zalu_lin" #rare/duplicate codes from fos_holocene   


jopl_fig3 <- jopl_fig3 %>% 
  filter(between(width, -1, 1)) %>% 
  mutate(taxa = factor(y, labels = taxa),#relabel
          percent = height/max(height) * 68,
          depth = (x - min(x))/(max(x) - min(x)) * 94, #scale
          depth = round(depth, 1)
  )

# jopl_fig3 %>% 
#   ggplot(aes(x = x + width/2, y = y, width = width, height = height, fill = factor(sign(width)))) +
#   geom_tile(show.legend = FALSE) +
#   scale_y_reverse()

jopl_fig3_fat <- jopl_fig3 %>% 
  select(-x, -width, -height, -re, -y) %>%
  spread(key = taxa, value = percent, fill = 0) %>%  
  filter(rep(c(TRUE, FALSE), length.out = nrow(.)))

jopl_fig3_fat  %>%  ggplot(aes(x = depth, y = Ortho)) + geom_col() 

bad <- c(6)
n = 5
full_join(
  jopl_fig3_fat %>% slice(n) %>% gather(key, value, -depth) %>% filter(value > 0) %>% arrange(key),
  
  silva_fos_holocene %>% slice(n) %>% gather(key, value) %>% filter(value > 0) %>% arrange(key), suffix = c("jopl", "holo"), by = "key")  %>% 
  ggplot(aes(x = valueholo, y = valuejopl)) + geom_point() + geom_abline()


full_join(
  jopl_fig3 %>% filter(depth == 93.7) %>% select(-(x:re)),
  silva_fos_holocene %>% select(-YearAD) %>% mutate(n = 1:n()) %>% filter(Sergent > 0, Chiro_Plu > 0, Tany.p > 0) %>% select_if(colSums(.) > 0) %>% gather(key, value, -n), 
  by = c(taxa = "key"), suffix = c("_jopl","_holo"))


##random tf
spp <- silva_fos_holocene %>% select(-YearAD) %>% slice(1:64) %>% select_if(colSums(. > 0) > 1)
random_wapls2 <- rerun(.n = 1000, rnorm(64)) %>% 
  map(WAPLS, y = sqrt(spp), npls = 2) %>% 
  map(crossval, verbose = FALSE) %>% 
  map(performance) %>% 
  map_df(~data_frame(
    apparent = .$object[2, "R2"],
    crossval = .$crossval[2, "R2"]
    ))
  
random_wapls2 %>% gather() %>% ggplot(aes(y = value, x = key)) + geom_violin()


decorana(sqrt(spp))


CA <- cca(sqrt(spp) ~ 1)
CA
eigenvals(CA)[1:6]/sum(eigenvals(CA))
eigenvals(CA)[1:4]/sum(eigenvals(CA)[1:4])
screeplot(CA, bstick = TRUE)

plot(CA, display = "sites", type = "p")
text(CA, display = "sites", labels = silva_fos_holocene$YearAD[1:64])


silva_fos_holocene %>% 
  mutate(Year = as.numeric(YearAD), delta = lag(Year)- Year) %>%  print(n = Inf) %>% ggplot(aes(x = Year, y = delta)) + geom_point() + ylim(0, NA) + geom_line()

#actual instrumental data
sil_inst <- read.table(file = "data/silvaplana/homog_mo_SIA.txt", skip = 27, header = TRUE) %>%
  filter(Month == 7) %>% select(year = Year, temperature = Temperature)

# digitised instrumental data for calibration in time
sil_cit_temp <- read.table("data/silvaplana/calibration_in_time.txt") %>% 
  select(1:2) %>% 
  set_names(c("year", "temperature")) %>% 
  mutate(year = round(year))
dim(sil_cit_temp)

sil_cit_temp %>% ggplot(aes(x = year, y = temperature)) +
  geom_line() +
  geom_point()

sil_cit_temp %>% mutate(year = as.character(year)) %>%
  anti_join(silva_fos_holocene, by = c(year = "YearAD"))

sil_space_temp <- read.table("data/silvaplana/calibration_in_space.txt") %>% 
  select(1:2) %>% 
   set_names(c("year", "temperature")) %>% 
  bind_rows(#overplotted points
    data_frame(year = 1933, temperature = 11.3144),
    data_frame(year = 1941, temperature = 11.1127),
    data_frame(year = 1944, temperature = 11.5991)) %>% 
  arrange(year)

#%>% 
  # mutate(year = round(year))
dim(sil_space_temp)


sil_space_temp %>% ggplot(aes(x = year, y = temperature)) +
  geom_line() +
  geom_point()


bind_rows(
  source = sil_inst %>% filter(year <= 2001),
  space = sil_space_temp, 
          time = sil_cit_temp, .id = "what") %>% 
  ggplot(aes(x = year, y = temperature, colour = what)) + 
  geom_point() + 
  geom_line()

cor(sil_cit_temp$temperature, sil_space_temp$temperature)

sil_space_recon <- read.table("data/silvaplana/calibration_in_space_recon.txt") %>% 
  select(1:2) %>% 
  set_names(c("year", "temperature"))
dim(sil_space_recon)
dim(sil_space_temp)

sil_space_recon %>% ggplot(aes(x = year, y = temperature)) +
  geom_line() +
  geom_point(alpha = 0.5)


cor(sil_space_recon$temperature, sil_space_temp$temperature)

#cit with instrumental data

sil_inst
holocene_year

sil_temp <- approx(x = sil_inst$year, y = sil_inst$temperature, xout = silva_fos_holocene %>% mutate(YearAD = as.numeric(YearAD)) %>% filter(YearAD > 1850) %>% pull(YearAD))

mod <- WAPLS(silva_fos_holocene %>% select(-YearAD) %>% slice(1:64) %>% select_if(colSums(. > 0) > 1) %>% sqrt(), sil_temp$y[1:64]) %>% crossval(verbose = FALSE)
performance(mod)
