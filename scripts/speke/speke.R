library(tidyverse)
library(magrittr)

speke_temp <- read.table("data/speke/speke.txt") %>% 
  select(1:2) %>% 
  setNames(c("year", "temp")) %>% 
  mutate(type = rep(c("recon", "inst"), each = 16))   

speke_temp %>% group_by(type) %>% summarise(m = mean(temp))
ggplot(speke_temp, aes(x = year, y = temp, colour = type)) + 
  geom_point() + 
  geom_line() +
  
  scale_x_reverse()

speke_temp2 <- speke_temp %>% 
  mutate(year = round(year)) %>% 
  spread(key = type, value = temp)

speke_temp2 %>% ggplot(aes(x=inst, y = recon)) +
  geom_abline()+
  geom_point() +
  geom_smooth(method = "lm")

speke_temp2 %>% mutate(inst_r = resid(lm(inst ~ year)),
                  recon_r = resid(lm(recon ~ year))) %>% 
  ggplot(aes(x=inst_r, y = recon_r)) +
  geom_abline()+
  geom_point() +
  geom_smooth(method = "lm")


speke_temp2 %$% cor.test(inst, recon)
speke_temp2 %$% cor(inst, year)
speke_temp2 %$% cor(recon, year)

speke_temp2 %$% cor(resid(lm(inst ~ year)), 
               resid(lm(recon ~ year)))


data_frame(year = speke_temp2$year, diff = c(NA, diff(year))) %>% ggplot(aes(x = year, y = diff)) + geom_point()

##
read_station <- function(file){
  x <- read.table(file, skip = 5, header = FALSE)
  names(x) <- c("year", month.abb)
  x
}

#station data
valley <- read_station("data/speke/valley_t3302.dat")
bidston <- read_station("data/speke/bidston_t3316.1.dat")
bidston[bidston == -999.9] <- NA

ggplot(speke_temp2, aes(x = year, y = inst)) + 
  geom_point(colour = "red") +
  geom_line(colour = "red")+
  geom_line(data = valley, mapping = aes(x = year, y = Jul), inherit.aes = FALSE) + 
  geom_smooth(data = valley, mapping = aes(x = year, y = Jul), inherit.aes = FALSE, span = 0.3) + 
  # geom_line(data = bidston, mapping = aes(x = year, y = Jul), inherit.aes = FALSE, col= 4) + 
  theme(legend.position = "none") +
  xlim(min(valley$year), max(valley$year))
  

valley %>% filter(between(year, 1989 - 2, 1989 + 2)) %>% 
  summarise(Jul = mean(Jul))
speke2 %>% filter(year == 1989)

b_v <- inner_join(valley, bidston, by = "year", suffix = c(".v", ".b")) %>% 
  select(year, matches("Jul")) 

b_v %$% cor(Jul.v, Jul.b, use = "pair")
b_v %>% filter(!is.na(Jul.b)) %>% select(-year) %>% colMeans()


cet_v <- inner_join(valley, cet, by = "year", suffix = c(".v", ".c")) %>% 
  select(year, matches("Jul"))

cet_v %$% cor(Jul.v, Jul.c, use = "pair")

cet_b <- inner_join(bidston, cet, by = "year", suffix = c(".v", ".b")) %>% 
  select(year, matches("Jul"))

cet_b %$% cor(Jul.v, Jul.b, use = "pair")

norway_train <- vegan::read.cep("data/brooks_birks/NCHIR7.ENV")
norway_spp <- vegan::read.cep("data/brooks_birks/NCHIR8%.CEP")

names(norway_spp)


july <- raster::raster("data/worldclim/wc2.0_10m_tavg_07.tif")
speke_location <- data_frame(long = -(2 + 52/60 + 23/3600), lat = 53 + 20/60 + 19/3600)
coordinates(speke_location) <- ~ long + lat

raster::extract(july, speke_location)

norway_train$Julyt %>% range()

speke_digitised <- read_csv("data/speke/SpekeHall_top16.csv", skip = 4) %>% 
  slice(-1) %>% 
  rename(sample = `Sample number`) %>% 
  rename_all(gsub, pattern = "-", replacement = "_") %>% 
  select_if(~sum(.) > 0)

speke_digitised %>% select(-sample) %>% rowSums()
speke_digitised %>% select(-sample) %>% estimate_n(digits = 0) %>% mutate(n = 1:n()) %>% arrange(est_n)

strat.plot(d = as.data.frame(speke_digitised[, -1]), yvar = speke_digitised$sample, scale.percent = TRUE, plot.line = FALSE, y.rev = TRUE, lwd.bar = 2)

speke_dict <- read_csv('name, code
Orthocladius_consobrinus_type, Orthcon
Eukiefferiella, Eukieffe
Tanytarsini_undiff, UnidTan
Cricotopus_sylvestris_type, Cricsyl
Cricotopus(Isocladius), 
Psectrocladius_sordidella_type, Psecsor 
Paracladopeima, Paraclad
Procladius, Procladi
Paratanytarsus_penicillatus_type, 
Microtendipes_pendellus, MicroP
Cladopelma, Cladopel
Chironomus_anthrocinus_type, Chirant
Ablabesmyia, Ablabesm
Dicrotendipes, Dicroten
Polypedilum_nubeculosus_type, Polynub
Corynoneura, 
Chironomus_plumosus_type, Chirplu
Parachironomus_vaus_type, 
Psectrotanypus, 
Endochironomus_albipennis_type, Endoalb
Glyptotendipes, Glyptote
Tanypus, Tanypus
')

'Abiskomy Acamptoc Allopsec Brillia Bryophae Chaetocl  Chirlar Cladman Cladspu Coryamb Corycor Coryedw Corylac Coryoli Coryscu Cric285 Orthtri Cricind Cricpul CricoA CricoB CricoC CricoD Cryptoch Demicryp Diamabe Diamspi Diplocla Endoimp Endoten Eukiilk  Euorthoc Georthoc GlyptA  Guttipel Heteapi Hetegri Hetemae Hetemar Hetesub Hydrjoh ZalutA Hydrnar Hydrbro Krenopel Krenosmi Labrundi Lauterbo Limnophy Macropel Mesocric Mesopsec Metrfus Metrhyg MicroP Microbi Microin Microra Microsp Microchi Microten Monodiam Monopelo Nanoclad Natarsia Omisus Orthund Ortho3 Orthocla Pagastie Para368 Paraaus Parabat Paracor Parafen Paranig Parapen Parachae Parachir Paraclop Parameri Paraphae Paratany Paratend Parokie Penta1 Penta4 Phaenops Polycon Polyped Potthast  Prodiame Propsilo Protanyp Psecsep Pseudoch Pseudodi Pseudops Pseudort Pseudosm Rheocric Rheotany Sergenti Smittia Stempell Stenochi Stictoch Synortho Tanychi Tanylu2 Tanylug Tanynos Tanypal Tanyspn  TanytB TanytC Thiemtl Thienmal Thienmia Tribelos UnidPen  Zalulin Zaluzal Zavrelia Zavrelim'

speke_dict %>% filter(is.na(code))
speke_dict <- speke_dict %>% filter(!is.na(code))

names(speke_digitised) <- plyr::mapvalues(names(speke_digitised), from = speke_dict$name, to = speke_dict$code)

mod <- WAPLS(sqrt(norway_spp), norway_train$Julyt) %>% crossval()
performance(mod)
nrow(norway_spp)

data_frame(sample = speke_digitised$sample,
           pred = predict(mod, sqrt(speke_digitised %>% select(-sample)))$fit[, "Comp02"]) %>% 
  ggplot(aes(x = sample, y = pred)) + geom_point()


ggplot(bind_cols(norway_train, norway_spp), aes(x = Julyt, y = MicroP)) + geom_point()


speke_digitised %>% select(-sample) %>% sqrt() %>% decorana

speke_digitised %>% select(-sample) %>% sqrt() %>% rda() %>% screeplot(bstick = TRUE)

tt <- speke_temp2 %>% arrange(desc(year))
ord <- rda(speke_digitised %>% select(-sample) %>% sqrt()  ~ inst, tt)
summary(ord) %>% head
ord %>% anova()
