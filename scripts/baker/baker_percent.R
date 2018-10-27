#baker lake

#import training set data downloaded from https://www.polardata.ca/pdcsearch/
#https://www.polardata.ca/pdcsearch/PDC_Metadata_Data_Download.ccin?action=displayPDCDownloadDataPage&ccin_ref_number=12506

baker_spp <- read_csv("data/baker/CCIN12506_20150911_CJFAS-2011-076_species.csv") %>% 
  filter(Lake_Name != "RA21(19#2)") %>% #duplicate sample
  arrange(Lake_Name)
baker_env <- read_csv("data/baker/CCIN12506_20150911_CJFAS-2011-076_water.csv", skip = 1) %>% 
  slice(-1) %>% #units
  mutate_at(vars(-Site, -Type), as.numeric) %>% 
  rename(Lake_Name = Site) %>% 
  arrange(Lake_Name)

stopifnot(identical(baker_env$Lake_Name, baker_spp$Lake_Name))

#import fossil data
baker_fosR <- read_excel("data/baker/thesisMedeiros-w3may7.xls", sheet = "core-R")


#names(baker_spp) %>% paste0(collapse= ",\n") %>% cat()
#names(baker_fosR)
dictionary <- read_csv(
"name, code
Abiskomyia, Abisk
Hydrobaenus Oliveridia spp., Hydrob
Heterotanytarsus, Hetero
Synorthocladius, Synth
Psectrocladius Allopsectrocladius, ParaAl
Psectrocladius Monopsectrocladius, Psecmo
Psectrocladius Psectrocladius spp., Psecp
Parakiefferiella sp. A, ParakA
Parakiefferiella sp. B., ParakB
Parakiefferiella nigra, ParakN
Parakiefferiella triquetra, ParaTQ
Pseudodiamesa, Pseudo
Zalutschia sp. C, ZalutC
Zalutschia lingulata pauca, ZalutL
Zalutschia zalutschicola, ZalutZ
Heterotrissocladius, Htot1
Mesocricotopus, Mesoc
Nanocladius, Nanoc
Georthocladius spp, Geoor
Cricotopus intersectus, Cinter
Orthocladius type i, Orthoi
Cricotopus tremulus group, CricTr
Cricotopus (Isocladius) trifasciatus, Crictrif
Cricotopus type P, CricP
Cricoptopus sylvestris, CricSy
Cricotopus cylindraceus, CricCy
Cricotopus bicintus, CricBi
Paracladius, Paracl
Smittia, Smittia
Paracricotopus, Paracri
Chaetocladius, Chaet
Corynoneura arctica, CorArC
Corynoneura sp. A, CoryA
Eukieffferiella Tvetenia spp., Eukif
Limnophyes, Limno
Orthocladius trigonolabis, Orthotr
Orthocladius type S, OrthoS
Orthocladinae sp. 2, Ortho2
Orthocladius consobrinus, Orthoc
Orthocladius lignicola, OrthoL
Orthocladius rivulorum, OrthoR
Diplocladius, Diplo
Metriocnemus, Metroc
Thienemanniella type E, ThienE
Tanytarsus lactescens, Tanylac
Tanytarsus undiff.,
Tanytarsus no spur, TanyNoSp
Tanytarsus mendax, TanyMend
Tanytarsus lugens, TanyLug
Tanytarsus pallidicornis, Tanypall
Tanytarsus chinyensis group, Tanychy
Micropsectra contracta, Mcont
Cladotanytarsus mancus, Cladman
Micropsectra padula, Mpadu
Micropsectra insignilobus, Minsig
Micropsectra radialus, MicroR
Paratanytarsus, ParaT
Corynocera ambigua, Cambig
Corynocera oliveri, Coryoliv
Zavrelia Stempellinella spp., ZavStemp
Constempellina,
Stempellina, Stmpina
Protanypus, Protany
Diamesa, Diamesa
Monodiamesa, Monodia
Cladopelma, Cpelma
Dicrotendipes, Dicrot
Microtendipes, Microt
Sergentia, Serg
Cryptochironomus, Crypto
Endochironomus, Endo
Pseudochironomus, Psedoc
Chironomus, Chiro
Parachironomus, Parach
Pagastiella, Pagast
Glycotendipes, Glycot
Stictochironomus, Sticto
Polypedilum, Polyp
Harneshia, Harnesh
Chironomini larvula, Clarvula
Ablabesmyia,
Arctopelopia / Thienemannimyia, ArcT
Procladius, Proclad
Chaoborus trivittatus, Chaob")

# ""  "CladA" "Consthiol"    "Dero"     "paraten"  "Psecme"    "Tanyglab"  "Tanynum"   "Trissoc"   "ZalutM" 

## MicroA     ParaAl identical is thesis file.
#sapply(baker_spp[, -1], cor, baker_sppx$MicroA) %>% sort()

dictionary %>% filter(!is.na(code)) %>% 
  group_by(name, code) %>% 
  do(data_frame(cr = cor(baker_spp[, .$name], baker_sppx[, .$code]) %>% as.vector())) %>% 
  arrange(cr)
  


## species lists
setdiff(names(baker_fosR), names(baker_spp))
setdiff(names(baker_spp), names(baker_fosR))
identical(sort(baker_spp$X__1), sort(baker_env$CodeName))


mod <- WAPLS(select(baker_spp, -X__1) %>% sqrt(), baker_env$MSSWT) %>% 
  crossval()

performance(mod) 

pred <- predict(mod, select(baker_fosR, -Depth) %>% sqrt())$fit %>% 
  as_data_frame() %>% 
  mutate(Depth = baker_fosR$Depth)

ggplot(pred, aes(x = Depth, y = Comp02)) +
  geom_point() +
  geom_line() +
  coord_flip() +
  scale_x_reverse()

select(baker_fosR, -Depth) %>% 
  sqrt() %>% 
  decorana() %>% 
  scores(choice = 1) %>% 
#  acf()
  ar()

