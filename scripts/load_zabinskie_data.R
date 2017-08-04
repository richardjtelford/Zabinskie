#load libraries
library("readxl")
library("dplyr")

## ---- load_Zabinskie_data

fname <- "data/zabinskie2015cit.xls"

#modern spp
spp <- read_excel(fname, sheet = "Training species")

#modern environment
env <- read_excel(fname, sheet = "Training temperature")

#remove low count sites from modern data
lowCount <- c("GOR", "KOS", "LEK", "SAL", "SZE", "SZOS", "TRZ", "WAS", "ZAB")
env <- env %>% filter(!Name %in% lowCount) %>% select(-Name)
spp <- spp %>% filter(!X__1 %in% lowCount) %>% select(-X__1)

# remove rare species from calibration set
spp <- spp[, colSums(spp > 0) > 0]# remove absent taxa - cannot find evidence of stricter inclusion criteria

#make env a vector to simplify later code
env <- env$Temp




#fossil spp
fos <- read_excel(fname, sheet = "Chironomids Zabinsk percentages") %>%
  rename(year = X__1)

chron <- fos %>% select(year)
fos <- fos %>% select(-year, -`Nb head capsules`)

#fossil_counts
fos_counts <- read_excel(fname, sheet = "Chironomids Zabinskie counts") %>% 
  select(-X__1, -Total)

#reconstruction
recon <- read_excel(fname, sheet = "Reconstruction ") %>% 
  rename(year = `Dates AD`, temperature = `Chironomid-inferred mean August temperature`)


#instrumental - digitised
instrumental <- read.table("data/instrumental.txt") %>% 
  select(year = V1, Aug = V2) %>% 
  mutate(year = round(year))

