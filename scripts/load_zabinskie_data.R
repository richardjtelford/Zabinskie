#load libraries
library("readxl")
library("dplyr")

## ---- load_Zabinskie_data

fname <- "data/zabinskie2015cit.xls"

#modern spp
spp_all <- read_excel(fname, sheet = "Training species")

#modern environment
env_all <- read_excel(fname, sheet = "Training temperature")

#check siteIDs match
assertthat::assert_that(assertthat::are_equal(spp_all$X__1, env_all$Name))


#remove low count sites from modern data
lowCount <- c("GOR", "KOS", "LEK", "SAL", "SZE", "SZOS", "TRZ", "WAS", "ZAB")
env <- env_all %>% filter(!Name %in% lowCount) 
spp <- spp_all %>% filter(!X__1 %in% lowCount) %>% select(-X__1)

spp_all <- spp_all %>% select(-X__1)

# remove rare species from calibration set
spp <- spp[, colSums(spp > 0) > 0]# remove taxa only in low count sites - cannot find evidence of stricter inclusion criteria

sites <- env %>% select(Lake = Name) %>% 
  mutate(source = c(rep("Poland", 39), rep("L2008", 13), rep("L06", 52), rep("L2008", 8))) %>% 
  mutate(Lake  = case_when(
    Lake == "Lake 29" ~  "Lake29", 
    Lake == "lake25" ~ "Lake25",
    TRUE ~ Lake))

sites_all <- env_all %>% select(Lake = Name) %>% 
  mutate(source = c(rep("Poland", 39 + length(lowCount)), rep("L2008", 13), rep("L06", 52), rep("L2008", 8))) %>% 
  mutate(Lake  = case_when(
    Lake == "Lake 29" ~  "Lake29", 
    Lake == "lake25" ~ "Lake25",
    TRUE ~ Lake))

#make env a vector to simplify later code
env <- env$Temp
env_all <- env_all$Temp




#fossil spp
fos <- read_excel(fname, sheet = "Chironomids Zabinsk percentages") %>%
  rename(year = X__1) 

chron <- fos %>% select(year)
fos <- fos %>% select(-year, -`Nb head capsules`)

#fossil_counts
fos_counts <- read_excel(fname, sheet = "Chironomids Zabinskie counts") %>% 
  select(-X__1, -Total) %>% 
  mutate_all(round, digits = 1)#remove spurious digits

#reconstruction
recon <- read_excel(fname, sheet = "Reconstruction ") %>% 
  rename(year = `Dates AD`, temperature = `Chironomid-inferred mean August temperature`)


# #instrumental - digitised
# instrumental <- read.table("data/instrumental.txt") %>% 
#   select(year = V1, Aug = V2) %>% 
#   mutate(year = round(year))


# instrumental from xml
temperature_chart <- read_xml("data/chart1.xml")

bad_format <- temperature_chart %>% 
  xml_find_all("//c:numCache") %>% 
  xml_find_all("//c:v") %>% 
  xml_double()

bad_format <- bad_format[!is.na(bad_format)]
instrumental_temperature <- bad_format %>% matrix(ncol = 4) %>%
  as_data_frame() %>% 
  select(year = V1, old = V2, new = V4)

