## ---- load_Zabinskie_data

# fname <- "data/zabinskie2015cit.xls"
# 
# #modern spp
# spp_all <- read_excel(fname, sheet = "Training species")
# 
# #modern environment
# env_all <- read_excel(fname, sheet = "Training temperature")
# 
# #check siteIDs match
# assertthat::assert_that(assertthat::are_equal(spp_all$...1, env_all$Name))


#remove low count sites from modern data
#lowCount <- c("GOR", "KOS", "LEK", "SAL", "SZE", "SZOS", "TRZ", "WAS", "ZAB")
#env <- env_all %>% filter(!Name %in% lowCount) 
# spp <- spp_all %>% filter(!...1 %in% lowCount) %>% select(-...1)
# 
# spp_all <- spp_all %>% select(-...1)
# 
# # remove rare species from calibration set
# spp <- spp[, colSums(spp > 0) > 0]# remove taxa only in low count sites - cannot find evidence of stricter inclusion criteria

zabinskie_sites <- function(env0){
  sites <- env0 %>% select(Lake = Name) %>% 
    mutate(source = c(rep("Poland", 39), rep("L2008", 13), rep("L06", 52), rep("L2008", 8))) %>% 
    mutate(Lake  = case_when(
      Lake == "Lake 29" ~  "Lake29", 
      Lake == "lake25" ~ "Lake25",
      TRUE ~ Lake))
  sites
}

zabinskie_all_sites <- function(env_all0, lowCount){
  sites_all <- env_all0 %>% select(Lake = Name) %>% 
  mutate(source = c(rep("Poland", 39 + length(lowCount)), rep("L2008", 13), rep("L06", 52), rep("L2008", 8))) %>% 
  mutate(Lake  = case_when(
    Lake == "Lake 29" ~  "Lake29", 
    Lake == "lake25" ~ "Lake25",
    TRUE ~ Lake))
  sites_all
}

# #make env a vector to simplify later code
# env <- env$Temp
# env_all <- env_all$Temp




#fossil spp
zabinskie_fossil_percent <- function(zabinskie_excel_file){
  fos <- read_excel(zabinskie_excel_file, sheet = "Chironomids Zabinsk percentages") %>%
    rename(year = ...1) 
  fos <- fos %>% select(-year, -`Nb head capsules`)
  fos
}

#chronology
zabinskie_chronology <- function(zabinskie_excel_file){
  chron <- read_excel(zabinskie_excel_file, sheet = "Chironomids Zabinsk percentages") %>% 
    select(year = ...1) %>% 
    mutate(year = if_else(year == 1927, 1925, year)) #harmonise  chronology for stratigraphic data with reconstruction and instrumental data
  chron
}

#fossil_counts
zabinskie_fossil_counts  <- function(zabinskie_excel_file){
  fos_counts <- read_excel(zabinskie_excel_file, sheet = "Chironomids Zabinskie counts") %>% 
    select(-...1, -Total) %>% 
    mutate_all(round, digits = 1)#remove spurious digits
  fos_counts 
}

#reconstruction
zabinskie_reconstruction <- function(zabinskie_excel_file){
  recon <- read_excel(zabinskie_excel_file, sheet = "Reconstruction ") %>% 
    rename(year = `Dates AD`, temperature = `Chironomid-inferred mean August temperature`)
  recon
}


# instrumental from xml
zabiniskie_instrumental <- function(instrumental_file = "data/chart1.xml"){
  temperature_chart <- xml2::read_xml(instrumental_file)
  
  bad_format <- temperature_chart %>% 
    xml2::xml_find_all("//c:numCache") %>% 
    xml2::xml_find_all("//c:v") %>% 
    xml2::xml_double()
  
  bad_format <- bad_format[!is.na(bad_format)]
  instrumental_temperature <- bad_format %>% 
    matrix(ncol = 4) %>%
    as_tibble() %>% 
    select(year = V1, old = V2, new = V4)
  
  instrumental_temperature
}