#baker lake

# #import training set data downloaded from https://www.polardata.ca/pdcsearch/
# #https://www.polardata.ca/pdcsearch/PDC_Metadata_Data_Download.ccin?action=displayPDCDownloadDataPage&ccin_ref_number=12506
# 
# baker_spp <- read_csv("data/baker/CCIN12506_20150911_CJFAS-2011-076_species.csv") %>% 
#   filter(Lake_Name != "RA21(19#2)") %>% #duplicate sample
#   arrange(Lake_Name)
# 
# baker_env <- read_csv("data/baker/CCIN12506_20150911_CJFAS-2011-076_water.csv", skip = 1) %>% 
#   slice(-1) %>% #units
#   mutate_at(vars(-Site, -Type), as.numeric) %>% 
#   rename(Lake_Name = Site) %>% 
#   arrange(Lake_Name)
# 
# stopifnot(identical(baker_env$Lake_Name, baker_spp$Lake_Name))

#import fossil data
excel_sheets("data/baker/thesisMedeiros-w3may7.xls")
baker_fosR <- read_excel("data/baker/thesisMedeiros-w3may7.xls", sheet = "core-R")

baker_spp0 <- read_excel("data/baker/thesisMedeiros-w3may7.xls", sheet = "TS-R") %>% 
  rename(lake_name = ...1)
baker_env0 <- read_excel("data/baker/thesisMedeiros-w3may7.xls", sheet = "env") %>% 
  rename(lake_name = CodeName)

#fix RS21 to RA19
baker_env <- baker_env0 %>%
  filter(lake_name != "RA21") %>%
  arrange(lake_name)
baker_spp <- baker_spp0 %>%  
  group_by(lake_name) %>%
  slice(1) %>% #remove duplicate
  ungroup() %>% 
  arrange(lake_name) 


#remove BL01 (Baker Lake) and AV01 (saline)
baker_spp <- baker_spp %>% 
  filter(!lake_name %in% c("BL01", "AV01"))  %>% 
  select_if(~max(.) > 2) #remove rare taxa
baker_env <- baker_env %>% 
  filter(!lake_name %in% c("BL01", "AV01"))

#remove TanyNoSp
baker_spp <- baker_spp %>% select(-TanyNoSp)
baker_fosR <- baker_fosR %>% select(-TanyNoSp)



stopifnot(all.equal(baker_env$lake_name, baker_spp$lake_name))
baker_spp <- baker_spp %>% select(-lake_name)

#transfer function model
baker_mod <- WAPLS(sqrt(baker_spp), baker_env$MSSWT) %>% 
  crossval()

performance(baker_mod) 

baker_pred <- predict(baker_mod, select(baker_fosR, -Depth) %>% sqrt())$fit %>% 
  as_tibble() %>% 
  mutate(Depth = baker_fosR$Depth)

baker_year <- c(129, 127, 125, 120, 116, 109, 103, 93, 77, 66, 62, 60, 51, 38, 22, 4, 2, 0)/133 * 60 + 1950#from figure 8
baker_year <- round(baker_year)

baker_pred <- baker_pred %>% 
  mutate(year = c(baker_year, rep(NA, nrow(.) - length(baker_year))))

ggplot(baker_pred, aes(x = Depth, y = Comp02)) +
  geom_point() +
  geom_line() +
  coord_flip() +
  scale_x_reverse()

ggplot(baker_pred %>% filter(!is.na(year)), aes(x = year, y = Comp02)) +
  geom_point() +
  geom_line() +
  coord_flip() 

baker_pred %>% filter(!is.na(year)) %>% left_join(baker_annual_anom ) %>% ggplot(aes(x = anomaly, y = Comp02)) + geom_point() 

baker_pred %>% filter(!is.na(year)) %>% left_join(baker_annual_anom ) %$% cor.test(anomaly, Comp02, use = "pair")

baker_pred %>% filter(!is.na(year)) %>% left_join(baker_annual_anom ) %$% cor.test(smo_anomaly, Comp02, use = "pair")
baker_pred %>% filter(!is.na(year)) %>% left_join(baker_annual_anom ) %$% cor.test(smo2, Comp02, use = "pair")


#residual distances
resLen <- analogue::residLen(
  X = sqrt(baker_spp), 
  env = baker_env$MSSWT,
  passive = baker_fosR %>% select(-Depth) %>% sqrt(),
  method = "cca"
)

autoplot(resLen, df = baker_pred, x_axis = "Depth")

baker_AD <- analogue_distances(baker_spp, baker_fosR %>% select(-Depth))

autoplot(baker_AD, df = baker_pred, x_axis = "Depth")

randomTF(sqrt(baker_spp), baker_env$MSSWT, sqrt(baker_fosR %>% select(-Depth)), fun = WAPLS, col = 2)


##chronology
baker_chron <- read_delim(delim = ",", trim_ws = TRUE,  "year, depth
  2007.5940860215, 0.5
  2007.1267821205, 1.0
  2006.3231645396, 1.5
  2004.9133147171, 2.0
  2002.5472906006, 2.5
  1999.4255005048, 3.0
  1996.4481418173, 3.5
  1992.0665546608, 4.0
  1985.2794815177, 4.5
  1980.2603825759, 5.0
  , 5.5
  1976.5786636763, 6.0
  , 6.5 
  1966.9636439156, 7.0
  1959.899754287, 7.5
  1951.5832857133, 8.0
  , 8.5 
  1949.6708620126, 9.0
  1943.0700450582, 9.5
  , 10.0 
  1932.6273367543, 10.5
  1918.97627194, 11.0
  , 11.5 
  1906.1870630408, 12.0
  1882.7459662099, 12.5"
)

with(na.omit(baker_chron), approx(depth, year, xout = baker_chron$depth)) %>% as_tibble() %>% rename(depth = x, year = y) %>% 
ggplot(aes(depth, year)) + geom_point()

