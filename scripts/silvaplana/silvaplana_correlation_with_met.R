## ---- correlations_with_climate_jopl
jopl_join <- silva_july %>% 
  left_join(recon_jopl %>% mutate(merged_year = Year)) %>% 
  fill(merged_year, .direction = "down") %>% 
  group_by(merged_year) %>% 
  mutate(meanT = mean(Temperature)) %>% 
  filter(!is.na(JulyT))

#jopl_join %$% cor.test(Temperature, JulyT, use = "pair")
jopl_cor <- jopl_join %$% cor.test(meanT, JulyT, use = "pair")


## ---- correlations_with_climate_holocene
holocene_join <- silva_july %>% 
  left_join(recon_holocene %>% mutate(merged_year = Year)) %>% 
  fill(merged_year, .direction = "down") %>% 
  group_by(merged_year) %>% 
  mutate(meanT = mean(Temperature)) %>% 
  filter(!is.na(JulyT))

#holocene_join %$% cor.test(Temperature, JulyT, use = "pair")
holocene_cor <- holocene_join %$% cor.test(meanT, JulyT, use = "pair")


## ---- correlations_with_climate_qsr
qsr_join <- silva_july %>% 
  mutate(Temperature = gtools::running(Temperature, fun = mean, width = 3, pad = TRUE, align = "center")[-c(1, 140)]) %>% #should change to triangular filter
  left_join(recon_qsr %>% mutate(merged_year = Year))



qsr_cor <- qsr_join %$% cor.test(Temperature, JulyT, use = "pair")

