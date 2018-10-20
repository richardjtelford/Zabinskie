## ---- zhang_fossil
zhang <- read_excel("data/zhang_et_al_2017/Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Key chironomid taxa of Tiancai", skip = 2)
zhang
zhang_meta <- zhang[, 1:3]
zhang_spp <- zhang[, -(1:3)]
dim(zhang_spp)

min_pc <- apply(zhang_spp, 1, function(x) min(x[x > 0]))

ggplot(zhang_meta, aes(x = `Year (AD)`, y = 100/min_pc)) + 
  geom_point()

ggplot(zhang_meta, aes(x = `Year (AD)`, y = rowSums(zhang_spp))) + 
  geom_point()

## ---- zhang_recon
recon_zhang <- read_excel("data/zhang_et_al_2017/Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Tiancai chironomid-inferred MJT", skip = 2)

met_zhang <- read_excel("data/zhang_et_al_2017/Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Lijiang station data", skip = 1)

ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_zhang, mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red")


met_smo_zhang <- met_zhang %>% 
  mutate(
    smoothed_July_T_L = rollmean(`Mean July temperature (°C)`, k = 3, fill = NA, align = "left"))#because data are sorted to have 2014 in the first row, this is actually aligned right from a time series perspective. IE tests is recon correlated with previous 3 years

ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_smo_zhang, mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "pink") +
  geom_line(data = met_smo_zhang, mapping = aes(y = scale(`smoothed_July_T_L`, scale = FALSE)), colour = "red")


ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_zhang %>% filter(`Year (AD)` %in% seq(1951, 2020, 3)), mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red") +
  labs(y = "Mean July temperature anomaly °C ")

zhang_smo_cor <- met_smo_zhang %>% 
  left_join(recon_zhang %>% 
              slice(-c(2, 3, 5)) %>% #multiple reconstructions for 2007, 2006
              mutate(`Year (AD)` = round(`Year (AD)`)),
            by  = "Year (AD)") %$%
  cor.test(smoothed_July_T_L, `Mean July temperature (°C).y`, use = "pair")



bad_smo <- met_zhang %>% 
  filter(`Year (AD)` %in% seq(1951, 2020, 3)) %$% 
  approx(x = `Year (AD)`, y = `Mean July temperature (°C)`, xout = 1951:2014) %>% 
  as.data.frame()                             

bad_smo %>% left_join(recon_zhang %>% 
                        slice(-c(2, 3, 5)) %>% 
                        mutate(`Year (AD)` = round(`Year (AD)`)),
                      by  = c(x = "Year (AD)")) %$% 
  cor.test(y, `Mean July temperature (°C)`, use = "pair")
