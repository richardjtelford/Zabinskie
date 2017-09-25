library("tidyverse")
library("readxl")
library("zoo")

zhang <- read_excel("Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Key chironomid taxa of Tiancai", skip = 2)
zhang
zhang_meta <- zhang[, 1:3]
zhang_spp <- zhang[, -(1:3)]
dim(zhang_spp)

min_pc <- apply(zhang_spp, 1, function(x) min(x[x > 0]))

ggplot(zhang_meta, aes(x = `Year (AD)`, y = 100/min_pc)) + 
  geom_point()

ggplot(zhang_meta, aes(x = `Year (AD)`, y = rowSums(zhang_spp))) + 
  geom_point()

# recon & instrumenal

recon_zhang <- read_excel("Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Tiancai chironomid-inferred MJT", skip = 2)

met_zhang <- read_excel("Zhang et al 2017_Climate of the Past_dataset.xlsx", sheet = "Lijiang station data", skip = 1)

ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_zhang, mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red")


met_smo_zhang <- met_zhang %>% 
  mutate(smoothed_July_T = rollmean(`Mean July temperature (°C)`, k = 3, fill = NA, align = "left"))

ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_smo_zhang, mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "pink") +
  geom_line(data = met_smo_zhang, mapping = aes(y = scale(`smoothed_July_T`, scale = FALSE)), colour = "red")

ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_zhang %>% filter(`Year (AD)` %in% seq(1951, 2020, 3)), mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red") 


ggplot(recon_zhang, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
  geom_line() +
  geom_line(data = met_zhang %>% filter(`Year (AD)` %in% seq(1951, 2020, 3)), mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red") +
  labs(y = "Mean July temperature anomaly °C ")

met_smo_zhang %>% 
  left_join(recon_zhang %>% 
              slice(-c(2, 3, 5)) %>% 
              mutate(`Year (AD)` = round(`Year (AD)`)),
            by  = "Year (AD)") %$%
  cor.test(smoothed_July_T, `Mean July temperature (°C).y`, use = "pair")

bad_smo <- met_zhang %>% filter(`Year (AD)` %in% seq(1951, 2020, 3)) %$% approx(x = `Year (AD)`, y = `Mean July temperature (°C)`, xout = 1951:2014) %>% as.data.frame()                             
bad_smo %>% left_join(recon_zhang %>% 
                        slice(-c(2, 3, 5)) %>% 
                        mutate(`Year (AD)` = round(`Year (AD)`)),
                      by  = c(x = "Year (AD)")) %$% 
  cor.test(y, `Mean July temperature (°C)`, use = "pair")
