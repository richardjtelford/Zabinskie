## ---- zhang_fossil
zhang_import <- function(){
  f = file_in("data/zhang_et_al_2017/Zhang et al 2017_Climate of the Past_dataset.xlsx")
  zhang <- read_excel(f, sheet = "Key chironomid taxa of Tiancai", skip = 2)
  meta <- zhang[, 1:3]
  fos <- zhang[, -(1:3)]  
  recon <- read_excel(f, sheet = "Tiancai chironomid-inferred MJT", skip = 2)
  
  inst <- read_excel(f, sheet = "Lijiang station data", skip = 1)
  
  zhang <- list(recon = recon, inst = inst, fos = fos, meta = meta)
  return(zhang)
}



# min_pc <- apply(fos, 1, function(x) min(x[x > 0]))
# 
# ggplot(meta, aes(x = `Year (AD)`, y = 100/min_pc)) + 
#   geom_point()
# 
# ggplot(meta, aes(x = `Year (AD)`, y = rowSums(fos))) + 
#   geom_point()

## ---- zhang_recon
zhang_calc_cor <- function(zhang_data){
  inst <- zhang_data$inst
  recon <- zhang_data$recon
  
  inst_smo <- inst %>% 
    mutate(
      smoothed_July_T_L = rollmean(`Mean July temperature (°C)`, k = 3, fill = NA, align = "center"))#Centred to match fig6a 
  
  #matches fig6a with smooth
  fig6a <- ggplot(recon, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
    geom_line() +
    geom_line(data = inst_smo, mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red") +
    geom_line(data = inst_smo, mapping = aes(y = scale(`smoothed_July_T_L`, scale = FALSE)), colour = "grey50") +
    xlim(1930, NA)
  
  #matches fig 6e
  fig6e <- ggplot(recon, aes(x = `Year (AD)`, y = scale(`Mean July temperature (°C)`, scale = FALSE))) + 
    geom_line() +
    geom_line(data = inst %>% filter(`Year (AD)` %in% seq(1951, 2020, 3)), mapping = aes(y = scale(`Mean July temperature (°C)`, scale = FALSE)), colour = "red") +
    labs(y = "Mean July temperature anomaly °C ") +
    xlim(1930, NA)
  
  correct_cor <- inst_smo %>% 
    left_join(recon %>% 
                slice(-c(2, 3, 5)) %>% #multiple reconstructions for 2007, 2006
                mutate(`Year (AD)` = round(`Year (AD)`)),
              by  = "Year (AD)") %$%
    cor.test(smoothed_July_T_L, `Mean July temperature (°C).y`, use = "pair")
  
  
  
  incorrect_smo <- inst %>% 
    filter(`Year (AD)` %in% seq(1951, 2020, 3)) %$% 
    approx(x = `Year (AD)`, y = `Mean July temperature (°C)`, xout = 1951:2014) %>% 
    as.data.frame()                             
  
  incorrect_cor <- incorrect_smo %>% left_join(recon %>% 
                          slice(-c(2, 3, 5)) %>% 
                          mutate(`Year (AD)` = round(`Year (AD)`)),
                        by  = c(x = "Year (AD)")) %$% 
    cor.test(y, `Mean July temperature (°C)`, use = "pair")
  
  list(incorrect_cor = incorrect_cor, correct_cor = correct_cor)
}