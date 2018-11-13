speke_import <- function(){
  if(!file.exists(file_in("data/speke/temperature.xls"))){
    return("Data will be archived by the authors of the Speke Hall Lake paper in 2019")
  }
  
  #import data
  speke_env <- read_excel(file_in("data/speke/temperature.xls"))
  speke_spp <- read_excel(file_in("data/speke/species.xls"))
  speke_fos <- read_excel(file_in("data/speke/core.xls"))
  
  #split off chron
  speke_chron <- speke_fos %>% 
    select(CodeNum:FullName) %>% 
    rename(year = CodeNum)
  
  speke_spp <- speke_spp %>% select(-(CodeNum:FullName))
  speke_fos <- speke_fos %>% select(-(CodeNum:FullName))
  
  # remove outliers following Lang et al.
  lowCount <- c(13, 111, 134, 139)#outliers, probably not low count
  
  speke_spp <- speke_spp %>% 
    filter(!speke_env$CodeNum %in% lowCount)
  speke_env <- speke_env %>% 
    filter(!CodeNum %in% lowCount)
  
  #remove rare taxa
  speke_spp <- speke_spp %>% select_if(~max(.) > 2)
  
  #trasform
  speke_fos1 <- decostand(speke_fos, method="hellinger")
  speke_spp1 <- decostand(speke_spp, method="hellinger")
  
  speke <- list(speke_env = speke_env,
       speke_spp = speke_spp,
       speke_fos = speke_fos,
       speke_chron = speke_chron,
       speke_spp1 = speke_spp1,
       speke_fos1 = speke_fos1)
  return(speke)
}


# #mod
# speke_mod <- WAPLS(speke_spp1, speke_env$July) %>% 
#   crossval()
# performance(speke_mod)
# rand.t.test(speke_mod)
# 
# autoplot(speke_mod, npls = 3)
# 
# speke_pred <- data_frame(
#   year = speke_chron$CodeNum,
#   year1 = lead(year),
#   pred = predict(speke_mod, speke_fos1)$fit[, "Comp03"])
# 
# speke_pred %>% ggplot(aes(x = year1, y = pred)) + 
#   geom_point() + 
#   geom_line() + 
#   scale_x_reverse() + 
#   geom_line(speke_temp2, mapping = aes(x = year, y = recon), colour = "red")


#analogue distances
speke_analogue_distances <- function(speke){
  if(class(speke) == "character"){
    return(speke)
  }
  with(speke, {
    speke_AD <- analogue_distances(speke_spp/100, speke_fos/100)  
    autoplot(speke_AD, df = speke_chron, x_axis = "year") + 
      labs(x = "Year CE", y = "Squared chord distance", fill = "Analogue quality")
  })
}

#residual length
speke_residual_length <- function(speke){
  if(class(speke) == "character"){
    return(speke)
  }
  with(speke, {
    rl <- analogue::residLen(speke_spp1, speke_env$July, speke_fos1, method = "cca")
    
    autoplot(rl, df = speke_chron, x_axis = "year", categories = c("Good", "Poor", "Very Poor")) +
      labs(y = expression(Squared~chi^2~residual~distance), x = "Year CE", fill = "Goodness of fit")
  })
}

speke_randomtf <- function(speke){
  if(class(speke) == "character"){
    return(speke)
  }
  with(speke, {
    randomTF(speke_spp1, speke_env$July, speke_fos1, fun = WAPLS, col = 3)
  })
}


#
# est_n <- estimate_n(speke_fos, digits = 2) %>% 
#   mutate(n = 1:n()) %>% 
#   arrange(est_n)
# 
# est_n %>% 
#   summarise(s = sum(est_n), m = mean(est_n))



  


# #coverage plot
# speke_n <- data_frame(taxa = names(speke_spp), 
#                       max = sapply(speke_spp, max), 
#                       n2 = Hill.N2(speke_spp))
# speke_fosn <- data_frame(taxa = names(speke_fos), 
#                       max = sapply(speke_fos, max), 
#                       n2 = Hill.N2(speke_fos)) %>% 
#   filter(max > 0) 
# 
# speke_max_n <- speke_n %>% 
#   full_join(speke_fosn, by = "taxa", suffix = c("_spp", "_fos")) %>% 
#   replace_na(list(max_spp = 0, n2_spp = 0, max_fos = 0, n2_fos = 0))
# 
# speke_max_n %>% 
#   ggplot(aes(x = max_spp, y = max_fos, colour = n2_spp > 5)) + 
#   geom_point() +
#   geom_abline() +
#   geom_text(data = filter(speke_max_n, max_fos > 2 * max_spp), aes(label = taxa, colour = n2_spp > 5))

# 
# speke_dca <- decorana(speke_fos1)
# speke_dca
# scores(speke_dca) %>% as.data.frame() %>% bind_cols(speke_pred) %>% select(-year, -year1) %>% cor()
# 
# speke_dca <- decorana(speke_fos1[1:16,])
# speke_dca
# scores(speke_dca) %>% as.data.frame() %>% bind_cols(speke_pred[1:16, ]) %>% select(-year, -year1) %>% cor()
#   

