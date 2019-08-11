

# #digitised luoto climate
# page5 <- readLines("data/luoto/outfile5u.pdf")
# start <- grep("/PlacedGraphic", page5)[1]
# end <- grep("[(Figur)20(e 4.)]TJ", page5, fixed = TRUE)
# 
# fig4 <- page5[start:end]
#   

#read digitised climate data
luoto_load_digitised_climate <- function(){
  f <- file_in("data/luoto/measured.txt")
  inst <- read.table(f) %>% 
    select(temp = V1, year = V2) %>% 
    mutate(year = round(year)) %>% 
    arrange(desc(year))
  return(inst)
}

# inst %>% ggplot(aes(x = year, y = temp)) +
#   geom_line() +
#   geom_point() +
#   coord_flip()





#digitise stratigraphy
luoto_digitise_stratigraphy <- function(){
  f <- file_in("data/luoto/outfile2.pdf")
  page4 <- readLines(f)
  start <- grep("/PlacedGraphic", page4)[2]
  end <- grep("[(Figur)20(e 2.)]TJ", page4, fixed = TRUE)
  
  page4 <- page4[start:end]
  
  fig2 <- page4[grepl("re$", page4)] %>%
    read.table(text = .) %>% 
    set_names(c("x", "y", "width", "height", "re"))
  
  taxa <- c("Abl_mon", "Dero", "Proc", "Thie", "Chi_ant", "Cla_lat", "Cryp", "Dic_ner", "End_alb", "End_imp", "End_ten", "Gly_pal", "Micr", "Mic_ped", "Pha_fla", "Pol_nub", "Seg_cor", "Trib", "Clad", "Cons", "Stem", "Stemlla", "Tan_chi", "Tan_gla", "Tan_lug", "Tan_men", "Tan_pal", "Tan_uid", "Cory_arc", "Cory_cor", "Cri_cyl", "Cri_int", "Nan_rec", "Par_bat", "Par_tri", "Pse_fla", "Pse_sep", "Pse_sor")

  fig2 <- fig2 %>% 
    filter(between(height, -2.4, -2.3)) %>% 
    mutate(taxa = factor(x, labels = taxa),#relabel
           percent = width/max(width) * 93,
           depth = (1 - (y - min(y))/(max(y) - min(y))) * 48, 
           depth = round(depth)
    )
    
# fig2 %>% 
#   ggplot(aes(x = x + width/2, y = depth, width = width, height = 1, fill = factor(x, levels = sample(unique(x))))) +
#   geom_tile(show.legend = FALSE) +
#   scale_y_reverse()

  fig2_fat <- fig2 %>% 
    select(-re, -height, -x, -width, -y) %>% 
    spread(key = taxa, value = percent, fill = 0) %>%
    as_tibble()

  return(fig2_fat)
}
  
    
# #estimate_n
# estimate_n(fig2_fat %>% select(-depth), digits = 2) %>% 
#   mutate(depth =  fig2_fat$depth) %>% 
#   arrange(est_n)
#         
# fig2 %>% filter(depth == 43)
# fig2 %>% filter(depth == 42)
# fig2 %>% filter(depth == 30)
# 
# fig2_fat %>% ggplot(aes(x = depth, y = Par_bat)) + 
#   geom_col() +
#   coord_flip() + 
#   scale_x_reverse()

#run calibration in time model
luoto_run_cit_mod <- function(fig2_fat, inst){
  mod <- fig2_fat %>% 
    select(-depth) %>% 
    slice(1:nrow(inst)) %>% 
    select_if(colSums(.) > 0) %>% 
    #sqrt() %>% 
    WA(x = inst$temp, tolDW = TRUE) %>% 
    crossval()
  return(mod)
}

#performance(mod)

#get optima from fitted model and from figures in papers.
luoto_get_optima <- function(mod){
  cit_optima <- coef(mod) %>% 
    as_tibble(rownames = "taxon") %>% 
    mutate(n2 = Hill.N2(fig2_fat %>% select(-depth))) %>% 
    filter(n2 > 5) %>% 
    arrange(taxon) 
  
  pub_optima <- read_csv(
  "taxon , cit_2017, cis_2017, cis_2014
  Abl_mon,    16.0,   13.8  , 
  Chi_ant,         ,        , 14.7
  Clad   ,  16.3   ,  14.25 , 14.4 
  Cla_lat,         ,        ,
  Cri_cyl,         ,        , 15.4
  Dero   ,         ,        , 
  Dic_ner,   16.4  ,  13.7  , 13.9
  Gly_pal,   17.0  ,   16.3 ,
  Par_bat, 15.7   ,  13.4   , 13.4
  Pol_nub,         ,        ,
  Proc   ,  16.9   ,  13.8  , 
  Pse_sor,  15.9   ,  13.2  , 13.1
  Seg_cor,  15.9   , 12.4   , 12.2
  Stem   ,  16.85  ,  15.2  , 
  Tan_gla,   16.65 ,  14.2  ,
  Tan_lug, 15.6    ,   12.2 , 12.0
  Tan_pal,         ,        , 14.0
  Thie   ,  16.15  ,   12.6 ,")
  
  optima <- cit_optima %>% left_join(pub_optima)
  optima
}

luoto_plot_optima <- function(optima){
   check_cit_optima <- optima %>%
    ggplot(aes(Optima, cit_2017, label = taxon)) +
    geom_point() +
    geom_text() +
    geom_abline()
  
  check_digitisation <- check_cit_optima + aes(cis_2017, cit_2017, label = taxon)
  cit_vs_cis2014 <- check_cit_optima + aes(cis_2014, Optima, label = taxon)
  check_cis_optima <- check_cit_optima + aes(cis_2014, cis_2017, label = taxon)
  
  return(list(check_cit_optima = check_cit_optima, 
              check_digitisation = check_digitisation, 
              cit_vs_cis2014 = cit_vs_cis2014, 
              check_cis_optima = check_cis_optima))
}

# pca <- fig2_fat %>% 
#   select(-depth) %>% 
#   sqrt() %>% 
#   downweight() %>% 
#   rda()
# summary(pca)[[6]]$import[,1:5]
# screeplot(pca, bstick = TRUE)
# 
# fig2_fat %>% 
#   select(-depth) %>% 
#   sqrt() %>%
#   downweight() %>% 
#   decorana()
# 
# plot(pca)
# rda <- rda(fig2_fat %>% select(-depth) %>% slice(1:nrow(inst)) %>% sqrt(), inst$temp)
# rda
# anova(rda)
# c_rda <- rda(fig2_fat %>% select(-depth) %>% slice(1:nrow(inst)) %>% sqrt(), inst$temp, inst$year)           
# 
# result <- tibble(
#   year = c(inst$year, seq(1827, 1785, length = 7)),
#   temp = c(inst$temp, rep(NA, 7)),
#   depth = fig2_fat$depth,
#   cit_fitted = c(fitted(mod)[, "WA.inv.tol"], rep(NA, 7)),
#   cit_pred = c(mod$predicted[, "WA.inv.tol"], rep(NA, 7)),
#   pc1 = scores(pca, choice = 1, display = "sites")[, "PC1"]
#   ) 
# 
# result %$% cor.test(temp, pc1, use = "pair")
# 
#   
# 
# result %>% 
#   gather(key  = key, value = temperature, -year, -depth) %>% 
#   mutate(key = factor(key, levels = c("temp", "pc1", "cit_fitted", "cit_pred")),
#          key = recode(key, temp = "Instrumental", pc1 = "PC1", cit_fitted = "CiT", cit_pred = "CiT Prediction")) %>% 
#   ggplot(aes(x = year, y = temperature)) + 
#   geom_line() +
#   facet_wrap(~ key, nrow = 1, scales = "free_x") + 
#   coord_flip() +
#   labs(x = "Year", y = "")

# #detrend
# result %>% 
#   filter(!is.na(temp)) %>%
#   mutate(
#     detrend_temp = resid(lm(temp ~ year)),
#     detrend_cit_pred = resid(lm(cit_pred ~ year))
#   ) %$% 
#   cor.test(detrend_temp, detrend_cit_pred)
# 


#estimate count sums
#fig2_fat %>% select(-depth) %>% estimate_n(digits = 0) %>% arrange(est_n)
