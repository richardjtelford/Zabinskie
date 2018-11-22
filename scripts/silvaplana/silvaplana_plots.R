## ---- plot_silvaplana_reconstructions

silv_plot_reconstructions <- function(recon_jopl, recon_holocene){
  recon_plot <- bind_rows(`140 yr JoPL` = recon_jopl, 
            `540 yr Holocene` = filter(recon_holocene, Year >= min(recon_jopl$Year)), 
        .id = "source")  %>% 
    ggplot(aes(x = Year, y = JulyT, colour = source, linetype = source)) +
    geom_point() +
    geom_line() +
    labs(x = "Year CE", y = "Reconstruction °C", colour = "", linetype = "") +
    scale_x_continuous(expand = c(0.02, 0)) +
    theme(legend.position = "none")#c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())
  return(recon_plot)
}

  #by stratigraphic
silva_put_recons_side_by_side <- function(recon_jopl, recon_holocene){
  side_by_side <- recon_jopl %>% 
    rowid_to_column(var = "Rank") %>% 
    bind_cols(recon_holocene %>% 
                slice(1:nrow(recon_jopl)) %>% 
                rename_all(paste0, ".Holocene"))  %>% 
    mutate(delta  = JulyT.Holocene - JulyT) %>% 
    mutate(offset = case_when(delta == 0 ~ "Zero",
                              delta %% 1 == 0 ~ "Integer",
                              TRUE ~ "Non-integer"))
  return(side_by_side)
}  
  

silva_plot_side_by_side <- function(side_by_side){
  recon_strat_plot <- side_by_side %>% 
    select(-delta) %>% 
    gather(key = source, value = JulyT, -Year, -Year.Holocene, -Rank, -offset) %>% 
    mutate(source = recode(source, "JulyT" = "140 yr JoPL", "JulyT.Holocene" = "540 yr Holocene")) %>%  
    ggplot(aes(x = Rank, y = JulyT, colour = source, linetype = source)) + 
    geom_line() +
    geom_point(aes(shape = offset)) +
    scale_shape_manual(values = c(16, 4, NA)) +
    scale_x_reverse(expand = c(0.02, 0)) +
    labs(x = "Stratigraphic Rank", y = "Reconstruction °C", colour = "Source", linetype = "Source") +
     theme(legend.position = "none")#c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())
  return(recon_strat_plot)
}  
  # # implied_chronology
  # chron_plot <- side_by_side %>% 
  #   ggplot(aes(x = Year, y = Year - Year.Holocene)) +
  #   geom_point(size = 1) +
  #   geom_line() +
  #   geom_abline() +
  # #  coord_equal() +
  #   labs(x = "Year CE", y = "Offset, years") 
  # 
  
silva_calc_max_difference <- function(side_by_side){
  max_diff <- side_by_side %>% 
    summarise(mx = max(Year - Year.Holocene)) %>% pull(mx)
}

  #combined figure
silva_combine_recon_figures <- function(recon_plot, recon_strat_plot){
  silvaplana_recons <- cowplot::plot_grid(recon_plot, recon_strat_plot, ncol = 1, align = "v",labels = c("a)", "b)"), hjust = -3)

  return(silvaplana_recons)
}

#estimate_count sums
silva_estimate_countSums <- function(fos_holocene){
  fos_holocene %>% 
    select(-YearAD) %>% 
    estimate_n(digits = 2)
}
#silva_estimate_countSums(silva_fos_holocene)


silva_last_sample <- function(silva_fos_holocene){
  last_sample <- silva_fos_holocene %>%
    tail(1) %>% 
    select_if(. != 0)
  return(last_sample)
}

silva_plot_climate <- function(silva_climate, silva_digitised_climate){
   g <-  silva_climate %>% 
    mutate(what = "Station") %>% 
    bind_rows(silva_digitised_climate) %>%
    mutate(what = factor(what, levels = c("cis", "can", "cit",  "Station"), labels = c("Swiss", "Canadian", "Time", "Station"))) %>% 
    ggplot(aes(x = year, y = temperature, colour = what, shape = what)) +
    geom_point() +
    geom_line() +    
    scale_colour_manual(values = RColorBrewer::brewer.pal(9, "Set1")[c(2, 3, 1, 9)]) + 
    scale_x_continuous(expand = c(0.02, 0)) +
    labs(x = "Year CE", y = "July temperature anomaly °C") +
    theme(legend.position = c(0.01, 0.99), legend.justification = c(0, 1), legend.title = element_blank(), legend.direction = "horizontal")
 return(g)
}
