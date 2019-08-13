## ---- weather_climate
weather_climate_process <- function(x){
  #process data
  cet2 <- x %>% 
    as_tibble() %>%
    set_names(c("year", month.abb, "annual")) %>% 
    #remove partial years
    filter(!apply(. == -99.9, 1, any)) %>% 
    #calculate mean summer
    mutate(summer = (Jun + Jul + Aug)/3)
  
  cet2
}
# #plot monthly data
# plotJA <- . %>% select(year, Jun, Aug) %>% gather(key = month, value = temperature, -year) %>% ggplot(aes(x = year, y = temperature, colour = month))
# cet %>% plotJA +  geom_path() + geom_smooth()

#aggregate timeseries into non-overlaping periods of 1-50 years
weather_climate <- function(N, month1 = "Jun", month2 = "Aug", dat = cet, cor = TRUE){
  dat <- dat %>% mutate(window = 0:(n() - 1) %/% N) %>% 
    group_by(window) %>%
    filter(n() > N/2) %>% #remove small partial groups
    summarise_all(mean) 
  ct <- cor.test(pull(dat, month1), pull(dat, month2))
  if(cor){
  tibble(N, low = ct$conf.int[1], high = ct$conf.int[2], est = ct$estimate, n = nrow(dat), last = max(dat$year))
  } else{
    dat
  }
}


weather_climate_plot <- function(wc_AS, wc_JA){
    JuneAugustSummer_plot <- bind_rows(`August-summer` = wc_AS,
                                       `August-June` = wc_JA,
                                       .id = "months") %>%
    ggplot(aes(x = N, y = est, colour = months, linetype = months, shape = months, label = months)) +    
      geom_point() +
      geom_smooth(se = FALSE, size = .7) +
      geom_dl(method = list(box.color = NA, "first.points", hjust = 0, vjust = 1, cex = 0.75))  +
      scale_x_continuous(trans = "sqrt", expand = c(0.02, 0)) +
      scale_y_continuous(expand = c(0.01, 0), limits = c(0, 1)) + 
      scale_color_brewer(type = "qual", palette = "Dark2") +
      labs(x = "Number of years", y = "Correlation")+
      theme(legend.position = "none")
    
  JuneAugustSummer_plot
}


calc_weather_climate_correlations <- function(cet2, climate){
  annual <- cet2 %>% 
    select(-year, -summer, -annual) 
  
  
  multidecadal <- cet2 %>% 
    select(-year, -summer, -annual) %>% 
    mutate(n = (1:n() - 1) %/% 30) %>% #group into 30-yr periods
    group_by(n) %>% 
    summarise_all(.funs = mean) %>% 
    select(-n) 
  
  space <- climate %>% select(Jan:Dec)
  
  process <- . %>% cor() %>% 
    as_tibble(rownames = "month1") %>% 
    gather(key = month2, value = correlation, -month1) %>% 
    filter(month1 != month2)
  
  all_correlations <- bind_rows(Annual = annual %>% process,
                                Multidecadal = multidecadal %>% process,
                                Space = space %>% process, 
                                .id = "what") %>% 
    mutate(month1 = factor(month1, levels = month.abb),
           month2 = factor(month2, levels = month.abb),
           what = factor(what, levels = c("Space", "Annual", "Multidecadal")))
  return(all_correlations)
}

plot_weather_climate_correlations <- function(all_correlations){
  weather_climate_plot <- all_correlations %>% 
    ggplot(aes(x = month1, y = month2, fill = correlation)) + 
    geom_raster() +
    scale_fill_continuous(type = "viridis", breaks = seq(0, 1, 0.2), limits = c(NA, 1)) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(fill = "r") +
    coord_equal() +
    facet_wrap(~ what) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
      axis.title = element_blank(),
      # legend.position = "bottom",
      panel.grid = element_blank(), 
      legend.box.spacing = unit(x = 5, units = "pt"), 
      legend.title.align = 0.2
    )
  return(weather_climate_plot)
}