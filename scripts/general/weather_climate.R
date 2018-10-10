## ---- weather_climate
weather_climate_process <- function(x){
  #process data
  cet2 <- x %>% 
    as.tibble() %>%
    set_names(c("year", month.abb, "annual")) %>% 
    #remove partial years
    filter(!apply(cet == -99.9, 1, any)) %>% 
    #calculate mean summer
    mutate(summer = rowMeans(data_frame(Jun, Jul, Aug)))
  
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
  data_frame(N, low = ct$conf.int[1], high = ct$conf.int[2], est = ct$estimate, n = nrow(dat), last = max(dat$year))
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