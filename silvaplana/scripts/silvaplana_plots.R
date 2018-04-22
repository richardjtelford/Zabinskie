## ---- plot_silvaplana_reconstructions

recon_plot <- bind_rows(`140 yr JoPL` = recon_jopl, 
          `540 yr Holocene` = filter(recon_holocene, Year > min(recon_jopl$Year)), 
      .id = "source")  %>% 
  ggplot(aes(x = Year, y = JulyT, colour = source, linetype = source)) +
  geom_point() +
  geom_line() +
  labs(x = "Year CE", y = "Reconstruction °C", colour = "", linetype = "") +
  scale_x_continuous(expand = c(0.02, 0)) +
  theme(legend.position = "none")#c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())




#by stratigraphic
side_by_side <- recon_jopl %>% 
  rowid_to_column(var = "Rank") %>% 
  bind_cols(recon_holocene %>% 
              slice(1:nrow(recon_jopl)) %>% 
              rename_all(paste0, ".Holocene"))  %>% 
  mutate(delta  = JulyT.Holocene - JulyT) %>% 
  mutate(offset = case_when(delta == 0 ~ "Zero",
                            delta %% 1 == 0 ~ "Integer",
                            TRUE ~ "Non-integer"))


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

#recon_strat_plot




# implied_chronology
chron_plot <- side_by_side %>% 
  ggplot(aes(x = Year, y = Year - Year.Holocene)) +
  geom_point(size = 1) +
  geom_line() +
  geom_abline() +
#  coord_equal() +
  labs(x = "Year CE", y = "Offset, years") 


max_diff <- side_by_side %>% 
  summarise(mx = max(Year - Year.Holocene)) %>% pull(mx)

#combined figure
silvaplana_recons <- cowplot::plot_grid(recon_plot, recon_strat_plot, ncol = 1, align = "v",labels = c("a)", "b)"), hjust = -3)
