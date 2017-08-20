## ---- compare_3_reconstruction

recon_jopl %>% mutate(source = "140 yr JoPL") %>% 
  bind_rows(
    recon_holocene %>% 
      filter(Year > min(recon_jopl$Year)) %>% 
      mutate(source = "540 yr Holocene"), 
    recon_qsr %>% 
      filter(Year > min(recon_jopl$Year)) %>% 
      mutate(source = "1000 yr QSR") %>% 
      mutate(JulyT  = scale(JulyT, scale = FALSE) + mean(recon_jopl$JulyT))# add mean to anomalies
  ) %>% 
  mutate(source = factor(source, levels = c("140 yr JoPL", "540 yr Holocene", "1000 yr QSR"))) %>% 
  ggplot(aes(x = Year, y = JulyT, colour = source)) +
  geom_point() +
  geom_line() +
  labs(x = "Year CE", y = "Reconstructed July Temperature °C", colour = "") +
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())

## ---- compare_2_reconstruction

recon_jopl %>% mutate(source = "140 yr JoPL") %>% 
  bind_rows(
    recon_holocene %>% 
      filter(Year > min(recon_jopl$Year)) %>% 
      mutate(source = "540 yr Holocene") 
  ) %>% 
  ggplot(aes(x = Year, y = JulyT, colour = source)) +
  geom_point() +
  geom_line() +
  labs(x = "Year CE", y = "Reconstructed July Temperature °C", colour = "") +
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())



## ---- join_by_reconstruction
#check for identical reconstructions
recon_jopl %>% filter(duplicated(JulyT))#none
recon_holocene %>% filter(duplicated(JulyT))#none

#join by reconstruction
tt <- recon_jopl %>% 
  full_join(recon_holocene %>% filter(Year >= 1834), 
            by = "JulyT", suffix = c(".2008", ".2009")) %>% 
  mutate(match = !(is.na(Year.2008)|is.na(Year.2009))) 

tt %>% filter(!match) %>% pn


ggplot(tt, aes(x = Year.2008, y = Year.2009, colour = match)) + geom_point() + geom_rug() +
  geom_abline(intercept = 0, slope = 1) +
  labs(colour = "Match")+
  theme(legend.position = c(0.1,.95), legend.justification  = c(0, 1))


## ---- side_by_side
side_by_side <- recon_jopl %>% 
  rowid_to_column(var = "Rank") %>% 
  bind_cols(recon_holocene %>% 
              slice(1:nrow(recon_jopl)) %>% 
              rename_all(paste0, ".Holocene")) 


side_by_side %>% gather(key = source, value = JulyT, -Year, -Year.Holocene, -Rank) %>% 
  mutate(source = recode(source, "JulyT" = "140 yr JoPL", "JulyT.Holocene" = "540 yr Holocene")) %>%  
  ggplot(aes(x = Rank, y = JulyT, colour = source)) + 
  geom_line() +
  scale_x_reverse() +
  labs(x = "Stratigraphic Rank", y = "Reconstructed temperature °C", colour = "Source") +
  theme(legend.position = c(.02, .98), legend.justification = c(0, 1), legend.title = element_blank())

total <- nrow(recon_jopl)
identical <- with(side_by_side, sum(JulyT == JulyT.Holocene))

integer_offsets <- side_by_side %>% 
  mutate(delta  = JulyT.Holocene - JulyT) %>% 
  filter(delta != 0, delta %% 1 == 0)


## ---- integer_offset_plot
side_by_side %>% 
  ggplot(aes(x = JulyT, y = JulyT.Holocene)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = c(-5:5))

## ---- implied_chronology
side_by_side %>% 
  ggplot(aes(x = Year, y = Year - Year.Holocene)) +
  geom_point(size = 1) +
  geom_line() +
  geom_abline() +
  coord_equal() +
  labs(x = "Year CE 140 yr JoPL", y = "Offset, years") 

## ---- overlay_jopl_6a 
makefig<-function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}

fig6a <- readPNG("images/JoPL_6a.png")

jopl <- ggplot(recon_jopl, aes(x = Year, y = JulyT)) +
  annotation_custom(
    rasterGrob(fig6a, width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1816, xmax = 2011, ymin = 4.2, ymax = 16.25) +
  geom_point(colour = "red", size = 1) +
  geom_line(colour = "red") + 
  scale_x_continuous(breaks = seq(1860, 2000, 20)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 50, l = 40))

makefig(jopl)
