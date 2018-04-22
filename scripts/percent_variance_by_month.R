library(vegan)
## ---- percent_variance
#predictive power of different months 
clim15 <- sites %>% 
  select(Lake) %>% 
  left_join(climate) %>% 
  filter(!is.na(Aug))
spp15 <- spp %>% filter(sites$Lake %in% clim15$Lake)

perform_by_month15 <- clim15 %>% 
  select(-(1:4), -summer, -country) %>% 
  gather(key = month, value = temperature) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  group_by(month) %>% 
  do({
    CCA <- cca(spp15 ~ temperature, .)
    pc_explained = eigenvals(CCA)[1]/sum(eigenvals(CCA)) * 100
    mod <- crossval(WAPLS(spp15, .$temperature, npls = 2))
    r2_mod <- performance(mod)$crossval[2, "R2"]
    pred <- predict(mod, fos)$fit[, 2]
    target <- fat_composite_as_zab_published[, as.character(.$month[1])]
    r_pred <- cor(pred, target)
    data_frame(pc_explained, r2_mod, r_pred = as.vector(r_pred))
  })

explained_plot <- ggplot(perform_by_month15, aes(x = month, y = pc_explained)) + 
  geom_point() + 
  geom_line(aes(group = 1)) +
  labs(x = "Month", y = "CCA inertia\nexplained %") +
  scale_y_continuous(limits = c(0, NA), expand = c(0.02, 0)) +
  scale_x_discrete(expand = c(0.02, 0.02)) + 
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)))

perform_plot <- explained_plot + aes(y = r2_mod) + 
  labs(y = expression(atop(Transfer, `function`~r^2)))

result_plot <- explained_plot + aes(y = r_pred) + 
  labs(y = "Reconstruction-\nInstrumental r") 

th <- theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank())
variance_plot <- cowplot::plot_grid(explained_plot + th,  
                                    perform_plot + th, 
                                    result_plot, 
                                    ncol = 1, 
                                    align = "v", 
                                    rel_heights = c(0.8, 0.8, 1), 
                                    labels = paste0(letters[1:3], ")")
                                    )

## ---- reconstruction_by_month

recon_by_month15 <- clim15 %>% select(-(1:4), -summer, -country) %>% 
  gather(key = month, value = temperature) %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  group_by(month) %>% 
  do({
    mod <- WAPLS(spp15, .$temperature, npls = 2)
    pred <- predict(mod, fos)$fit[, 2]
    chron %>% mutate(pred = pred)
  })

recon_by_month_plot <- recon_by_month15 %>% 
  ggplot(aes(x = year, y = pred, colour = month, label = month)) + 
  geom_line() + 
  labs(x = "Year CE", y = "Reconstructed temperature °C", colour = "") +
  geom_dl(method = list(dl.trans(x = x - 0.05), "first.bumpup", cex = 0.8)) +
  theme(legend.position = "none") +
  scale_color_brewer(type = "qual", palette = "Paired") +
  scale_x_continuous(limits = c(1890, NA), expand = c(0.02, 0)) +     scale_y_continuous(expand = c(0.02, 0))
