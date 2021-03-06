## ---- replicating_figure_2
zabinskie_figure2 <- function(spp_all, env_all){
  keep <- colSums(spp_all > 0) >= 3 #Eukiefferiella fittkaui is included
  
  pca <- rda(sqrt(spp_all[, keep]))
  
  fpca <- fortify(pca, display = "sites", scaling = 1)
  
  #Find missing lakes
  #plot(-fpca$Dim1, -fpca$Dim2)
  #abline(v = 0, h = 0)
  #missing <- identify(-fpca$PC1, -fpca$PC2)
  missing <-  c(3, 48, 66, 67, 68, 69, 70, 71, 72, 73, 114, 115, 116, 117, 118, 119, 120, 121)
  
  #temperature cuts to match published version
  breaks <-  c(3, 11, 16, 23, 28)
  
  fpca <- fpca %>% mutate(
    temp = cut(env_all, breaks = breaks, include.lowest = TRUE, labels = paste0(breaks[-length(breaks)], "-", breaks[-1],"°C")), 
    country =  c(rep("Poland", 48), rep("Canada", 73))
  )
  
  fig2 <- ggplot(fpca, aes(x = -PC1, y = -PC2, colour = temp, shape= country)) + 
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(size = 2) + 
    coord_equal() + 
    geom_point(data = fpca[missing, ], colour = "red", pch = 21, size = 3) +
    scale_colour_manual(values = c("skyblue", "salmon", "black", "green")) +
    scale_shape_manual(values = c(15, 18)) +
    labs(
      x = paste0("PCA1 (", round(eigenvals(pca)[1]/pca$tot.chi * 100, 1),"%)"),
      y = paste0("PCA2 (", round(eigenvals(pca)[2]/pca$tot.chi * 100, 1),"%)"), colour = "", shape = "")
  
  return(fig2)
}


## ---- supplementary_data_fig_1
zabinskie_sup_data_fig1 <- function(spp_all, env_all, fos){
  mod <- rda(sqrt(spp_all) ~ env_all)
  
  scaling <- "sites"
  frda <- fortify(mod, display = "sites", scaling = scaling) %>% 
    mutate(country = c(rep("Poland", 48), rep("Canada", 73)))#country information
  
  sdf1 <- ggplot(frda, aes(x = -RDA1, y = PC1, colour = country, shape = country)) + #axis 1 flipped to match published figure
    geom_vline(xintercept = 0, colour = "grey50") +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(size = 2) + 
    coord_equal() +
    labs(x = "RDA1", y = "PCA1", shape = "Country", colour = "Country") 
  
  tt <- analogue::timetrack(X = spp_all, passive = fos, env = env_all, method = "rda", transform = "sqrt", scaling = scaling)
  tt_fit <- as.data.frame(tt$fitted.values)
  
  sdf1 <- sdf1 + 
    geom_point(aes(x = -RDA1, y = PC1), data = tt_fit, inherit.aes = FALSE, colour = "limegreen", shape = 17) + 
    scale_shape_manual(name = "", limits = c("Canada", "Poland", "Fossil"), values = c(15, 18, 17)) +
    scale_colour_manual(name = "", limits = c("Canada", "Poland", "Fossil"), values = c("#E31A1C", "#1F78B4", "#33A02C"))
  
  return(sdf1)
}


zabinskie_ordination_composite <- function(fig2, sdf1){
  th <- theme(legend.position = "bottom")
  ordination_composite <- cowplot::plot_grid(
    readd(fig2) + th + 
      guides(colour = guide_legend(nrow=2,byrow=TRUE)) +
      guides(shape = guide_legend(nrow=2,byrow=TRUE)),
    readd(sdf1) + th, 
    nrow = 1, labels = c("a)", "b)"), align = "h", vjust = 2, hjust  = -4.4)

  return(ordination_composite)
}