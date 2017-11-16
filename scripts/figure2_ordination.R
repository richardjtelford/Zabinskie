## ---- replicating_figure_2

keep <- colSums(spp_all > 0) >= 3 #Eukiefferiella fittkaui is included

pca <- rda(sqrt(spp_all[, keep]))

fpca <- fortify(pca, display = "sites", scaling = 1)

#Find missing lakes
#plot(-fpca$Dim1, -fpca$Dim2)
#abline(v = 0, h = 0)
#missing <- identify(-fpca$Dim1, -fpca$Dim2)
missing <-  c(3, 48, 66, 67, 68, 69, 70, 71, 72, 73, 114, 115, 116, 117, 118, 119, 120, 121)

#temperature cuts to match published version
breaks <-  c(3, 11, 16, 23, 28)

fpca$temp <- cut(env_all, breaks = breaks, include.lowest = TRUE, labels = paste0(breaks[-length(breaks)], "\u2012", breaks[-1],"°C"))
fpca$country <- c(rep("Poland", 48), rep("Canada", 73))

fig2 <- ggplot(fpca, aes(-Dim1, -Dim2, colour = temp, shape= country)) + 
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



## ---- supplementary_data_fig_1
mod <- rda(sqrt(spp_all) ~ env_all)

scaling <- "sites"
frda <- fortify(mod, display = "sites", scaling = scaling)

#country information
frda$country <- c(rep("Poland", 48), rep("Canada", 73))

sdf1 <- ggplot(frda, aes(-Dim1, Dim2, colour = country, shape = country)) + #axis 1 flipped to match published figure
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
