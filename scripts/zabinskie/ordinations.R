## ---- ordinations
dca_fos <- decorana(sqrt(fos))
dca_spp <- decorana(sqrt(spp))

#apply(dca_fos$rproj, 2, max)
#apply(dca_spp$rproj, 2, max)


ca_fos <- cca(sqrt(fos))
#screeplot(ca_fos)

cca_fos <- cca(sqrt(fos) ~ old, data = instrumental_temperature %>% arrange(desc(year)))
pc_explained <- eigenvals(cca_fos)[1]/sum(eigenvals(cca_fos)) * 100
L1L2 <- eigenvals(cca_fos)[1]/eigenvals(cca_fos)[2]


anova_fos <- anova(cca_fos)$Pr[1]
