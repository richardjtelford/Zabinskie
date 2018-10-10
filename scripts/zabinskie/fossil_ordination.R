## ---- fossil_ordination


dca_fos <- decorana(sqrt(fos))
dca_mod <- decorana(sqrt(spp))
apply(dca_fos$rproj, 2, max)[1]
apply(dca_mod$rproj, 2, max)[1]

CCA <- cca(sqrt(fos))
screeplot(CCA, bstick = TRUE)


PCA <- rda(sqrt(fos))
screeplot(PCA, bstick = TRUE)
cor(recon$temperature, scores(PCA, display = "sites")[,1])

