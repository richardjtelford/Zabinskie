## ---- fossil_ordination

decorana(sqrt(fos))

CCA <- cca(sqrt(fos))
screeplot(CCA, bstick = TRUE)


PCA <- rda(sqrt(fos))
screeplot(PCA, bstick = TRUE)
cor(recon$temperature, scores(PCA, display = "sites")[,1])

