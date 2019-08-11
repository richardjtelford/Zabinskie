## ---- experiment_one
#effect of gradient length on performance (RMSEP/r2)
#vary length with constant density - pure length effect
#vary length with constant n - mixed length and density effect
#  Performance statistics (esp RMSEP)
#  ??optima stability
# real or simulated data
# ? availability of large ~uniform dataset for subsampling


## ---- glengthFunctions
library("palaeoSig")
library("ggplot2")
glengthPerform <- function(glength, nlake){
  env.var <- make.env(n = nlake, elen = c(glength, rep(50, 3)), emean = rep(50, 4), edistr = 'uniform', ndim = 4)
  spec.abun <- abundances(env.var, spec, 100)
  spp <- spec.abun$spp/100

  performance(crossval(WA(spp, env.var[, 1])))$crossval[1, "RMSE"]
}

## ---- gdata
glengths <- seq(20, 100, 10)
nlakes <- 50
nrep <- 50

#species
spec <- species(nspp = 30, ndim = 4, Amax = runif, fun = runif, xpar = c(-50,150),
                srange = 200, alpha = 4, gamma = 4)

res <- plyr::rdply(.n = nrep, {
  constantn <- sapply(glengths, glengthPerform, nlake = nlakes)
  
  constantd <- mapply(glengthPerform, glength = glengths, nlake = nlakes * glengths/min(glengths))
  
  out <- tibble(glengths, constantn, constantd)
  out <- tidyr::gather(out, key = constant, value = RMSEP, -glengths) %>% 
    mutate(grp = paste0(constant, glengths))
  out
})

ggplot(res, aes(x = glengths, y = RMSEP, colour = constant, group = grp)) +
  geom_boxplot() +
  geom_smooth(aes(group = constant)) +
  scale_colour_discrete(name = "Constant", labels = c("Lake density", "Lake number")) +
  xlab("Gradient length")


mod <- lm(RMSEP ~ glengths * as.factor(constant), data = res)
summary(mod)
