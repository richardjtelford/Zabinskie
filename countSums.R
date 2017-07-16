#chironomid calibration experiment ideas

## ---- experiment_two
# Two components of count sum
# Ideal - large count
# count precision - small count during cross-validation
# optima stability - small count during calibration
# both - check additivity
# effect of n
# effect of n_lakes
#
# simulated data or real
# n_lakes = c(50, 100)


## ---- countfunctions
library("rioja")
library("ggplot2")
library("dplyr")

crossval2 <- function(train, test, env, method, ...){
  keep <- colSums(train > 0) > 1
  test <- test[, keep]
  train <- train[, keep]
  fit.func <- get(paste0(method, ".fit"))
  predict.func <- paste0("predict.internal.", method)
  predict.func <- getFromNamespace(predict.func, ns = "rioja")
  train <- as.matrix(train)
  test <- as.matrix(test)
  preds <- sapply(1:nrow(train), function(n){
    train <- train[-n, ]
    env <- env[-n]
    test <- test[n, , drop = FALSE]
    mod <- fit.func(train, env, lean = TRUE, ...)
    predict.func(mod, test)
  })
  t(preds)
}


RMSEP <- function(env, preds){
  apply(preds, 2, function(comp){
    sqrt(mean((comp - env)^2))
  })
}

resampleSpp <- function(spp, n){
  out <- apply(spp, 1, rmultinom, n = 1, size = n)
  out <- as.data.frame(t(out))
  dimnames(out) <- dimnames(spp)
  out
}

stratified <- function(env, n, ndiv = 5){
  groups <- cut(env, breaks = quantile(env, probs = seq(0, 1, length = ndiv + 1)), include.lowest = TRUE)
  keep <- lapply(levels(groups), function(g){
    sample(which(groups ==g), size = n/ndiv)
  })
  unlist(keep)
}

runCuts <- function (spp, env, countSums, nlakes = NULL, type, nrep = 20, seed = 42, whichcol = 1, transform = I){
  require("dplyr")
  set.seed(seed)
  out <- sapply(countSums, function(n){
    replicate(nrep, {
      if(!is.null(nlakes)){
        keep <- stratified(env, n = nlakes)
        spp <- spp[keep, ]
        env <- env[keep]
      }
      
      smallSamp <- resampleSpp(spp, n = n)
      largeSamp <- resampleSpp(spp, n = max(countSums))
      if (type == "train") {
        train <- smallSamp
        test <- largeSamp
      } else if (type == "test") {
        train <- largeSamp
        test <- smallSamp
      } else if (type == "both") {
        train <- smallSamp
        test <- smallSamp
      } else {
        stop("Unrecognised type")
      }
      
      train <- train/rowSums(train)
      test <- test/rowSums(test)
      test <- transform(test)
      train <- transform(train)
      preds <- crossval2(train = train, test = test, env = env, method = "WA")
      RMSEP(env = env, preds = preds)[whichcol]
    })
  })
  colnames(out) <- countSums
  out <- tidyr::gather(as.data.frame(out), key = countSum, value = RMSEP) %>% 
    mutate(countSum = as.numeric(countSum)) %>%
    mutate(type = type)
  out
}

testtrainboth <- function(spp, env, countSums, nrep, nlakes = NULL){
  smallCV <- runCuts(spp = spp, env = env, countSums = countSums, type = "test", nrep = nrep, nlakes = nlakes)
  
  # case 2. RMSEP small training set
  smallTrain <- runCuts(spp = spp, env = env, countSums = countSums, type = "train", nrep = nrep, nlakes = nlakes)
  
  # case 3. RMSEP small training set & cross-validation
  smallboth <- runCuts(spp = spp, env = env, countSums = countSums, type = "both", nrep = nrep, nlakes = nlakes)
  
  ## RMSEPplots
  #plot everything together
  if(is.null(nlakes)) nlakes <- nrow(spp)
  small <- rbind(smallCV, smallTrain, smallboth) %>% mutate(grp = paste0(countSum, type), nlakes = nlakes)
  small
}



predictionStability <- function(countSums, type, nrep = 20, seed = 42){
  set.seed(seed)
  pred_sd <- plyr::adply(countSums, 1, function(n){
    preds <- replicate(nrep, {
      large <- spp
      small <- resampleSpp(spp, n = n)
      if(type == "test"){
        train <- large
        test <- small
      } else if(type == "train"){
        train <- small
        test <- large
      } else{
        stop("unrecognised type")
      }
      train <- train/rowSums(train)
      test <- test/rowSums(test)
      crossval2(train = train, test = test, env = env, method = "WA")[, 1]
    })
    data_frame(countSum = n, sd = apply(preds, 1, sd), type = type, lake = rownames(spp))
  })
  pred_sd
}



## ---- data
# make code for SWAP - assume counts of >350 valves. Switch to read chironomid data later
data("SWAP")
spp <- SWAP$spec
env <- SWAP$pH
dictionary <- as.data.frame(SWAP$names, stringsAsFactors = FALSE)
countSums <- c(10, 20, 30, 40, 50, 60, 75, 100, 125, 150, 200, 250, 300, 350)
nrep <- 100
th <- theme()

1/(apply(SWAP$spec, 1, function(x)min(x[x > 0]))/100)#estimiated count sums

## ---- testFunctions
spp1 <- spp
spp1 <- resampleSpp(spp = spp1, n = 350)
spp1 <- spp1/rowSums(spp1)
spp1 <- spp1[, colSums(spp1 > 0) > 1]
check <- crossval2(spp1, spp1, env, "WA")
RMSEP(env, check)
performance(crossval(WA(spp1, env)))$crossval
performance(crossval(WA(sqrt(spp1), env)))$crossval

predict(WA(spp, env), spp[1:2, ])

## ---- RMSEPAnalysis
allLakes <- testtrainboth(spp = spp, env = env, countSums = countSums, nrep = nrep)
lakes60 <- testtrainboth(spp = spp, env = env, countSums = countSums, nrep = nrep, nlakes = 60)

allRMSEP <- rbind(allLakes, lakes60)

## ---- RMSEPplots
ggplot(filter(allRMSEP, !is.na(RMSEP)), aes(x = countSum, y = RMSEP, group = grp, colour = type)) + 
  geom_boxplot() + 
  geom_smooth(mapping = aes(group = type), 
              method = "nls", 
              formula = y ~ asym + beta * x ^ expo, 
              method.args = list(start = list(asym = 0, beta = 1, expo = -1)), 
              se = FALSE) +
  labs(x = "Count size") + 
  facet_wrap(~nlakes)

# different nlakes




## ---- RMSEPmodels
library("nlme")
modn <- nls(RMSEP ~ asym + beta * countSum ^ expo, data = allRMSEP, start = list(asym = 0, beta = 1, expo = -1), subset = type  == "test" & nlakes == 167)
modn
update(modn, subset = type  == "test" & nlakes == 60)
update(modn, subset = type  == "train" & nlakes == 167)
update(modn, subset = type  == "train" & nlakes == 60)
update(modn, subset = type  == "both" & nlakes == 167)
update(modn, subset = type  == "both" & nlakes == 60)




small %>% filter(countSum == 350) %>% group_by(type) %>% summarise(m =mean(RMSEP))
small %>% filter(countSum == 10) %>% group_by(type) %>% summarise(m =mean(RMSEP))


## ---- checkWithAlternate
set.seed(42)
both <- replicate(50, {
  spp2 <- resampleSpp(spp = spp, n = 350)
  spp2x <- resampleSpp(spp = spp, n = 350)#too keep seed on track
  spp2<- spp2/rowSums(spp2)
  spp2 <- spp2[, colSums(spp2 > 0) > 1]
  performance(crossval(WA(spp2, env)))$crossval[1, "RMSE"]
})

boxplot(x = list(both, smallboth%>%filter(countSum == 350) %>% select(RMSEP)%>% as.data.frame()%>% unlist()), notch = TRUE)
performance(crossval(WA(spp, env)))$crossval[1, "RMSE"]


t.test(x = both, y = smallboth%>%filter(countSum == 350) %>% select(RMSEP)%>% as.data.frame()%>% unlist())

## ---- predictionStability
trainStability <- predictionStability(countSums = countSums, type = "train", nrep = nrep)

testStability <- predictionStability(countSums = countSums, type = "test", nrep = nrep)

## -- plotPredictionStability
rbind(trainStability, testStability) %>% 
  mutate(grp = paste0(countSum, type)) %>%
  ggplot(aes(x = countSum, y = sd, group = grp, colour = type )) + 
  geom_boxplot() +   
  geom_smooth(aes(group = type), method = "nls", formula = y ~ beta0 + beta1 * x ^ expo, method.args = list(start = list(beta0 = 0, beta1 = 1, expo = -1)), se = FALSE) + 
  labs(x = "Count size", y = "SD of predictions")


## ---- predictionStabilityModel  
mod <- nls(sd ~ beta0 + beta1 * countSum^expo, data = testStability, start = list(beta0 = 0, beta1 = 1, expo = -1))
mod
nls(sd ~ beta1 * countSum^expo, data = pred_sd, start = list(beta1 = 1, expo = -1))

update(mod, data = trainStability)

## ---- optimaStability
#get sd of optima with different n
set.seed(42)
n2 <- Hill.N2(spp[, colSums(spp > 0) > 1])
n2 <- data_frame(species = names(n2), n2 = n2)

optimaStability <- plyr::ldply(countSums, function(n){
  out <- replicate(nrep, { 
    train <- resampleSpp(spp, n = n)
    train <- train[, colSums(train > 0) > 1]
    train <- train/rowSums(train)
    optima <- coef(WA(train, env))
    data_frame(species = rownames(optima), optima = optima[, "Optima"])
  }, simplify = FALSE)
  opts <- Reduce(function(...)full_join(..., by = "species"), out)
  sd2 <- plyr::adply(opts, .margins = 1, function(r){
    data_frame(species = r$species, sd = sd(unlist(r[-1]), na.rm = TRUE))
    })
  sd2 <- left_join(sd2, n2)
  sd2$countSum <- n
  sd2
})

## ---- plotOptimaStability
#all optima
optimaStability %>% filter(n2 >= 5, countSum == max(countSums)) %>% 
  tidyr::gather(key = key, value = optima, -species, -sd, -n2, -countSum) %>% 
  ggplot(aes(x = species, y = optima, colour = n2)) + 
  geom_violin() + 
  th +
  theme(axis.text.x = element_text(angle = 90))

## ---- optimaSelectedTaxa
plotspp <- function(sppName, counts = c(20, 40, 75, 150, 350)){
  require("RColorBrewer")
  abun <- cbind(env = env, spp[, sppName, drop = FALSE])
  abun <- abun %>% tidyr::gather(key = species, value = abun, -env) %>% 
    left_join(dictionary, by = c("species" = "CODE"))
  
  g <- optimaStability %>% 
    filter(species %in% sppName, countSum %in% counts) %>% 
    tidyr::gather(key = key, value = optima, -species, -sd, -n2, -countSum) %>%
    left_join(dictionary, by = c("species" = "CODE")) %>%
    ggplot(aes(x = optima, colour = as.factor(countSum))) + 
      geom_line(aes(y = ..density..), stat = "density") + 
      geom_rug(mapping = aes(x = env, size = abun), data = abun, alpha = 0.3, inherit.aes = FALSE) +
      scale_size(range = c(0, 6), name = "%") + 
      scale_color_manual(name = "Count Size", values = brewer.pal(length(counts) + 1,"Blues")[-1]) + 
    facet_wrap( ~ TaxonName, scales = "free_y") + 
    guides(colour = guide_legend(order = 1), size = guide_legend(order = 2))
  g
}
plotspp(c("AC9968", "FR005A", "EU047A", "EU014A"))

## ---- optimaStabilityCountsum
ggplot(filter(optimaStability, n2 >= 20), aes(x = countSum, y = sd, colour = n2, group = species)) + 
  geom_line() 

## ---- optimaStabilityN2
#ggplot(optimaStability, aes(x = n2, y = sd)) + geom_point() + facet_wrap(~countSum)
ggplot(filter(optimaStability, countSum == 100), aes(x = n2, y = sd)) + 
  geom_point() + 
  labs(x = "N2", y = "SD of optima")

## ---- optimaCountNLSplot
filter(optimaStability, n2 > 5) %>% mutate(large = n2 > 20, grp = paste0(countSum, large)) %>%
ggplot(aes(x = countSum, y = sd, group = grp, fill = large)) + 
  geom_boxplot() + 
  geom_smooth(mapping = aes(group = large, colour = large), method = "nls", formula = y ~ beta0 + beta1 * x ^ expo, method.args = list(start = list(beta0 = 0, beta1 = 1, expo = -1)), se = FALSE, show.legend = FALSE) + 
  labs(x = "Count size", y = "SD of optima") +
  scale_fill_discrete(name = "N2 > 20")


## ---- optimaNLS
mod <- nls(sd ~ beta1 * countSum ^ expo, data = optimaStability, start = list(beta1 = 1, expo = -1), subset = n2 > 20)
mod

## ---- optimaNLScoef
speciesCoef <- optimaStability %>% 
  select(species, n2) %>% 
  distinct(species, .keep_all = TRUE) %>% 
  group_by(species, n2) %>%
  do({
  mod <- nls(sd ~ beta1 * countSum ^ expo, data = optimaStability, start = list(beta1 = 1, expo = -1), subset = species == .$species)
  as.data.frame(t(coef(mod)))
})

ggplot(speciesCoef, aes(x = n2, y = beta1)) + geom_point() + geom_smooth()
ggplot(filter(speciesCoef, n2 > 5), aes(x = n2, y = expo)) + geom_point() + geom_smooth()

## ---- weirdTaxa 
#small N2?
tmp <- filter(speciesCoef ,expo > 0)$species
ggplot(optimaStability %>% filter(species %in% tmp[1]), aes(x = countSum, y = sd, colour = species)) + 
  geom_point(show.legend = FALSE) + 
  geom_smooth(method = "nls", formula = y ~ beta1 * x ^ expo, method.args = list(start = list( beta1 = 1, expo = -1)), se = FALSE, show.legend = FALSE)



## --- quadrature
rmsep <- 1
extra <- c(seq(3, 0, -.01))
tot <- sqrt(rmsep^2 + extra^2)
ggplot(data = data_frame(extra, tot), aes(x = extra, y = extra/tot)) + geom_line()
ggplot(data = data_frame(extra, tot), aes(x = extra, y = tot)) + geom_line()

#tot = sqrt(rmsep^2 + extra^2)
#tot^2 = rmsep^2 + extra^2


## ---- MADLarocque2001
#resample to 50. Add ten. repeat to 200. 
#find mean absolute difference in reconstruction from full 
library("osDesign")
set.seed(42)
spp200 <- resampleSpp(spp, n = 200)
spp200 <- spp[, colSums(spp200 > 0) > 1]
WA.mod <- WA(spp200/200, env)
full <- crossval(WA.mod)$predicted[,"WA.inv"]

madcounts <- seq(190, 10, -10)

hyperSample <- function(spp, n){
  out <- apply(spp, 1, function(r){
      rmvhyper(unlist(r), n)
  })
  out <- as.data.frame(t(out))
  dimnames(out) <- dimnames(spp)
  out
}

MADcounts <- list()
for(i in 1:length(madcounts)){
  if(i == 1){
    MADcounts[[i]] <- hyperSample(spp200, madcounts[i])
  } else {
    MADcounts[[i]] <- hyperSample(MADcounts[[i-1]], madcounts[i])
  }
}

MAD <- sapply(MADcounts, function(m){
  m <- m/rowSums(m)
  pred <- crossval2(train = spp200, test = m, env = env, method = "WA")[,1]
  mean(abs(pred - full))
})
MADres <- data_frame(count = madcounts, MAD = MAD)

ggplot(MADres, aes(x = count, y = MAD)) + geom_point()

## ---- other
# get count size
# get L1/L2
# estimate taxonomic precision (reference list/species list)
# error partitioning ?? (at least informal)
# recommendations
# non-uniform gradients