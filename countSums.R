#chironomid calibration experiment ideas

## ---- experiment_one
#effect of gradient lenght on performance (RMSEP/r2)
#vary length with constant density - pure length effect
#vary length with constant n - mixed length and density effect
#  Performance statistics (esp RMSEP)
#  ??optima stability
# real or simulated data
# ? availability of large ~uniform dataset for subsampling


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

library("rioja")
library("ggplot2")

crossval2 <- function(train, test, env, method, ...){
  train <- train[, colSums(train > 0) > 1]
  preds <- sapply(1:nrow(train), function(n){
    train <- train[-n, ]
    env <- env[-n]
    test <- test[n, , drop = FALSE]
    mod <- method(train, env, ...)
    predict(mod, test)$fit
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

runCuts <- function (spp, countSums, type, nrep = 20, seed = 42){
  set.seed(seed)
  out <- sapply(countSums, function(n){
    replicate(nrep, {
      if (type == "train") {
        train <- resampleSpp(spp, n = n)
        test <- spp
      } else if (type == "test") {
        train <- spp
        test <- resampleSpp(spp, n = n)
      } else if (type == "both") {
        train <- resampleSpp(spp, n = n)
        test <- train
      } else {
        stop("Unrecognised type")
      }
      
      train <- train/rowSums(train)
      test <- test/rowSums(test)
      preds <- crossval2(train = sqrt(train), test = sqrt(test), env = env, method = WA)
      RMSEP(env = env, preds = preds)[1]
    })
  })
  colnames(out) <- countSums
  out <- tidyr::gather(as.data.frame(out), key = countSum, value = RMSEP) %>% 
    mutate(countSum = as.numeric(countSum)) %>%
    mutate(type = type)
  out
}

# make code for SWAP - assume count of 300 valves. Switch to read chironomid data later
data("SWAP")
spp <- round(SWAP$spec * 300 /100)
env <- SWAP$pH
countSums <- c(10, 20, 30, 40, 50, 60, 75, 100, 125, 150, 200, 250, 300)
nrep <- 50

##test functions
check <- crossval2(SWAP$spec, SWAP$spec, env, WAPLS)
RMSEP(env, check)
performance(crossval(WAPLS(SWAP$spec, SWAP$pH)))$crossval
predict(WA(spp, env), spp[1:2, ])

# case 1. RMSEP small cross-validation
smallCV <- runCuts(spp = spp, countSums = countSums, type = "test", nrep = nrep)
mod <- lm(RMSEP ~ I(1/countSum), data = smallCV)
g <- ggplot(smallCV, aes(x = countSum, y = RMSEP, group = countSum)) + 
  geom_boxplot() + 
  geom_smooth(formula = y ~ I(1/x), group = 1)
g

# case 2. RMSEP small training set
smallTrain <- runCuts(spp = spp, countSums = countSums, type = "train", nrep = nrep)
mod2 <- lm(RMSEP ~ I(1/countSum), data = smallTrain)
#plot(mod2)
g %+% smallTrain

# case 3. RMSEP small training set & cross-validation
smallboth <- runCuts(spp = spp, countSums = countSums, type = "both", nrep = nrep)

mod3 <- lm(RMSEP ~ I(1/countSum), data = smallboth)
#plot(mod3)
mod3
g %+% smallboth

#plot everything together
small <- rbind(smallCV, smallTrain, smallboth)
ggplot(small, aes(x = countSum, y = RMSEP, group = countSum, colour = type)) + 
  geom_boxplot() + 
  geom_smooth(mapping = aes(group = type), method = "lm", formula = y ~ I(1/x)) +
  geom_smooth(mapping = aes(group = type), method = "nls", formula = y ~ asym + x ^ expo, method.args = list(start = list(asym = 0, expo = -1)), linetype = "dotted", se = FALSE) +
  facet_wrap(~type)

library("nlme")
modn <- nls(RMSEP ~ asym + countSum ^ expo, data = smallCV, start = list(asym = 0, expo = -1))
modn


## ---- other
# get count size
# get L1/L2
# estimate taxonomic precision (reference list/species list)
# error partitioning ?? (at least informal)
# recommendations
# non-uniform gradients