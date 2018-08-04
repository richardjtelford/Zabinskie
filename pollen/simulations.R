library("tidyverse")
library("magrittr")
library("leaps")

## ---- run_pollen_simulations

#config
n <- 10000
ntaxa <- 6
nyear <- 49
set.seed(7931) #from random.org


#white
white_runs <- rerun(.n = n, {
  data <- rnorm((nyear - 2) * (ntaxa + 1)) %>% 
      matrix(ncol = ntaxa + 1) %>% 
      as_data_frame() %>%
      filter(!is.na(V1)) %>% 
      rename(response = V1)
  
  mod <- lm(response ~ ., data = data)
  
  data_frame(
    response = data$response,
    pred = response - resid(mod)/(1 - lm.influence(mod, do.coef = FALSE)$hat)#identical to loo cv - but faster
  )
}) %>% 
  map_df(~data_frame(
    r =  cor(.$response, .$pred), 
    r2 = r^2
  ))



##autocorrelation


mk_data <- function(nyear, ntaxa){
  rnorm(nyear * (ntaxa + 1)) %>% 
  matrix(ncol = ntaxa + 1) %>% 
  zoo::rollmean(na.pad = TRUE, k = 2, align = "left") %>% 
  zoo::rollmean(na.pad = TRUE, k = 2, align = "right") %>% 
  as_data_frame() %>%
  filter(!is.na(V1)) %>% 
  rename(response = V1)
}

auto_runs <- rerun(.n = n, {
  data <- mk_data(nyear = nyear, ntaxa = ntaxa)

  mod <- lm(response ~ ., data = data)
  
  data_frame(
    response = data$response,
    pred = response - resid(mod)/(1 - lm.influence(mod, do.coef = FALSE)$hat)#identical to loo cv - but faster
  )
}) %>% 
  map_df(~data_frame(
    r =  cor(.$response, .$pred), 
    r2 = r^2
    ))

##predictor selection
##response selection

ntaxa <- 11

best_lm <- function(formula, data, ...){
  all_mod <- regsubsets(x = formula, data = data, intercept=TRUE, ...)
  summ <- summary(all_mod)
  #assume model includes intercept - so will also grab response
  predictors  <- summ$which[which.min(summ$bic), ]
  lm(formula, data = data[, predictors])
}

auto_pred_runs <- rerun(.n = n, {
  data <- mk_data(nyear = nyear, ntaxa = ntaxa )
  
  mod <- best_lm(formula  = response ~ ., data = data,method = "exhaustive", nvmax = ntaxa)
  
  data_frame(
    response = data$response,
    pred = response - resid(mod)/(1 - lm.influence(mod, do.coef = FALSE)$hat)#identical to loo cv - but faster
  )
}) %>% 
  map_df(~data_frame(
    r =  cor(.$response, .$pred), 
    r2 = r^2
  ))




## ---- plot_pollen_simulations
all_runs <- bind_rows(
  Ideal = white_runs,
  Autocorrelated = auto_runs,
  Autocorrelated_selected = auto_pred_runs, 
  .id = "run"
) %>% 
  mutate(run = factor(run, levels = c("Ideal", "Autocorrelated", "Autocorrelated_selected")))

all_runs %>% 
  ggplot(aes(x = r2, fill = run)) + 
  geom_histogram(boundary = 0, show.legend = FALSE) +
  geom_vline(data = all_runs %>% group_by(run) %>% do(data_frame(p = c(0.95, .99), pp = paste("p =", p), q = quantile(.$r2, prob = p))), mapping = aes(xintercept = q, linetype = pp), colour = "black") +
  scale_linetype_manual(values = c("dashed", "dotted")) +
  scale_x_continuous(expand = c(0.02, 0)) +
  facet_wrap( ~ run, ncol = 1, scales = "free_y") +
  geom_vline(xintercept = 0.44, colour  = scales::muted("red")) +
  labs(x = expression(Leave~one~out~r^2), y = "Count", linetype = "Percentile", fill = "Run")
