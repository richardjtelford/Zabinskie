calibration_sets <- list()


#SWAP
data(SWAP)
calibration_sets$SWAP <- list(group = "diatoms", spp = SWAP$spec, env = SWAP$pH)

#Larocque 2001

env <- read_excel("abisko/data/SwedenEnvData.xlsx", sheet = "EnvData") %>% 
  mutate(`Lake number` = as.numeric(`Lake number`)) %>% 
  filter(!is.na(`Lake number`))

spp <- read_excel("abisko/data/Sweden chiro.xlsx", sheet = "Sheet1") %>% 
  verify(...1 == paste0("T", env$`Lake number`)) %>% 
  select(-...1)

calibration_sets$Larocque2001 <- list(group = "chironomids", spp = spp, env = env$`Tjul (°C)`)



multi_mod <- function(cal, FUN, ...){
  mod <- list()
  with(cal, {
    mod$raw <<- FUN(spp, env, ...) %>% crossval()
    mod$sqrt <<- FUN(sqrt(spp), env, ...) %>% crossval()
    mod$log <<- FUN(log1p(spp), env, ...) %>% crossval()
    mod$log1000 <<- FUN(log1p(spp * 10), env, ...) %>% crossval()
  })
  result <- mod %>% map(performance) %>% map("crossval") %>% 
    map(as.data.frame) %>% 
    map_df(rownames_to_column, var = "setting", .id = "transform")
  result
}


result <- calibration_sets %>% map_df(multi_mod, FUN = WAPLS, .id = "cali")

result %>% 
  mutate(transform = factor(transform, levels = c("raw", "sqrt", "log", "log1000")), 
         setting = gsub("Comp", "", setting), 
         setting = as.numeric(setting)) %>% 
  ggplot(aes(x = setting, y = RMSE, colour = transform)) +
  geom_line() +
  facet_wrap(~ cali, scales = "free_y") +
  labs(x = "Number of components", y = "RMSEP", colour = "Transformation")
