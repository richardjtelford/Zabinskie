library(readxl)
library(tidyverse)

min_pc <- function(x) min(x[x > 0])

getCore <- function(lake){
  LAKE <- read_excel("data/abisko/abisko2004 Gaute.xlsx", sheet = lake, col_names = FALSE)
  depth <- unlist(LAKE[1, -1])
  spp <- LAKE[-1, 1] %>% pull()
  LAKE <- t(LAKE[-1, -1]) %>%
    as_data_frame() %>% 
    mutate_all(as.numeric)
  
  #assume NA == 0
  LAKE[is.na(LAKE)] <- 0
  colnames(LAKE) <- spp
  list(depth = depth, chiron = LAKE)
}

Njulla_fos <- getCore("Njulla")
Vuoskku_fos <- getCore("Vuoskku")
L850_fos <- getCore("850")

Njulla_est <- data_frame(depth = as.numeric(Njulla_fos$depth), min_pc = apply(Njulla_fos$chiron, 1, min_pc), est_count = 100/min_pc)

Njulla_est %>% filter(est_count < 50)

Njulla_plot <- ggplot(Njulla_est, aes(x = depth, y = est_count)) + 
  geom_hline(yintercept = 50, colour = "red") + 
  geom_point(aes(colour = est_count > 50), show.legend = FALSE) 
Njulla_plot

#Vuoskku
Vuoskku_est <- data_frame(depth = as.numeric(Vuoskku_fos$depth), min_pc = apply(Vuoskku_fos$chiron, 1, min_pc), est_count = 100/min_pc)

Vuoskku_est %>% filter(est_count < 50)

Njulla_plot %+% Vuoskku_est

#Lake 850
L850_est <- data_frame(
  depth = as.numeric(gsub("^(\\d{1,3})-.*", "\\1", L850_fos$depth)), 
  min_pc = apply(L850_fos$chiron, 1, min_pc),
  est_count = 100/min_pc)

L850_est %>% filter(est_count < 50)
L850_est %>% count(est_count)

Njulla_plot %+% L850_est

gridExtra::grid.arrange(
  Njulla_plot %+% L850_est + ggtitle("Lake850"),
  Njulla_plot+ ggtitle("Njulla"), 
  Njulla_plot %+% Vuoskku_est+ ggtitle("Vuoskku"), 
  ncol = 1 )


##redo reconstructions
library("assertr")
env <- read_excel("data/abisko/SwedenEnvData.xlsx", sheet = "EnvData") %>% 
  mutate(`Lake number` = as.numeric(`Lake number`)) %>% 
  filter(!is.na(`Lake number`))

spp <- read_excel("data/abisko/Sweden chiro.xlsx", sheet = "Sheet1") %>% 
  verify(X__1 == paste0("T", env$`Lake number`)) %>% 
  select(-X__1)

#WAPLS mod
library("rioja")
mod <- WAPLS(log1p(spp), env$`Tjul (°C)`) %>% crossval()
performance(mod)$crossval

#V
length(Vuoskku_fos$depth)
nrow(Vuoskku)
V_recons <- data_frame(
  depth = Vuoskku_fos$depth,
  Age = c(Vuoskku$Age, max(Vuoskku$Age) + seq(100, 400, 100)),
  Temperature = predict(mod, log1p(Vuoskku_fos$chiron))$fit[, 2],
  recon = "New"
) %>% bind_rows(
  Vuoskku %>% mutate(recon = "archive")
)

V_recons_plot <- ggplot(V_recons, aes(x = Age, y = Temperature, colour = recon, linetype = recon)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.02, .98), legend.justification = c(0, 1), legend.title = element_blank())
V_recons_plot

#850
length(L850_fos$depth)
nrow(L850)
L850_recons <- data_frame(
  depth = L850_est$depth,
  Age = L850$Age,
  Temperature = predict(mod, log1p(L850_fos$chiron))$fit[, 2],
  recon = "New"
) %>% bind_rows(
    data_frame(
      depth = L850_est$depth,
      Age = L850$Age,
      Temperature = predict(mod, (L850_fos$chiron))$fit[, 2],
      recon = "no log"
    ) , 
    L850 %>% mutate(recon = "archive")
  )

V_recons_plot %+% L850_recons

#N
length(Njulla_fos$depth)
nrow(Njulla)
N_recons <- data_frame(
  depth = as.numeric(Njulla_fos$depth),
  Age = c(Njulla$Age, max(Njulla$Age) + seq(100, 500, length = 14)),
  Temperature = predict(mod, log1p(Njulla_fos$chiron))$fit[, 2],
  recon = "New"
) %>% 
  bind_rows(
    data_frame(
      depth = as.numeric(Njulla_fos$depth),
      Age = c(Njulla$Age, max(Njulla$Age) + seq(100, 500, length = 14)),
      Temperature = predict(mod, (Njulla_fos$chiron))$fit[, 2],
      recon = "no log"
    ) ,
    Njulla %>% mutate(recon = "archive")
  )


V_recons_plot %+% N_recons



##stratigraphic plots
Njulla_fos$chiron %>% 
  select(which(colSums(. > 0) > 3)) %>% 
  select(which(sapply(., max) > 5)) %>% as.data.frame( ) %>% 
strat.plot(., yvar = as.numeric(Njulla_fos$depth), scale.percent = TRUE, y.rev = TRUE)

#
L850_fos$chiron %>% 
  select(which(colSums(. > 0) > 3)) %>% 
  select(which(sapply(., max) > 5)) %>% as.data.frame( ) %>% 
  strat.plot(., yvar = L850_est$depth, scale.percent = TRUE, y.rev = TRUE)

#
Vuoskku_fos$chiron %>% 
  select(which(colSums(. > 0) > 3)) %>% 
  select(which(sapply(., max) > 5)) %>% as.data.frame( ) %>% 
  strat.plot(., yvar = Vuoskku_est$depth, scale.percent = TRUE, y.rev = TRUE)

####
Njulla_fos$depth
Vuoskku_fos$depth
L850_fos$depth
Njulla_fos$chiron %>% slice(1:2) %>% select_if(~sum(.) > 0) %>% as.data.frame()
