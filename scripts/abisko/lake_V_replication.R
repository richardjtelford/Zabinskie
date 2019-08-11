#Lake V digitised
library(tidyverse)
library(rioja)

Lake_V <- read.csv("abisko/data/Lake_V_strat.csv", skip = 5)
Lake_V
Lake_V %>% 
  select(-Year, -counts) %>% 
  rowSums()


clust <- chclust(dist(sqrt(select(Lake_V, -Year, -counts))), method="coniss")
bstick(clust)


x <- strat.plot(d = select(Lake_V, -Year, -counts), yvar = Lake_V$Year, scale.percent = TRUE, plot.bar = TRUE, plot.line = FALSE, xLeft = .14, lwd.bar = 2, col.bar = scales::muted("red"), clust = clust)

# add zones
addClustZone(x, clust, 5, col="red")


########### load calibration set
library("assertr")
library("readxl")
env <- read_excel("abisko/data/SwedenEnvData.xlsx", sheet = "EnvData") %>% 
  mutate(`Lake number` = as.numeric(`Lake number`)) %>% 
  filter(!is.na(`Lake number`))

spp <- read_excel("abisko/data/Sweden chiro.xlsx", sheet = "Sheet1") %>% 
  verify(...1 == paste0("T", env$`Lake number`)) %>% 
  select(-...1)

list(sort(names(Lake_V)), sort(names(spp)))

mod_raw <- WAPLS(spp, env$`Tjul (°C)`, npls = 5) %>% crossval()
performance(mod_raw)
rand.t.test(mod_raw)
coef(mod_raw) %>% as.data.frame()  %>% pairs(gap = 0)



mod <- WAPLS(log1p(spp), env$`Tjul (°C)`) %>% crossval()
performance(mod)

v_dict <- read.csv(comment = "#", strip.white = TRUE, stringsAsFactors = FALSE, text = 'long, short
"Allopsectrocladius",            "Psecga"
"Corynocera_oliveri",               "Corygoli"
"Coynocera_ambigua",                "Cory amb"           
"Crictopus_undiff",                 "Cricind"
"Dicrotendipes",                    "Dicrind"
"H_brundini",                    "Hetagmaa"
"Heterotanytarsus",              "Hetaind"
"H_grimshawi",                   "Hetaggri" 
"H_marcidus",                    "Hetagmar"
"Micropsectra_insignilobus",       "Micpgins" 
"Microtendipes",                 "Mictind"
"Orthocladius_undiff",           "Orthind"
"Paratanytarsus",                "Pataind"
"Pentaneurini",                  "Pentind"
"Procladius",                    "Procind"
"Psectrocladius_septentrionalis", "Psecgb"
"Sergentia",                          "Sergind" 
"Tanytarsus_lugens.group",          "Tany lug"
"Tanytarsus_spB",                   "Tanygb"
"Zalutschia_sp",                 "Zalu lin" #perhaps
"Zalutschia_zalutschicola",           "Zalu zal"
')

# unused calibration sett axa: "Abisind" "Chirgant" "Chirgplu" "Clapglat" "Clatgma1" "Cons bre" "Corygscu" "Criortho" "Diamind" "Einfind" "Heanind" "Hetagsub" "Hydrind" "Micpgkap" "Micpind" "Nanoind" "Olivind" "Paga oro" "Pakiind"  "Polyind" "Protind" "Psecgc" "Psecind" "Pseuind" "Tanygc" "Tanygpal" "Tanyind" 


names(Lake_V) <- plyr::mapvalues(names(Lake_V), from = v_dict$long, to = v_dict$short)
names(Lake_V)


preds <- predict(mod, log1p(select(Lake_V, -Year, -counts)))

ggplot(tibble(year = Lake_V$Year, pred = preds$fit[, "Comp02"]), aes(year, pred)) + geom_line() + geom_point()
