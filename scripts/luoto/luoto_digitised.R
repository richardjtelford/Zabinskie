library(tidyverse)
library(rioja)
library(broom)
library(magrittr)
library("countChecker")
library(palaeoSig)
library("vegan")

#digitised luoto climate
page5 <- readLines("luoto/data/outfile5u.pdf")
start <- grep("/PlacedGraphic", page5)[1]
end <- grep("[(Figur)20(e 4.)]TJ", page5, fixed = TRUE)

fig4 <- page5[start:end]
  




inst <- read.table("luoto/data/measured.txt") %>% 
  select(temp = V1, year = V2) %>% 
  mutate(year = round(year)) %>% 
  arrange(desc(year))

inst %>% ggplot(aes(x = year, y = temp)) +
  geom_line() +
  geom_point() +
  coord_flip()





#digitised luoto
page4 <- readLines("luoto/data/outfile2.pdf")
start <- grep("/PlacedGraphic", page4)[2]
end <- grep("[(Figur)20(e 2.)]TJ", page4, fixed = TRUE)

page4 <- page4[start:end]

fig2 <- page4[grepl("re$", page4)] %>%
  read.table(text = .) %>% 
  set_names(c("x", "y", "width", "height", "re"))

taxa <- c("Abl_mon", "Dero", "Proc", "Thie", "Chi_ant", "Cla_lat", "Cryp", "Dic_ner", "End_alb", "End_imp", "End_ten", "Gly_pal", "Micr", "Mic_ped", "Pha_fla", "Pol_nub", "Seg_cor", "Trib", "Clad", "Cons", "Stem", "Stemlla", "Tan_chi", "Tan_gla", "Tan_lug", "Tan_men", "Tan_pal", "Tan_uid", "Cory_arc", "Cory_cor", "Cri_cyl", "Cri_int", "Nan_rec", "Par_bat", "Par_tri", "Pse_fla", "Pse_sep", "Pse_sor")


fig2 <- fig2 %>% 
  filter(between(height, -2.4, -2.3)) %>% 
  mutate(taxa = factor(x, labels = taxa),#relabel
         percent = width/max(width) * 93,
         depth = (1 - (y - min(y))/(max(y) - min(y))) * 48, 
         depth = round(depth)
  )
  

fig2 %>% 
  ggplot(aes(x = x + width/2, y = depth, width = width, height = 1, fill = factor(x, levels = sample(unique(x))))) +
  geom_tile(show.legend = FALSE) +
  scale_y_reverse()


fig2_fat <- fig2 %>% 
  select(-re, -height, -x, -width, -y) %>% 
  spread(key = taxa, value = percent, fill = 0) %>%
  as_tibble()
  
#estimate_n
estimate_n(fig2_fat %>% select(-depth), digits = 2) %>% 
  mutate(depth =  fig2_fat$depth) %>% 
  arrange(est_n)
        
fig2 %>% filter(depth == 43)
fig2 %>% filter(depth == 42)
fig2 %>% filter(depth == 30)


mod <- fig2_fat %>% 
  select(-depth) %>% 
  slice(1:nrow(inst)) %>% 
  select_if(colSums(.) > 0) %>% 
  #sqrt() %>% 
  WA(x = inst$temp, tolDW = TRUE) 


mod_loo <- crossval(mod)
performance(mod_loo)



#h-block
tdist <- as.matrix(fig2_fat %>% 
                     select(depth) %>% 
                     slice(1:nrow(inst)) %>% 
                     dist()
)
mod_h <- crossval(mod,
    cv.method = "h-block",
    h.cutoff = 2, 
    h.dist = tdist)
performance(mod_h)

RNE <- fig2_fat %>% 
  select(-depth) %>% 
  slice(1:nrow(inst)) %>% 
  select_if(colSums(.) > 0) %>% 
  rne(env = inst$temp, geodist = tdist, fun = WA, neighbours = c(0, 1.001, 2.001), tolDW = TRUE)
RNE
plot(RNE, which = 3)


pca <- rda(fig2_fat %>% select(-depth) %>% sqrt())
screeplot(pca, bstick = TRUE)

plot(pca)


result <- data_frame(
  year = c(inst$year, seq(1827, 1785, length = 7)),
  temp = c(inst$temp, rep(NA, 7)),
  depth = fig2_fat$depth,
  cit_fitted = c(fitted(mod_loo)[, "WA.inv.tol"], rep(NA, 7)),
  cit_pred = c(mod_loo$predicted[, "WA.inv.tol"], rep(NA, 7)),
  cit_pred_h = c(mod_h$predicted[, "WA.inv.tol"], rep(NA, 7)),
  pc1 = -scores(pca, choice = 1, display = "sites")[, "PC1"]
  ) 

result %>% ggplot(aes(x = temp, y = cit_pred)) +
  geom_abline(colour = "grey40", linetype = "dashed") +
  geom_point() +
#  geom_smooth() +
  coord_equal() +
  labs(x = "Measured July temperature °C", y = "Predicted July temperature °C")

inst %>% mutate(d = temp - lag(temp)) %>% ggplot(aes(x = year, y = d)) + geom_line()


result %$% cor.test(temp, pc1, use = "pair")
result %$% cor.test(temp, cit_pred_h, use = "pair")

lapply(1:42, function(i) result %>% slice(-i)) %>% 
  purrr::map(~cor.test(.$temp, .$cit_pred, use = "pair")) %>% 
  map_df(tidy)


result %>% ggplot(aes(x = temp, y = pc1, colour = year)) + geom_point() + geom_path() + geom_smooth()

ggplot(result, aes(x = temp, y = cit_pred, label = depth)) + geom_text()

result %$% cor.test(temp, cit_fitted, use = "pair")
result %$% cor.test(temp, cit_pred, use = "pair")

coef(mod)

#detrend
result %>% 
  filter(!is.na(temp)) %>%
  mutate(
    detrend_temp = resid(lm(temp ~ year)),
    detrend_cit_pred = resid(lm(cit_pred ~ year))
  ) %$% 
  cor.test(detrend_temp, detrend_cit_pred)
  

result %>% 
  gather(key  = key, value = temperature, -year, -depth) %>% 
  mutate(key = factor(key, levels = c("temp", "pc1", "cit", "cit_pred")),
         key = recode(key, temp = "Instrumental", pc1 = "PC1", cit = "CiT", cit_pred = "CiT Prediction")) %>% 
  
  ggplot(aes(x = year, y = temperature)) + 
  geom_line() +
  facet_wrap(~ key, nrow = 1, scales = "free_x") + 
  coord_flip() +
  labs(x = "Year", y = "")

lm(temp ~ year, inst) %>% summary()
inst %$% cor(year, temp)


data_frame(
  dc = fig2_fat %>% 
    select(-depth) %>% 
    vegdist() %>% 
    as.vector(),
  dy = dist(fig2_fat$depth) %>% 
    as.vector()
  ) %>% 
  ggplot(aes(x = dy, y = dc)) + 
  geom_point() + 
  geom_smooth()

data_frame(
  dc = inst %>% 
    select(temp) %>% 
    dist() %>% 
    as.vector(),
  dy = dist(inst$year) %>% 
    as.vector()
) %>% 
  ggplot(aes(x = dy, y = dc)) + 
  geom_point() + 
  geom_smooth()


data_frame(
  dc = fig2_fat %>% 
    select(-depth) %>%
    slice(1:nrow(inst)) %>% 
    vegdist() %>% 
    as.vector(),
  dt = dist(inst$temp) %>% 
    as.vector(),
  dy = dist(inst$year) %>% 
    as.vector(), 
  dd = fig2_fat %>% 
    select(depth) %>%
    slice(1:nrow(inst)) %>% 
    dist() %>% 
    as.vector()
  
) %>% 
  ggplot(aes(x = dt, y = dc, colour = dd == 1)) + 
  geom_point() + 
  geom_smooth() 




d <- fig2_fat %>% 
  select(-depth) %>% 
  vegdist() %>% 
  as.matrix() 
diag(d) <- Inf
which(d < 0.25, arr.ind = TRUE)

fig2_fat %>% 
  select(-depth) %>% 
  vegdist() %>%
  as.vector() %>% 
  data_frame() %>% 
  ggplot(aes(x = .)) + geom_histogram()


fig2 %>% filter(depth %in% (c(5, 6) - 1)) %>% select(taxa, percent, depth) %>% spread(depth, percent)

fig2 %>% filter(depth %in% (c(40, 41) - 1)) %>% select(taxa, percent, depth) %>% spread(depth, percent)






spp_info <- data_frame(
    name = names(fig2_fat), 
    N2 = Hill.N2(fig2_fat), 
    max = apply(fig2_fat, 2, max)
  ) %>% 
  slice(-1)

spp_info %>% 
  ggplot(aes(x = max, y = N2, label = name)) + 
  geom_point() +
  ggrepel::geom_text_repel()

spp_info %>% ggplot(aes(x = N2)) +geom_histogram()

fig2 %>% 
  left_join(spp_info, by = c("taxa" = "name")) %>% 
  mutate(N2 = cut(N2, breaks = c(0, 5, 10, Inf), labels = c("< 5", "5 - 10", "> 10"))) %>% 
  group_by(depth, N2) %>% 
  summarise(sum = sum(percent)) %>% 
  complete(depth, N2, fill = list(sum = 0)) %>% 
  ggplot(aes(x = depth, y = sum, fill = N2)) +
  geom_area(position = "stack") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "YlOrRd")

