#import data
speke_env <- read_excel("data/speke/temperature.xls")
speke_spp <- read_excel("data/speke/species.xls")
speke_fos <- read_excel("data/speke/core.xls")

speke_chron <- speke_fos %>% select(CodeNum:FullName)

speke_spp <- speke_spp %>% select(-(CodeNum:FullName))
speke_fos <- speke_fos %>% select(-(CodeNum:FullName))

lowCount <- c(13, 111, 134, 139)#outliers, probably not low count

# remove outliers
speke_spp <- speke_spp %>% filter(!speke_env$CodeNum %in% lowCount)
speke_env <- speke_env %>% filter(!CodeNum %in% lowCount)

#remove rare taxa
speke_spp <- speke_spp %>% select_if(~max(.) > 2)


#trasform
speke_fos1 <- decostand(speke_fos, method="hellinger")
speke_spp1 <- decostand(speke_spp, method="hellinger")




#fit model
# speke_mod0 <- WAPLS(speke_spp1, speke_env$July) %>% 
#   crossval()
# performance(speke_mod0)
# 
# data_frame(measured = speke_mod0$x, 
#            fitted = speke_mod0$fitted.values[, "Comp02"],
#            predicted = speke_mod0$predicted[, "Comp02"], 
#            lowc = 1:nrow(speke_spp) %in% lowCount) %>% 
#   ggplot(aes(x = measured, y = predicted, colour = lowc)) + geom_point()


#refit mod
speke_mod <- WAPLS(speke_spp1, speke_env$July) %>% 
  crossval()
performance(speke_mod)
rand.t.test(speke_mod)

data_frame(measured = speke_mod$x, 
           fitted = speke_mod$fitted.values[, "Comp03"],
           predicted = speke_mod$predicted[, "Comp03"]) %>% 
  ggplot(aes(x = measured, y = predicted)) + geom_point()


speke_pred <- data_frame(
  year = speke_chron$CodeNum,
  pred = predict(speke_mod, speke_fos1)$fit[, "Comp03"])

speke_pred %>% ggplot(aes(x = year, y = pred)) + 
  geom_point() + 
  geom_line() + 
  scale_x_reverse() + 
  geom_line(speke_temp2, mapping = aes(y = recon), colour = "red")


#analogue distances
d <- paldist(speke_spp) %>% 
  as.matrix() %>% 
  as.dist() %>% #hist()
  quantile(probs = c(.05, .1))

mat <- MAT(speke_spp, speke_env$July)
speke_pred$dist <- predict(mat, speke_fos)$dist.n[, 1] 

ggplot(speke_pred, aes(x = year, y = dist)) + geom_point() + 
  geom_hline(yintercept = d)

#residual length
rl <- analogue::residLen(speke_spp1, speke_env$July, speke_fos1, method = "cca")

plot(rl)
speke_pred$resLen <- rl$passive

ggplot(speke_pred, aes(x = year, y = resLen)) + geom_point() + 
  geom_hline(yintercept = quantile(rl$train, probs = c(.85, .95))) +
  scale_x_reverse()


#
est_n <- estimate_n(speke_fos, digits = 2) %>% 
  mutate(n = 1:n()) %>% 
  arrange(est_n)

est_n %>% 
  summarise(s = sum(est_n), m = mean(est_n))

percent_checker(speke_fos, digits = 2) %>% 
  select(-one_max, -one_min, -est_max, -est_min)


valley2 <- valley %>% 
  select(year,Jul) %>% 
  mutate(
    smo3 = rollmean(Jul, k = 3, na.pad = TRUE, align = "center"),
    smo5 = rollmean(Jul, k = 5, na.pad = TRUE, align = "center")) %>% 
  select(-Jul) %>% 
  gather(key = smooth, value = Jul, -year)

speke_chron %>% 
  mutate(top = CodeNum, base = lead(CodeNum)) %>% 
  crossing(valley2) %>% 
  filter(year == base) %>% 
  ggplot(aes(x = base, y = Jul, colour = smooth)) + 
  geom_line() + geom_point() +
  geom_line(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) + 
  geom_point(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) +
  geom_line(data = valley, aes(x = year, y = Jul), colour = "grey40")

speke_reported_climate <- speke_chron %>%
  mutate(top = CodeNum, base = lead(CodeNum)) %>%
  crossing(valley) %>%
  filter(year >= base, year < top) %>%
  group_by(base) %>%
  summarise(Jul = mean(Jul)) %>% 
  arrange(desc(base)) %>% 
  bind_cols(speke_pred %>% slice(1:17)) %>% 
  slice(-17) 

speke_reported_climate %>% 
  ggplot(aes(x = base, y = Jul)) + 
  geom_line(colour = "red") + 
  geom_point(colour = "red") +
  geom_line(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) + 
  geom_point(data = speke_temp2, aes(x = year, y = inst), colour  = "blue", alpha = 0.5) +
  geom_line(data = valley, aes(x = year, y = Jul), colour = "grey40")

speke_reported_climate %$% 
  cor.test(Jul, pred) 
  

#time track
analogue::timetrack(speke_spp1, speke_fos1, speke_env$July) %>% plot()

#coverage plot
speke_n <- data_frame(taxa = names(speke_spp), 
                      max = sapply(speke_spp, max), 
                      n2 = Hill.N2(speke_spp))
speke_fosn <- data_frame(taxa = names(speke_fos), 
                      max = sapply(speke_fos, max), 
                      n2 = Hill.N2(speke_fos)) %>% 
  filter(max > 0) 

speke_max_n <- speke_n %>% 
  full_join(speke_fosn, by = "taxa", suffix = c("_spp", "_fos")) 

speke_max_n %>% 
  ggplot(aes(x = max_spp, y = max_fos, colour = n2_spp > 5)) + 
  geom_point() +
  geom_abline() +
  geom_text(data = filter(speke_max_n, max_fos > 2 * max_spp), aes(label = taxa, colour = n2_spp > 5))





####
fos <- decostand(speke_fos, method="hellinger")
spp <- decostand(speke_spp, method="hellinger")
env <- speke_env$July
plot(cca(spp~env))

rlen<-analogue::residLen(spp, env, fos, method="cca")
rlen
plot(rlen) # distribution of modern and fossil residual lengths
plot(speke_chron$CodeNum, rlen$passive, ylab="Squared residual length", xlab="Depth")
abline(h=quantile(rlen$train, probs = c(0.75,0.95)), col=c("orange", "red"))
#

goodpoorbad <- quantile(rlen$train, prob=c(0.75, 0.95))
qualitybands <- data.frame(xmin = rep(-Inf, 3),
                           xmax = rep(Inf, 3),
                           ymax = c(goodpoorbad, Inf),
                           ymin = c(-Inf, goodpoorbad),
                           fill = factor(c("Good", "Fair", "None"), levels = c("None", "Fair", "Good")))

fillscale <-  scale_fill_manual(values = c("salmon", "lightyellow", "skyblue"), name = "Analogue Quality")

g <- ggplot(data.frame(chron = speke_chron$CodeNum, analogue =  rlen$passive)) +
  geom_point(aes(x = chron, y = analogue)) +
  labs(x = "Date CE", y = "Squared residual length") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), qualitybands, alpha = .5) +
  fillscale
print(g)
