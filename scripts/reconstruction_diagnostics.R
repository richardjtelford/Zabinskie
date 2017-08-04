## ---- reconstruction_diagnostics

#distance to nearest neigbour in calibration set
mat <- MAT(spp, env)
dist1_mod <- mat$dist.n[, 1]

goodpoorbad <- quantile(dist1_mod, probs = c(0.75, 0.95))

qualitybands <- data.frame(xmin = rep(-Inf, 3), 
                           xmax = rep(Inf, 3), 
                           ymax = c(goodpoorbad, Inf), 
                           ymin = c(-Inf, goodpoorbad), 
                           fill = factor(c("Good", "Fair", "None"), levels = c("None", "Fair", "Good")))

#fossil distances
pred_mat <- predict(mat, fos)
dist_to_analogues <- chron %>% 
  mutate(dist_to_analogues = pred_mat$dist.n[, 1])


## ---- dist_to_analogues_plot
fillscale <-  scale_fill_manual(values = c("salmon", "lightyellow", "skyblue"), name = "Analogue Quality")

dist_to_analogues_plot <- ggplot(dist_to_analogues, aes(x = year, y = dist_to_analogues)) + 
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), qualitybands, alpha = .5, inherit.aes = FALSE) + 
  geom_point() + 
  labs(x = "Date CE", y = "Squared chord distance to nearest analogue") +
  fillscale

## ---- dist_to_analogues_output
sum(dist_to_analogues$dist_to_analogues > goodpoorbad[2])
sum(dist_to_analogues$dist_to_analogues > max(dist1_mod))


## ---- residual_length

rlen <- analogue::residLen(sqrt(spp), env, sqrt(fos), method="cca")
rlen_thresholds <- quantile(rlen$train, probs = c(0.9, 0.95, 0.99))

sqd_plot <- chron %>% mutate(rlen = rlen$passive) %>% 
  ggplot(aes(x = year, y = rlen)) + 
  geom_point() + 
  geom_hline(yintercept = rlen_thresholds, colour = c("blue", "orange", "red"), linetype = c(1, 1, 1)) +
  labs(x = "Date CE", y = "Squared residual distance")

sum(rlen$passive > quantile(rlen$train, probs = 0.99))
sum(rlen$passive > quantile(rlen$train, probs = 0.95))
sum(rlen$passive > quantile(rlen$train, probs = 0.90))
nrow(fos)

## ---- randomTF
library("palaeoSig")
rtf <- randomTF(as.data.frame(spp), env, fos, n = 999, fun = WAPLS, col = 2)

## ---- plot_rtf
plot(rtf)
