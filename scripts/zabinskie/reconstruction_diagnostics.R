## ---- reconstruction_diagnostics

#distance to nearest neigbour in calibration set
zabinskie_distance_to_nearest_neighbour <- function(spp, env, fos, chron){
  mat <- MAT(spp/100, env)
  dist1_mod <- mat$dist.n[, 1]
  
  goodpoorbad <- quantile(dist1_mod, probs = c(0.75, 0.95))
  
  #fossil distances
  pred_mat <- predict(mat, fos/100)
  dist_to_analogues <- chron %>% 
    mutate(
      dist_to_analogues = pred_mat$dist.n[, 1],
      quality  = cut(dist_to_analogues, breaks = c(0, goodpoorbad, Inf), labels = c("good", "poor", "bad"))
    )
  attr(dist_to_analogues, which = "goodpoorbad") <- goodpoorbad
  return(dist_to_analogues)
}



# ## ---- dist_to_analogues_output
# sum(dist_to_analogues$dist_to_analogues > goodpoorbad[2])
# sum(dist_to_analogues$dist_to_analogues > max(dist1_mod))


## ---- residual_length
zabinskie_residual_length <- function(spp, env, fos, chron){
  rlen <- analogue::residLen(sqrt(spp), env, sqrt(fos), method="cca")
  rlen_thresholds <- quantile(rlen$train, probs = c(0.9, 0.95))
  
  rlen_quality <- chron %>% 
    mutate(
      rlen = rlen$passive,
      quality  = cut(rlen, breaks = c(0, rlen_thresholds, Inf), labels = c("good", "poor", "very poor"))
    )
  attr(rlen_quality, "goodpoorbad") <- rlen_thresholds
  return(rlen_quality)
}

# sqd_plot <- chron %>% mutate(rlen = rlen$passive) %>% 
#   ggplot(aes(x = year, y = rlen)) + 
#   geom_point() + 
#   geom_hline(yintercept = rlen_thresholds, colour = c("blue", "orange", "red"), linetype = c(1, 1, 1)) +
#   labs(x = "Date CE", y = "Squared residual distance")
# 
# sum(rlen$passive > quantile(rlen$train, probs = 0.99))
# sum(rlen$passive > quantile(rlen$train, probs = 0.95))
# sum(rlen$passive > quantile(rlen$train, probs = 0.90))
# nrow(fos)

# ## ---- randomTF
# rtf <- randomTF(sqrt(as.data.frame(spp)), env, fos, n = 999, fun = WAPLS, col = 2)
# 
# ## ---- plot_rtf
# plot(rtf)
