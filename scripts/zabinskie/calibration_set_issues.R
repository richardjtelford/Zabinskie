#Calibration set errors
## ---- Lac_AH
zabinskie_lac_AH <- function(spp, sites){
  lac_AH <- spp %>% 
    filter(sites$Lake %in% c("Lac A","Lac H")) %>% 
    select_if(colSums(.)>0) 
  
  rowSums_lac_H <- rowSums(lac_AH)[2]
  min_lac_H <- min(lac_AH[2, ])
  
  lac_AH[2, ] <- lac_AH[2, ]/min_lac_H
  assertthat::assert_that(isTRUE(
      n_distinct(round(lac_AH, 4)) == 1
    ))
  lac_AH <- list(min_lac_H = min_lac_H,  sum_lac_H = rowSums_lac_H)
  return(lac_AH)
}

# ## ---- count_sums
# countSums <- spp_all %>% 
#   rowwise() %>% 
#   do(tibble(min = min(unlist(.)[unlist(.) > 0]))) %>% 
#   mutate(apparent_count = 100/min) %>% 
#   bind_cols(sites_all) %>% 
#   mutate(lowCount = Lake %in% lowCount) %>% 
#   ungroup() %>% 
#   mutate(multiple = {
#     offsets <- (spp_all/min) %% 1
#     offsets[offsets > 0.5] <- offsets[offsets > 0.5] - 1
#     offsets <- abs(offsets)
#     apply(offsets < 0.1 | matrix(between(as.matrix(offsets), 0.45, 0.55), nrow = nrow(offsets)), 1, all)
#     })
# 
# countSums %>% filter(source == "Poland") %>% 
#   ggplot(aes(x = lowCount, y = apparent_count, colour = multiple)) +
#   geom_hline(yintercept = 50, colour = "grey40", linetype = "dashed")+
#   geom_jitter(height = 0, width = 0.3) +
#   ylim(0, NA)
# 
# countSums %>% filter(source != "Poland") %>% 
#   ggplot(aes(x = lowCount, y = apparent_count, colour = multiple)) +
#   geom_hline(yintercept = 50, colour = "grey40", linetype = "dashed")+
#   geom_jitter(height = 0, width = 0.3) +
#   ylim(0, NA)
# 
# 
# ## ---- fos_count_sums
# 
# fos_countSums <- fos %>% 
#   rowwise() %>% 
#   do(tibble(min = min(unlist(.)[unlist(.) > 0]))) %>% 
#   mutate(apparent_count = 100/min) %>% 
#   ungroup() %>% 
#   mutate(reported_count = rowSums(fos_counts))
# 
# fos_countSums %>% ggplot(aes(x = reported_count, y = apparent_count)) +
#   geom_abline(slope = 1, colour = "grey40", linetype = "dashed") + 
#   geom_abline(slope = 2, colour = "grey60", linetype = "dashed") + 
#   geom_point()
# 
# #check multiples
# 
# k <- spp_all/countSums$min 
#   k2 <- (k < 0.1 | matrix(between(as.matrix(k), 0.45, 0.55), nrow = nrow(k)))
#   
# colSums(!k2)
