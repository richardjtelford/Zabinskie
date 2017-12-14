## ---- tf_choice

wapls2 <- WAPLS(sqrt(spp), env) %>% crossval()
pls3 <- WAPLS(sqrt(spp), env, iswapls = FALSE) %>% crossval()
wa_mono <- WA(sqrt(spp), env, mono = TRUE) %>% crossval()
ml <- MLRC(spp/100, env, mono = TRUE) %>% crossval()
mat <- MAT(spp, env) 

perform <- list(
  `WAPLS-2` = performance(wapls2)$crossval["Comp02", , drop = FALSE],
  `PLS-3` = performance(pls3)$crossval["Comp03", , drop = FALSE],
  `WA-mono` = performance(wa_mono)$crossval["WA.m", , drop = FALSE],
  `ML` = performance(ml)$crossval,
  `MAT` = performance(mat)$object["N03.wm", , drop = FALSE]
) %>% 
  map_df(as_data_frame, .id = "Method") %>% 
  select(Method, RMSEP = RMSE, R2, `Maximum Bias` = Max.Bias)


recons <- data_frame(
  year = chron$year, 
  WAPLS_2 = predict(wapls2, sqrt(fos), npls = 2)$fit[, 2],
  PLS_3 = predict(pls3, sqrt(fos))$fit[, 3],
  WA_mono = predict(wa_mono, sqrt(fos))$fit[, "WA.m"],
  ML = predict(ml, fos/100)$fit[, 1],
  MAT = predict(mat, fos, k = 3)$fit[, "MAT.wm"]
  ) 

tf_cors <- cor(select(recons, -year))

cor_time <- recons %>% left_join(instrumental_temperature) %>% select(-new) %>% summarise_at(.vars = vars(WAPLS_2:MAT), .funs = cor, y = .$old)

tf_table <-  bind_cols(perform, data.frame(cor = t(cor_time)))

tf_plot <- recons %>% 
  gather(key = method, value = recon, -year) %>% 
  ggplot(aes(x = year, y = recon, colour = method)) +
  geom_path() +
  labs(x = "Year CE", y = "August air temperature °C", colour = "Method")

