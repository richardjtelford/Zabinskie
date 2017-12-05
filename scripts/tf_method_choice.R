## ---- tf_choice

wapls2 <- WAPLS(sqrt(spp), env) %>% crossval()
pls3 <- WAPLS(sqrt(spp), env, iswapls = FALSE) %>% crossval()
wa_mono <- WA(sqrt(spp), env, mono = TRUE) %>% crossval()
ml <- MLRC(spp/100, env, mono = TRUE) %>% crossval()
mat <- MAT(spp, env) 

rbind(
  performance(mat)$object["N03.wm", , drop = FALSE],
  performance(wapls2)$crossval["Comp02", , drop = FALSE],
  performance(pls3)$crossval["Comp03", , drop = FALSE],
#  performance(wa_mono)$crossval["WA.m", , drop = FALSE],
  performance(ml)$crossval
) %>% as.data.frame() %>% 
  mutate(method = c("MAT-w3", "WAPLS-2", "PLS-3", "ML")) %>% 
  select(method, RMSEP = RMSE, R2)

recons <- data_frame(
  year = chron$year, 
  WAPLS_2 = predict(wapls2, sqrt(fos), npls = 2)$fit[, 2],
#  WA_mono = predict(wa_mono, sqrt(fos))$fit[, "WA.m"],
  PLS_3 = predict(pls3, sqrt(fos))$fit[, 3],
  ML = predict(ml, fos/100)$fit[, 1],
  MAT = predict(mat, fos, k = 3)$fit[, "MAT.wm"]
  ) 

cor(select(recons, -year))

recons %>% 
  gather(key = method, value = recon, -year) %>% 
  ggplot(aes(x = year, y = recon, colour = method)) +
  geom_path() +
  labs(x = "Year CE", y = "August air temperature °C", colour = "Method")

