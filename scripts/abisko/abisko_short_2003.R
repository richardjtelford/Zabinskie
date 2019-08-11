## ---- reported_correlations

r_p <- function(r, n, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  df <- n - 2
  STAT <- r/sqrt((1 - r^2)/df)
  PVAL <- switch(alternative, 
         less = pt(STAT, df), 
         greater = pt(STAT, df, lower.tail = FALSE), 
         two.sided = 2 * min(pt(STAT, df), pt(STAT, df, lower.tail = FALSE)))
  return(PVAL)
}

# x <- rnorm(10)
# y <- rnorm(10)
# cor.test(x, y)
# r_p(cor(x, y), 10)
abisko_reported <- function(){
  abisko_short <- read_csv( 
  "Lake,   n_strat, n_recon,    r, p
  Njulla,       20,      19, 0.39, 0.05
  Lake850,     22,      23, 0.365, 0.1
  Vuoskkujavri, 17,      14, 0.35, 0.05
  Alanen,       24,      24, 0.37, 0.05")
  abisko_short
}

## ---- recalc_pval
# abisko_short %>% 
#   rowwise() %>% 
#   mutate(p_greater = r_p(r, n_recon, "greater"))

## ---- similar_correlations
abisko_similar_correlations <- function(abisko_short){
  cv <- matrix(c(1, rep(mean(abisko_short$r), 2), 1), nrow = 2)#mean correlation
  
  sd_cor_sim <- replicate(10000, {
    cors <- sapply(abisko_short$n_recon, function(n){
      sim <- mvtnorm::rmvnorm(n = n, sigma = cv)
      cor(sim[, 1], sim[, 2])
    })
    sd(cors)
  })
  p_low_cor <- mean(sd_cor_sim <= sd(abisko_short$r))
  p_low_cor
}
# tibble(sd = sd_cor_sim) %>% 
#   ggplot(aes(x = sd)) + 
#   geom_histogram() + 
#   geom_vline(xintercept = sd(abisko_short$r))

## ---- digitised
abisko_digitised <- function(){
  #lake850
  lake850 <- read.table(file_in("data/abisko/lake850.txt")) %>% 
    select(1:2) %>% 
    setNames(c("date", "temp")) %>% 
    mutate(type = rep(c("reconstruction", "instrumental"), each = 23),
           date = round(date))
  
  time_plot850 <- lake850 %>% ggplot(aes(x = date, y = temp, colour = type), shape = type) +
    geom_point()
  time_plot850
  
  #lakeV
  lakeV <- read.table(file_in("data/abisko/lakeV.txt")) %>% 
    select(1:2) %>% 
    setNames(c("date", "temp")) %>% 
    mutate(type = rep(c("reconstruction", "instrumental"), each = 14),
           date = round(date))
  
  time_plot850 %+% lakeV
  
  #lakeA
  lakeA <- read.table(file_in("data/abisko/lakeA.txt")) %>% 
    select(1:2) %>% 
    setNames(c("date", "temp")) %>% 
    mutate(type = rep(c("reconstruction", "instrumental"), each = 24),
           date = round(date))
  
  time_plot850 %+% lakeA 
  
  #lakeN
  lakeN <- read.table(file_in("data/abisko/lakeN.txt")) %>% 
    select(1:2) %>% 
    setNames(c("date", "temp")) %>% 
    mutate(type = rep(c("reconstruction", "instrumental"), each = 19),
           date = round(date))
  
  
  lakeN_ages <- c(1999, 1997, 1995, 1993, 1991, 1989, 1987, 1984, 1982, 1978, 1975, 1971, 1967, 1961, 1955, 1949, 1942, 1935, 1929, 1918)
  
  time_plot850 %+% lakeN + 
    geom_vline(xintercept = lakeN_ages, data = NULL)
  
  all_lakes <- bind_rows(Njulla = lakeN, 
                         Lake850 = lake850, 
                         Vuoskkujavri = lakeV, 
                         Alanen = lakeA, .id = "lake") %>%
    spread(key = type, value = temp) 
  
  all_lakes
}

abisko_plot_all_lakes <- function(abisko_all_lakes){
  abisko_all_lakes %>%    
    mutate(lake = factor(lake, levels = rev(c("Njulla", "Lake850", "Vuoskkujavri", "Alanen")), labels = rev(c("Njulla", "Lake850", "Vuoskkujavri", "Alanen Laanijavri")))) %>%
    ggplot(aes(x = instrumental, y = reconstruction, colour = lake)) + 
    geom_smooth(method = "lm") +
    geom_point() +
    coord_equal() +
    labs(x = "Instrumental July temperature °C", y = "Reconstructed July temperature °C") +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
    theme(legend.position = c(0.01, 0.99), legend.justification =  c(0, 1), legend.title = element_blank(), legend.background = element_blank())
}
## ---- all_lakes_corelation
abisko_correlations <- function(abisko_all_lakes){ 
  abisko_all_lakes %>% 
    group_by(lake) %>% 
    do(tibble(r = cor(.$reconstruction, .$instrumental)))
}

abisko_check <- function(abisko_short, abisko_cor){
  joined <- abisko_cor %>% 
    left_join(abisko_short, by = c("lake" = "Lake")) %>% 
    mutate(agrees = abs(r.x - r.y) < 0.01)
  #Alanen correlation is correct
  allen_correct <- joined %>% filter(lake == "Alanen") %>% pull(agrees)
  #Only Alanen is correct
  only <- sum(joined$agrees) == 1
  stopifnot(allen_correct, only)
  TRUE
}

# ## ---- overlay_images
# makefig<-function(g){
#   gt <- ggplot_gtable(ggplot_build(g))
#   gt$layout$clip[gt$layout$name == "panel"] <- "off"
#   grid.draw(gt)
# }
# 
# library(magick)
# library(grid)
# fig5va <- image_read("data/abisko_short5a.png")
# 
# 
# V_plot <- ggplot(lakeV, aes(x = date, y = temp, colour = type)) +
#   annotation_custom(
#     rasterGrob(as.raster(fig5va), width=unit(1,"npc"), height=unit(1,"npc")),
#     xmin = 1900, xmax = 2003, ymin = -3, ymax = 15) +
#   geom_point(size = 2, shape = 1) +
# #  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
#   theme_classic() +
#   theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))
# 
# makefig(V_plot)
# 
# 
# #A
# A_plot <- ggplot(lakeA, aes(x = date, y = temp, colour = type)) +
#   annotation_custom(
#     rasterGrob(as.raster(fig5va), width=unit(1,"npc"), height=unit(1,"npc")),
#     xmin = 1933, xmax = 2001.5, ymin = 7.2, ymax = 21) +
#   geom_point(size = 2, shape = 1) + 
#   #  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
#   theme_classic() +
#   theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))
# 
# makefig(A_plot)
# 
# 
# #n
# fig5n <- image_read("data/abisko_short5n.png")
# 
# 
# N_plot <- ggplot(lakeN, aes(x = date, y = temp, colour = type)) +
#   annotation_custom(
#     rasterGrob(as.raster(fig5n), width=unit(1,"npc"), height=unit(1,"npc")),
#     xmin = 1902, xmax = 2003, ymin = 4, ymax = 12.7) +
#   geom_point(size = 2, shape = 1) + 
#   #  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
#   theme_classic() +
#   theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))
# 
# makefig(N_plot)


# ## ---- digitised_instrumental
# kiruna <- read.table("data/abisko/kiruna.txt") %>% 
#   select(1:2) %>% 
#   setNames(c("date", "temp")) %>% 
#   mutate(date = round(date)) %>% 
#   full_join(data.frame(date = min(.$date):max(.$date))) %>% 
#   arrange(date)
# 
# diff(kiruna$date)
# 
# 
# 
# kiruna <- kiruna %>%
#   mutate(
#     smo = rollapply(temp, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = "c"),
#     smo2 = {temp2 <- c(rep(NA, 5),temp, rep(NA, 5))
#             temp2[is.na(temp2)] <- mean(temp, na.rm = TRUE)
#             out <- rollapply(temp2, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = "c")
#             out[-c(1:5, (length(out) - 4):length(out))]
#             }
#          )
# 
# kiruna %>% ggplot(aes(x = date, y = temp)) + 
#   geom_point() +
#   geom_line(aes(y = smo)) +
#   geom_line(aes(y = smo2), colour = 2)
