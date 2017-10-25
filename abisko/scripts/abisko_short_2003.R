library(zoo)
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

abisko_short <- read_csv( 
"Lake,n_strat,n_recon,r,p
Njulla,20,19,.39,.05
Lake 850,22,23,.365,.1
Vuoskkujavri,17,14, 0.35, 0.05
Alanen,24,24,0.37, 0.05")

## ---- recalc_pval
abisko_short %>% 
  rowwise() %>% 
  mutate(p_greater = r_p(r, n_recon, "greater"))

## ---- n_samples
sum(abisko_short$n_recon)

## ---- similar_correlations
cv <- matrix(c(1, rep(mean(abisko_short$r), 2), 1), nrow = 2)

sd_cor_sim <- replicate(10000, {
  cors <- sapply(abisko_short$n_recon, function(n){
    sim <- mvtnorm::rmvnorm(n = n, sigma = cv)
    cor(sim[, 1], sim[, 2])
  })
  sd(cors)
})
mean(sd_cor_sim <= sd(abisko_short$r) )

## ---- digitised

#lake850
lake850 <- read.table("abisko/data/lake850.txt") %>% 
  select(1:2) %>% 
  setNames(c("date", "temp")) %>% 
  mutate(type = rep(c("reconstruction", "instrumental"), each = 23),
         date = round(date))

time_plot850 <- lake850 %>% ggplot(aes(x = date, y = temp, colour = type), shape = type) +
  geom_point()
time_plot850

#lakeV
lakeV <- read.table("abisko/data/lakeV.txt") %>% 
  select(1:2) %>% 
  setNames(c("date", "temp")) %>% 
  mutate(type = rep(c("reconstruction", "instrumental"), each = 14),
         date = round(date))

time_plot850 %+% lakeV

#lakeA
lakeA <- read.table("abisko/data/lakeA.txt") %>% 
  select(1:2) %>% 
  setNames(c("date", "temp")) %>% 
  mutate(type = rep(c("reconstruction", "instrumental"), each = 24),
         date = round(date))

time_plot850 %+% lakeA 

#lakeN
lakeN <- read.table("abisko/data/lakeN.txt") %>% 
  select(1:2) %>% 
  setNames(c("date", "temp")) %>% 
  mutate(type = rep(c("reconstruction", "instrumental"), each = 19),
         date = round(date))


lakeN_ages <- c(1999, 1997, 1995, 1993, 1991, 1989, 1987, 1984, 1982, 1978, 1975, 1971, 1967, 1961, 1955, 1949, 1942, 1935, 1929, 1918)

time_plot850 %+% lakeN + 
  geom_vline(xintercept = lakeN_ages, data = NULL)

## ---- all_lakes_plot
all_lakes <- bind_rows(Njulla = lakeN, Lake850 = lake850, Vuoskkujavri = lakeV, Alanen = lakeA, .id = "lake") %>% 
  mutate(lake = factor(lake, levels = c("Njulla", "Lake850", "Vuoskkujavri", "Alanen"))) %>% 
  spread(key = type, value = temp) %>% 
  group_by(lake)

all_lakes %>% 
  ggplot(aes(x = instrumental, y = reconstruction, colour = lake)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  coord_equal() +
  labs(x = "Instrumental °C", y = "Reconstructed °C") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) + 
  theme(legend.position = c(0.99, .01), legend.justification =  c(1, 0), legend.title = element_blank())

## ---- all_lakes_corelation
all_lakes %>% 
  do(data_frame(r = cor(.$reconstruction, .$instrumental)))


## ---- overlay_images
makefig<-function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}

library(magick)
library(grid)
fig5va <- image_read("data/abisko_short5a.png")


V_plot <- ggplot(lakeV, aes(x = date, y = temp, colour = type)) +
  annotation_custom(
    rasterGrob(as.raster(fig5va), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1900, xmax = 2003, ymin = -3, ymax = 15) +
  geom_point(size = 2, shape = 1) +
#  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))

makefig(V_plot)


#A
A_plot <- ggplot(lakeA, aes(x = date, y = temp, colour = type)) +
  annotation_custom(
    rasterGrob(as.raster(fig5va), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1933, xmax = 2001.5, ymin = 7.2, ymax = 21) +
  geom_point(size = 2, shape = 1) + 
  #  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))

makefig(A_plot)


#n
fig5n <- image_read("data/abisko_short5n.png")


N_plot <- ggplot(lakeN, aes(x = date, y = temp, colour = type)) +
  annotation_custom(
    rasterGrob(as.raster(fig5n), width=unit(1,"npc"), height=unit(1,"npc")),
    xmin = 1902, xmax = 2003, ymin = 4, ymax = 12.7) +
  geom_point(size = 2, shape = 1) + 
  #  scale_x_continuous(breaks = seq(0, 10000, 2000)) +
  theme_classic() +
  theme(plot.margin = margin(t = 10, r = 10, b = 70, l = 10))

makefig(N_plot)


## ---- digitised_instrumental
kiruna <- read.table("abisko/data/kiruna.txt") %>% 
  select(1:2) %>% 
  setNames(c("date", "temp")) %>% 
  mutate(date = round(date)) %>% 
  full_join(data.frame(date = min(.$date):max(.$date))) %>% 
  arrange(date)

diff(kiruna$date)



kiruna <- kiruna %>%
  mutate(
    smo = rollapply(temp, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = "c"),
    smo2 = {temp2 <- c(rep(NA, 5),temp, rep(NA, 5))
            temp2[is.na(temp2)] <- mean(temp, na.rm = TRUE)
            out <- rollapply(temp2, width = 5, FUN = mean, na.rm=TRUE, fill = NA, align = "c")
            out[-c(1:5, (length(out) - 4):length(out))]
            }
         )

kiruna %>% ggplot(aes(x = date, y = temp)) + 
  geom_point() +
  geom_line(aes(y = smo)) +
  geom_line(aes(y = smo2), colour = 2)

