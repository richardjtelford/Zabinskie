## ---- weather_climate

#check file exists
if(!file.exists("data/cetml1659on.dat")){
  stop("Central England Temperature series missing. Download monthly mean data from http://www.metoffice.gov.uk/hadobs/hadcet/data/download.html")
}

#import data
cet <- read.table("data/cetml1659on.dat", skip = 7, header = FALSE) %>% as.tibble()
names(cet) <- c("year", month.abb, "annual")

#remove partial years
cet <- cet %>% filter(!apply(cet == -99.9, 1, any))

#calculate mean summer
cet <- cet %>% mutate(summer = rowMeans(data_frame(Jun, Jul, Aug)))

#plot monthly data
plotJA <- . %>% select(year, Jun, Aug) %>% gather(key = month, value = temperature, -year) %>% ggplot(aes(x = year, y = temperature, colour = month))
cet %>% plotJA +  geom_path() + geom_smooth()

#aggregate timeseries into non-overlaping periods of 1-50 years

weather_climate <- function(N, month1 = "Jun", month2 = "Aug", dat = cet, cor = TRUE){
  dat <- dat %>% mutate(window = 0:(n() - 1) %/% N) %>% 
    group_by(window) %>%
    filter(n() > N/2) %>% #remove small partial groups
    summarise_all(mean) 
  ct <- cor.test(pull(dat, month1), pull(dat, month2))
  if(cor){
  data_frame(N, low = ct$conf.int[1], high = ct$conf.int[2], est = ct$estimate, n = nrow(dat), last = max(dat$year))
  } else{
    dat
  }
}

wc_JA <- map_df(1:50, weather_climate, month1 = "Jun", month2 = "Aug")

wc_As <- map_df(1:50, weather_climate, month1 = "Aug", month2 = "summer")



corPlot <- ggplot(wc_JA, aes(x = N, y = est)) +
  geom_point() +
  geom_smooth(se = FALSE, size = .7) +
  scale_x_continuous(trans = "sqrt", expand = c(0.02, 0)) +
  scale_color_brewer(type = "qual", palette = "Dark2") +
  labs(x = "Number of years", y = "Correlation")


JuneAugustSummer_plot <- corPlot %+% 
  (wc_As %>% mutate(months = "August-summer") %>%
     bind_rows(wc_JA %>% mutate(months = "August-June"))) + 
  aes(colour = months, linetype = months, shape = months, label = months) +
  labs(colour = "", shape = "", linetype = "") + 
  geom_dl(method = list(box.color = NA, "first.points", hjust = 0, vjust = 1, cex = 0.75)) +
  scale_y_continuous(expand = c(0.01, 0), limits = c(0, 1)) + 
  theme(legend.position = "none")
#  theme(legend.position = c(.69, .23), legend.justification = c(0, 1), legend.title = element_blank(), legend.key.width = unit(1, 'cm'))
