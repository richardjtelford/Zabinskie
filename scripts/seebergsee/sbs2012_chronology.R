library(tidyverse)
data_frame(depth = c(4, 16, 32, 48, 61, 70, 79, 87, 95, 105, 122, 133, 163, 178, 193, 211, 224, 236, 248), dateCE = seq(1970, 1070, -50), age = 2005 - dateCE) %>% ggplot(aes(x = depth, y = age)) + geom_point() + geom_line() + coord_flip() + scale_x_reverse() + scale_y_continuous(breaks = seq(0, 1000, 200))
#looks OK