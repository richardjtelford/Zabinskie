## ---- overlay_fun
makefig<-function(g){
  gt <- ggplot_gtable(ggplot_build(g))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  grid.draw(gt)
}


## ---- read_reconstructions
#Vuoskkujavri
Vuoskku <- read_table("data/abisko2004.txt", skip = 92, n_max = 63, col_names = FALSE) %>% 
  setNames(c("Age", "Temperature"))

#Lake 850
L850 <- read_table("data/abisko2004.txt", skip = 159, n_max = 120, col_names = FALSE) %>% 
  setNames(c("Age", "Temperature"))

#Lake Njulla
Njulla <- read_table("data/abisko2004.txt", skip = 284, n_max = 61, col_names = FALSE) %>% 
  setNames(c("Age", "Temperature"))
