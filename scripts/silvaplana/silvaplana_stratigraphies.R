## ---- magick_markdown_setup
#library(knitr)
"print.magick-image" <- function(x, ...){
  ext <- ifelse(length(x), tolower(image_info(x[1])$format), "gif")
  tmp <- tempfile(fileext = paste0(".", ext))
  image_write(x, path = tmp)
  knitr::include_graphics(tmp)
}

dev.off <- function(){
  invisible(grDevices::dev.off())
}

cleanup_images <- function(){
  lapply(ls(globalenv()), function(name){
    if(name %in% c("frink"))
      return()
    if(inherits(get(name, globalenv()), "magick-image"))
      rm(list = name, envir = globalenv())
  })
  invisible(gc())
}

## ---- load_published_stratigraphies

strat_qsr <- image_read("data/silvaplana/images/silvaplana_qsr.png")
# strat_qsr %>% image_crop(geometry = "1443x190+0+650")
# strat_qsr %>% image_crop(geometry = "1443x83+0+757")

strat_jopl <- image_read("data/silvaplana/images/JoPL-06.png")
strat_jopl <- strat_jopl  %>%  
  image_rotate(90)
# strat_jopl %>% image_crop("2200x1180+380+470")
# strat_jopl %>% image_crop("2200x810+380+810")

  
strat_holocene <- image_read("data/silvaplana/images/Holocene-05.png")
strat_holocene <- strat_holocene %>% 
  image_rotate(90) 
# strat_holocene %>% image_crop("3000x632+190+500")
# strat_holocene %>% image_crop("3000x354+190+778")

qsr_cropper <- c("%dx190+%d+650",  "%dx83+%d+757")
jopl_cropper <- c("%dx1180+%d+470", "%dx812+%d+808")
holocene_cropper <- c("%dx633+%d+469", "%dx354+%d+778")

cropper <- function(image, cropper, width, xoffset, dropNames = FALSE){
  if(!dropNames){
    cropper <- cropper[1]
    sf <- "x1180"
  } else{
    cropper = cropper[2]
    sf <- "x812"
    }
  image_crop(image = image, geometry = sprintf(cropper, width , xoffset)) %>% 
    image_scale(sf)
}

make_fig <- function(gg){
  fig <- image_graph(width = 250, height = 812, res = 96)
  print(gg)
  dev.off()
  fig
}

## axes
qsr_chron <- strat_qsr %>% cropper(qsr_cropper, 26, 19, TRUE)
jopl_chron <- strat_jopl %>% cropper(jopl_cropper, 100, 340, TRUE)
holo_chron <- strat_holocene %>% cropper(holocene_cropper, 60, 180, TRUE)


## ---- Microtendipes
microt <- ggplot(fos_holocene, aes(x = recon_holocene$Year, y = Microt)) + 
  geom_col() +
  scale_x_continuous(limits = c(1850, 2001), expand = c(0.01, 0), breaks = seq(1840, 2000, 20)) +
  scale_y_continuous(position = "right", breaks = seq(0, 100, 20)) +
  coord_flip() +
  labs(x = "Year CE", y = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(1, 1, 1, 1))


microtendipes <- image_append(c(
  jopl_chron,
  strat_jopl %>% cropper(jopl_cropper, 80, 1000, TRUE),
  make_fig(microt),
  holo_chron,
  strat_holocene %>% cropper(holocene_cropper, 90, 380, TRUE),
  qsr_chron,
strat_qsr %>% cropper(qsr_cropper, 30, 270, TRUE)
))

microtendipes


## ---- Cricotopus
names <- TRUE
Cricotopus <- image_append(c(
  jopl_chron,
  strat_jopl %>% cropper(jopl_cropper, 90, 1700, names),
  make_fig(microt + aes(y = Cricoto)),
  holo_chron,
  strat_holocene %>% cropper(holocene_cropper, 140, 2110, names),
  qsr_chron,
  strat_qsr %>% cropper(qsr_cropper, 60, 735, names)
))
Cricotopus



## ---- Procladius
names <- TRUE
Procladius <- image_append(c(
  jopl_chron,
  strat_jopl %>% cropper(jopl_cropper, 70, 1140, names),
  make_fig(microt + aes(y = Proclad)),
  holo_chron,
  strat_holocene %>% cropper(holocene_cropper, 65, 1355, names),
  qsr_chron,
  strat_qsr %>% cropper(qsr_cropper, 33, 430, names)
))
Procladius


