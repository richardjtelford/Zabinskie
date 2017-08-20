## ---- magick_markdown_setup
library(knitr)
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

strat_qsr <- image_read("images/silvaplana_qsr.png")
# strat_qsr %>% image_crop(geometry = "1443x190+0+650")
# strat_qsr %>% image_crop(geometry = "1443x83+0+757")

strat_jopl <- image_read("images/JoPL-06.png")
strat_jopl <- strat_jopl  %>%  
  image_rotate(90)
# strat_jopl %>% image_crop("2200x1180+380+470")
# strat_jopl %>% image_crop("2200x810+380+810")

  
strat_holocene <- image_read("images/Holocene-05.png")
strat_holocene <- strat_holocene %>% 
  image_rotate(90) 
# strat_holocene %>% image_crop("3000x632+190+500")
# strat_holocene %>% image_crop("3000x354+190+778")

qsr_cropper <- c("%dx190+%d+650",  "%dx83+%d+757")
jopl_cropper <- c("%dx1180+%d+470", "%dx810+%d+810")
holocene_cropper <- c("%dx632+%d+470", "%dx354+%d+778")

cropper <- function(image, cropper, width, xoffset, dropNames = FALSE){
  if(!dropNames){
    cropper <- cropper[1]
    sf <- "x1180"
  } else{
    cropper = cropper[2]
    sf <- "x810"
    }
  image_crop(image = image, geometry = sprintf(cropper, width , xoffset)) %>% 
    image_scale(sf)
}


## axes
qsr_chron <- strat_qsr %>% cropper(qsr_cropper, 30, 15, TRUE)
jopl_chron <- strat_jopl %>% cropper(jopl_cropper, 100, 340, TRUE)
holo_chron <- strat_holocene %>% cropper(holocene_cropper, 60, 180, TRUE)


## ---- Microtendipes

microtendipes <- image_append(c(
  jopl_chron,
  strat_jopl %>% cropper(jopl_cropper, 80, 1000, TRUE),
  holo_chron,
  strat_holocene %>% cropper(holocene_cropper, 90, 380, TRUE),
  qsr_chron,
strat_qsr %>% cropper(qsr_cropper, 30, 270, TRUE)
))

image_draw(microtendipes)
x11()
microt <- ggplot(fos_holocene, aes(x = recon_holocene$Year, y = Microt)) + 
  geom_col() +
  coord_flip() +
  xlim(1849, 2001) +
  labs(x = "Year CE")

microt


## ---- Cricotopus
names <- TRUE
Cricotopus <- image_append(c(
  jopl_chron,
  strat_jopl %>% cropper(jopl_cropper, 90, 1700, names),
  holo_chron,
  strat_holocene %>% cropper(holocene_cropper, 140, 2110, names),
  qsr_chron,
  strat_qsr %>% cropper(qsr_cropper, 60, 735, names)
))
print(Cricotopus)
x11()
microt + aes(y = Cricoto) 
