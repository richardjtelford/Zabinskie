#import package
library("drake")#

library("tidyverse")#
library("magrittr")

library("readr")
library("readxl")#
library("xml2")

library("gridExtra")
library("directlabels")#
library("assertthat")

library("sp")
library("rioja")
library("vegan")
library("ggvegan")
library("laketemps")
library("palaeoSig")
library("zoo")
library("nlme")
library("broom")

#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")

#analogue, stringi, rticles, english, bibtex also needed


#helper functions
as.english <- function(x){ # problem with new version of rmarkdown
  as.character(english::as.english(x))
}
as.English <- function(x){ #sentence case
  stringi::stri_trans_totitle(
    as.character(as.english(x)), 
    opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
}


#import scripts
source("scripts/pages2k.R")
# knitr::read_chunk("scripts/load_zabinskie_data.R")
# knitr::read_chunk("scripts/regional_composite.R")
# knitr::read_chunk("scripts/correlation_in_space.R")
source("scripts/weather_climate.R")
# knitr::read_chunk("scripts/percent_variance_by_month.R")
# knitr::read_chunk("scripts/age_uncertainty.R")
# knitr::read_chunk("scripts/reconstruction_diagnostics.R")
# knitr::read_chunk("scripts/regional_composite.R")
# knitr::read_chunk("scripts/zabinskie_temperature_composite.R")
# knitr::read_chunk("scripts/air_water_correlation.R")
# knitr::read_chunk("scripts/ordinations.R")
# knitr::read_chunk("scripts/figure2_ordination.R")
# knitr::read_chunk("scripts/effect_low_counts.R")
# knitr::read_chunk("scripts/curiousCounts.R")
# knitr::read_chunk("scripts/calibration_set_issues.R")
# knitr::read_chunk("abisko/scripts/abisko_short_2003.R")
# knitr::read_chunk("silvaplana/scripts/silvaplana_load.R")
# knitr::read_chunk("silvaplana/scripts/silvaplana_plots.R")
# knitr::read_chunk("silvaplana/scripts/seebergsee_occur.R")
# knitr::read_chunk("seebergsee/seebergsee_counts.R")
# knitr::read_chunk("seebergsee/seeberg_climate.R")
# knitr::read_chunk("zhang_et_al/zhang_et_al.R")



#construct drake plan
analyses <- drake_plan(
  
  ## pages2k data - "scripts/pages2k.R"
  pagesHi = pages2k_load(pages2k_data_file = file_in("data/sdata201788-s3.xlsx")),
  
  
  #weather_climate - "scripts/weather_climate.R"
  cet = read.table(file_in("data/cetml1659on.dat"), skip = 7, header = FALSE),
  cet2  = weather_climate_process(cet), 
  wc_JA = map_df(1:50, weather_climate, month1 = "Jun", month2 = "Aug", dat = cet2),
  wc_AS = map_df(1:50, weather_climate, month1 = "Aug", month2 = "summer", dat = cet2),
  weather_clim_plot = weather_climate_plot(wc_AS = wc_AS, wc_JA = wc_JA),
  
  
  #import & clean data
  
  #make plots
  


  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("vegan", "rioja", "analogue", "palaeoSig"), 
    old_bib = file_in("Rmd/extra/chironomid.bib"), 
    new_bib = file_out("Rmd/extra/chironomid2.bib")),
  
  #knit manuscript
  manuscript = target(
    command = rmarkdown::draft(
      file = knitr_in("Rmd/limitations_of_high_resolution_quant_palaeo.Rmd"),       template = "elsevier_article", 
      package = "rticles"), 
    trigger = trigger(change = biblio2)  
    ),
  
  strings_in_dots = "literals"
)

#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

#show dependency graph
vis_drake_graph(config)
vis_drake_graph(config, targets_only = TRUE)
