#import package
library("drake")

library("tidyverse")
library("magrittr")

library("readr")
library("readxl")
library("xml2")

library("gridExtra")
library("directlabels")
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

#analogue, stringi, rticles, english also needed


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




#construct drake plan
analyses <- drake_plan(
  
  #import & clean data
  
  #make plots
  
  #get bibliography

  
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("vegan", "rioja", "analogue", "palaeoSig"), 
    old_bib = file_in("Rmd/extra/chironomid.bib"), 
    new_bib = file_out("Rmd/extra/chironomid2.bib")),
  
  #knit manuscript
  manuscript = rmarkdown::draft(
    file = knitr_in("Rmd/limitations_of_high_resolution_quant_palaeo.Rmd"), 
    template = "elsevier_article", 
    package = "rticles"),
  
  strings_in_dots = "literals"
)

#configure and make drake plan
config <- drake_config(analyses)
outdated(config)        # Which targets need to be (re)built?
make(analyses)          # Build the right things.

#voew dependency graph
vis_drake_graph(config, targets_only = TRUE)
