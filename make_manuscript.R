library("drake")

r_outdated(drake_source = "drake_plan_manuscript.R")        # Which targets need to be (re)built?
r_make(source = "drake_plan_manuscript.R") # Build the right things.

float_tex("Rmd/Telford_supplementary_data.tex", clean = FALSE)
setwd("Rmd/")#only appears to work when tex file is in working directory
tinytex::pdflatex("limitations_of_high_resolution_quant_palaeo.tex", clean=TRUE)
tinytex::pdflatex("Telford_supplementary_data.tex", clean=TRUE)
setwd("../")

system("evince Rmd/limitations_of_high_resolution_quant_palaeo.pdf", wait = FALSE)#display pdf - only linux

system("evince Rmd/Telford_supplementary_data.pdf", wait = FALSE)#display pdf - only linux

#show dependency graph
vis_drake_graph(config)
vis_drake_graph(config, targets_only = TRUE, main = "Zabinskie ms dependency graph")
options(digits = 6)#reset