source("make_limitations_of_high_res_quant_palaeo_manuscript.R")

presentation <- drake_plan(
  presentation = rmarkdown::render(
    input = knitr_in("Rmd/eecrg_2018_10_26.Rmd"), 
    knit_root_dir = "../", 
    output_dir = "./output")
)

pres <- bind_rows(analyses, presentation)
pres_config <- drake_config(pres)
make(pres)
vis_drake_graph(pres_config, targets_only = TRUE)

