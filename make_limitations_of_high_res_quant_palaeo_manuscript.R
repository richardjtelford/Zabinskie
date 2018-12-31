#import packages
library("drake")

library("tidyverse")
library("magrittr")

library("readr")
library("readxl")

library("gridExtra")
library("directlabels")
library("assertthat")

library("sp")
library("rioja")
library("vegan")
#devtools::install_github("gavinsimpson/ggvegan")
library("ggvegan")
library("palaeoSig")

#devtools::install_github("richardjtelford/rjt.misc")
library("rjt.misc")
#devtools::install_github("richardjtelford/count_checker")
library("countChecker")
#devtools::install_github("richardjtelford/ggpalaeo")
library("ggpalaeo")

#import scripts
source("scripts/general/utils.R")
source("scripts/general/pages2k.R")
source("scripts/general/weather_climate.R")
source("scripts/general/air_water_correlation.R")

source("scripts/zabinskie/load_zabinskie_data.R")
source("scripts/zabinskie/regional_composite.R")
source("scripts/zabinskie/correlation_in_space.R")
source("scripts/zabinskie/percent_variance_by_month.R")
source("scripts/zabinskie/age_uncertainty.R")
source("scripts/zabinskie/reconstruction_diagnostics.R")
source("scripts/zabinskie/figure2_ordination.R")
source("scripts/zabinskie/effect_low_counts.R")
source("scripts/zabinskie/calibration_set_issues.R")

source("scripts/abisko/abisko_short_2003.R")

source("scripts/silvaplana/silvaplana_load.R")
source("scripts/silvaplana/silvaplana_plots.R")

source("scripts/seebergsee/seebergsee_counts.R")

source("scripts/luoto/luoto_digitised.R")

source("scripts/zhang_et_al_2017/zhang_et_al.R")

source("scripts/speke/speke_original.R")

#drake configuration
pkgconfig::set_config("drake::strings_in_dots" = "literals")

#construct drake plan
analyses <- drake_plan(
  #### General code
  # pages2k data - "scripts/pages2k.R"
  pagesHi = pages2k_load(),
  
  #weather_climate - "scripts/general/weather_climate.R"
  cet = read.table(file_in("data/general/cetml1659on.dat"), skip = 7, header = FALSE),
  cet2  = weather_climate_process(cet), 
  weather_clim_cor = calc_weather_climate_correlations(cet2, climate),
  weather_clim_plot = plot_weather_climate_correlations(weather_clim_cor),
  
  #lake-air temperature correlations - "scripts/air_water_correlation.R"
  max_area = 2,
  min_depth = 5,
  lake_air_correlations = calculate_lake_air_correlations(max_area = max_area, min_depth = min_depth, min_latitude = 40, min_years = 10),
  
  #### Zabinskie
  #load data - "scripts/load_zabinskie_data.R"
  zabinskie_excel_file = file_in("data/zabinskie/zabinskie2015cit.xls"),
  #modern spp
  spp_all0 = read_excel(zabinskie_excel_file, sheet = "Training species"),
  #modern environment
  env_all0 = read_excel(zabinskie_excel_file, sheet = "Training temperature"),
  #check siteIDs match
  check_z1 = assertthat::assert_that(assertthat::are_equal(spp_all0$X__1, env_all0$Name)),
  #sites with low counts
  lowCount = c("GOR", "KOS", "LEK", "SAL", "SZE", "SZOS", "TRZ", "WAS", "ZAB"),
  #environment without low count sites
  env0 = env_all0 %>% filter(!Name %in% lowCount),
  #species without low count sites or absent taxa
  spp = spp_all0 %>% 
    filter(!X__1 %in% lowCount) %>% 
    select(-X__1) %>% 
    select_if(~(sum(.) > 0)),# remove taxa only in low count sites - cannot find evidence of stricter inclusion criteria
  
  #species at all sites without site names
  spp_all = spp_all0 %>% select(-X__1),
  
  #make env a vector to simplify later code
  env = env0$Temp,
  env_all = env_all0$Temp,
  
  #sites & countries
  sites = zabinskie_sites(env0),
  sites_all = zabinskie_all_sites(env_all0, lowCount),
  #fossil percent
  fos = zabinskie_fossil_percent(zabinskie_excel_file),
  #chronology
  chron = zabinskie_chronology(zabinskie_excel_file),
  #fossil counts
  fos_counts = zabinskie_fossil_counts(zabinskie_excel_file),
  #reconstruction
  recon = zabinskie_reconstruction(zabinskie_excel_file),
  #instrumental data
  instrumental_temperature = zabiniskie_instrumental(file_in("data/zabinskie/chart1.xml")), 
  
  #calibration set climate
  climate  = zabinskie_calibration_climate(zabinskie_excel_file, sites),

  #climate time series - "scripts/regional_composite.R"
  fat_composite = zabinskie_regional_composite(),
  fat_composite_as_zab_published = fat_composite %>% 
    ungroup() %>% 
    inner_join(recon %>% mutate(recon_year = year)) %>% 
    arrange(desc(recon_year)),
  
  #performance_by_month "scripts/zabinskie/percent_variance_by_month.R"
  perform_by_month = zabinskie_perform_by_month(climate, spp, fos, fat_composite_as_zab_published),
  perform_by_month_plot = zabinskie_plot_perform_by_month(perform_by_month),
  
  #reconstruction_by_month "scripts/zabinskie/percent_variance_by_month.R"
  recon_by_month = zabinskie_reconstruction_by_month(climate, spp, fos, chron), 
  recon_by_month_plot = zabinskie_plot_reconstruction_by_month(recon_by_month),
  
  #age uncertainty
  age_sim_data = prep_sim(),
  age_sim = run_simulation(age_sim_data, recon = recon, nrep = 1000),
  
  #ordinations
  cca_fos = cca(X = sqrt(fos), Y = instrumental_temperature$old),
  pc_explained = eigenvals(cca_fos)[1]/sum(eigenvals(cca_fos)) * 100,
  L1L2 = eigenvals(cca_fos)[1]/eigenvals(cca_fos)[2],
  anova_fos = anova(cca_fos)$Pr[1],
  
  # replicating_figure_2 - "scripts/figure2_ordination.R"
  rep_fig2 = zabinskie_figure2(spp_all, env_all),
  # supplementary_data_fig_1 - "scripts/figure2_ordination.R"
  rep_sdf1 = zabinskie_sup_data_fig1(spp_all, env_all, fos),

  #reconstruction diagnostics
  dist_to_analogues = zabinskie_distance_to_nearest_neighbour(spp, env, fos, chron),
  zabinskie_dist_analogue_plot = ggpalaeo:::plot_diagnostics(x = dist_to_analogues, x_axis = "year", y_axis = "dist_to_analogues", goodpoorbad = attr(dist_to_analogues, "goodpoorbad"), fill = c("salmon", "lightyellow", "skyblue"), categories = c("Good", "Fair", "None")) + 
    labs(x = "Year CE", y = "Squared chord distance", fill = "Analogue quality"),
  residual_len = zabinskie_residual_length(spp, env, fos, chron),
  zabinskie_residLen_plot = ggpalaeo:::plot_diagnostics(x = residual_len, x_axis = "year", y_axis = "rlen", goodpoorbad = attr(residual_len, "goodpoorbad"), fill = c("salmon", "lightyellow", "skyblue"), categories = c("Good", "Poor", "Very Poor")) + 
    labs(x = "Year CE", y = expression(Squared~chi^2~residual~distance), fill = "Goodness of fit"),
  rtf = randomTF(sqrt(as.data.frame(spp)), env, fos, n = 999, fun = WAPLS, col = 2),
  
  
  #reconstruction-instrumental correlations
  inst_recon  = recon %>% full_join(instrumental_temperature),
  all_correlation = inst_recon %$% cor(temperature, old),
  incorrect_correlation = inst_recon %>%
    filter(year < 1939) %$% 
    cor(temperature, old),
  correct_correlation = inst_recon %>% 
    filter(year < 1939) %$% 
    cor(temperature, new),
  
  #effect low counts
  fos_resid_sd = fos_residuals_sd(recon, instrumental_temperature),
  estimated_countsum = estimate_n(spp, digits = 2),
  est_count_error = count_error(spp, env, sites, estimated_countsum, fos_counts),
  
  #curious counts 
  min_count = chron %>% 
    mutate(min_count = apply(fos_counts, 1, function(r) min(r[r>0]))),
  #lac_AH -  "scripts/calibration_set_issues.R"
  lac_AH = zabinskie_lac_AH(spp, sites),
  
  ###Abisko
  abisko_short = abisko_reported(),
  abisko_similar_cor = abisko_similar_correlations(abisko_short),
  abisko_all_lakes = abisko_digitised(),
  abisko_cor = abisko_correlations(abisko_all_lakes),
  abisko_checked = abisko_check(abisko_short, abisko_cor),
  abisko_all_lakes_plot = abisko_plot_all_lakes(abisko_all_lakes),
  
  ###Silvaplana
  #load data
  silva_recon_holocene = silva_load_recon_holocene(),
  silva_recon_jopl = silva_load_recon_jopl(),
  silva_fos_holocene = silva_load_fos_holocene(),
    
  #make plots of reconstructions
  silva_recon_plot = silv_plot_reconstructions(silva_recon_jopl, silva_recon_holocene),
  silva_side_by_side = silva_put_recons_side_by_side(silva_recon_jopl, silva_recon_holocene),
  silva_recon_strat_plot = silva_plot_side_by_side(silva_side_by_side),
  silva_max_diff = silva_calc_max_difference(silva_side_by_side),
  silva_est_countSums = silva_estimate_countSums(silva_fos_holocene),
  
  silva_est_countSum = silva_estimate_countSums(silva_fos_holocene),
  silva_last = silva_last_sample(silva_fos_holocene),
  silva_climate = silva_load_climate(old = TRUE),
  silva_new_climate = silva_load_climate(old = FALSE),
  silva_digitised_climate = silva_load_digitised_climate(),
  silva_climate_plot = silva_plot_climate(silva_climate, silva_digitised_climate),
  silva_version_r2 = silva_calc_version_r2(silva_climate, silva_new_climate),
  
  
  ###Seebergsee
  seeberg_count = seeberg_read_counts(),
  seeberg_merged = seeberg_merge_counts(seeberg_count),
  seeberg_pc = seeberg_calc_percent(seeberg_merged),
  seeberg_sums = seeberg_calc_countsums(seeberg_count),
  seeberg_n = seeberg_calc_noccur(seeberg_count),
 
  seeberg_climate = seeberg_load_climate(),
  seeberg_digitised_climate = seeberg_load_digitised_climate(),
  seeberg_climate_plot = seeberg_plot_climate(seeberg_digitised_climate, seeberg_climate),
    
  
  seeberg_cit_mod = WAPLS(sqrt(seeberg_pc), seeberg_digitised_climate$july) %>% crossval(),
  seeberg_cit_perf = seeberg_cit_mod %>% performance(),
  seeberg_nrep = 1000, 
  seeberg_random_perform = seeberg_calc_random_perform(seeberg_pc, seeberg_nrep),
  seeberg_ca = cca(sqrt(seeberg_pc)),
  seeberg_cit_plot = autoplot(seeberg_cit_mod, npls = 1, show_apparent = TRUE, smooth = FALSE) + labs(x = "Measured July air temperature anomaly °C", y = "Predicted July air temperature anomaly °C"),
  
  
  ###Baker
  
  
  ###Luoto
  luoto_climate = luoto_load_digitised_climate(),
  luoto_fos = luoto_digitise_stratigraphy(),
  luoto_cit_mod = luoto_run_cit_mod(luoto_fos, luoto_climate),
  luoto_cit_perform = performance(luoto_cit_mod),
  luoto_cit_plot = autoplot(luoto_cit_mod, show_apparent = TRUE, smooth = FALSE, column = "WA.inv.tol") + labs(x = "Measured July air temperature °C", y = "Predicted July air temperature °C"),
  
  ###Zhang
  zhang_data = zhang_import(),
  zhang_cor = zhang_calc_cor(zhang_data),
  
  ###Speke Hall Lake
  speke = speke_import(),
  speke_ana_dist_plot = speke_analogue_distances(speke),
  speke_resLen_plot = speke_residual_length(speke),
  
  
  ##prepare ms
  #add extra packages to bibliography
  biblio2 = package_citations(
    packages = c("vegan", "rioja", "analogue", "palaeoSig"), 
    old_bib = file_in("Rmd/extra/chironomid.bib"), 
    new_bib = file_out("Rmd/extra/chironomid2.bib")),
  
  #knit manuscript
  manuscript = target(
    command = rmarkdown::render(
      input = knitr_in("Rmd/limitations_of_high_resolution_quant_palaeo.Rmd"),
      knit_root_dir = "../", 
   #   output_dir = "./output", 
      clean = FALSE), 
    trigger = trigger(change =list(biblio2, supplementary_data))
    ),
  supplementary_data = target(
    command = rmarkdown::render(
      input = knitr_in("Rmd/Telford_supplementary_data.Rmd"),
      knit_root_dir = "../",
    #  output_dir = "./output", 
      clean = FALSE),
    trigger = trigger(change =list(biblio2))
  )
)

#configure and make drake plan
config <- drake_config(analyses)
#outdated(config)        # Which targets need to be (re)built?
make(analyses, jobs = 2) # Build the right things.

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