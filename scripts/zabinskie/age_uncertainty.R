#age-uncertain correlations
#Monte Carlo
#perfect reconstruction 
#aggregated to match observed resolution

## ---- age_uncertainty_setup

#main function
run_simulation <- function(data, recon, nrep = 100){ 
  len <- nrow(data)
  #reconstruction - to match aggregation
  recon2 <- recon %>% 
    select(-temperature) %>% 
    mutate(year_recon = year)
  
  data1 <- data %>% select(year, temperature)
  data1r <- data1 %>% 
    left_join(recon2) %>% 
    fill(year_recon, .direction = "up") %>% 
    group_by(year_recon)
  
  plyr::rdply(.n = nrep, .expr = {

    #missing/extra years
    missing <- rbinom(n = len, prob = data$pmissing, size = 1)
    extra <- rbinom(n = len, prob = data$pextra, size = 1)
    
    #resample data
    data2 <- data1 %>% filter(extra == 0) %>%
      bind_rows(data1 %>% filter(missing == 1)) %>% 
      arrange(desc(year)) %>%
      mutate(actualyear = year) %>% 
      mutate(year = seq(2010, by = -1, length = nrow(.)))
    
    data3 <- data1r %>% 
      left_join(data2, by = "year", suffix  = c("_o", "_d"))
    
    data4 <- data3 %>% 
      summarise(
        temperature_o = mean(temperature_o),
        temperature_d = mean(temperature_d)
        )
    #number mismatching years
    mismatch <- with(data3, sum(year != actualyear, na.rm = TRUE))
    
    #raw correlation
    correlation <- with(data3, cor(temperature_o, temperature_d, use = "pair"))
    
    #aggregated correlation
    ag_correlation <- with(data4, cor(temperature_o, temperature_d, use = "pair"))
    pre39_correlation <- with(data4 %>% filter(year_recon < 1939), cor(temperature_o, temperature_d, use = "pair"))
    post1939_correlation <- with(filter(data4, year_recon >= 1939), cor(temperature_o, temperature_d, use = "pair"))
    
    tibble(correlation, mismatch, ag_correlation, pre39_correlation, post1939_correlation)
  })
}#end run_simulation


## ---- chron_no_extra_error
#1933 CE it is −0/+2 years, increasing to −5/+2
prep_sim <- function(extra_err = 0){
  data <- data.frame(
    year = 2010:1896, 
    pmissing = extra_err, 
    pextra = extra_err)
  
  len <- nrow(data)
  
  data <- data %>% 
    mutate(temperature = rnorm(len)) %>% 
    mutate(# dates of uncertain varves from Prof Tylmann
      pmissing = if_else(year %in% c(1926, 1924, 1912, 1903, 1899), 1/3, pmissing), #perhaps year missing  
      pextra = if_else(year %in% c(1964, 1963), 1/3, pextra)#perhaps extra year needs deleting
      ) 
}
#set.seed(9524)#random.org
#simulation <- run_simulation(data, nrep = 1000)

# ## ---- output
# mean(simulation$mismatch == 0)
# mean(simulation$correlation)
# mean(simulation$ag_correlation)
# mean(simulation$pre39_correlation)
# mean(simulation$post1939_correlation)
# 
# 
# simulation %>% ggplot(aes(x = mismatch)) + geom_histogram()
# simulation %>% ggplot(aes(x = correlation)) + geom_histogram()
# simulation %>% ggplot(aes(x = ag_correlation)) + geom_histogram()
# simulation %>% ggplot(aes(x = pre39_correlation)) + geom_histogram()
# simulation %>% ggplot(aes(x = post1939_correlation)) + geom_histogram()
# 
#                             
# ## ---- chron_extra_error
# #with non-zero base 
# dataz <- data.frame(year = 2010:1896, pmissing = 0.005, pextra = 0.005)
# 
# dataz <- dataz %>% 
#   mutate(temperature = rnorm(len)) %>% 
#   mutate(# dates of uncertain varves from Prof Tylmann
#     pmissing = if_else(year %in% c(1926, 1924, 1912, 1903, 1899), 1/3, pmissing),   
#     pextra = if_else(year %in% c(1964, 1963), 1/3, pextra)) 
# 
# set.seed(7777)#random.org
# simulationz <- run_simulation(dataz, nrep = 1000)
# 
# ## ---- output2
# mean(simulationz$mismatch == 0)
# mean(simulationz$correlation)
# mean(simulationz$ag_correlation)
# mean(simulationz$pre39_correlation)
# mean(simulationz$post1939_correlation)
# 
# 
# simulationz %>% ggplot(aes(x = mismatch)) + geom_histogram()
# simulationz %>% ggplot(aes(x = correlation)) + geom_histogram()
# simulationz %>% ggplot(aes(x = ag_correlation)) + geom_histogram()
# simulationz %>% ggplot(aes(x = pre39_correlation)) + geom_histogram()
# simulationz %>% ggplot(aes(x = post1939_correlation)) + geom_histogram()
