## ---- curiousCounts
min_count <- chron %>% 
  mutate(min_count = apply(fos_counts, 1, function(r) min(r[r>0])))

## ---- curiousCounts_plots
min_count %>% ggplot(aes(x = year, y = min_count)) + 
  geom_point() +
  labs(x = "Year CE", y = "Minimum count")

fos_counts %>% filter(min_count$min_count == 4) %>% select_if(function(x){sum(x) > 0}) %>% as.data.frame()

mean(min_count$min_count > 1)
min_count %>% filter(year >= 1939) %>% summarise(mean = mean(min_count > 1))
min_count %>% filter(year < 1939) %>% summarise(mean = mean(min_count > 1))

# integer multiples
min_count$not_int_mult <- rowSums((fos_counts/min_count$min_count)%%1 != 0)

min_count %>% filter(min_count > 1, not_int_mult == 0)
