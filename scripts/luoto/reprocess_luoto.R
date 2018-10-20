inst <- read.table("luoto/data/measured.txt") %>% 
  select(temp = V1, year = V2) %>% 
  mutate(year = round(year))

inst %>% ggplot(aes(x = year, y = temp)) + geom_point() + coord_flip() + geom_line()

cit <- read.table("luoto/data/cit.txt") %>% #fubar
  select(temp = V1, year = V2) %>% 
  mutate(year = round(year), temp = (temp - mean(temp)) * -1 *1.47 + 14.6) %>% 
  slice(-(1:nrow(inst)))

cit %>% ggplot(aes(x = year, y = temp)) + geom_point() + coord_flip() + geom_line()

cis <- read.table("luoto/data/cis.txt") %>% #fubar
  select(temp = V1, year = V2) %>% 
  mutate(year = round(year), temp = temp * - 1 + 15 * 2) %>% 
  slice(-(1:(nrow(inst) + nrow(cit)))) %>% 
  mutate(temp = (temp - 15) *-1 +15) %>% 
  mutate(year = if_else(year == 2011, 2010, year))


inst_cit <- left_join(inst, cit, by = "year", suffix = c(".inst", ".cit")) %>% mutate(n = (n()-1):0)
inst_cit %$% cor(temp.inst, temp.cit)
inst_cit %>% filter(!year %in% c(2012, 2014)) %$% cor.test(temp.inst, temp.cit)
inst_cit %$% cor.test(temp.inst, temp.cit, method = "spear")
inst_cit %>% #filter(!year %in% c(2012, 2014))%>%
  ggplot(aes(x = temp.inst, y = temp.cit, colour = year, label = year)) + 
  geom_point() + 
  geom_abline() +
  geom_text(size = 2) +
  geom_smooth(method = "lm")


left_join(inst, cis, by = "year", suffix = c(".inst", ".cit")) %$% cor(temp.inst, temp.cit)

left_join(cit, cis, by = "year", suffix = c(".cit", ".cis")) %$% cor(temp.cis, temp.cit)

bind_rows(inst = inst, cit = cit, cis = cis, .id = "type") %>% 
  ggplot(aes(x = year, y = temp, colour = type)) + 
  geom_point() + 
  geom_line() + 
  coord_flip()


acf(cit$temp)
acf(inst$temp)
cor(inst$year, inst$temp)
library(ggfortify)
inst_cit %$% ccf(temp.inst, temp.cit) %>% autoplot()

inst_cit %>% mutate(temp.inst = lag(temp.inst, 1)) %$% 
  cor(temp.cit, temp.inst, use = "pair")


### optima
glyp_pal <- read.table("luoto/data/glyp_pal.txt") %>% 
  select(perc = V1, samp = V2) %>% 
  mutate(samp = round(samp))

ggplot(glyp_pal, aes(x = samp, y = perc)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_reverse()

glyp_pal <- inst %>% arrange(desc(year)) %>% mutate(samp = 0:(n()-1)) %>% left_join(glyp_pal) %>% mutate(perc = if_else(is.na(perc), 0, perc))
ggplot(glyp_pal, aes(x = temp, y = perc)) + geom_point() + geom_vline(xintercept = glyp_pal %$% weighted.mean(temp, perc))

glyp_pal %$% weighted.mean(temp, perc)



### tany_men
tany_men <- read.table("luoto/data/tany_men.txt") %>% 
  select(perc = V1, samp = V2) %>% 
  mutate(samp = round(samp))

ggplot(tany_men, aes(x = samp, y = perc)) + 
  geom_col() + 
  coord_flip() + 
  scale_x_reverse()

tany_men <- inst %>% arrange(desc(year)) %>% mutate(samp = 0:(n()-1)) %>% left_join(tany_men) %>% mutate(perc = if_else(is.na(perc), 0, perc))
ggplot(tany_men, aes(x = temp, y = perc)) + geom_point() + geom_vline(xintercept = tany_men %$% weighted.mean(temp, perc))

tany_men %$% weighted.mean(temp, perc)

tany_men %>% filter(year != 1999) %$% weighted.mean(temp, perc)


tany_men

