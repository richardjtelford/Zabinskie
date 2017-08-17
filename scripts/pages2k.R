## ---- pages2k
if(!file.exists("data/sdata201788-s3.xlsx")){
  stop("Download data from https://www.nature.com/articles/sdata201788, or simply delete this chunk")
}

pages <- read_excel("data/sdata201788-s3.xlsx", sheet = "Table S1")
lakes <- pages %>% 
  filter(
    `archive type` == "lake sediment", 
    proxy != "pollen") %>% 
  mutate(proxy = recode(proxy, "midge" = "chironomid"),
         `resolution (yr)` = as.numeric(`resolution (yr)`)) 
lakesHi <- lakes %>% filter(`resolution (yr)` <= 2)


  
## ---- pages2k_plots
lakesHi %>% count(proxy)

lakesHi %>% select(proxy, `site name`, `reference 1 doi`)

lakes %>% ggplot(aes(x = proxy, fill = `resolution (yr)` <= 2)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

lakes %>% ggplot(aes(x = `resolution (yr)`)) + geom_histogram()



