## ---- pages2k

pages2k_load <- function(){
  pages2k_data_file = file_in("data/general/sdata201788-s3.xlsx")
  
  pages <- read_excel(pages2k_data_file, sheet = "Table S1")
  lakes <- pages %>% 
    filter(
      `archive type` == "lake sediment", 
      proxy != "pollen") %>% 
    mutate(proxy = recode(proxy, "midge" = "chironomid"),
           `resolution (yr)` = as.numeric(`resolution (yr)`)) 
  lakesHi <- lakes %>% filter(`resolution (yr)` <= 2)
  lakesHi
}
