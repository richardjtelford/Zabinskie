## ---- pages2k

pages2k_load <- function(){
  pages2k_data_file <- "data/sdata201788-s3.xlsx"
  if(!file.exists(pages2k_data_file)){
    message("Downloading file from https://www.nature.com/articles/sdata201788")
    download.file("https://media.nature.com/original/nature-assets/sdata/2017/sdata201788/extref/sdata201788-s3.xlsx", destfile = pages2k_data_file)
  }
  
  pages <- read_excel(file_in(pages2k_data_file), sheet = "Table S1")
  lakes <- pages %>% 
    filter(
      `archive type` == "lake sediment", 
      proxy != "pollen") %>% 
    mutate(proxy = recode(proxy, "midge" = "chironomid"),
           `resolution (yr)` = as.numeric(`resolution (yr)`)) 
  lakesHi <- lakes %>% filter(`resolution (yr)` <= 2)
  lakesHi
}
