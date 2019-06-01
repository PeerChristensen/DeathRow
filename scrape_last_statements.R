# DEATH ROW LAST STATEMENTS
# Script for scraping last statements of inmates on Death Row
# Source: https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html

library(rvest)
library(tidyverse)

baseUrl   <- "https://www.tdcj.texas.gov/death_row/"

sourceUrl <- "https://www.tdcj.texas.gov/death_row/dr_executed_offenders.html"

sourceHTML <- read_html(sourceUrl)

# get meta data
meta <- sourceHTML %>% 
  html_table()     %>% 
  .[[1]]           %>% 
  as_tibble(.name_repair = "unique") %>%
  mutate(Name = paste(`First Name`, `Last Name`)) %>%
  select(-`First Name`, -`Last Name`,-Link...2,-Link...3)

# get list of urls to scrape
urls <- sourceHTML       %>% 
  html_nodes("div tr a") %>%
  html_attr("href")      %>%
  str_subset("last")     %>%
  str_subset("no_last_statement", negate = T) %>%
  map_chr(function(x) paste0(baseUrl,x))

# scrape statements

NamesNumbers <- NULL
statement <- NULL

for (url in urls) {
  
   tryCatch(current <- url %>%  
     read_html() %>%
     html_nodes("p") %>%
     html_text(), error = function(x) print("error"))
   
   tryCatch(name <- current %>%
              .[[4]], error = function(x) name = NA)
   
   NamesNumbers <- c(NamesNumbers,name) 
   
   tryCatch(statmnt <- current %>%
     .[[6]], error = function(x) statmnt = NA)
   
   statement <- c(statement,statmnt) 
   statmnt = NA
   name = NA
   print(url)
}
   
df <- tibble(NamesNumbers,statement)

# clean up

df$TDCJNumber <- df$NamesNumbers %>%
  str_extract_all("[:digit:].*") %>%
  map_chr(paste, collapse = "") %>%
  str_trim() %>%
  str_replace_all("^$","NA") %>%
  as.numeric()

df$Name2 <- df$NamesNumbers %>%
  str_remove_all("#:*") %>%
  str_remove_all("[:digit:]") %>%
  str_trim() %>%
  str_split(",") %>%
  map(1) %>%
  unlist()

df <- df %>%
  left_join(meta,by = "TDCJNumber") %>%
  rename(Statement = statement) %>%
  select(TDCJNumber,Name,Name2,Statement,everything()) %>%
  filter(!is.na(Statement)) %>%
  select(-NamesNumbers)
  
# write to file

write_csv(df,"last_statements.csv") # clean up, add meta

#################################################
# UNUSED
  
  
  # number <- url %>%  
  #   read_html() %>%
  #   html_nodes("p") %>%
  #   html_text() %>%
  #   .[[4]] %>%
  #   str_split("#") %>%
  #   map(2) %>%
  #   unlist() %>%
  #   as.numeric()
  # 
  # #TDCJNumber <- c(TDCJNumber,number) 
  # df <- rbind(df,cbind(name,number))
  # #d <- cbind(name,number)
  # #df <- rbind(df,d)
  


  