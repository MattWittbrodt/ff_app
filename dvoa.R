# Getting Football Outsider Information

dvoa <- function() {
  
  library(rvest)
  library(tidyverse)
  
  # Total Team Efficiency
  total_efficiency <- "https://www.footballoutsiders.com/stats/teameff/2019" %>%
         read_html() %>%
         html_nodes(xpath = '//*[@id="node-62599"]/div/div/table[1]') %>%
         html_table() %>%
         .[[1]] %>%
         .[,-1]
  
  total_names <- colnames(total_efficiency) %>%
                 str_to_lower() %>%
                 str_replace_all("[^\\w]+","_") %>% 
                 str_replace_all("(?<=offense|defense)dvoa","_dvoa")
  
  colnames(total_efficiency) <- total_names
  
  # subsetting columns
  total_efficiency <- total_efficiency %>%
                      select(-last_week,
                             -contains("s_t_"),
                             -w_l) %>%
                      mutate(total_dvoa = as.numeric(sapply(total_dvoa, function(x) str_remove(x, '%'))),
                             total_dave = as.numeric(sapply(total_dave, function(x) str_remove(x, '%'))),
                             offense_dvoa = as.numeric(sapply(offense_dvoa, function(x) str_remove(x, '%'))),
                             defense_dvoa = as.numeric(sapply(defense_dvoa, function(x) str_remove(x, '%'))))
                 
  
  
  
}