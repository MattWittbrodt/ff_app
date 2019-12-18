# Matchup WR vs CB


wr_vs_cv <- function(file) {

  library(tidyverse)
  
  # Read in file
  d <- read_html(file) %>% 
       html_table() %>%
       .[[1]] %>%
       select(-Day, -Tm, -Pos, -CB, -'M/U', -YPT, -Opp) %>%
       mutate('Tar %' = as.numeric(str_remove(`Tar %`, "%")),
              'C %' = as.numeric(str_remove(`C %`, "%")),
              matchup = factor(ifelse(`(+/-)` == "+", "plus",
                              ifelse(`(+/-)` == "N", "neutral", "minus")), 
                              ordered = T, levels = c("minus", "neutral", "plus")),
              WR = str_remove_all(as.character(WR), "[:punct:]")) %>%
       select(-"(+/-)")
  
  names(d) <- names(d) %>%
              str_to_lower() %>%
              str_remove_all("[:punct:]")
  
  names(d) <- paste("vs_cb", names(d), sep = "_")
  
  write.csv(d, str_replace(file, ".html", ".csv"))
  
  return(d)
}
