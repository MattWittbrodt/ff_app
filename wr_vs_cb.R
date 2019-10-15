# Matchup WR vs CB


wr_vs_cv <- function(file) {

  library(tidyverse)
  
  # Read in file
  d <- read.csv(file) %>%
       select(-Day, -Tm, -Pos, -CB, -M.U, -YPT, -Opp) %>%
       mutate(Tar.. = as.numeric(str_remove(Tar.., "%")),
              C.. = as.numeric(str_remove(C.., "%")),
              matchup = factor(ifelse(X..... == " ", "plus",
                              ifelse(X..... == "N", "neutral", "minus")), 
                              ordered = T, levels = c("minus", "neutral", "plus")),
              WR = as.character(WR)) %>%
       select(-X.....)
  
  names(d) <- names(d) %>%
              str_to_lower() %>%
              str_remove_all("[:punct:]")
  
  names(d) <- paste("vs_cb", names(d), sep = "_")
  
  return(d)
}
