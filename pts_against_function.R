## Fantasy Points Against Stats

pts_against <- function(position) {
  
  library(rvest)
  library(tidyverse)
  
  # Read in Data from Pro-Football-Reference
  pts_vs <-  paste("https://www.pro-football-reference.com/years/2019/fantasy-points-against-",
                    position,
                    ".htm",
                    sep = "") %>%                
              read_html() %>%
              html_table(fill = T) %>%
              .[[1]]
  
  n <- paste(names(pts_vs), pts_vs[1,], sep = "_") %>%
       str_remove("^_") %>%
       str_to_lower() %>%
       str_replace("receiving","rec") %>%
       str_replace("rushing", "rush") %>%
       str_replace_all("\\s","_")
       
  names(pts_vs) <- n
  
  pts_vs <- pts_vs[-1,] %>%
            select(-fantasy_per_game_dkpt,
                   -fantasy_per_game_fantpt,
                   -fantasy_fantpt,
                   -fantasy_dkpt,           
                   -fantasy_fdpt)
  
  names(pts_vs) <- paste("pts_vs", names(pts_vs), sep = "_")
  
  # Changing to correct team names
  source("~/ff_shiny_app/ff_app/find_names.R")
  pts_vs[["pts_vs_tm"]] <- sapply(pts_vs[["pts_vs_tm"]], function(x) find_names(x, "full_name"))
  
  
  return(pts_vs)
  
}