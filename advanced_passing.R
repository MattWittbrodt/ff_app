
advanced_passing_stats <- function() {
  
  
  link <- "https://www.pro-football-reference.com/years/2020/passing_advanced.htm" %>%
           read_html() %>%
           html_table(fill = T)
  
  dfs <- lapply(link, function(x) {
    
    print(x)
    
    # Making better column names which are in the first row
    descriptions <- x[1,]
    x <- x[-1,] # removing first row
    
    names <- colnames(x)
    
    # Creating a combination 
    for(ii in 1:length(names)) {
      
      names[ii] <- paste("adv",names[ii],descriptions[ii], sep = "_") %>%
                   str_to_lower() %>%
                   str_replace("__","_") %>%
                   str_replace("/", "_per_") %>%
                   str_replace("%","_per")
    }
    print(names)
    # Alter / standardize the column names - adding adv_passing as prefix
    #names <- colnames(x) %>% str_to_lower()
    
    #names <- paste("adv",names, sep = "_")
    colnames(x) <- names
    
    # Fixing a few issues with columns
    x2 <- x %>%
          select(-adv_rk,
                 -adv_tm,
                 -adv_age,
                 -adv_pos,
                 -adv_games_g,
                 -adv_games_gs,
                 -adv_passing_cmp,
                 -adv_passing_att,
                 -adv_passing_yds) %>%
          filter(adv_player != "Player")
    
    print("The last table processing has been completed")
      
    return(x2)
    
  })
  
  # Merging into one DF
  all_df <- left_join(dfs[[1]], dfs[[2]], by = "adv_player") %>%
            left_join(dfs[[3]], by = "adv_player") %>%
            left_join(dfs[[4]], by = "adv_player")
  
  # Fixing a few issues with columns
  all_df[,-1] <- apply(all_df[,-1], c(1,2), function(x) {str_remove(x, "%") %>% as.numeric()})
  
  
  
  
  
  
  
}