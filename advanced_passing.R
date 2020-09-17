
advanced_passing_stats <- function() {
  
  # Getting link and reading in HTML table
  link <- "https://www.pro-football-reference.com/years/2020/passing_advanced.htm" %>%
           read_html() %>%
           html_table(fill = T)
  
  # 
  dfs <- lapply(link, function(x) {
    
    # Making better column names which are in the first row
    descriptions <- x[1,]
    x <- x[-1,] # removing first row
    
    names <- colnames(x)
    
    # Creating a combination column name
    for(ii in 1:length(names)) {
      
      names[ii] <- paste("adv",names[ii],descriptions[ii], sep = "_") %>%
                   str_to_lower() %>%
                   str_replace("__","_") %>%
                   str_replace("/", "_per_") %>%
                   str_replace("%","_per")
    }
    
    # Add back into the DF
    colnames(x) <- names
    
    # Selecting onl the relevant columns
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
  
  return(all_df)
  
}

advanced_rushing_stats <- function() {
  
  # Reading in data
  link <- "https://www.pro-football-reference.com/years/2020/rushing_advanced.htm" %>%
          read_html()
  link_comments <- gsub("!--","", link) # the actual information is commented out, so need to remove in HTML
  
  link2 <- read_html(link_comments) %>% html_table(fill = T)
  link_data <- link2[[1]]
  
}
  



advanced_receiving_stats <- function() {
  
  # Reading in data
  link <- "https://www.pro-football-reference.com/years/2020/receiving_advanced.htm" %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="node-76118"]/div/div/table[1]') %>%
    html_table()html_table(header= T)
}






