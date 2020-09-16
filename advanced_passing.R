
advanced_passing_stats <- function() {
  
  
  link <- "https://www.pro-football-reference.com/years/2020/passing_advanced.htm" %>%
           read_html() %>%
           html_table(fill = T)
  
  link2 <- lapply(link, function(x) {
    
    print(x)
    
    # Make first row into column names
    colnames(x) <- x[1,]
    x <- x[-1,]
    
    # Alter / standardize the column names - adding adv_passing as prefix
    names <- colnames(x) %>% str_to_lower()
    
    names <- paste("adv_passing",names, sep = "_")
    colnames(x) <- names
    
    # Fixing a few issues with columns
    x2 <- x %>%
          select(-adv_passing_rk,
                 -adv_passing_tm,
                 -adv_passing_age,
                 -adv_passing_pos,
                 -adv_passing_g,
                 -adv_passing_gs,
                 -adv_passing_cmp,
                 -adv_passing_att,
                 -adv_passing_yds)
    
    print("The last table processing has been completed")
      
    return(x2)
    
  })
  
  
  
  
  
  
  
  
  
  
}