#finding team names
find_names <- function(x,col) {
  
  # Checking if the opponent is listed as '@' - or away
  if(str_detect(x,"^@") == T) {
    
    # Get name
    d <- filter(tm_names, tm_names[[col]] == str_extract(x,"(?<=@)\\w+")) %>%
      .[["pfr_abbreviation"]]
    d <- paste("@",d,sep = "") # Adding back the away
  } 
  else
  {
    # Get name
    d <- filter(tm_names, tm_names[[col]] == x) %>%
      .[["pfr_abbreviation"]]
  } 
  return(d)
}