### Fixing Vegas Lines that are missing
library(tidyverse)
d <- readxl::read_xlsx("C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to9_combined.xlsx")

# separate DF for na or no na 
d2 <- filter(d, is.na(line) == T) %>% select(-line, -total, -favorite, -implied_total)
d3 <- filter(d, is.na(line) == F)

# fixing the small vegas issues
vegas_fixes <- read.csv("C:/Users/mattw/Desktop/fixing_vegas.csv")
d4 <- left_join(d2, vegas_fixes, by = colnames(vegas_fixes)[1:3])

# putting back togeather
all <- rbind(d3, d4)


writexl::write_xlsx(all, "C:/Users/mattw/Documents/ff_shiny_app/ff_app/2020_data/merged_data/2020_weeks_2to9_combined_vegas_fixes.xlsx")
