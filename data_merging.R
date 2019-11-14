#### test
library(tidyverse)

f <- as.list(list.files("~/ff_shiny_app/ff_app/data/",
                pattern = "all_data",
                full.names = T)
)

f2 <- lapply(f, function(x) {
  d <- readxl::read_xlsx(x)
  return(t(as.data.frame(names(d))))
  }) %>%
  bind_cols(fill = NA)


