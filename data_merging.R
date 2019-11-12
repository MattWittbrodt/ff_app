#### test

f <- as.list(list.files("~/ff_shiny_app/ff_app/data/",
                pattern = "all_data",
                full.names = T)
)

f2 <- lapply(f, readxl::read_xlsx)
