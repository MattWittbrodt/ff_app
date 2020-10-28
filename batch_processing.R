#!/usr/bin/env Rscript

# Finding what week it is ------
# 8 is the next Sunday, so subtracting date # (1-7) from 8, then adding to date
date <- as.character(Sys.Date() + (8 - lubridate::wday(Sys.Date())))

wk_num <- switch(date,
                 "2020-11-01" = 8,
                 "2020-11-08" = 9,
                 "2020-11-15" = 10,
                 "2020-11-22" = 11,
                 "2020-11-29" = 12,
                 "2020-12-06" = 13,
                 "2020-12-13" = 14,
                 "2020-12-20" = 15,
                 "2020-12-27" = 16,
                 "2021-01-03" = 17)

cat(paste0("-----Doing analysis for Week ",wk_num,"----- \n"))

print("Starting Shiny DF Creation")

# Getting data ----
source("C:/Users/mattw/Documents/ff_shiny_app/ff_app/shiny_df_creation.R")

upname <- shiny_df(wk_num)

cat(paste0("Shiny DF for week ", wk_num, " has been created with filename: ", upname, "! \n"))

# Deploying app ----
# Specifying the specific files - this also helps to alleviate errors
#files <- c("app.R","global.R",paste0("data/",fname))
files <- c("app.R","global.R",upname)

# Code to actually deploy app
rsconnect::deployApp("C:/Users/mattw/Documents/ff_shiny_app/ff_app/",
                     appFiles = files,
                     forceUpdate = T,
                     appName = "ff_app",
                     account = "mattwittbrodt")

cat("Deployed App! \n")

# Writing the log to the app
write(paste0("DF and App updated on: ", Sys.time()),
      file = "C:/Users/mattw/Documents/ff_shiny_app/ff_app/publish_log.txt",
      append = T)

