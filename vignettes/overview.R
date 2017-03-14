## ---- echo = FALSE, include = FALSE--------------------------------------
library(fars)
library(dplyr)
library(maps)

## ----fars_read_example---------------------------------------------------
filename <- system.file("extdata/accident_2013.csv.bz2", package = "fars")
fars_read(filename)

## ----fars_summarize_years_example----------------------------------------
setwd(system.file("extdata", package = "fars"))
fars_summarize_years(2013:2015)

## ----fars_map_state_example----------------------------------------------
setwd(system.file("extdata", package = "fars"))
fars_map_state(45, 2015)

