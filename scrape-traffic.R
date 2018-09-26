library(jsonlite)
library(tidyverse)

this_dir <- function(directory)
setwd(file.path(getwd(), directory))

file <- read.csv("validation.csv")
data <- fromJSON("https://data.cityofchicago.org/resource/t2qc-9pjd.json") %>%
  select(last_updated = "_last_updt", region_id = "_region_id", current_speed)
data <- unique(rbind(file, data))
write.table(data, "validation.csv", quote = FALSE, sep = ",", row.names = FALSE)
