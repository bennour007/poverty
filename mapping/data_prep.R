# prepare data

files <- read_rds("~/Projects/poverty/data_clean/names_of_files.rds")

data <- map(files, function(x) paste("~/Projects/poverty/data_clean/",x,".csv", sep = "")) %>%
  map(., read_csv) 