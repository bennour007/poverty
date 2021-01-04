library(raster)
library(sp)
library(tidyverse)

geodata <- getData(name = "GADM", 
                   country  = "TUN", 
                   level = 1)

states <- fortify(geodata,region = "HASC_1")
states$id %>% unique

data <- read_csv("~/Projects/poverty/data_clean/state_details.csv")

s_abv <- c("TU", "AN", "BA", "MN", "NB", "ZA", "BZ", "BJ", "JE", "KF", "SL", 
           "SS", "MS", "MH", "SF", "KR", "KS", "SZ", "GB", "ME", "TA", "GF", 
           "TO", "KB") %>% 
  paste("TN.", ., sep = "")


data <- data %>%
  mutate(s_id = s_abv)

################################################################################

i <- match(states$id, data$s_id)
states$poverty_15 = data$poverty_E15[i]

states %>%
  ggplot(aes(x = long,
             y = lat,
             group = group)) +
  geom_polygon(aes(fill = poverty_15), 
               color = 'black', 
               size = 0.1) +
  theme_void()


################################################################################
# Lets do the same thing with the dropout data 

lesnoms <- readRDS("~/Projects/poverty/names_of_files")[1:24] 
data_file <- "~/Projects/poverty/data_clean/"

data_all <- paste(data_file,lesnoms,".csv", sep ="") %>%
  map(read_csv)

almost_final_date <- data_all %>%
  bind_rows() %>%
  group_by(state) %>%
  summarise(dropout_primary = mean(dropout_rate_primary),
            dropout_secondary = mean(dropout_rate_secondary),
            dropout_all = mean(dropout_rate_ps))

s_abv_2 <- c("TN.AN", "TN.BJ", "TN.BA", "TN.BZ", "TN.GB", "TN.GF", "TN.JE", "TN.KR", 
  "TN.KS", "TN.KB", "TN.KF", "TN.MH", "TN.MN", "TN.ME", "TN.MS", "TN.NB",
  "TN.SL", "TN.SF", "TN.SZ", "TN.SS", "TN.TA", "TN.TO", "TN.TU", "TN.ZA")

almost_final_1 <- almost_final_date %>%
  mutate(s_id = s_abv_2)

i <- match(states$id, almost_final_1$s_id)
states$primary = almost_final_1$dropout_primary[i]
states$secondary = almost_final_1$dropout_secondary[i]

i <- match(states$id, almost_final_1$s_id)

states$primary = almost_final_1$dropout_primary[i]
states$secondary = almost_final_1$dropout_secondary[i]

saveRDS(states, "~/my_thesis/data/states_data")

graph_state_dropouts  <- states %>% 
  pivot_longer(cols = primary:secondary,
               names_to = "dropout", 
               values_to = "rate") %>%
 # filter(dropout == "primary") %>%
  ggplot(aes(x = long,
             y = lat,
             group = group)) +
  geom_polygon(aes(fill = rate), 
               color = 'black', 
               size = 0.1) + 
  facet_wrap(dropout~.) +
  scale_fill_viridis_b() +
  theme_void() 

saveRDS(graph_state_dropouts, "~/Projects/poverty/graph")  
saveRDS(graph_state_dropouts, "~/my_thesis/graph")