
library(tidyverse)
library(tabulizer)


# I will denote ECBNV_2015 as E15, and RPGH as R14  
report <- "report.pdf"

# Split the long 111 pages report to its pages in the directory pages_single

split_pdf(report, outdir = "pages_single/")

# create a vector containing the names of each single page : 
# I will have to parse this way, am too lazy to explain it now.

a <- vector("list", 111)

for(i in 1:9){
  a[i] <- paste("pages_single/report00", i, ".pdf", sep = "")
} 

for(i in 10:99){
  a[i] <- paste("pages_single/report0", i, ".pdf", sep = "")
} 

for(i in 100:111){
  a[i] <- paste("pages_single/report", i, ".pdf", sep = "")
} 

list_tables <- vector("list", 26)

# Loop on each of these elements and extract the tables if they exisit.

data_raw_complete <- map(a, extract_tables)


# Cleaning the table in page 20
list_tables[[26]] <- data_raw_complete[20] %>%
  as.data.frame() %>%
  .[-c(1,2,3,4),] %>%
  separate(.,X2, sep = " ", into = c("poverty_E15", "sd_E15", "lower_E15", "upper_E15")) %>%
  separate(.,X4, sep = " ", into = c("sd_R14", "Abs_diff_E15_R14")) %>%
  rename(state = X1, poverty_R14 = X3) %>%
  as_tibble()
#########################################
#DONE!
#########################################

# Cleaning table in page 40 : Tunis state

list_tables[[1]] <- data_raw_complete[40] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()

##########################################
#DONE!
##########################################

#cleaning table in page 42 state of ariana

list_tables[[2]] <- data_raw_complete[42] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()

#########################################
#DONE!
#########################################

# cleaning table in page 44 state of ben arous 

list_tables[[3]] <- data_raw_complete[44] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
# Cleaning table in page 45 stat of manouba 

list_tables[[4]] <- data_raw_complete[45] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()

#########################################
#cleaning table in page 49 state of nabeul 

list_tables[[5]] <- data_raw_complete[49] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()



#########################################
#cleaning table in page 50 state of zaghouan 

list_tables[[6]] <- data_raw_complete[50] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 52 Bizerte 

list_tables[[7]] <- data_raw_complete[52] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()



#########################################
#cleaning table in page 55 Beja

list_tables[[8]] <- data_raw_complete[55] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 56 Jendouba 

list_tables[[9]] <- data_raw_complete[56] %>% 
  as.data.frame() %>%
  .[-c(1:3), c(2,5,8,11,14)] %>%
  rename(county = X2, 
         dropout_rate_primary = X5,
         dropout_rate_secondary = X8, 
         dropout_rate_ps = X11, 
         poverty = X14) %>%
  as_tibble()


#########################################
#cleaning table in page 58 el kef 

list_tables[[10]]<- data_raw_complete[58] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()



#########################################
#cleaning table in page 62 seliana 

list_tables[[11]] <- data_raw_complete[62] %>%
  as.data.frame() %>%
  .[-c(1:3), -2] %>%
  rename(county = X1, 
         dropout_rate_primary = X3,
         dropout_rate_secondary = X4, 
         dropout_rate_ps = X5, 
         poverty = X6) %>%
  as_tibble()


#########################################
#cleaning table in page 66 sousse 

list_tables[[12]] <- data_raw_complete[66] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 68 monastir

list_tables[[13]] <- data_raw_complete[68] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 69 Mahdia  

list_tables[[14]] <- data_raw_complete[69] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()



#########################################
#cleaning table in page 71 sfax 

list_tables[[15]] <- data_raw_complete[71] %>%
  as.data.frame() %>%
  .[-c(1:5),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()



#########################################
#cleaning table in page 73 Kairouan

list_tables[[16]] <- data_raw_complete[73] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 75 

list_tables[[17]] <- data_raw_complete[75] %>%
  as.data.frame() %>% 
  .[-c(1:5), c(2,5,8,11,14)] %>%
  rename(county = X2, 
         dropout_rate_primary = X5,
         dropout_rate_secondary = X8, 
         dropout_rate_ps = X11, 
         poverty = X14) %>%
  as_tibble()


#########################################
#cleaning table in page 76 sidi bouzid 

list_tables[[18]] <- data_raw_complete[76] %>%
  as.data.frame() %>%
  .[-c(1:5),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table in page 79 Gabes

list_tables[[19]] <- data_raw_complete[79] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################
#cleaning table 1 in page 82 medinine

list_tables[[20]] <- data_raw_complete[82][[1]][1] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()

#########################################
#cleaning table 2 in page 82 state of tatouine

list_tables[[21]] <- data_raw_complete[82][[1]][2] %>%
  as.data.frame() %>%
  .[-c(1:3), -2] %>%
  rename(county = X1, 
         dropout_rate_primary = X3,
         dropout_rate_secondary = X4, 
         dropout_rate_ps = X5, 
         poverty = X6) %>%
  as_tibble()

#########################################
#cleaning table in page 85 Gafsa

list_tables[[22]] <- data_raw_complete[85] %>%
  as.data.frame() %>%
  .[-c(1:3), -2] %>%
  rename(county = X1, 
         dropout_rate_primary = X3,
         dropout_rate_secondary = X4, 
         dropout_rate_ps = X5, 
         poverty = X6) %>%
  as_tibble()

#########################################
#cleaning table in page 86 touzeur 

list_tables[[23]] <- data_raw_complete[86] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()

#########################################
#cleaning table in page 87 kebili

list_tables[[24]] <- data_raw_complete[87] %>%
  as.data.frame() %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         dropout_rate_primary = X2,
         dropout_rate_secondary = X3, 
         dropout_rate_ps = X4, 
         poverty = X5) %>%
  as_tibble()


#########################################

# Cleaning that table in pages 95 to 100

t25 <- extract_tables(report, pages = c(94:100))
list_tables[[25]] <- rbind(as.data.frame(t25[1]), 
             as.data.frame(t25[2]), 
             as.data.frame(t25[3]), 
             as.data.frame(t25[4]), 
             as.data.frame(t25[5]), 
             as.data.frame(t25[6]), 
             as.data.frame(t25[7])) %>%
  .[-c(1:3),] %>%
  rename(county = X1, 
         code = X2,
         population = X3, 
         poverty = X4, 
         sd = X5) %>%
  as_tibble()
################################################################################

tables_new <- map(list_tables, 
                  function(x){
                    x %>%
                      sapply(., str_replace, ",", ".") %>%
                      as_tibble()
                    }
                  )

#for now i will only convert the dat to double for the state wise tables

states_tables <- vector("list", 24)

tables_new[c(1:24)] <- map(tables_new[c(1:24)], 
                  function(x){
                    x %>%
                      sapply(., str_replace, ",", ".") %>%
                      as_tibble() %>%
                      mutate(dropout_rate_primary  = as.double(dropout_rate_primary)) %>%
                      mutate(dropout_rate_secondary = as.double(dropout_rate_secondary)) %>%
                      mutate(dropout_rate_ps = as.double(dropout_rate_ps)) %>%
                      mutate(poverty = as.double(poverty))
                    }
                  )
# PLEASE BE CAREFUL, THE PAGE NUMBER 93 WAS MADE INTO A ROW
# IT ALMOST GAVE ME DEPRESSION. ILL MAKE SURE IT WON'T STAY THERE

tables_new[[25]][,c(2:ncol(tables_new[[25]]))] <- 
  tables_new[[25]][,c(2:ncol(tables_new[[25]]))] %>% 
  sapply(., as.double)

tables_new[[25]] <- tables_new[[25]] %>% 
  na.omit()


tables_new[[26]][,c(2:ncol(tables_new[[26]]))] <- 
  tables_new[[26]][,c(2:ncol(tables_new[[26]]))] %>% 
  sapply(., as.double) 

states_tables <- tables_new[c(1:24)]


n_counties <- states_tables %>%
  sapply(., nrow) 


states <- c("Tunis","Ariana","Ben Arous","Manouba","Nabeul","Zagouan","Bizerte","Beja","Jandouba", "Kef","Seliana","Sousse","Monastir","Mahdia","Sfax","Kairouan","Kasserine","Sidi Bouzid","Gabes","Médnine","Tataouine","Gafsa","Tozeur","Kebili")

nstates <- map2(states, n_counties, rep)

tidy_tables <- map2(states_tables, nstates, 
                    function(x,y){
                      x %>%
                        mutate(state = y)
                    }
)

tables_new[c(1:24)] <- tidy_tables

tidy_all <- tables_new

# saving everything to a clean data folder 

# It would e a good idea if I name each table with the name of its state. not now.

lesnoms <- states %>% append(c("county_details", "state_details"))

paths <- map(lesnoms, 
             function(x){
               paste("data_clean/",x,".csv", sep = "")
               }
             )

map2(tidy_all, paths, write_csv)

# to import in a vecotrized way you can use the this RDS for the names vector: 

lesnoms %>% write_rds("data_clean/names_of_files.rds")

