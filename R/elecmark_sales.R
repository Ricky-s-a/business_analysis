
# load library ------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "stringr")


# import dataset --------------------------------------------------------

path_to_github <- "https://raw.githubusercontent.com/Ricky-s-a/business_analysis/main/data/elecmart_sales.csv"
df_raw <- read_csv(path_to_github)


# inspect the dataset -----------------------------------------------------

glimpse(df_raw)
summary(df_raw)


# tidy dataset ------------------------------------------------------------

df_tidy <- df_raw %>% 
  clean_names()

# show 
glimpse(df_tidy)

# chekc the categorical data
lapply(df_tidy[,2:7], unique)

# 

df <- df_tidy %>% 
  mutate(total_cost = as.double(str_remove(total_cost, "$")),
         high_item = as.double(str_remove(high_item, "$")))

pacman::p_load("stringr")
as.double(str_remove("$1.33", pattern = "$"))