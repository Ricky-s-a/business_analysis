
# load packages -----------------------------------------------------------

packages <- c("readr", "dplyr", "janitor", "stringr", "tidyr")
lapply(packages, library, character.only = TRUE)

# import data -------------------------------------------------------------

df_raw <- read_csv("./data/income_comparison.csv")


# data wrangling ----------------------------------------------------------

# tidy columns
df_tidy <- df_raw %>%
  clean_names() 

# rename columns by removing `x` from their columns
df_tidy <- `colnames<-`(df_tidy, str_remove(colnames(df_tidy), "x"))

# create a new dataframe
df <- tibble(
  "2007" = c(df_tidy[[1]], df_tidy[[3]], df_tidy[[5]]),
  "2017" = c(df_tidy[[2]], df_tidy[[4]], df_tidy[[6]])
)

# show 
glimpse(df)


# practice problem 1. --------------------------------------------------------

result1 <- tibble(
 mean_value = lapply(df, mean, na.rm = T),
 median_value = lapply(df, median, na.rm = T),
 sd_value = lapply(df, sd, na.rm = T),
 q_25 = lapply(df, quantile, probs = 0.25, na.rm = T),
 q_75 = lapply(df, quantile, probs = 0.75, na.rm = T),
 q_95 = lapply(df, quantile, probs = 0.95, na.rm = T)
)

# add some modification
result1 <- 
  unnest(result1) %>% # unnest 
  t() %>% # exchange rows and columns
  `colnames<-`(value = c("2007", "2017"))

# show
result1


# practice problem 2. -----------------------------------------------------


