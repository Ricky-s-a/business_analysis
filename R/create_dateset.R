# load library ------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "stringr", "lubridate")


# import dataset --------------------------------------------------------

path_to_github <- "https://raw.githubusercontent.com/Ricky-s-a/business_analysis/main/data/ST2187_coursework_dataset_2022_23.csv"
df_raw <- read_csv(path_to_github)

# tidy
df_tidy <- df_raw %>% 
  clean_names() %>% 
  mutate(
    order_date = as.Date(order_date, format = "%m/%d/%Y"),
    ship_date = as.Date(ship_date, format = "%m/%d/%Y"),
    order_year = year(order_date), 
    order_month = month(order_date),
    ship_year = year(ship_date),
    ship_month = month(ship_date),
    gap_date = as.numeric(difftime(ship_date, order_date, units = "days")),
    profit_ratio = profit/sales,
    price = sales / (1-discount) / quantity,
    profit_per = profit / quantity,
    shipping_cost_per = shipping_cost / quantity,
    others_cost = price - shipping_cost_per,
    others_cost_per = others_cost / quantity,
    profit_ratio_per = profit_per / price
  ) %>% 
  arrange(desc(order_date)) 

# show 
glimpse(df_tidy)


# merge with other datasets -----------------------------------------------

# import
path_to_datasets <- "~/R/Musashi_University/2022_fall/business_analysis/data/"
df_pop <- read_csv(paste0(path_to_datasets, "population_data.csv"))
df_gdp <- read_csv(paste0(path_to_datasets, "real_gdp_in_dollars.csv"))

# tidy df_pop
df_tidy_pop <- df_pop %>%
  clean_names() %>% 
  select(-country_code) %>% 
  pivot_longer(cols = c(-country_name, -country_code),
               names_to = "year",
               values_to = "population") %>% 
  mutate(year = str_remove(year, "x") %>% as.numeric(),
         country = country_name)
  
# tidy df_gdp
df_tidy_gdp <- df_gdp %>% 
  clean_names() %>% 
  select(c(-country_code)) %>% 
  pivot_longer(cols = c(-country, -indicator_name, -indicator_code),
               names_to = "year",
               values_to = "gdp") %>% 
  mutate(year = str_remove(year, "x") %>% as.numeric()) 

# merge three datasets
df <- df_tidy %>% 
  left_join(df_tidy_gdp, by = c("country", "order_year" = "year")) 
  
df <- df %>% 
  left_join(df_tidy_pop, by = c("country", "order_year" = "year"))

# rename the columns ------------------------------------------------------

df_finished <- df %>% 
  `colnames<-`(c(colnames(df_raw), "Order Year", "Order Month", "Ship Year", "Ship Month", 
                 "Gap Date", "Profit Ratio", "Price", "Profit Per", "Shipping Cost Per", 
                 "Others Cost", "Others Cost Per", "Profit Ratio Per", "Indicator Name", 
                 "Indicator Code", "GDP", ))