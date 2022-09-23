
# load library ------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "stringr", "lubridate", "patchwork")


# import dataset --------------------------------------------------------

path_to_github <- "https://raw.githubusercontent.com/Ricky-s-a/business_analysis/main/data/ST2187_coursework_dataset_2018_19.csv"
df_raw <- read_csv(path_to_github)


# inspect the dataset -----------------------------------------------------

glimpse(df_raw)

# tidy dataset ------------------------------------------------------------

df_tidy <- df_raw %>% 
  clean_names()

# show 
glimpse(df_tidy)


# tidy
df <- df_tidy %>% 
  mutate(
    order_date = as.Date(order_date, format = "%m/%d/%Y"),
    ship_date = as.Date(ship_date, format = "%m/%d/%Y"),
    order_year = year(order_date), 
    order_month = month(order_date),
    ship_year = year(ship_date),
    ship_month = month(ship_date),
    split_tf = round(runif(nrow(df_tidy), min = 1, max = 5)),
    gap_date = as.numeric(difftime(ship_date, order_date, units = "days"))
  ) %>% 
  arrange(desc(order_date))

# show 
df


# explore the dataset -----------------------------------------------------

# summary of numeric data
df %>% 
  select(where(is.numeric)) %>% 
  summary()

# date range
df %>% 
  select(ends_with("date")) %>% 
  lapply(range)

# check the number of unique categorical variables
df_tidy %>% 
  select(where(is.character)) %>% 
  lapply(unique) %>%
  lapply(length)  


# the number of orders in each month
df$order_date %>% table() %>% plot()

# the number of ships in each month
df$order_date %>% table() %>% plot()

g1 <- 
  df %>%
  group_by(order_year, order_month) %>% 
  ggplot() + 
  aes(order_date) +
  geom_bar() 

g2 <- 
  g1 <- 
  df %>%
  group_by(order_year, order_month) %>% 
  ggplot() + 
  aes(order_date) +
  geom_bar() 


# basic info --------------------------------------------------------------

# the ration of regions in profit on the annual basis
df %>% 
  group_by(order_year, order_month, region) %>%
  summarise(sum_profit = sum(profit)) %>% 
  ggplot(aes(order_year, sum_profit, fill = region)) + 
  geom_col() 

# total profit by year
profit_by_year <- 
  df %>% group_by(order_year) %>% 
  summarise(annual_profit = sum(profit))

# profit ratio by year
df %>% 
  group_by(order_year, order_month, region) %>%
  summarise(sum_profit = sum(profit)) %>% 
  mutate(profit_ratio_by_year = sum_profit/filter(profit_by_year, order_year == order_year)[[2]]) %>% 
  plot()

# Q. the most profitable market, product, category, sub_category, 
df %>% 
  group_by(order_year, market) %>% 
  summarise(profit = sum(profit)) %>% 
  arrange(order_year,desc(profit)) %>% 
  view()

# -> especially the markets in APAc and EU are expanding. 

# Q. which country?
df %>% 
  filter(market %in% c("APAC", "EU")) %>% 
  group_by(order_year, country) %>% 
  summarise(market_profit = sum(profit)) %>% 
  arrange(order_year, desc(market_profit)) %>% 
  top_n(3, market_profit)

# -> the presence of China and India is growing.

# which product is sold in those region? 
df %>% 
  filter(country %in% c("India", "China")) %>% 
  group_by(order_year, category) %>% 
  summarise(profit_by_category = sum(profit)) %>% 
  top_n(10, profit_by_category) 

%>%
  ungroup() %>% 
  select(category) %>% 
  unique()

# Who bought the most?


# formulate hypothesis -------------------------------------------------

# there must be some variations in the gap between the order date and the ship date.
# Q. how is the gap between the order date and the ship date?
df %>% 
  ggplot(aes(gap_date, region)) +
  geom_boxplot()
  
# A. there is no significant difference across countires. 

# there must be some variations in the shipping cost across countries.
# Q. how much is that? 
g_corporate <- df %>% 
  filter(segment == "Corporate") %>% 
  ggplot(aes(shipping_cost, market)) +
  geom_boxplot() +
  labs(title = "Corporate")

g_consumer <- df %>% 
  filter(segment == "Consumer") %>% 
  ggplot(aes(shipping_cost, market)) +
  geom_boxplot() + 
  labs(title = "Consumer")

g_home_office <- df %>% 
  filter(segment == "Home Office") %>% 
  ggplot(aes(shipping_cost, market)) +
  geom_boxplot() +
  labs(title = "Home Office")

g_corporate / g_consumer / g_home_office

# split data for computational capacity

df1 <- df %>% filter(split_tf == 1)
