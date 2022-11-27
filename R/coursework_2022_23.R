# load library ------------------------------------------------------------

pacman::p_load("tidyverse", "janitor", "stringr", "lubridate", "patchwork", "plotly")


# import dataset --------------------------------------------------------

path_to_github <- "https://raw.githubusercontent.com/Ricky-s-a/business_analysis/main/data/ST2187_coursework_dataset_2022_23.csv"
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
    gap_date = as.numeric(difftime(ship_date, order_date, units = "days"))
  ) %>% 
  arrange(desc(order_date))

# show 
glimpse(df)


# explore the dataset -----------------------------------------------------

path <- "./graphic/coursework22_23/"

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
  geom_bar() +
  labs(title = "the number of orders by month")

g2 <- 
  g1 <- 
  df %>%
  group_by(order_year, order_month) %>% 
  ggplot() + 
  aes(order_date) +
  geom_bar() +
  labs(title = "the number of ships by month")

g3 <- g1/g2

ggsave(paste0(path, "ships_orders.png"), plot = g3)

# hypothesis --------------------------------------------------------------


# kinds of goods
# elasticity: discount vs sales
g4_1 <- 
  df %>% 
  ggplot(aes(discount, sales)) +
  geom_jitter() +
  geom_smooth(method = "lm")

library(ggExtra)
g4_11 <- 
  ggExtra::ggMarginal(
    g4_1,
    type = "density",
    margins = "y",
    size = 5
  )

ggsave(paste0(path, "discout_vs_sales.png"), plot = g4_11)

# elasticity: discount vs profits
g4_2 <- 
  df %>% 
  ggplot(aes(discount, profit)) +
  geom_jitter() +
  geom_smooth(method = "lm")

g4_21 <- 
  ggExtra::ggMarginal(
    g4_2,
    type = "density",
    margins = "y",
    size = 5
  )

ggsave(paste0(path, "discout_vs_profits.png"), plot = g4_21)

# the outlier profits
df_profit_outlier <-
  df %>% 
  mutate(profit_outlier = case_when(
                            profit > 2000  ~ "high",
                            profit < -2000 ~ "low",
                            TRUE           ~ "normal"
                            )
         ) %>%
  filter(profit_outlier %in% c("high", "low")) 

g5 <- 
  df_profit_outlier %>% 
  ggplot(aes(discount, profit, colour = sub_category)) +
  geom_point() +
  geom_label(label = df_profit_outlier$sub_category)

ggplotly(g5)
