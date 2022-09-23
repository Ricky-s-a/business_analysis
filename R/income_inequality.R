
# load library ------------------------------------------------------------

pacman::p_load("tidyverse")


# import data -------------------------------------------------------------

path_to_github <- "https://raw.githubusercontent.com/Ricky-s-a/business_analysis/main/data/income_inequality.csv"
df_raw <- read_csv(path_to_github)

# fix some data entry
df_tidy <- df_raw %>% 
  mutate(country = Country) %>% 
  select(country, wage_inequality_ratio, unemployment_rate)


# visualization -----------------------------------------------------------

g1 <- df_tidy %>% 
  ggplot(aes(wage_inequality_ratio, unemployment_rate)) +
  geom_point(aes(colour = country)) + 
  geom_smooth(method = "lm", se = F, colour = "gray70") +
  geom_label(aes(colour = country), label = df_tidy$country, nudge_x = 0.02) +
  scale_y_continuous(breaks = seq(0, 0.075, length.out = 6)) +
  labs( 
    title = str_wrap("A Scatter Plot Between Changes in Wage Inequality Ratio and Changes in Unemployment Rate By Countries", 80),
    subtitle = "Seemingly Japan is an outliter.",
    x = "Changes in Wage Inequality Ratio from 1980 to 1995",
    y = "Changes in Unemployment Rate from 1980 to 1995") +
  theme_classic() +
  theme(legend.position = "none") 

ggsave("./graphic/income_inequality_with_Japan.png", plot = g1)

g2 <- df_tidy %>% 
  filter(country != "Japan") %>% 
  ggplot(aes(wage_inequality_ratio, unemployment_rate)) +
  geom_point(aes(colour = country)) + 
  geom_smooth(method = "lm", se = F, colour = "gray70") +
  geom_label(aes(colour = country), 
             label = df_tidy %>% filter(country != "Japan") %>% select(country) %>% unlist() %>% unname(), 
             nudge_x = 0.02) +
  scale_y_continuous(breaks = seq(0, 0.075, length.out = 6)) +
  labs(
    title = str_wrap("A Scatter Plot Between Changes in Wage Inequality Ratio and Changes in Unemployment Rate By Countries", 80),
    subtitle = "The same scatter plot as shown above except without Japan.",
    x = "Changes in Wage Inequality Ratio from 1980 to 1995",
    y = "Changes in Unemployment Rate from 1980 to 1995") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("./graphic/income_inequality_without_japan.png", g2)
