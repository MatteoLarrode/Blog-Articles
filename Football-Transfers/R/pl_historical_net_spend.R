# Idea 3: Premier League Net Spend, Through the Years
library(tidyverse)

# Data Wrangling -----------

data <- read_csv("data/pl_net_spend_03_22.csv")

pl_net_spend_df <- data %>%
  mutate(
    Expenditure_in_millions = case_when(
      grepl("m", Expenditure, ignore.case = TRUE) ~ as.numeric(sub("€([0-9.]+)m", "\\1", Expenditure)),
      grepl("k", Expenditure, ignore.case = TRUE) ~ as.numeric(sub("€([0-9.]+)k", "\\1", Expenditure)) * 1e-3,
      TRUE ~ 0
    ),
    Income_in_millions = case_when(
      grepl("m", Income, ignore.case = TRUE) ~ as.numeric(sub("€([0-9.]+)m", "\\1", Income)),
      grepl("k", Income, ignore.case = TRUE) ~ as.numeric(sub("€([0-9.]+)k", "\\1", Income)) * 1e-3,
      TRUE ~ 0
    ))%>%
  mutate(
    Net_Spend = Expenditure_in_millions - Income_in_millions)


pl_net_spend_year <- pl_net_spend_df %>%
  filter(Year > 2011)%>%
  group_by(Year)%>%
  summarise(Expenditure = sum(Expenditure_in_millions),
            Income = sum(Income_in_millions),
            Net_Spend = sum(Net_Spend))%>%
  ungroup()


# Data Viz --------------

