# Idea 2: Net Spending x League Position
library(tidyverse)
library(readxl)

# Data Cleaning ----------
data <- read_excel("data/net_spend_league_pos.xlsx")


# Reshaping the dataset
reshaped_df <- data %>%
  gather(key, value, -Club) %>%
  separate(key, into = c("Variable", "Year"), sep = "_") %>%
  spread(Variable, value)

final_df <- reshaped_df %>%
  mutate(Year = as.factor(Year),
         leaguePos = as.numeric(leaguePos),
         netSpend = as.numeric(gsub(",", "", netSpend)))%>%
  filter()


# Data viz -------------

plot <- ggplot(final_df, aes(x = netSpend, y = -leaguePos, col = Year))+
  geom_point()

plot


