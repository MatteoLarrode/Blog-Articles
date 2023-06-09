# Clean datasets

library(tidyverse)
library(readxl)

#load datasets ------------
spending_df <- read_csv("data/clubs_spending_12_23.csv")
owners_df <- read_excel("data/reshaped_owners.xlsx")
final_df <- read_excel("data/final_df.xlsx")



# #reshape dataset
# reshaped_owners_df <- owners_df %>%
#   gather(key, value, -Club) %>%
#   separate(key, into = c("Variable", "Year"), sep = "_") %>%
#   spread(Variable, value)


#clean data-----------------

big5 <- c("Ligue 1", "Premier League", "Serie A", "LaLiga", "Bundesliga")
socios <- c("FC Barcelona", "Real Madrid")
local_owners <- c("France", "Spain", "United Kingdom", "Italy", "Germany")
foreign_owners_MENA <- c("Qatar", "Saudi Arabia", "United Arab Emirates")
foreign_owners_else <- c("China", "Russia", "United States")


club_spending_df <- spending_df %>%
  mutate(
    Expenditure = parse_number(str_remove(Expenditure, "€")),
    Arrivals = as.numeric(Arrivals),
    Income = parse_number(str_remove(Income, "€")),
    Departures = as.numeric(Departures),
    Balance = parse_number(str_remove(Balance, "€")),
    Competition = as.factor(Competition))%>%
  #only keep top 15 clubs of every season
  filter(`#` <= 15)%>%
  #only keep big 5 clubs
  filter(Competition %in% big5)%>%
  #filter(!(Club %in% socios))%>%
  #fix Inter Milan
  mutate(Club = ifelse(Club == "FC Internazionale", "Inter Milan", Club))


league_spending_df <- club_spending_df %>%
  group_by(Competition, Year)%>%
  summarise(Spending = sum(Expenditure), .groups = "keep")%>%
  ungroup()

# add info about owner ---------------

club_spending_simple_df <- club_spending_df %>%
  select(`#`, Club, Expenditure, Income, Year)

#join dataset on club and year
final_df <- left_join(club_spending_simple_df, owners_df, join_by("Club", "Year"))%>%
  filter(!is.na(Nationality))


# final tweaks ------------

#group by majority shareholder nationality
nationality_spending_df <- final_df  %>%
  mutate(Nationality = case_when(
    Nationality %in% local_owners ~ Nationality,
    Nationality %in% foreign_owners_MENA ~ Nationality,
    Nationality %in% foreign_owners_else ~ Nationality,
    TRUE ~ "Other"
  ))%>%
  group_by(Nationality, Year)%>%
  summarise(Spending = sum(Expenditure))%>%
  ungroup()%>%
  complete(Nationality, Year, fill = list(Spending = 0))





#TEST

test_df <- club_spending_simple_df%>%
  filter(!is.na(Income))%>%
  group_by(Year)%>%
  summarise(Spending = sum(Expenditure),
            Income = sum(Income))%>%
  mutate(Diff = Spending - Income)
