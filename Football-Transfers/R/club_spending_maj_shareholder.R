# Idea 1: Club Spending x Majority Shareholder
library(tidyverse)
library(readxl)
library(ggsankey)
library(ggstream)

# Data Wrangling ---------------

spending_df <- read_csv("data/clubs_spending_12_23.csv")
owners_df <- read_excel("data/reshaped_owners.xlsx")
final_df <- read_excel("data/final_df.xlsx")


# #reshape dataset
# reshaped_owners_df <- owners_df %>%
#   gather(key, value, -Club) %>%
#   separate(key, into = c("Variable", "Year"), sep = "_") %>%
#   spread(Variable, value)


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


# add info about owner
club_spending_simple_df <- club_spending_df %>%
  select(`#`, Club, Expenditure, Income, Year)

#join dataset on club and year
final_df <- left_join(club_spending_simple_df, owners_df, join_by("Club", "Year"))%>%
  filter(!is.na(Nationality))


# final tweaks
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


# Data Viz -----------------

# Majority shareholder nationality spending ---------

foreign_invest_sankey <- ggplot(nationality_spending_df, aes(x = Year, 
                                                             node = Nationality, 
                                                             fill = Nationality, 
                                                             value = Spending)) +
  geom_sankey_bump(space = 1,
                   color = "transparent", 
                   smooth = 6,
                   alpha = 0.8) +
  scale_fill_viridis_d(option = "A") +
  scale_y_continuous(position = "right",
                     labels = scales::dollar_format(prefix = "€", suffix = " k million", scale = 1e-3),
                     breaks = seq(1e3, 3e3, by = 1e3))+
  scale_x_discrete(breaks = c("2012", "2014", "2016", "2018", "2020", "2022"))+
  theme_sankey_bump(base_size = 16) +
  labs(title = "Spending of Big 5 European Football Leagues (2012-2023)",
       caption = "Source: Transfermarkt | Chart by Matteo Larrode",
       x = NULL,
       y = NULL,
       fill = NULL,
       color = NULL) +
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(t = 0, r = 0.5, b = 0, l = 0.5, unit = "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "#dcdbd8"),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(margin = margin(l=3), size = 10),
        legend.title = element_blank(),
        legend.key.width = unit(25,"pt"),
        legend.key.height = unit(15, "pt"),
        legend.key = element_blank(),
        axis.text = element_text(size = rel(0.8), color = "gray8"),
        axis.line.x  = element_line(color = "gray8"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = rel(1), hjust = 0, face = "bold"),
        plot.caption = element_text(hjust = 0, size = 9, colour = "#4B4B4B"))

foreign_invest_sankey


#Stream diagram
foreign_invest_sankey2 <- ggplot(nationality_spending_df, aes(Year, Spending,
                                                              fill = Nationality)) +
  geom_stream(type = "proportional") +
  scale_fill_viridis_d(option = "A", alpha = .8) +
  labs(x = NULL,
       y = "Expenditures (€ mn)",
       fill = NULL,
       color = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "Spending of Big 5 European Football Leagues (2012-2023)")

foreign_invest_sankey2



#line graph
foreign_invest_line <- ggplot(nationality_spending_df, 
                              aes(Year, Spending, fill = Nationality)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "A", alpha = .8) +
  labs(x = NULL,
       y = "Expenditures (€ mn)",
       fill = NULL,
       color = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "Spending of Big 5 European Football Leagues (2012-2023)")

foreign_invest_line