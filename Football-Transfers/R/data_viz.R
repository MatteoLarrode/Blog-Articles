# Visualizations
library(tidyverse)
library(ggsankey)
library(ggstream)


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


# League Spending -------------

#Sankey Bump diagram
league_sankey <- ggplot(league_spending_df, aes(x = Year, 
                                    node = Competition, 
                                    fill = Competition, 
                                    value = Spending)) +
  geom_sankey_bump(space = 0, 
                   type = "alluvial", 
                   color = "transparent", 
                   smooth = 6) +
  scale_fill_viridis_d(option = "A", 
                       alpha = .8) +
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

league_sankey


#Stream diagram
league_sankey2 <- ggplot(league_spending_df, aes(Year, Spending,
                                     fill = Competition)) +
  geom_stream() +
  scale_fill_viridis_d(option = "A", alpha = .8) +
  labs(x = NULL,
       y = "Expenditures (€ mn)",
       fill = NULL,
       color = NULL) +
  theme(legend.position = "bottom") +
  labs(title = "Spending of Big 5 European Football Leagues (2012-2023)")

league_sankey2



#Club spending ---------------

club_line <- ggplot(club_spending_df, aes(Year, Expenditure, colour = Club))+
  geom_line()


club_line



#Spending & Income ------------

plot_new <- ggplot(club)
