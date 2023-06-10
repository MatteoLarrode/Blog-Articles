# Idea 3: Premier League Net Spend, Through the Years
library(tidyverse)
library(camcorder)
library(scales)
library(ggtext) 
library(grid)

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
  ungroup()%>%
  mutate(Season = as.factor(paste0(as.character(Year), "/", substr(as.character(Year+1), 3, 4))))





# Data Viz --------------

redPoint <- "#f6423c"
bluePoint <- "#017480"
greyLine <- "#858585"



gg_record(
  dir = "recording_plot", # where to save the recording
  device = "png", # device to use to save images
  width = 6,      # width of saved image
  height = 4,     # height of saved image
  units = "in",   # units for width and height
  dpi = 300       # dpi to use when saving image
)

gg_resize_film(
  width = 6,
  height = 4,
  units = "in",
  dpi = 350
)


chelsea <- grobTree(textGrob("The 2022/2023 season set a new record \nfor net transfer expenditure, with Chelsea's\n€611.5m spent under new American owner\nTodd Boehly (37% of total league spend)", 
                             x=0.65,  y=0.23, hjust=0,
                             gp=gpar(col="#858585", 
                                     fontsize=7,
                                     family = "Roboto Condensed")))


plot <- ggplot(pl_net_spend_year) +
  geom_segment(aes(x = Income, 
                   y = Season,
                   xend = Expenditure,
                   yend = Season),
               color = greyLine, linewidth = 0.6, alpha = 1) +
  geom_segment(aes(x = 0, 
                   y = Season,
                   xend = Income,
                   yend = Season),
               color = greyLine, linewidth = 0.1, alpha = 1) +
  geom_point(aes(x = Income, y = Season), color=bluePoint, size=2)+
  geom_point(aes(x = Expenditure , y = Season), color=redPoint, size=2)+
  
  geom_text(aes(x = (Income + Expenditure) / 2, 
                y = Season, 
                label = paste0("", round((Net_Spend/1000), 2),"")),
            color = greyLine, size = 2.5, vjust = -.7, fontface = "bold") +
  
  labs(title = "Record Shopping Spree for Premier League Clubs",
       subtitle = "Total transfer
       <span style = 'color: #017480'>**Income**</span>, 
       <span style = 'color: #f6423c'>**Expenditure**</span>, and
       <span style = 'color: #858585'>**Net Spend**</span>
       by Premier League clubs over last 10 seasons, €bn",
       caption = "Source: Transfermarkt",
       x = NULL,
       y = NULL)+
  
  scale_y_discrete(limits = rev(levels(pl_net_spend_year$Season)))+
  scale_x_continuous(labels = scales::label_number(scale = 1e-3),
                     breaks = seq(500, 4000, by = 500))+
  theme(text = element_text(family = "Roboto Condensed"),
        plot.margin = margin(t = 0.5, r = 0, b = 0, l = 0, unit = "cm"),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x =  element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text = element_text(size = rel(0.7), color = "gray8"),
        axis.line.x  = element_line(color = "gray8"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = rel(1.1), hjust = -0.18, face = "bold"),
        plot.subtitle = element_markdown(hjust = 1.8, size = rel(0.95)),
        plot.caption = element_text(hjust = -0.09, vjust = 5, size = 7, colour = "#4B4B4B"))+
  annotation_custom(chelsea)

plot



gg_playback(
  name = "recording_plot/vignette_gif.gif",
  first_image_duration = 5,
  last_image_duration = 15,
  frame_duration = .2,
)
