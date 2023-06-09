# Visualizing Wilfires in Canada

# Load data & package ----------
library(tidyverse)
library(readxl)

data <- read_excel("data/burned_areas_weekly.xlsx")



# Visualization ---------------
barplot <- ggplot(data, aes(x = as.factor(Week))) +
  geom_bar(aes(y = avg_10_year), 
           stat = "identity", fill=alpha("blue", 0.3))+
  geom_bar(aes(y = year_2023), 
           stat = "identity", fill=alpha("red", 0.3))+
  
  # Limits of the plot = very important. 
  #The negative value controls the size of the inner circle, 
  #the positive one is useful to add size over each bar
  ylim(-200,800) +
  
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  )

barplot
