# Total balance Premier League Scraping
library(rvest)
library(tidyverse)

# Extract Data Function ------------
read_and_format_table <- function(url) {
  html <- read_html(url)
  table <- html %>% html_element("#yw1") %>% html_table(header = FALSE)
  new_table <- table %>% select(-X1, -X2, -X9) %>% slice(-1)
  names(new_table) <- c("Club", "Expenditure", "Arrivals", "Income", "Departures", "Balance")
  return(as_tibble(new_table))
}

# Initialize the final dataset
final_data <- tibble()


# Scrape data from 2012 to 2022/2023
for (year in 2003:2022) {
  url <- paste0("https://www.transfermarkt.com/premier-league/einnahmenausgaben/wettbewerb/GB1/plus/0?ids=a&sa=&saison_id=", year, "&saison_id_bis=", year, "&nat=&pos=&altersklasse=&w_s=&leihe=&intern=0")

  # Scrape and format table from the first page
  data <- read_and_format_table(url)
  
  # Add a column indicating the year
  data <- data %>% 
    mutate(Year = year)
  
  # Append the data to the final dataset
  final_data <- bind_rows(final_data, data)
}

# View the final dataset
View(final_data)

# Save as csv 
write_csv(final_data, "data/pl_net_spend_03_22.csv")
