# Web Scrape Club Transfers from Transfermarkt
library(rvest)
library(tidyverse)

# Extract Data Function ------------
read_and_format_table <- function(url) {
  html <- read_html(url)
  table <- html %>% html_element("#yw1") %>% html_table(header = FALSE)
  new_table <- table %>% select(-X2, -X10) %>% slice(-1)
  names(new_table) <- c("#", "Club", "Competition", "Expenditure", "Arrivals", "Income", "Departures", "Balance")
  return(as_tibble(new_table))
}

# Initialize the final dataset
final_data <- tibble()

# Scrape data from 2012 to 2022/2023
for (year in 2012:2022) {
  url1 <- paste0("https://www.transfermarkt.com/transfers/einnahmenausgaben/statistik/a/ids/a/sa//saison_id/", year, "/saison_id_bis/", year, "/land_id/0/nat/0/kontinent_id/0/pos//w_s//intern/0/plus/1")
  url2 <- paste0("https://www.transfermarkt.com/transfers/einnahmenausgaben/statistik/a/ids/a/sa//saison_id/", year, "/saison_id_bis/", year, "/land_id/0/nat/0/kontinent_id/0/pos//w_s//intern/0/plus/1/page/2")
  
  # Scrape and format table from the first page
  data1 <- read_and_format_table(url1)
  
  # Scrape and format table from the second page
  data2 <- read_and_format_table(url2)
  
  # Combine data from both pages
  data <- bind_rows(data1, data2)
  
  # Add a column indicating the year
  data <- data %>% 
    mutate(Year = year)
  
  # Append the data to the final dataset
  final_data <- bind_rows(final_data, data)
}

# View the final dataset
View(final_data)

# Save as csv 
write_csv(final_data, "data/clubs_spending_12_23.csv")