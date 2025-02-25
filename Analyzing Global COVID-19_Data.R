# Load necessary packages
install.packages("tidyverse")
library(tidyverse)

# Read the dataset
covid_data <- read.csv("D:\\R projects\\Analyzing Global COVID-19 Data\\owid-covid-data.csv")

# View dataset structure
str(covid_data)

# View first few rows
head(covid_data)

# Select relevant columns
covid_clean <- covid_data %>% 
  select(location, date, total_cases, total_deaths, new_cases, new_deaths, population)

# Convert date to Date format
covid_clean$date <- as.Date(covid_clean$date)

# Remove rows with missing values
covid_clean <- na.omit(covid_clean)

# View summary statistics
summary(covid_clean)

covid_clean %>%
  group_by(location) %>%
  summarise(Total_Cases = max(total_cases, na.rm = TRUE)) %>%
  arrange(desc(Total_Cases)) %>%
  head(10)

global_cases <- covid_clean %>%
  group_by(date) %>%
  summarise(Total_Cases = sum(total_cases, na.rm = TRUE))

ggplot(global_cases, aes(x = date, y = Total_Cases)) +
  geom_line(color = "blue") +
  labs(title = "Global COVID-19 Cases Over Time", x = "Date", y = "Total Cases")

top_10_deaths <- covid_clean %>%
  group_by(location) %>%
  summarise(Total_Deaths = max(total_deaths, na.rm = TRUE)) %>%
  arrange(desc(Total_Deaths)) %>%
  head(10)

ggplot(top_10_deaths, aes(x = reorder(location, Total_Deaths), y = Total_Deaths)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Countries by COVID-19 Deaths", x = "Country", y = "Total Deaths")

covid_clean %>%
  filter(location %in% c("United States", "India", "Brazil", "Russia", "UK")) %>%
  ggplot(aes(x = total_cases/population * 1e6, y = total_deaths/population * 1e6, color = location)) +
  geom_point() +
  labs(title = "Cases vs. Deaths Per Million", x = "Cases per Million", y = "Deaths per Million")
