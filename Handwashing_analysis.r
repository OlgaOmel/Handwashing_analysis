library(tidyverse)

yearly <- read_csv('yearly_deaths_by_clinic.csv')
yearly


yearly <- yearly %>% 
    mutate(proportion_deaths = deaths/births)
yearly

ggplot(yearly, 
       aes(x = year, y = proportion_deaths, color = clinic)) +
       geom_line()

monthly <- read_csv('monthly_deaths.csv') %>%
mutate(proportion_deaths = deaths/births)
head(monthly)

ggplot(monthly, aes(x = date, y = proportion_deaths)) +
    geom_line() +
    labs(x = "Year", y = "Proportion Deaths")

handwashing_start = as.Date('1847-06-01')
monthly <- monthly %>%
    mutate(handwashing_started = date >= handwashing_start)

ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started)) +
    geom_line()

monthly_summary <- monthly %>%
    group_by(handwashing_started) %>%
    summarize(mean_proportion_deaths = mean(proportion_deaths))
monthly_summary

test_result <- t.test(proportion_deaths ~ handwashing_started, data = monthly)
test_result
