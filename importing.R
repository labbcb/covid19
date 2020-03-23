library(tidyverse)
library(rvest)
library(lubridate)
library(gganimate)

## Get Population
page = "https://www.worldometers.info/world-population/population-by-country/"
tmp = read_html(page) %>%
  html_node("table") %>%
  html_table() %>%
  as_tibble()
population = tmp[, 2:3]
names(population) = c("Country", "Population")
rm(tmp)

population = population %>%
  mutate(Country = case_when(
    Country == "Czech Republic (Czechia)" ~ "Czechia",
    Country == "Côte d'Ivoire" ~ "Cote d'Ivoire",
    TRUE ~ as.character(Country)
))


population = population %>%
  mutate(Population=as.integer(str_replace_all(Population, ",", "")))

path = "/Volumes/Disk2/github/COVID-19/csse_covid_19_data/csse_covid_19_time_series"

confirmed = file.path(path, "time_series_19-covid-Confirmed.csv")

conf = read_csv(confirmed) %>%
  select(-`Province/State`, -Lat, -Long) %>%
  rename(Country=`Country/Region`) %>%
  gather(Date, Cases, -Country) %>%
  drop_na() %>% 
  mutate(Date = mdy(Date))

## TODO: fix country names in 'conf'; check problems with left_join
##       Country %in% c("Cruise Ship", "Kosovo", "Saint Vicent") are not being imported

conf = conf %>% 
  mutate(Country = case_when(
    Country == "US" ~ "United States",
    Country == "Korea, South" ~ "South Korea",
    Country == "Taiwan*" ~ "Taiwan",
    Country == "Congo (Kinshasa)" ~ "Congo",
    Country == "Congo (Brazzaville)" ~ "Congo",
    Country == "Bahamas, The" ~ "Bahamas",
    Country == "Gambia, The" ~ "Gambia",
    TRUE ~ as.character(Country)
  ))

conf = conf %>% 
  group_by(Country, Date) %>% 
  summarise(Cases=sum(Cases)) %>% 
  ungroup()


conf %>% left_join(population) %>%
  arrange(-Cases) %>%
  filter(is.na(Population)) %>% 
  select(Country, Cases) %>% distinct() %>% as.data.frame()


### Continue here

my_countries = c("China", "Brazil", "United Kingdom",
                 "United States", "France", "South Korea")
conf %>%
  inner_join(population) %>% 
  filter(Country %in% my_countries) %>% 
  ggplot(aes(Date, Cases, colour=Country)) +
  geom_line() + geom_line() +
  scale_y_log10() +
  transition_time(Date) + 
  ggtitle("COVID-19 \t Date : {frame_time}") + 
  theme_bw()

