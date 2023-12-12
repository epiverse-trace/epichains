## code to prepare `covid_sa` dataset

library(dplyr)
library(lubridate)
library(usethis)

# Link to data
data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/1943f5e0d80fa296d9171ced473eebd3f2cde109/data/covid19za_timeline_confirmed.csv" # nolint: line_length_linter.

# Read the data in using the url
covid19_sa <- read.csv(data_url)

# Clean and subset the data we need
covid19_sa <- covid19_sa %>%
  select(date) %>%
  mutate(date = lubridate::dmy(date)) %>%
  filter(date <= min(date) + lubridate::days(15)) %>%
  count(date)

use_data(covid19_sa, overwrite = TRUE)
