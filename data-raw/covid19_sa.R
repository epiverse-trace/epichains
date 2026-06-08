## code to prepare `covid19_sa` dataset
# Data are sourced from the COVID-19 Data Repository for South Africa,
# created and maintained by the Data Science for Social Impact (DSFSI)
# research group, led by Dr. Vukosi Marivate at the University of Pretoria,
# South Africa. The repository is available at
# https://github.com/dsfsi/covid19za and is licensed under MIT License.
# The URL below is pinned to a specific commit to ensure reproducibility.
# The raw data are provided as a linelist of individual confirmed cases.
# Here, the date column is extracted, parsed, and the first 16 days of the
# outbreak are retained (5 March 2020 to 20 March 2020). Cases are then
# aggregated by day to create a daily incidence time series.
#
# Citation:
# Marivate, V. and Combrink, H. M. (2020). Use of available data to inform
# the COVID-19 outbreak in South Africa: a case study.
# arXiv preprint arXiv:2004.04813. \doi{10.48550/arXiv.2004.04813}

library(dplyr)
library(lubridate)
library(usethis)

# Link to data (pinned to a specific commit for reproducibility)
data_url <- "https://raw.githubusercontent.com/dsfsi/covid19za/1943f5e0d80fa296d9171ced473eebd3f2cde109/data/covid19za_timeline_confirmed.csv" # nolint: line_length_linter.

# Read the data in using the url
covid19_sa <- read.csv(data_url)

# Subset to the date column, convert to Date class, retain only the first
# 16 days of the outbreak, and aggregate cases per day
covid19_sa <- covid19_sa %>%
  select(date) %>%
  mutate(date = lubridate::dmy(date)) %>%
  filter(date <= min(date) + lubridate::days(15)) %>%
  count(date, name = "cases")

use_data(covid19_sa, overwrite = TRUE)
