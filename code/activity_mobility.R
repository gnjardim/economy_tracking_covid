#' Plot economic activity index against COVID data

# clean environment -------------------------------------------------------
rm(list = ls())


# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)


# load data ---------------------------------------------------------------
# google mobility report
mobility <- read_csv("input/Global_Mobility_Report.csv") %>% 
    filter(country_region == "Brazil") %>% 
    mutate(sub_region_1    = str_remove_all(sub_region_1, "State of "),
           iso_3166_2_code = str_remove_all(iso_3166_2_code, "BR-"))

# brasil.io (caso_full)
covid <- read_csv("input/caso_full.csv") %>% 
    filter(place_type == "state", is_repeated == FALSE)


# calculate empirical relation --------------------------------------------
#' The empirical relationship between mobility and GDP growth rate across 
#' economies in the first quarter of 2020 is used to convert a country’s
#' economic activity based on people’s mobility levels.

brazil_growth_q1 <- -1.5

brazil_mobility_q1 <- mobility %>% 
    filter(month(date) <= 3 & is.na(sub_region_1))


# doubling days of covid --------------------------------------------------
# https://en.wikipedia.org/wiki/Doubling_time

