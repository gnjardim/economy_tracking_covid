# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(zoo)


# load data ---------------------------------------------------------------
energy_ramo <- read_csv2("input/energia_data.csv") %>% 
    rename(ramo = `Ramo de atividade`) %>% 
    mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws(),
           ramo = iconv(ramo, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws()) %>% 
    clean_names() %>% 
    mutate(data   = as.Date(data, "%d/%m/%Y")) %>% 
    mutate(semana = week(data),
           ano    = year(data)) %>% 
    arrange(estado, data, ramo) %>% 
    group_by(estado, ramo, semana, ano) %>% 
    summarise(consumo = sum(consumo_m_wh),
              data    = min(data)) %>% 
    mutate(pos_covid  = ifelse(data >= as.Date("2020-02-25"), 1, 0)) %>% 
    filter(data >= as.Date("2019-01-01"))


# compare with previous ---------------------------------------------------
without_acr <- energy_ramo %>% 
    filter(ramo != "ACR") %>% 
    group_by(estado, semana, ano) %>% 
    summarise(consumo = sum(consumo),
              data    = min(data)) %>% 
    select(-data) %>% 
    pivot_wider(names_from = ano, values_from = consumo) %>% 
    mutate(change_from = (`2020` - `2019`) / `2019`) %>% 
    filter(semana >= 9)

with_acr <- energy_ramo %>% 
    group_by(estado, semana, ano) %>% 
    summarise(consumo = sum(consumo),
              data    = min(data)) %>% 
    select(-data) %>% 
    pivot_wider(names_from = ano, values_from = consumo) %>% 
    mutate(change_from = (`2020` - `2019`) / `2019`) %>% 
    filter(semana >= 9)


# plots -------------------------------------------------------------------
with_acr %>% 
    ggplot(aes(x = semana, y = change_from)) +
    geom_line(color = "steelblue") +
    geom_hline(yintercept = 0, color = "red") +
    facet_wrap(~estado)

without_acr %>% 
    ggplot(aes(x = semana, y = change_from)) +
    geom_line(color = "steelblue") +
    geom_hline(yintercept = 0, color = "red") +
    facet_wrap(~estado)



