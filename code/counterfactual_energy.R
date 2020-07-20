#' Counterfactual for energy data

# clean environment -------------------------------------------------------
rm(list = ls())


# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)


# load data ---------------------------------------------------------------
energy <- read_csv2("input/energia_data.csv") %>% 
    rename(ramo = `Ramo de atividade`) %>% 
    mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws()) %>% 
    clean_names() %>% 
    mutate(data = as.Date(data, "%d/%m/%Y")) %>% 
    arrange(data, classe, ramo) %>% 
    group_by(data, estado, classe) %>% 
    summarise(consumo = sum(consumo_m_wm))


# functions ---------------------------------------------------------------
bsm_model_fit <- function(df_ts) {
    
    in_sample <- df_ts %>% 
        pull(consumo_mensal) %>% 
        ts(start = c(2018, 7), frequency = 12) %>% 
        window(end = c(2020, 2))
    
    bsm <- in_sample %>% 
        StructTS(type = c("BSM"))
    
}

counterfact_df <- function(df) {
    
    # agregar dados por mês e dividir df por estado
    list_states_df <- df %>% 
        group_by(data, estado) %>% 
        summarise(consumo_diario = sum(consumo)) %>% 
        mutate(pre_covid = ifelse(data <= as.Date("2020-02-25"), 1, 0) %>% factor(),
               mes       = month(data),
               ano       = year(data)
        ) %>% 
        group_by(estado, ano, mes) %>% 
        summarise(consumo_mensal = sum(consumo_diario)) %>% 
        group_by(estado) %>% 
        group_split()
    
    # fitar modelo bsm para cada estado na lista
    bsm_model <- map(list_states_df, bsm_model_fit)
    
}
