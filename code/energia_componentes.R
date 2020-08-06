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
    mutate(data       = as.Date(data, "%d/%m/%Y")) %>% 
    mutate(Componente = case_when(
        ramo == "QUIMICOS"               ~ "INDUSTRIA",
        ramo == "MANUFATURADOS DIVERSOS" ~ "INDUSTRIA",
        ramo == 'TEXTEIS'                ~ "INDUSTRIA",
        str_detect(ramo, "METALURGIA")   ~ "INDUSTRIA",
        ramo == "SANEAMENTO"             ~ "PUBLICO",
        ramo == "SERVICOS"               ~ "SERVICOS",
        ramo == "COMERCIO"               ~ "SERVICOS",
        ramo == "TRANSPORTE"             ~ "SERVICOS",
        ramo == "TELECOMUNICACOES"       ~ "SERVICOS"),
        semana = week(data),
        ano    = year(data)) %>% 
    arrange(estado, data, ramo) %>% 
    group_by(estado, ramo, semana, ano) %>% 
    summarise(consumo = sum(consumo_m_wh),
              data    = min(data)) %>% 
    mutate(pos_covid  = ifelse(data >= as.Date("2020-02-25"), 1, 0))

#energy_ramo %>% filter(is.na(Componente)) %>% select(ramo) %>% unique() %>% View()


# plots -------------------------------------------------------------------
# somente ACR
acr <- energy_ramo %>% filter(ramo == "ACR") %>% 
    ggplot(aes(x = data, y = consumo)) +
    geom_line(aes(color = ramo)) +
    geom_vline(xintercept = as.Date("2020-02-25"), color = "red") +
    facet_wrap(~estado, ncol = 3, scales = "free_y") +
    xlab("Data") + ylab("Consumo Semanal")

# sem ACR
no_acr <- energy_ramo %>% filter(ramo != "ACR") %>% 
    ggplot(aes(x = data, y = consumo)) +
    geom_line(aes(color = ramo)) +
    geom_vline(xintercept = as.Date("2020-02-25"), color = "red") +
    facet_wrap(~estado, ncol = 3, scales = "free_y") +
    xlab("Data") + ylab("Consumo Semanal")


# regressions -------------------------------------------------------------
normalize <- function(x){
    return((x - min(x)) / (max(x) - min(x)))
}

by_ramo <- energy_ramo %>% 
    group_by(ramo, estado) %>% 
    mutate(consumo = normalize(consumo)) %>% 
    group_by(ramo) %>% 
    nest() %>% 
    mutate(models = map(data, ~lm(consumo ~ pos_covid, data = .x)),
           coef   = map_dbl(models, ~coef(.x)[2])) %>% 
    select(-models, -data) %>% 
    arrange(coef)


# principal ramo ----------------------------------------------------------
main_ramo <- energy_ramo %>% 
    filter(ramo != "ACR", pos_covid == 0) %>% 
    group_by(estado, semana) %>% 
    filter(consumo == max(consumo)) %>% 
    group_by(estado) %>% 
    count(estado, ramo) %>%
    #slice(which.max(n)) %>% 
    left_join(by_ramo) %>% 
    arrange(estado, desc(n))


# export ------------------------------------------------------------------
h <- 24
w <- 18

ggsave("onlyACR.pdf", acr,
       height = h, width = w)

ggsave("noACR.pdf", no_acr,
       height = h, width = w)

