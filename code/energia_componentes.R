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
              data    = min(data))

energy_ramo %>% filter(is.na(Componente)) %>% select(ramo) %>% unique() %>% View()

                       
# plots -------------------------------------------------------------------
# somente ACR
acr <- energy_ramo %>% filter(ramo == "ACR") %>% 
    ggplot(aes(x = data, y = consumo)) +
    geom_line(aes(color = ramo)) +
    facet_wrap(~estado, ncol = 3, scales = "free_y")

# sem ACR
no_acr <- energy_ramo %>% filter(ramo != "ACR") %>% 
    ggplot(aes(x = data, y = consumo)) +
    geom_line(aes(color = ramo)) +
    facet_wrap(~estado, ncol = 3, scales = "free_y")


# export ------------------------------------------------------------------
h <- 24
w <- 18

ggsave("onlyACR.pdf", acr,
       height = h, width = w)

ggsave("noACR.pdf", no_acr,
       height = h, width = w)

