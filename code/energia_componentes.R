# packages ----------------------------------------------------------------
library(tidyverse)


# file --------------------------------------------------------------------
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
           ramo == "TELECOMUNICACOES"       ~ "SERVICOS")
           )

energy_ramo %>% filter(is.na(Componente)) %>% select(ramo) %>% unique() %>% View()

                       
                       