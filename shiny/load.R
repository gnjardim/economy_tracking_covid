# options -----------------------------------------------------------------
theme_set(theme_bw())


# functions ---------------------------------------------------------------
source("../code/_plot_functions.R", encoding = "UTF-8")


# load data ---------------------------------------------------------------
brasil  <- read_csv("data/brasil.csv")
estados <- read_csv("data/estados.csv")

choices <- c("Brasil", unique(estados %>% pull(estado))) %>% 
    str_replace("Amapa", "Amapá") %>% 
    str_replace("Ceara", "Ceará") %>% 
    str_replace("Espirito Santo", "Espírito Santo") %>% 
    str_replace("Goias", "Goiás") %>% 
    str_replace("Maranhao", "Maranhão") %>% 
    str_replace("Para", "Pará") %>% 
    str_replace("Paraiba", "Paraíba") %>% 
    str_replace("Parana", "Paraná") %>% 
    str_replace("Piaui", "Piauí") %>% 
    str_replace("Rondonia", "Rondônia") %>% 
    str_replace("Sao Paulo", "São Paulo")
