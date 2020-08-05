# packages ----------------------------------------------------------------
library(tidyverse)


# load data ---------------------------------------------------------------
pib_estadual <- read_csv2("input/pib_estadual.csv") %>% 
    rename(estado = 1, agropecuaria = 2, industria = 3, 
           servicos = 4, administracao = 5) %>% 
    select(-administracao) %>% 
    pivot_longer(cols = c(agropecuaria, industria, servicos),
                 names_to = "Componente") %>% 
    mutate(estado = iconv(estado, from = "UTF-8", to = "ASCII//TRANSLIT")) %>% 
    left_join(owdbr::uflist(), by = c("estado" = "State"))
    

# plot --------------------------------------------------------------------
comp_pib <- ggplot(pib_estadual, aes(fill = Componente, y = value, x = estado)) + 
    geom_bar(position = "dodge", stat = "identity") +
    facet_wrap(~ regiao, scales = "free_x") +
    xlab("Estado") + ylab("Participação do Valor Adicionado (%)") +
    scale_x_discrete(guide = guide_axis(n.dodge = 3))

