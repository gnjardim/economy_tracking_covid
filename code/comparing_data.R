#' @author Maria 
#' @description Comparing data


# packages ----------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(readr)
library(readxl)
library(writexl)

# source ------------------------------------------------------------------
bases_estados_df <- read_csv("meta/bases_estados_df.csv")
PET_Brazil_20200705 <- read_excel("meta/PET-Brazil_20200705.xlsx")


# rio de janeiro ----------------------------------------------------------
bases_estados_rj <- bases_estados_df %>% 
    dplyr::select(state, date,
                   retail_and_recreation_percent_change_from_baseline,
                   grocery_and_pharmacy_percent_change_from_baseline,
                   workplaces_percent_change_from_baseline,
                   mobility,
                   activity,
                   moving_avg_mobility = smth_mobility,
                   date) %>%
    dplyr::arrange(state, date)
    

PET_Brazil_rj <- PET_Brazil_20200705 %>% 
    dplyr::arrange(Date)


comparacao <- full_join(PET_Brazil_rj,bases_estados_rj, 
                        by = c('Date'= 'date', "ISO" = "state")) %>% 
    dplyr::select(ISO, Date, PET_Mobility = Mobility, mobility, moving_avg_mobility, 
                  retail_and_recreation_percent_change_from_baseline,
                  grocery_and_pharmacy_percent_change_from_baseline,
                  workplaces_percent_change_from_baseline) %>% 
    dplyr::arrange(ISO, Date) %>% 
    filter(!is.na(PET_Mobility))


# export ------------------------------------------------------------------
write_xlsx(comparacao, "Comparacao.xlsx")
    

