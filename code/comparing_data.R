#' @author Maria 
#' @description Comparing data


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(readr)
library(readxl)

# source ------------------------------------------------------------------
bases_estados_df <- read_csv("meta/bases_estados_df.csv")
PET_Brazil_20200705 <- read_excel("meta/PET-Brazil_20200705.xlsx")


# rio de janeiro ----------------------------------------------------------
bases_estados_rj <- bases_estados_df %>% 
    dplyr:: filter(state == 'RJ')%>%
    dplyr:: select(retail_and_recreation_percent_change_from_baseline,
                   grocery_and_pharmacy_percent_change_from_baseline,
                   workplaces_percent_change_from_baseline,
                   mobility,
                   activity,
                   smth_mobility,
                   date)%>%
    dplyr:: arrange(date)
    

PET_Brazil_rj <- PET_Brazil_20200705 %>% 
    dplyr:: filter (ISO == 'RJ')%>%
    dplyr:: arrange(Date)


comparacao <- full_join(PET_Brazil_rj,bases_estados_rj, by = c('Date'= 'date'))%>% 
    dplyr::select(Date,Mobility, mobility, retail_and_recreation_percent_change_from_baseline,
                  grocery_and_pharmacy_percent_change_from_baseline,
                  workplaces_percent_change_from_baseline)

