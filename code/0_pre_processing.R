# load data ---------------------------------------------------------------
# google mobility report
mobility <- read_csv("input/Global_Mobility_Report.csv") %>% 
    filter(country_region == "Brazil") %>% 
    mutate(sub_region_1    = str_remove_all(sub_region_1, "State of "),
           iso_3166_2_code = str_remove_all(iso_3166_2_code, "BR-")) %>%
    mutate(across(contains("baseline"), ~ 1 + ./100),
           iso_3166_2_code = ifelse(is.na(sub_region_1), "BR", iso_3166_2_code)) %>% 
    filter(!is.na(iso_3166_2_code))

# energy data (não tem RR)
energy <- read_csv2("input/energia_data.csv") %>% 
    rename(ramo = `Ramo de atividade`) %>% 
    mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws(),
           ramo = iconv(ramo, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws()) %>% 
    clean_names() %>% 
    mutate(data = as.Date(data, "%d/%m/%Y")) %>% 
    arrange(data, classe, ramo) %>% 
    group_by(data, estado, ramo) %>% 
    summarise(consumo = sum(consumo_m_wm))

# COVID data (https://covid.saude.gov.br/)
covid <- read_csv2("input/PAINEL_COVIDBR.csv") %>% 
    filter(!is.na(estado)) %>% 
    mutate(date = as.Date(data, "%d/%m/%Y"))

# feriados
feriados <- read_csv2("input/feriados.csv") %>% 
    mutate(data = as.Date(data, "%d/%m/%Y"))

# PET phases
phases <- read_csv2("input/PET_phases.csv") %>% 
    filter(!is.na(State)) %>% 
    mutate(State = str_remove_all(State, "State of ") %>% 
                   iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
                   trimws() %>% 
                   str_replace_all("Federal District", "Distrito Federal")
          )


# divide into brazil and states -------------------------------------------
brasil <- covid %>% 
    group_by(date) %>% 
    summarise(last_available_deaths    = sum(obitosAcumulado),
              last_available_confirmed = sum(casosAcumulado)) %>% 
    full_join(mobility %>% filter(is.na(sub_region_1))) %>% 
    select(-starts_with("country_region"), -starts_with("sub_region")) %>% 
    arrange(date) %>% 
    left_join(phases %>% filter(State == "Brasil"), by = c("date" = "Date")) %>% 
    select(-State)

uf_estado <- owdbr::uflist()

estados <- covid %>% 
    group_by(estado, date) %>%
    summarise(last_available_deaths    = sum(obitosAcumulado),
              last_available_confirmed = sum(casosAcumulado)) %>%
    rename(state = estado) %>% 
    full_join(mobility %>% filter(!is.na(sub_region_1)),
              by = c("state" = "iso_3166_2_code", "date" = "date")) %>%
    select(-starts_with("country_region"), -starts_with("sub_region"),  -starts_with("census_fips")) %>% 
    left_join(uf_estado, by = c("state" = "UF")) %>%
    left_join(phases, by = c("State" = "State", "date" = "Date")) %>%
    rename(estado = State) %>% 
    arrange(regiao, state, date)


# run functions in data ---------------------------------------------------
brasil <- brasil %>% 
    mutate(across(matches("last_available"), ~replace(., is.na(.), 0))) %>% 
    mutate(half_cum_cases  = last_available_confirmed / 2,
           half_cum_deaths = last_available_deaths / 2)

doubl_days <- map2(brasil$half_cum_cases, brasil$date, 
                   ~ doubling_days(.x, .y, brasil)) %>% 
    unlist()

brasil_df <- brasil %>% 
    mutate(doubl_days = doubl_days) %>% 
    create_columns() %>% 
    filter(!is.na(mobility))


# states ------------------------------------------------------------------
estados <- estados %>%
    mutate(across(matches("last_available"), ~replace(., is.na(.), 0))) %>% 
    mutate(half_cum_cases  = last_available_confirmed / 2,
           half_cum_deaths = last_available_deaths / 2) %>% 
    group_by(state)

# split by state
list_estados <- group_split(estados)

bases_estados_df <- map(list_estados, doubl_dates_st) %>% 
    bind_rows() %>% 
    group_by(state) %>% 
    arrange(date) %>% 
    fill(doubl_days) %>%
    ungroup() %>%
    arrange(state, date, regiao) %>% 
    create_columns() %>% 
    filter(!is.na(mobility))


# consumo total -----------------------------------------------------------
pre_covid <- energy %>% 
    pre_covid_df_fun()

total_energy_df <- energy %>% 
    energy_df_fun(pre_covid) %>% 
    select(1:4, regiao, 19:28, PET_Phase)

# por ramo
total_energy_ramo <- energy %>% 
    energy_df_ramo_fun(pre_covid)


# consumo acl -------------------------------------------------------------
acl_pre_covid <- energy %>% 
    filter(ramo != "ACR") %>% 
    filter(estado != "Amapa") %>%   # só tem dados de distribuidor
    pre_covid_df_fun() %>% 
    filter(!is.na(pred))

acl_energy_df <- energy %>% 
    filter(ramo != "ACR") %>% 
    energy_df_fun(acl_pre_covid) %>% 
    filter(!is.na(pred)) %>% 
    select(1:4, regiao, 19:28, PET_Phase) %>% 
    rename(consumo_diario_acl  = consumo_diario ,
           pred_acl            = pred           ,
           ma_consumo_acl      = ma_consumo     ,
           ma_pred_acl         = ma_pred        ,
           dif_baseline_acl    = dif_baseline   ,
           ma_dif_baseline_acl = ma_dif_baseline)


# aggregate all country ---------------------------------------------------
brasil_energy <- total_energy_df %>% 
    group_by(data) %>% 
    summarise(consumo_diario = sum(consumo_diario),
              pred           = sum(pred)) %>% 
    mutate(ma_consumo = rollmean(consumo_diario, 7, na.pad = T, align = "right"),
           ma_pred = rollmean(pred, 7, na.pad = T, align = "right"),
           dif_baseline = (ma_consumo - ma_pred) / ma_pred,
           dif_consumo  = ma_consumo - ma_pred,
           ma_dif_baseline = rollmean(dif_baseline, 7, na.pad = T, align = "right")) 
    

brasil_energy_acl <- acl_energy_df %>% 
    group_by(data) %>% 
    summarise(consumo_diario_acl = sum(consumo_diario_acl),
              pred_acl           = sum(pred_acl)) %>% 
    mutate(ma_consumo_acl = rollmean(consumo_diario_acl, 7, na.pad = T, align = "right"),
           ma_pred_acl = rollmean(pred_acl, 7, na.pad = T, align = "right"),
           dif_baseline_acl = (ma_consumo_acl - ma_pred_acl) / ma_pred_acl,
           dif_consumo_acl  = ma_consumo_acl - ma_pred_acl,
           ma_dif_baseline_acl = rollmean(dif_baseline_acl, 7, na.pad = T, align = "right")) 


# merge common data -------------------------------------------------------
brasil <- brasil_energy %>% 
    left_join(brasil_energy_acl) %>% 
    full_join(brasil_df, by = c("data" = "date")) %>% 
    select(1:15, 29:33, PET_Phase)
    
estados <- total_energy_df %>% 
    left_join(acl_energy_df)
    

# export data -------------------------------------------------------------
write_csv(brasil, "tmp/brasil.csv")
write_csv(estados, "tmp/estados.csv")

