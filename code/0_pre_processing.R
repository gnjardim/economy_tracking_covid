# load data ---------------------------------------------------------------
# google mobility report
mobility <- read_csv("input/Global_Mobility_Report.csv") %>% 
    filter(country_region == "Brazil") %>% 
    mutate(sub_region_1    = str_remove_all(sub_region_1, "State of "),
           iso_3166_2_code = str_remove_all(iso_3166_2_code, "BR-")) %>%
    mutate(across(contains("baseline"), ~ 1 + ./100))

# energy data (não tem RR)
energy <- read_csv2("input/energia_data.csv") %>% 
    rename(ramo = `Ramo de atividade`) %>% 
    mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws(),
           ramo = iconv(ramo, from = "UTF-8", to = "ASCII//TRANSLIT") %>% 
               trimws()) %>% 
    clean_names() %>% 
    mutate(data        = as.Date(data, "%d/%m/%Y")) %>% 
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


# divide into brazil and states -------------------------------------------
brasil <- covid %>% 
    group_by(date) %>% 
    summarise(last_available_deaths    = sum(obitosAcumulado),
              last_available_confirmed = sum(casosAcumulado)) %>% 
    full_join(mobility %>% filter(is.na(sub_region_1))) %>% 
    select(-starts_with("country_region"), -starts_with("sub_region")) %>% 
    arrange(date)

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
    energy_df_fun(pre_covid)


# consumo acl -------------------------------------------------------------
acl_pre_covid <- energy %>% 
    filter(ramo != "ACR") %>% 
    filter(estado != "Amapa") %>%   # só tem dados de distribuidor
    pre_covid_df_fun() %>% 
    filter(!is.na(pred))

acl_energy_df <- energy %>% 
    filter(ramo != "ACR") %>% 
    energy_df_fun(acl_pre_covid) %>% 
    filter(!is.na(pred))
