# calculate empirical relation --------------------------------------------
# The empirical relationship between mobility and GDP growth rate across economies in the first quarter of 2020 is used to convert a country’s economic activity based on people’s mobility levels.

relation_eq <- function(mobility) {
    activity <- 0.3886 * mobility + 0.61
}


# doubling days of covid --------------------------------------------------
# More precisely, it is days between the current date (the date at which the measurement is taken) and the most recent prior date at which the confirmed cases were half the current level
nearest_n <- function(x, num) {
    x[which.min(abs(x - num))]
}

aux_doubl <- function(df, current_date, doubl_dates) {
    cases_past <- df %>% 
        filter(date == doubl_dates) %>% 
        pull(last_available_confirmed)
    
    cases_current <- df %>% 
        filter(date == current_date) %>% 
        pull(last_available_confirmed)
    
    dist_date  <- as.numeric(current_date - doubl_dates)
    diff_cases <- cases_current / cases_past
    
    doubling <- dist_date * log(2) / log(diff_cases)
    
    # remove NaN
    doubling <- ifelse(!is.nan(doubling), doubling, NA)
}

doubling_days <- function(half_num, current_date, df) {
    
    # get only previous dates
    half_cases <- df %>% 
        filter(date < current_date)
    
    # get nearest number of cases
    n_half <- nearest_n(half_cases$last_available_confirmed, half_num)
    
    # find last possible date
    last_date <- half_cases %>% 
        filter(last_available_confirmed == n_half) %>% 
        slice(n()) %>% 
        pull(date)
    
    if(length(last_date) != 0) {
        doubl_days <- aux_doubl(df, current_date, last_date)
    } else {
        doubl_days <- NA
    }
    
    return(doubl_days)
}


doubl_dates_st <- function(df) {
    dates <- map2(df$half_cum_cases, df$date, ~ doubling_days(.x, .y, df)) %>%
        unlist()
    
    df_dt <- df %>% 
        mutate(doubl_days = dates) %>% 
        mutate(doubl_days = ifelse(half_cum_cases == 0, NA, doubl_days))
    
    return(df_dt)
}

create_columns <- function(df) {
    
    post_df <- df %>% 
        mutate(mobility    = ((retail_and_recreation_percent_change_from_baseline + 
                                   grocery_and_pharmacy_percent_change_from_baseline) / 2 +  
                                  workplaces_percent_change_from_baseline) / 2,
               smth_date   = rollmean(doubl_days, 7, na.pad = T, align = "right"),
               smth_mob    = rollmean(mobility, 7, na.pad = T, align = "right"),
               activity    = relation_eq(smth_mob)
        )
    
}


# energy ------------------------------------------------------------------
pre_covid_df_fun <- function(df) {
    
    list_df <- df %>% 
        left_join(feriados) %>% 
        group_by(data, estado, feriado) %>% 
        summarise(consumo_diario = sum(consumo)) %>% 
        mutate(pre_covid  = ifelse(data <= as.Date("2020-02-25"), 1, 0),
               mes        = month(data) %>% factor(),
               ano        = year(data),
               dia_semana = wday(data),
               trend      = as.numeric(data) - as.numeric(as.Date("2018-08-01")),
               trend2     = trend^2,
               d_feriado  = ifelse(!is.na(feriado), feriado, 0) %>% factor()
        ) %>% 
        group_by(estado) %>% 
        group_split()
    
    # regressao nas dummies
    mod <- map(list_df,
               ~ lm(consumo_diario ~ mes + factor(ano) + d_feriado +
                        factor(dia_semana) + trend + trend2, 
                    data = .x %>% filter(pre_covid == 1)))
    
    r2 <- tibble(estado = unique(df$estado) %>% sort(),
                 r2 = map(mod, summary) %>% map_dbl("r.squared"))
    
    
    # adicionar coluna de predito
    pred_df <- map2(.x = list_df, .y = mod, 
                    ~ add_predictions(.x, .y)) %>% 
        bind_rows()
    
    return(pred_df)
    
}

energy_df_fun <- function(df, df_pre_cov) {
    
    post_df <- df %>% 
        group_by(data, estado) %>% 
        summarise(consumo_diario = sum(consumo)) %>% 
        left_join(df_pre_cov) %>% 
        full_join(bases_estados_df, by = c("data" = "date", "estado" = "estado")) %>%
        filter(estado != "Roraima") %>% 
        group_by(estado) %>% 
        arrange(estado, data) %>% 
        mutate(ma_consumo = rollmean(consumo_diario, 7, na.pad = T, align = "right"),
               ma_pred = rollmean(pred, 7, na.pad = T, align = "right"),
               dif_baseline = (ma_consumo - ma_pred) / ma_pred,
               ma_dif_baseline = rollmean(dif_baseline, 7, na.pad = T, align = "right")) %>% 
        fill(regiao) %>% 
        fill(regiao, .direction = "up") 
    
}