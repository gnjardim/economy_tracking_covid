plot_activity_mobility <- function(df) {
    
    p <- ggplot(df, aes(x = smth_date, y = activity)) +
        geom_path(color = "steelblue", size = 0.8) +
        #geom_vline(xintercept = 19, linetype = "dotted", size = 0.8) +
        #geom_point(data = brasil_mob_b, shape = 22, fill = "white") +
        geom_hline(yintercept = 1, color = "red") +
        xlim(0, 40) + 
        facet_wrap(~ state) +
        ylab("Activity Index") + xlab("Doubling days of confirmed cases")
    
    return(p)
}


plot_regiao <- function(reg) {
    
    base_regiao <- bases_estados_df %>% 
        filter(regiao == reg)
    
    return(plot_activity_mobility(base_regiao)) 
}


# energia -----------------------------------------------------------------
plot_activity_energy <- function(df) {
    
    p <- df %>% 
        ggplot(aes(x = smth_date, y = ma_dif_baseline)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 0, color = "red") +
        xlim(0, 40) + 
        facet_wrap(~estado, scales = "free") +
        ylab("Change from predicted in energy consumption") + xlab("Doubling days of confirmed cases")
    
    return(p)
}

plot_fit_energy <- function(df) {
    
    p <- df %>% 
        ggplot(aes(x = data)) +
        geom_line(aes(y = ma_consumo), color = "black", size = 0.8) +
        geom_line(aes(y = pred), color = "steelblue", size = 0.8) +
        facet_wrap(~estado, scales = "free") +
        ylab("7-days Moving Average of Energy Consumption") + 
        xlab("Time") +
        scale_color_manual(values = c("black", "steelblue"), 
                           labels = c("Actual", "Fitted"), name = "")
    
    return(p)
}


plot_regiao_energia <- function(df, reg, fplot) {
    
    base_regiao <- df %>% 
        filter(regiao == reg)
    
    return(fplot(base_regiao)) 
}

plot_energy_mobility <- function(df, reg){
    
    base_regiao     <- df %>% 
        filter(regiao == reg)
    
    start <- base_regiao %>% 
        filter(!is.na(smth_mob) & !is.na(ma_consumo))%>%
        filter(data == min(data))

    p <-base_regiao%>%
        ggplot(aes(x = ma_consumo, y = smth_mob)) +
        geom_path(color = "steelblue", size = 0.8) +
        facet_wrap(~estado, scales = "free") +
        geom_point(start,mapping=aes(x=ma_consumo,y=smth_mob), colour="red")+
        ylab("Mobility") + xlab("Moving average of Energy Consumption")
    
    return(p)
    
    
}

