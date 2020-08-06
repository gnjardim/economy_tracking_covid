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
        ylab("Change in energy consumption") + xlab("Doubling days of confirmed cases")
    
    return(p)
}

plot_fit_energy <- function(df) {
    
    p <- df %>% 
        ggplot(aes(x = data)) +
        geom_line(aes(y = ma_consumo), color = "black", size = 0.8) +
        geom_line(aes(y = ma_pred), color = "steelblue", size = 0.8) +
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
    
    base_regiao <- df %>% 
        filter(regiao == reg)
    
    start <- base_regiao %>% 
        filter(!is.na(smth_mob) & !is.na(ma_consumo))%>%
        filter(data == min(data))

    p <- base_regiao %>%
        ggplot(aes(x = ma_consumo, y = smth_mob)) +
        geom_path(color = "steelblue", size = 0.8) +
        facet_wrap(~estado, scales = "free") +
        geom_point(start, mapping = aes(x = ma_consumo, y = smth_mob), 
                   colour = "red") +
        ylab("Mobility") + xlab("Moving average of Energy Consumption")
    
    return(p)
    
    
}

plot_comparacao_estado <- function(UF){
  
    # filtrando por estado
    base_UF <- bases_estados_df %>% 
        filter(estado == UF)
    
    total_energy_UF <- total_energy_df %>% 
        filter(estado == UF)
    
    acl_energy_UF <- acl_energy_df %>% 
        filter(estado == UF)

    # definindo limites    
    ymin <- min(min(base_UF$activity, na.rm = TRUE) - 1,
                min(total_energy_UF$ma_dif_baseline, na.rm = TRUE),
                min(acl_energy_UF$ma_dif_baseline, na.rm = TRUE))
    
    ymax <- max(max(base_UF$activity, na.rm = TRUE),
                max(total_energy_UF$ma_dif_baseline, na.rm = TRUE) + 1,
                max(acl_energy_UF$ma_dif_baseline, na.rm = TRUE) + 1)
    

    # plots
    plot_mob <- base_UF %>% 
        ggplot(aes(x = smth_date, y = activity)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 1, color = "red") +
        xlim(0, 40) +
        ylim(ymin+1, ymax)+
        ylab("Activity Index")+
        theme(axis.title.x = element_blank())

    plot_total <- total_energy_UF %>% 
        ggplot(aes(x = smth_date, y = ma_dif_baseline)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 0, color = "red") +
        xlim(0, 40) +
        ylim(ymin, ymax-1)+
        ylab("Change in energy consumption")+
        theme(axis.title.x = element_blank())
    
    plot_acl <- acl_energy_UF %>% 
        ggplot(aes(x = smth_date, y = ma_dif_baseline)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 0, color = "red") +
        xlim(0, 40) +
        ylim(ymin, ymax-1)+
        ylab("Change in energy consumption") +
        theme(axis.title.x = element_blank())
    
    # join plots
    plot <- plot_grid(plot_mob, 
                      plot_total,
                      plot_acl, 
                      align = 'h', nrow = 1, ncol = 3, scale = 1)
    
    x.grob <- textGrob("Doubling days of confirmed cases")
    title.grob <- textGrob(paste0(UF), gp = gpar(fontface = "bold"))
    
    grid.arrange(arrangeGrob(plot, bottom = x.grob, top = title.grob))
}


plot_ramo <- function(reg){
  
  df <- total_energy_ramo %>%
    filter(data <= as.Date("2020-02-25") & regiao == reg) %>%
    group_by(ramo, estado) %>%
    summarize(Media_Consumo_MhW = mean(dif_consumo, na.rm = TRUE),
              Media_Dif_Perc    = mean(dif_baseline, na.rm = TRUE)*100)
           
  max_lim <- as.integer(ceiling(max(df$Media_Dif_Perc)))
  min_lim <- as.integer(floor(min(df$Media_Dif_Perc)))
  
  p <- df %>% 
    ggplot(aes(ramo)) +
    geom_col(mapping = aes(y = Media_Dif_Perc), position = "dodge")+ 
    scale_x_discrete(name = "Ramo")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
    scale_y_continuous(name = "Média Percentual", limits = c(min_lim, max_lim))+
    facet_wrap(~estado, scales = "free") 
  
  return(p)
  
  
}
