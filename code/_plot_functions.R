plot_activity_mobility <- function(df) {
    
    p <- ggplot(df, aes(x = smth_date, y = activity)) +
        geom_path(color = "steelblue", size = 0.8) +
        #geom_vline(xintercept = 19, linetype = "dotted", size = 0.8) +
        #geom_point(data = brasil_mob_b, shape = 22, fill = "white") +
        geom_hline(yintercept = 1, color = "red") +
        xlim(0, 40) + 
        facet_wrap(~ state) +
        ylab("Índice de Atividade") + 
        xlab("Dias Necessários para dobrar os casos")
    
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
        ylab("Mudança no consumo de energia") + 
        xlab("Dias Necessários para dobrar os casos")
    
    return(p)
}


plot_fit_energy <- function(df) {
    
    df <- df %>% 
        ungroup() %>% 
        select(data, Observado = ma_consumo, Predito = ma_pred) %>% 
        pivot_longer(cols = c(Observado, Predito))
  
    p <- df %>% 
        ggplot(aes(x = data)) +
        geom_line(aes(y = value, color = name), size = 0.8) +
        ylab("Média móvel de 7 dias do Consumo de Energia") + 
        xlab("Data") +
        scale_color_manual(values = c("black", "steelblue")) +
        theme(legend.title = element_blank())
    
    return(p)
}


plot_uf_energia <- function(df, UF) {
  
  base_regiao <- df %>% 
    filter(estado == UF)
  
  return(plot_fit_energy(base_regiao)) 
}


plot_regiao_energia <- function(df, reg, fplot) {
    
    base_regiao <- df %>% 
        filter(regiao == reg)
    
    return(fplot(base_regiao) + facet_wrap(~estado, scales = "free")) 
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
        ylab("Mobilidade") + xlab("Média móvel de 7 dias do Consumo de Energia")
    
    return(p)
}


plot_comparacao_estado <- function(df, UF){
  
    # filtrando por estado
    base_UF <- df %>% 
        filter(estado == UF)
    
    # PET phase
    phases <- base_UF %>% 
        group_by(PET_Phase) %>% 
        summarise(data = min(data)) %>% 
        filter(!is.na(PET_Phase))
    
    # total days
    total_days_mob <- max(base_UF %>% filter(!is.na(mobility)) %>% pull(data)) - 
        phases %>% 
        filter(PET_Phase == "Response") %>% 
        pull(data)
    
    total_days_energy <- max(base_UF %>% filter(!is.na(consumo_diario)) %>% pull(data)) - 
        phases %>% 
        filter(PET_Phase == "Response") %>% 
        pull(data)
    
    # definindo limites    
    ymin <- min(min(base_UF$activity, na.rm = TRUE) - 1,
                min(base_UF$ma_dif_baseline, na.rm = TRUE),
                min(base_UF$ma_dif_baseline_acl, na.rm = TRUE))
    
    ymax <- max(max(base_UF$activity, na.rm = TRUE),
                max(base_UF$ma_dif_baseline, na.rm = TRUE) + 1,
                max(base_UF$ma_dif_baseline_acl, na.rm = TRUE) + 1)
    

    # plots
    plot_mob <- base_UF %>% 
        ggplot(aes(x = smth_date, y = activity)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 1, color = "red") +
        xlim(0, 40) +
        ylim(ymin+1, ymax) +
        ylab("Índice de Atividade") +
        theme(axis.title.x = element_blank()) +
        ggtitle("Atividade")

    plot_total <- base_UF %>% 
        ggplot(aes(x = smth_date, y = ma_dif_baseline)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 0, color = "red") +
        xlim(0, 40) +
        ylim(ymin, ymax-1) +
        ylab("Mudança no consumo de energia") +
        theme(axis.title.x = element_blank()) +
        ggtitle("Energia Total")
    
    plot_acl <- base_UF %>% 
        ggplot(aes(x = smth_date, y = ma_dif_baseline_acl)) +
        geom_path(color = "steelblue", size = 0.8) +
        geom_hline(yintercept = 0, color = "red") +
        xlim(0, 40) +
        ylim(ymin, ymax-1) +
        ylab("Mudança no consumo de energia") +
        theme(axis.title.x = element_blank()) +
        ggtitle("Apenas ACL")
    
    # join plots
    plot <- plot_grid(plot_mob, 
                      plot_total,
                      plot_acl, 
                      align = 'h', nrow = 1, ncol = 3, scale = 1)
    
    x.grob <- textGrob("Doubling days of confirmed cases")
    title.grob <- textGrob(paste0(UF), gp = gpar(fontface = "bold"))
    
    grid.arrange(arrangeGrob(plot, bottom = x.grob, top = title.grob))
}


# energia detalhado -------------------------------------------------------
plot_ramo <- function(reg, pond = FALSE){
  
    df <- total_energy_ramo %>%
      filter(data >= as.Date("2020-02-25") & regiao == reg & !is.na(ramo)) %>%
      group_by(ramo, estado) %>%
      summarize(Media_Consumo_MhW = mean(dif_consumo, na.rm = TRUE),
                Media_Dif_Perc    = mean(dif_baseline, na.rm = TRUE)*100,
                Media_Dif_Pond    = mean(dif_baseline*pond, na.rm = TRUE)*100)
             
    
    p <- df %>% 
      ggplot(aes(ramo)) +
      scale_x_discrete(name = "Ramo") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                       vjust = 0.5, size = 10)) +
      geom_hline(yintercept = 0, colour = "red") +
      facet_wrap(estado ~ ., dir = "v", scales = "free_y") 
  
  if(pond) {
      p <- p + geom_col(mapping = aes(y = Media_Dif_Pond), position = "dodge") +
        scale_y_continuous(name = "Diferença Percentual Média Ponderada")
  } else {
      p <- p + geom_col(mapping = aes(y = Media_Dif_Perc), position = "dodge") +
        scale_y_continuous(name = "Diferença Percentual Média")
  }
  
  return(p)
}

