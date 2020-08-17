plot_activity_mobility <- function(df) {
    
    p <- ggplot(df, aes(x = smth_date, y = activity)) +
        geom_path(color = "steelblue", size = 0.8) +
        #geom_vline(xintercept = 19, linetype = "dotted", size = 0.8) +
        #geom_point(data = brasil_mob_b, shape = 22, fill = "white") +
        geom_hline(yintercept = 1, color = "red") +
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
        ylab("Mudança no consumo de energia") + 
        xlab("Dias Necessários para dobrar os casos")
    
    return(p)
}


plot_fit_energy <- function(df, plotly = FALSE) {
    
    if(plotly) {
        total <- df %>% 
          ungroup() %>% 
          select(data, Observado = ma_consumo, Predito = ma_pred) %>% 
          pivot_longer(cols = c(Observado, Predito))
        
        df_acl <- df %>% 
          ungroup() %>% 
          select(data, Observado = ma_consumo_acl, Predito = ma_pred_acl) %>% 
          pivot_longer(cols = c(Observado, Predito))
        
        # plot 1
        p1 <- total %>% 
          ggplot(aes(x = data, group = 1,
                     text = paste('Média móvel de 7 dias do Consumo de Energia:', round(value, 2),
                                  '<br>Data:', data))) +
          geom_line(aes(y = value, color = name), size = 0.8) +
          geom_vline(xintercept = as.numeric(lubridate::ymd("2020-02-25")), 
                     linetype = "dashed", color = "red") +
          ylab("Média móvel de 7 dias do Consumo de Energia") + 
          xlab("Data") +
          scale_color_manual(values = c("black", "steelblue")) +
          theme(legend.title = element_blank())
        
        p1 <- p1 %>% 
          ggplotly(tooltip = "text") %>% 
          layout(annotations = list(text = "Energia Total",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        # plot 2
        p2 <- df_acl %>% 
          ggplot(aes(x = data, group = 1,
                     text = paste('Média móvel de 7 dias do Consumo de Energia:', round(value, 2),
                                  '<br>Data:', data))) +
          geom_line(aes(y = value, color = name), size = 0.8) +
          geom_vline(xintercept = as.numeric(lubridate::ymd("2020-02-25")), 
                     linetype = "dashed", color = "red") +
          ylab("Média móvel de 7 dias do Consumo de Energia") + 
          xlab("Data") +
          scale_color_manual(values = c("black", "steelblue")) +
          theme(legend.title = element_blank())
        
        p2 <- p2 %>% 
          ggplotly(tooltip = "text") %>% 
          layout(annotations = list(text = "Energia (apenas ACL)",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        # join plots
        p <- subplot(list(style(p1, showlegend = FALSE), p2), nrows = 2, 
                     shareY = TRUE, titleY = TRUE,
                     titleX = TRUE, margin = 0.1) %>% 
            layout(xaxis  = list(title = ""),
                   xaxis2 = list(title = ""),
                   yaxis  = list(title = ""),
                   yaxis2 = list(title = ""),
                   annotations = list(
                     list(x = -0.09,
                          text = "Média móvel de 7 dias do Consumo de Energia",
                          textangle = 270,
                          showarrow = F, xref='paper', yref='paper', size=46)),
                   height = 600, width = 870)

    } else {
      
        df <- df %>% 
          ungroup() %>% 
          select(data, estado, regiao, Observado = ma_consumo, Predito = ma_pred) %>% 
          pivot_longer(cols = c(Observado, Predito))
        
        p <- df %>% 
          ggplot(aes(x = data)) +
          geom_line(aes(y = value, color = name), size = 0.8) +
          geom_vline(xintercept = as.Date("2020-02-25"), linetype = "dashed", color = "red") +
          ylab("Média móvel de 7 dias do Consumo de Energia") + 
          xlab("Data") +
          scale_color_manual(values = c("black", "steelblue")) +
          theme(legend.title = element_blank())
    }
    
    return(p)
}


plot_uf_energia <- function(UF) {
  
  if (UF == "Brasil") {
    base_regiao <- brasil
  } else {
    base_regiao <- estados %>% 
      filter(estado == UF)
  }
  
  return(plot_fit_energy(base_regiao, plotly = TRUE)) 
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
        filter(!is.na(smth_mob) & !is.na(ma_consumo)) %>%
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

plot_comparacao_estado <- function(UF, plotly = FALSE) {
  
    if (UF == "Brasil") {
        base_UF <- brasil
    } else {
        base_UF <- estados %>% 
          filter(estado == UF)
    }
    
    # PET phase
    phases <- base_UF %>% 
      group_by(PET_Phase) %>% 
      summarise(data = min(data)) %>% 
      filter(!is.na(PET_Phase))
    
    # start of each phase
    start_response <- phases[phases$PET_Phase == "Response", ]$data
    start_trough   <- phases[phases$PET_Phase == "Trough", ]$data
    start_recovery <- phases[phases$PET_Phase == "Recovery", ]$data
  
    # total days
    base_UF <- base_UF %>% 
      mutate(pos_response = data - start_response)
    
    
    # definindo limites    
    ymin <- min(min(base_UF$smth_mob, na.rm = TRUE) - 1,
                min(base_UF$activity, na.rm = TRUE) - 1,
                min(base_UF$ma_dif_baseline, na.rm = TRUE),
                min(base_UF$ma_dif_baseline_acl, na.rm = TRUE))
    
    ymax <- max(max(base_UF$smth_mob, na.rm = TRUE),
                max(base_UF$activity, na.rm = TRUE),
                max(base_UF$ma_dif_baseline, na.rm = TRUE) + 1,
                max(base_UF$ma_dif_baseline_acl, na.rm = TRUE) + 1)
    
    # plots
    shapes <- c("Response" = 1, "Trough" = 0)
    
    plot_mob <- base_UF %>%
      ggplot(aes(x = smth_date, y = smth_mob, group = 1,
                 text = paste('Índice de Mobilidade:', round(smth_mob, 2),
                              '<br>Dias necessários para dobrar os casos:', round(smth_date, 2),
                              '<br>Dias após a fase de "Response":', pos_response,
                              '<br>Data:', data))
      ) +
      geom_hline(yintercept = 1, color = "red") +
      geom_path(color = "steelblue", size = 0.8) +
      ylab("Índice de Mobilidade") +
      ggtitle("Mobilidade") +
      ylim(ymin+0.99, ymax) +
      theme(axis.title.x = element_blank(),
            legend.position = "none") +
      geom_point(data = base_UF[base_UF$data == start_response, ],
                 mapping = aes(shape = "Response"), 
                 size = 3) +
      geom_point(data = base_UF[base_UF$data == start_trough, ],
                 mapping = aes(shape = "Trough"),
                 size = 2.75) +
      scale_shape_manual(name = "", 
                         breaks = c("Response", "Trough"),
                         values = shapes,
                         labels = c("Response", "Trough"))
    
    plot_activ <- base_UF %>%
      ggplot(aes(x = smth_date, y = activity, group = 1,
                 text = paste('Índice de Atividade:', round(activity, 2),
                              '<br>Dias necessários para dobrar os casos:', round(smth_date, 2),
                              '<br>Dias após a fase de "Response":', pos_response,
                              '<br>Data:', data))
      ) +
      geom_hline(yintercept = 1, color = "red") +
      geom_path(color = "steelblue", size = 0.8) +
      ylab("Índice de Atividade") +
      ggtitle("Atividade/Mobilidade") +
      ylim(ymin+0.99, ymax) +
      theme(axis.title.x = element_blank(),
            legend.position = "none") +
      geom_point(data = base_UF[base_UF$data == start_response, ],
                 mapping = aes(shape = "Response"), 
                 size = 3) +
      geom_point(data = base_UF[base_UF$data == start_trough, ],
                 mapping = aes(shape = "Trough"),
                 size = 2.75) +
      scale_shape_manual(name = "", 
                         breaks = c("Response", "Trough"),
                         values = shapes,
                         labels = c("Response", "Trough"))
    
    
    plot_total <- base_UF %>% 
      ggplot(aes(x = smth_date, y = ma_dif_baseline, group = 1,
                 text = paste('Mudança no Consumo de Energia:', round(ma_dif_baseline, 2),
                              '<br>Dias necessários para dobrar os casos:', round(smth_date, 2),
                              '<br>Data:', data))
      ) +
      geom_hline(yintercept = 0, color = "red") +
      geom_path(color = "steelblue", size = 0.8) +
      #geom_point(data = base_UF[base_UF$data == start_response, ], 
      #           mapping = aes(shape = "Response"), 
      #           size = 3) +
      #geom_point(base_UF[base_UF$data == start_trough, ],
      #           mapping = aes(shape = "Trough"),
      #           size = 2.75) +
      ylim(ymin, ymax-1) +
      ylab("Mudança no consumo de energia") +
      scale_shape_manual(name = "", 
                         breaks = c("Response", "Trough"),
                         values = shapes,
                         labels = c("Response", "Trough"))+
      theme(axis.title.x = element_blank(),
            legend.position = "none") +
      ggtitle("Energia Total")
    
    plot_acl <- base_UF %>% 
      ggplot(aes(x = smth_date, y = ma_dif_baseline_acl, group = 1,
                 text = paste('Mudança no Consumo de Energia:', round(ma_dif_baseline_acl, 2) ,
                              '<br>Dias necessários para dobrar os casos:', round(smth_date, 2),
                              '<br>Data:', data))
      ) +
      geom_hline(yintercept = 0, color = "red") +
      geom_path(color = "steelblue", size = 0.8) +
      geom_point(data = base_UF[base_UF$data == start_response, ], 
                 mapping = aes(y = 10000, shape = "Response"), 
                 size = 3) +
      geom_point(data = base_UF[base_UF$data == start_trough, ],
                 mapping = aes(y = 10000, shape = "Trough"),
                 size = 2.75)+
      ylim(ymin, ymax-1) +
      ylab("Mudança no consumo de energia")  +
      scale_shape_manual(name = "", 
                         breaks = c("Response", "Trough"),
                         values = shapes,
                         labels = c("Response", "Trough"))+
      theme(axis.title.x = element_blank()) +
      ggtitle("Energia (apenas ACL)")
  
  
    if(length(start_recovery) != 0) {
        plot_mob <- plot_mob +
          geom_point(data = base_UF[base_UF$data == start_recovery, ],
                     mapping = aes(x = smth_date, y = smth_mob),
                     shape = 0, size = 2.75)
      
        plot_total <- plot_total +
          geom_point(data = base_UF[base_UF$data == start_recovery, ],
                     mapping = aes(x = smth_date, y = ma_dif_baseline),
                     shape = 0, size = 2.75)
      
        plot_acl <- plot_acl +
          geom_point(data = base_UF[base_UF$data == start_recovery, ],
                     mapping = aes(x = smth_date, y = ma_dif_baseline_acl),
                     shape = 0, size = 2.75)
    }
  
    # join plots
    if (plotly) {
        h <- 800
        w <- 950
    
        # plotly options
        plot_mob <- ggplotly(plot_mob, tooltip = "text") %>% 
          layout(annotations = list(text = "Mobilidade",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        plot_activ <- ggplotly(plot_activ, tooltip = "text") %>% 
          layout(annotations = list(text = "Atividade/Mobilidade",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        plot_total <- ggplotly(plot_total, tooltip = "text") %>% 
          layout(annotations = list(text = "Energia Total",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        plot_acl <- ggplotly(plot_acl, tooltip = "text") %>% 
          layout(annotations = list(text = "Energia (apenas ACL)",
                                    xref = "paper",
                                    yref = "paper",
                                    yanchor = "bottom",
                                    xanchor = "center",
                                    align = "center",
                                    x = 0.5,
                                    y = 1,
                                    showarrow = FALSE))
        
        # join plots
        plot <- subplot(list(style(plot_mob, showlegend = FALSE), 
                             style(plot_activ, showlegend = FALSE), 
                             style(plot_total, showlegend = FALSE), 
                             plot_acl), 
                        titleX = TRUE, titleY = TRUE, nrows = 2,
                        widths = c(0.5, 0.5), margin = 0.05,
                        which_layout = FALSE) %>% 
          layout(xaxis  = list(title = ""),
                 xaxis2 = list(title = ""),
                 xaxis3 = list(title = "Dias necessários para dobrar os casos"),
                 xaxis4 = list(title = "Dias necessários para dobrar os casos"),
                 height = h, width = w,
                 legend = list(title=list(text='<b> Fase </b>')))
    
     } else {
        plot <- plot_grid(plot_mob, 
                          plot_activ, 
                          plot_total,
                          plot_acl, 
                          align = 'h', nrow = 2, ncol = 2, scale = 1,
                          rel_widths = c(0.5, 0.5))
        
        x.grob <- textGrob("Doubling days of confirmed cases")
        title.grob <- textGrob(paste0(UF), gp = gpar(fontface = "bold"))
        
        grid.arrange(arrangeGrob(plot, bottom = x.grob, top = title.grob))
      }
  
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


# shiny function ----------------------------------------------------------
plot_shiny <- function(UF, tipo) {
    UF <- UF %>% iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  
    if(tipo == "atividade") {
        p <- plot_comparacao_estado(UF, plotly = TRUE)
    } else {
        p <- plot_uf_energia(UF)
    }
  
  return(p)
}


