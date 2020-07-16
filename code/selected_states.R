# functions ---------------------------------------------------------------
plot_activity_acl_energy <- function(df, estado) {
    
    p <- ggplot(df, aes(x = smth_date, y = smth_consumo)) +
        geom_path(color = "steelblue", size = 0.8) +
        xlim(0, 40) + 
        ylab("Change from baseline in energy consumption") + 
        xlab("Doubling days of confirmed cases") +
        labs(title = paste0(estado, " - Energy (ACL)"))
    
    path <- paste0(estado, "_ACL_Energia.png")
    ggsave(file.path("output", path))
}

plot_activity_energy <- function(df, estado) {
    
    p <- ggplot(df, aes(x = smth_date, y = smth_consumo)) +
        geom_path(color = "steelblue", size = 0.8) +
        xlim(0, 40) + 
        ylab("Change from baseline in energy consumption") + 
        xlab("Doubling days of confirmed cases") +
        labs(title = paste0(estado, " - Total Energy"))
    
    path <- paste0(estado, "_Energia.png")
    ggsave(file.path("output", path))
}

plot_activity_mobility <- function(df, estado) {
    
    p <- ggplot(df, aes(x = smth_date, y = smth_mob)) +
        geom_path(color = "steelblue", size = 0.8) +
        xlim(0, 40) + 
        ylab("Mobility Index") + xlab("Doubling days of confirmed cases") +
        labs(title = paste0(estado, " - Mobility"))
    
    path <- paste0(estado, "_Mobilidade.png")
    ggsave(file.path("output", path))
}


# plots -------------------------------------------------------------------
estados_list <- c("Rio de Janeiro", "Sao Paulo", "Amazonas", "Rio Grande do Sul")
siglas_list <- c("RJ", "SP", "AM", "RS")

plots_mob <- walk2(siglas_list, estados_list, 
                   ~ plot_activity_mobility(bases_estados_df %>% 
                                            filter(state == .x), .y))

plots_total <- walk(estados_list, 
                ~ plot_activity_energy(total_energy_df %>% 
                                       filter(estado == .x), .x))

plots_acl <- walk(estados_list, 
                   ~ plot_activity_acl_energy(acl_energy_df %>% 
                                              filter(estado == .x), .x))

