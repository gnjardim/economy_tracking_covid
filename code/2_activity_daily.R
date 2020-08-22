# packages ----------------------------------------------------------------
library(tidyverse)
library(rlang)


# load data ---------------------------------------------------------------
brasil <- read_csv("shiny/data/brasil.csv")


# plot --------------------------------------------------------------------
theme_set(theme_classic() +
          theme(axis.text.x = element_text(angle = 90)))

plot_index <- function(var, title, filename) {
    
    p <- ggplot(brasil, aes(x = data, y = {{ var }})) +
        geom_line(color = "steelblue", size = 1.2) +
        scale_x_date(limits = c(as.Date("2020-03-17"), as.Date("2020-08-14")),
                     date_breaks = "1 week") +
        scale_y_continuous(limits = c(0.40, 1.10),
                           breaks = scales::pretty_breaks(n = 20)) +
        xlab("") + ylab("") +
        ggtitle(title)
    
        ggsave(filename = paste0("output/", filename, ".png"),
               width = 6, height = 4)
        
    return(p)
}

# mobilidade
plot_index(smth_mob, "Mobilidade", "Mobilidade")

# atividade/mobilidade
plot_index(activity, "Atividade/Mobilidade", "Atividade_Mobilidade")

# energia
plot_index(1 + ma_dif_baseline, "Energia", "Energia")

# energia acl
plot_index(1 + ma_dif_baseline_acl, "Energia (apenas ACL)", "Energia (apenas ACL)")


