# options -----------------------------------------------------------------
theme_set(theme_bw())


# functions ---------------------------------------------------------------
source("../code/_plot_functions.R", encoding = "UTF-8")


# load data ---------------------------------------------------------------
brasil  <- read_csv("data/brasil.csv")
estados <- read_csv("data/estados.csv")

choices <- c("Brasil", unique(estados %>% pull(estado)))
