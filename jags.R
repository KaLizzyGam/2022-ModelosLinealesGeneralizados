library(readr)
library(dplyr)
library(R2jags)

bloqueo <- read_csv("data/BloqueoDatos.csv")

bloqueo %>% glimpse()

bloqueo %>% 
  group_by()