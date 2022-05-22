library(readr)
library(dplyr)
library(tidyr)
library(R2jags)

bloqueo <- read_csv("data/BloqueoDatos.csv")

bloqueo %>% glimpse()

bloqueo_agg <- bloqueo %>% 
  mutate(GrupoEdad = if_else(Edad < 40, "Grupo1", 
                             ifelse(Edad < 60, "Grupo2", "Grupo3"))) %>%
  mutate(GrupoIMC = if_else(IMC < 18.5, "PorDebajo", 
                            ifelse(IMC <= 24.9, "Saludable",
                            ifelse(IMC <= 29.9, "Sobrepeso", "Obesidad")))) %>%
  mutate(VAS_1M_DIS = replace_na(VAS_1M_DIS,"Nulo")) %>% 
  mutate(TipoCancer2 = if_else(  TipoCancer == "CACU", "CACU", "OTRO")) %>%
  group_by( VAS_AB_DIS, VAS_1M_DIS) %>% 
  summarise(Observados = n()) 












 