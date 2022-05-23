library(readr)
library(dplyr)
library(tidyr)
library(R2jags)
library(skimr)
library(ggplot2)

bloqueo <- read_csv("data/BloqueoDatos.csv")

bloqueo %>% glimpse()
skimr::skim(bloqueo)


bloqueo_df <-bloqueo %>%  
  mutate(
    GrupoEdad = if_else(Edad < 50, "Grupo1", "Grupo2"),
    GrupoIMC = case_when(
      IMC < 18.5 ~ "PorDebajo",
      IMC < 24.9 ~ "Saludable",
      T ~ "Sobrepeso"
    ),
    TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")
  ) %>% 
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor  ) %>%
  group_by(Sexo, GrupoEdad, TipoCancer2, GrupoIMC) %>% 
  summarise(
    n = n(),
    VAS_AB = mean(VAS_AB, na.rm = T), 
    VAS_1M = mean(VAS_1M, na.rm = T),
    .groups = "drop"
  ) %>% 
  arrange(n)

bloqueo_df %>% glimpse()

n<-nrow(bloqueo_df)
n

############################ EFECTOS CONSTANTES   #####################

#-Defining data-

data<-list("n"=n,"y"= bloqueo_df$VAS_1M ,"vasab"=bloqueo_df$VAS_AB )
inits_efc_con <- function(){list(theta=1,yf1=rep(1,n)) }
par_efc_con <-c("theta","yf1")


##OpenBUGS
#mod_efc_con_bugs <- bugs(
#  data,
#  inits_efc_con,
#  par_efc_con,
#  model.file="efc_con.txt",
#  n.iter=50000,
#  n.chains=2
#  ,n.burnin=5000)

#JAGS
mod_efc_con_jags <-jags(
  data,
  inits_efc_con,
  par_efc_con,
  model.file="efc_con.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)



# Resultado
mod_efc_con_jags

#traceplot(mod_efc_con_jags)

out_mod_efc_con_simulaciones <- mod_efc_con_jags$BUGSoutput$sims.list
out_mod_efc_con_resumen <- mod_efc_con_jags$BUGSoutput$summary 
out_mod_efc_con_dic <- mod_efc_con_jags$BUGSoutput$DIC; out_mod_efc_con_dic

names <- rownames(out_mod_efc_con_resumen)
efc_con_pred <-  as_data_frame(out_mod_efc_con_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_con = mean) %>% 
  select(efc_con)

bloqueo_df <- bloqueo_df %>% cbind(efc_con_pred)


                                   

## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_con)) + 
  geom_point() +
  ylim( y = c(1,5)) +
  xlim(c(1,5))




############################ EFECTOS INDEPENDIENTES  #####################

data<-list("n"=n,"y"= bloqueo_df$VAS_1M  ,"vasab"= bloqueo_df$VAS_AB)
inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
pars_ind<-c("theta","yf1")



##OpenBUGS
#mod_efc_ind_bugs <- bugs(
#  data,
#  inits_efc_ind,
#  pars_ind,
#  model.file="efc_ind.txt",
#  n.iter=50000,
#  n.chains=2
#  ,n.burnin=5000)

#JAGS
mod_efc_ind_jags <-jags(
  data,
  inits_efc_ind,
  pars_ind,
  model.file="efc_ind.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)


mod_efc_ind_jags

out_mod_efc_ind_simulaciones <- mod_efc_ind_jags$BUGSoutput$sims.list
out_mod_efc_ind_resumen <- mod_efc_ind_jags$BUGSoutput$summary 
out_mod_efc_ind_dic <- mod_efc_ind_jags$BUGSoutput$DIC ; out_mod_efc_ind_dic

names <- rownames(out_mod_efc_ind_resumen)
efc_ind_pred <-  as_data_frame(out_mod_efc_ind_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_ind = mean) %>% 
  select(efc_ind)

bloqueo_df <- bloqueo_df %>% cbind(efc_ind_pred)


## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_ind)) + 
  geom_point() +
  ylim( y = c(1,5)) +
  xlim(c(1,5))

  



############################ EFECTOS INTERCAMBIABLES   #####################


data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
inits_efc_inter <-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
pars_inter <-c("theta","eta","yf1")



##OpenBUGS
#mod_efc_inter_bugs <- bugs(
#  data,
#  inits_efc_ind,
#  pars_ind,
#  model.file="efc_inter.txt",
#  n.iter=50000,
#  n.chains=2
#  ,n.burnin=5000)

#JAGS
mod_efc_inter_jags <-jags(
  data,
  inits_efc_inter,
  pars_inter,
  model.file="efc_inter.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)


mod_efc_inter_jags

out_mod_efc_inter_simulaciones <- mod_efc_inter_jags$BUGSoutput$sims.list
out_mod_efc_inter_resumen <- mod_efc_inter_jags$BUGSoutput$summary 
out_mod_efc_inter_dic <- mod_efc_inter_jags$BUGSoutput$DIC; out_mod_efc_inter_dic

names <- rownames(out_mod_efc_inter_resumen)
efc_inter_pred <-  as_data_frame(out_mod_efc_inter_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_inter = mean) %>% 
  select(efc_inter)

bloqueo_df <- bloqueo_df %>% cbind(efc_inter_pred)


## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_inter)) + 
  geom_jitter()+
  ylim( y = c(1,5)) +
  xlim(c(1,5))






############################ EFECTOS INTERCAMBIABLES CON VARIABLES   #####################


data<-list(
  "n"=n,
  "y"=bloqueo_df$VAS_1M,
  "sexo"=bloqueo_df$Sexo,
  "edad"=bloqueo_df$GrupoEdad,
  "cancer"=bloqueo_df$TipoCancer2,
  "imc"=bloqueo_df$GrupoIMC, 
  "vasab" = bloqueo_df$VAS_AB)


inits_efc_ind_var <- function(){
  list(
   yf1=rep(1,n),
   beta_sexo=rep(1,2), 
   beta_edad=rep(1,2), 
   alpha = 1, 
   beta_cancer=rep(1,2),
   beta_imc=rep(1,3) ) }

pars_ind_var <-c(
  "theta",
  "yf1",
  "alpha_adj",
  "beta_sexo_adj",
  "beta_edad_adj", 
  "beta_cancer_adj",
  "beta_imc_adj")





##OpenBUGS
#mod_efc_ind_var_bugs <- bugs(
#  data,
#  inits_efc_ind,
#  pars_ind,
#  model.file="efc_intercambiables.txt",
#  n.iter=50000,
#  n.chains=2
#  ,n.burnin=5000)

#JAGS
mod_efc_ind_var_jags <-jags(
  data,
  inits_efc_ind_var,
  pars_ind_var,
  model.file="efc_ind_var.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)


mod_efc_ind_var_jags

out_mod_efc_ind_var_simulaciones <- mod_efc_ind_var_jags$BUGSoutput$sims.list
out_mod_efc_ind_var_resumen <- mod_efc_ind_var_jags$BUGSoutput$summary 
out_mod_efc_ind_var_dic <- mod_efc_ind_var_jags$BUGSoutput$DIC

names <- rownames(out_mod_efc_ind_var_resumen)
efc_ind_var_pred <-  as_data_frame(out_mod_efc_ind_var_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_ind_var = mean) %>% 
  select(efc_ind_var)

bloqueo_df <- bloqueo_df %>% cbind(efc_ind_var_pred)


## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_ind_var)) + 
  geom_jitter()+
  ylim( y = c(1,5)) +
  xlim(c(1,5))




 