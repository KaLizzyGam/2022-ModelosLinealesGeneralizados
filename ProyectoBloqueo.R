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
      IMC < 29.9 ~ "Sobrepeso",
      T ~ "Obesidad"
    ),
    TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")
  ) %>% 
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor  ) %>% 
  select (Sexo, GrupoEdad, TipoCancer2, GrupoIMC, VAS_AB, VAS_1M )

bloqueo_df %>% glimpse()

n<-nrow(bloqueo_df)


############################ EFECTOS CONSTANTES   #####################

#-Defining data-

data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
data<-list("n"=n,"y"=bloqueo_df$VAS_1M,"sexo"=bloqueo_df$Sexo,"edad"=bloqueo_df$GrupoEdad,
            "cancer"=bloqueo_df$TipoCancer2,"imc"=bloqueo_df$GrupoIMC, "vasab" = bloqueo_df$VAS_AB)

#-Defining inits-
inits_efc_con <- function(){list(theta=1,yf1=rep(1,n)) }

#-Selecting parameters to monitor-
par_efc_con <-c("theta","yf1")

parsd<-c("alpha.adj","beta.adj","gama.adj","delta.adj","yf1")

#-Running code-
#OpenBUGS
mod_efc_con_bugs <- bugs(
  data,
  inits_efc_con,
  par_efc_con,
  model.file="efc_con.txt",
  n.iter=50000,
  n.chains=2
  ,n.burnin=5000)

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
out_mod_efc_con_dic <- mod_efc_con_jags$BUGSoutput$DIC

names <- rownames(out_mod_efc_con_resumen)
efc_con_pred <-  as_data_frame(out_mod_efc_con_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_con = round(mean)) %>% 
  select(efc_con)

bloqueo_df <- bloqueo_df %>% cbind(efc_con_pred)


                                   

## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_con)) + 
  geom_jitter()




############################ EFECTOS INDEPENDIENTES  #####################

data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
pars_ind<-c("theta","yf1")


#-Running code-
#OpenBUGS
mod_efc_ind_bugs <- bugs(
  data,
  inits_efc_ind,
  pars_ind,
  model.file="efc_ind.txt",
  n.iter=50000,
  n.chains=2
  ,n.burnin=5000)

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
out_mod_efc_ind_dic <- mod_efc_ind_jags$BUGSoutput$DIC

names <- rownames(out_mod_efc_ind_resumen)
efc_ind_pred <-  as_data_frame(out_mod_efc_ind_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_ind = round(mean)) %>% 
  select(efc_ind)

bloqueo_df <- bloqueo_df %>% cbind(efc_ind_pred)


## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_ind)) + 
  geom_jitter()




############################ EFECTOS INTERCAMBIABLES   #####################


data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
pars_ind<-c("theta","yf1")


#-Running code-
#OpenBUGS
mod_efc_ind_bugs <- bugs(
  data,
  inits_efc_ind,
  pars_ind,
  model.file="efc_ind.txt",
  n.iter=50000,
  n.chains=2
  ,n.burnin=5000)

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
out_mod_efc_ind_dic <- mod_efc_ind_jags$BUGSoutput$DIC

names <- rownames(out_mod_efc_ind_resumen)
efc_ind_pred <-  as_data_frame(out_mod_efc_ind_resumen) %>% 
  cbind(names)  %>% 
  tibble() %>% 
  select(mean, names) %>% 
  filter(grepl('yf1', names)) %>% 
  mutate(efc_ind = round(mean)) %>% 
  select(efc_ind)

bloqueo_df <- bloqueo_df %>% cbind(efc_ind_pred)


## COMPARACIONES
bloqueo_df %>% as_tibble() %>% 
  ggplot2::ggplot(aes(x=VAS_1M, y=efc_ind)) + 
  geom_jitter()






#####################################3
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




inits_efc_int <- function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
inits_var_exp <- function(){list(alpha=0,beta=rep(0,2),gama=rep(0,2),delta=rep(0,2),yf1=rep(1,n))}








 