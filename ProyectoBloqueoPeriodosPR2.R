library(R2jags)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(rlang)

###### Carga de datos
bloqueo <- read_csv("data/BloqueoDatosPeriodo.csv")

# Categotizacion de variables
bloqueo <- bloqueo %>% 
  mutate(
  GrupoEdad = if_else(Edad < 50, "Grupo1", "Grupo2"),
  VAS_6M = as.numeric(VAS_6M),
  GrupoIMC = case_when(
    IMC < 18.5 ~ "PorDebajo",
    IMC < 24.9 ~ "Saludable",
    T ~ "Sobrepeso"),
  TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")) %>% 
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor)

skimr::skim(bloqueo)

#Filtrado
vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()
vect_names

funct_base <- function(bloqueo, vect_names, i){
bloqueo_agrupada <- bloqueo %>% 
  select("Sexo","GrupoEdad","GrupoIMC","TipoCancer2", "VAS_AB", vect_names[i]) %>%  
  na.omit() %>% 
  group_by(Sexo, GrupoEdad, TipoCancer2, GrupoIMC) %>% 
  summarise(
    n = n(),
    VAS_AB = mean(VAS_AB, na.rm = T), 
    OTRO_VAS = mean(!!sym(vect_names[i]), na.rm = T),#mean(bloqueo(vect_names[1]), na.rm = T),
    .groups = "drop"
  ) %>% 
  arrange(n) 

nuevos_nombres <- c(bloqueo_agrupada %>% select(-7) %>% names(),  vect_names[i])
names(bloqueo_agrupada) <- nuevos_nombres

bloqueo_agrupada
}

 

#################### INICIANDO VARIABLES ####################

dic_efc_ctes <- list()
pR2_efc_ctes <- list()

dic_efc_ind <- list()
pR2_efc_ind <- list()

dic_efc_int <- list()
pR2_efc_int <- list()

dic_efc_ind_con_var <- list()
pR2_efc_ind_con_var <- list()





############################ EFECTOS CONSTANTES #####################

#vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()
#bloqueo_df <- funct_base(bloqueo, vect_names, i)
#n <- nrow(bloqueo_df)
#var_ini <- bloqueo_df$VAS_AB
#var_in <-vect_names[i]
#print(var_in)
#fun_efc_ctes(var_in,bloqueo_df, var_ini )

fun_efc_ctes <- function(var_in, bloqueo_df, var_ini, dic_efc_ctes, pR2_efc_ctes, n) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in]),"vasab"=var_ini)
  inits_efc_con <- function(){list(theta=1,yf1=rep(1,n)) }
  par_efc_con <-c("theta","yf1")
  
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
  out_mod_efc_con_dic <- mod_efc_con_jags$BUGSoutput$DIC; 

  
  #dic_efc_ctes[var_in] <- out_mod_efc_con_dic
  
  names <- rownames(out_mod_efc_con_resumen)
  efc_con_pred <-  as.data.frame(out_mod_efc_con_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_con = mean) %>% 
    select(efc_con)
  
  df <- bloqueo_df %>% cbind(efc_con_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_con)) + 
    geom_point()  + 
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos constantes: Y vs Y predicha ",var_in) )
 
  
  # PSEUDO R2
  #pR2_efc_ctes[var_in]  <- cor(pull(bloqueo_df[var_in]) , pull(efc_con_pred))^2
  
  modelo <- list()
  ##modelo["summary"] <-  as.matrix( out_mod_efc_con_resumen)
  modelo["n"] <- n
  modelo["modelo"] <- "Efectos constantes"
  modelo["var_in"]<-  var_in
  modelo["DIC"]<- out_mod_efc_con_dic
  modelo["R2"]<- cor(pull(bloqueo_df[var_in]) , pull(efc_con_pred))^2
  print(modelo)
  print(ggp)
  
  return(modelo)
  
}

############################ EFECTOS INDEPENDIENTES  #####################
fun_efc_ind <- function(var_in, bloqueo_df) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in]) ,"vasab"= var_ini)
  inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
  pars_ind<-c("theta","yf1")
  
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
  
  # Resultado
  mod_efc_ind_jags
  
  out_mod_efc_ind_simulaciones <- mod_efc_ind_jags$BUGSoutput$sims.list
  out_mod_efc_ind_resumen <- mod_efc_ind_jags$BUGSoutput$summary 
  out_mod_efc_ind_dic <- mod_efc_ind_jags$BUGSoutput$DIC ; 

  dic_efc_ind[var_in]  <- out_mod_efc_ind_dic
  
  names <- rownames(out_mod_efc_ind_resumen)
  efc_ind_pred <-  as.data.frame(out_mod_efc_ind_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind = mean) %>% 
    select(efc_ind)
  
  df <- bloqueo_df %>% cbind(efc_ind_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_ind)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos independientes: Y vs Y predicha ",var_in) )
    
  print(ggp)
  
  # PSEUDO R2
  pR2_efc_ind[var_in] <- cor( pull(bloqueo_df[var_in]), pull(efc_ind_pred))^2
}

############################ EFECTOS INTERCAMBIABLES   #####################
fun_efc_int <- function(var_in, bloqueo_df) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in]),"vasab"= var_ini )
  inits_efc_inter <-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
  pars_inter <-c("theta","eta","yf1")
  
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
  
  # Resultados
  mod_efc_inter_jags
  
  out_mod_efc_inter_simulaciones <- mod_efc_inter_jags$BUGSoutput$sims.list
  out_mod_efc_inter_resumen <- mod_efc_inter_jags$BUGSoutput$summary 
  out_mod_efc_inter_dic <- mod_efc_inter_jags$BUGSoutput$DIC; 
  
  dic_efc_int[var_in] <- out_mod_efc_inter_dic
  
  names <- rownames(out_mod_efc_inter_resumen)
  efc_inter_pred <-  as.data.frame(out_mod_efc_inter_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_inter = mean) %>% 
    select(efc_inter)
  
  df <- bloqueo_df %>% cbind(efc_inter_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_inter)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos intercambiables: Y vs Y predicha ",var_in) )
  
  print(ggp)
  
  # PSEUDO R2
  pR2_efc_int[var_in] <- cor(pull(bloqueo_df[var_in]),  pull(efc_inter_pred))^2

}
  
  
############################ EFECTOS INDEPENDIENTES CON VARIABLES   #####################
fun_efc_ind_con_var <- function(var_in, bloqueo_df) {
  data<-list(
    "n"=n,
    "y"=pull(bloqueo_df[var_in]),
    "sexo"=bloqueo_df$Sexo,
    "edad"=bloqueo_df$GrupoEdad,
    "cancer"=bloqueo_df$TipoCancer2,
    "imc"=bloqueo_df$GrupoIMC, 
    "vasab" = var_ini)
  
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
  
  # Resultados
  mod_efc_ind_var_jags
  
  out_mod_efc_ind_var_simulaciones <- mod_efc_ind_var_jags$BUGSoutput$sims.list
  out_mod_efc_ind_var_resumen <- mod_efc_ind_var_jags$BUGSoutput$summary 
  out_mod_efc_ind_var_dic <- mod_efc_ind_var_jags$BUGSoutput$DIC

  dic_efc_ind_con_var[var_in] <- out_mod_efc_ind_var_dic
  
  names <- rownames(out_mod_efc_ind_var_resumen)
  efc_ind_var_pred <-  as.data.frame(out_mod_efc_ind_var_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind_var = mean) %>% 
    select(efc_ind_var)
  
  df <- bloqueo_df %>% cbind(efc_ind_var_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_ind_var)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos independientes con variables: Y vs Y predicha ",var_in) )
  print(ggp)
  
  # PSEUDO R2
  pR2_efc_ind_con_var <<- cor(pull(bloqueo_df[var_in]),  pull(efc_ind_var_pred))^2
}

### ------------------------------ Función que corre todos los modelos para cada período


#i=2
#bloqueo_df <- funct_base(bloqueo, vect_names, i)
#n <- nrow(bloqueo_df); n
#var_ini <- bloqueo_df$VAS_AB
#var_in <-vect_names[i] 


       
i=1   
vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()


modelos finales

modelos<- function( vect_names,bloqueo) {
  modelos_fin <- list()
  
  
  bloqueo_df <- funct_base(bloqueo, vect_names, i)
  print(bloqueo_df)
  n <- nrow(bloqueo_df)
  var_ini <- bloqueo_df$VAS_AB
  var_in <-vect_names[i]
  print(var_in)
  modelos_fin[var_in] <- fun_efc_ctes(var_in, bloqueo_df, var_ini, dic_efc_ctes, pR2_efc_ctes, n)

  
  
  }
  #fun_efc_ind(var_in, bloqueo_df)
  #fun_efc_int(var_in, bloqueo_df)
  #fun_efc_ind_con_var(var_in,bloqueo_df)
}




modelos ( i, vect_names,bloqueo)






# CORRIDA COMPLETA
# Efectos constantes ----------------------------------------------------------------------------------------------------------------

t_g <- c("Efectos constantes a las 24 hrs.","Efectos constantes a los 7 días","Efectos constantes al mes","Efectos constantes a los 3 meses","Efectos constantes a los 6 meses")
# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ctes)
print("Efectos constantes")
print(dic_efc_ctes)
print(pR2_efc_ctes)

# Efectos independientes ------------------------------------------------------------------------------------------------------------

t_g <- c("Efectos independientes a las 24 hrs.","Efectos independientes a los 7 días","Efectos independientes al mes","Efectos independientes a los 3 meses","Efectos independientes a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind)
print("Efectos independientes")
print(dic_efc_ind)
print(pR2_efc_ind)

# Efectos intercambiables ----------------------------------------------------------------------------------------------------------------

t_g <- c("Efectos intercambiables a las 24 hrs.","Efectos intercambiables a los 7 días","Efectos intercambiables al mes","Efectos intercambiables a los 3 meses","Efectos intercambiables a los 6 meses")
# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_int)

print("Efectos intercambiables")
print(dic_efc_int)
print(pR2_efc_int)

# Efectos independientes con variables ----------------------------------------------------------------------------------------------------------------

t_g <- c("Efectos independientes con variables a las 24 hrs.",
         "Efectos independientes con variables a los 7 días",
         "Efectos independientes con variables al mes",
         "Efectos independientes con variables a los 3 meses",
         "Efectos independientes con variables a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind_con_var)

print("Efectos independientes con variables")
print(dic_efc_ind_con_var)
print(pR2_efc_ind_con_var)




# ---> NO CORRER
############################ EFECTOS INTERCAMBIABLES CON VARIABLES -> pendiente 

data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
inits_efc_inter <-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
pars_inter <-c("theta","eta","yf1")

data<-list(
  "n"=n,
  "y"=bloqueo_df$VAS_1M + 1,
  "sexo"=bloqueo_df$Sexo,
  "edad"=bloqueo_df$GrupoEdad,
  "cancer"=bloqueo_df$TipoCancer2,
  "imc"=bloqueo_df$GrupoIMC, 
  "vasab" = bloqueo_df$VAS_AB + 1)

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
efc_inter_pred <-  as.data.frame(out_mod_efc_inter_resumen) %>% 
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
