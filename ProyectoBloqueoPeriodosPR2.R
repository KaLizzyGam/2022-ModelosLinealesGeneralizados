library(R2jags)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(rlang)

set.seed(1234)

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
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor) %>%
  mutate(
    VAS_3M = if_else(is.na(VAS_3M), VAS_1M, VAS_3M),
    VAS_6M = if_else(is.na(VAS_6M), VAS_3M, VAS_6M)
  )

skimr::skim(bloqueo)


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

############################ EFECTOS CONSTANTES #####################
fun_efc_ctes <- function(var_in, bloqueo_df, var_ini, n) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in] +1 ),"vasab"=var_ini +1 )
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
  
 
  out_mod_efc_con_simulaciones <- mod_efc_con_jags$BUGSoutput$sims.list
  out_mod_efc_con_resumen <- mod_efc_con_jags$BUGSoutput$summary 
  out_mod_efc_con_dic <- mod_efc_con_jags$BUGSoutput$DIC; 

  

  
  names <- rownames(out_mod_efc_con_resumen)
  efc_con_pred <-  as.data.frame(out_mod_efc_con_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_con = mean -1 ) %>% 
    select(efc_con)
  
  df <- bloqueo_df %>% cbind(efc_con_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_con)) + 
    geom_point()  + 
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos constantes: Y vs Y predicha ",var_in) )
 

  modelo <- list()
  modelo["n"] <- n
  modelo["modelo"] <- "Efectos constantes"
  modelo["var_in"]<-  var_in
  modelo["DIC"]<- out_mod_efc_con_dic
  modelo["R2"]<- cor(pull(bloqueo_df[var_in]) , pull(efc_con_pred))^2

  print(paste ("Efectos constantes:  ",var_in))
  print(out_mod_efc_con_resumen)
  print(ggp)  
  
  return(modelo)
  
}

############################ EFECTOS INDEPENDIENTES  #####################
fun_efc_ind <- function(var_in, bloqueo_df, var_ini, n) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in] + ß1)  ,"vasab"= var_ini +1 )
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

  names <- rownames(out_mod_efc_ind_resumen)
  efc_ind_pred <-  as.data.frame(out_mod_efc_ind_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind = mean -1) %>% 
    select(efc_ind)
  
  df <- bloqueo_df %>% cbind(efc_ind_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_ind)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos independientes: Y vs Y predicha ",var_in) )

  # PSEUDO R2
  
  modelo <- list()
  modelo["n"] <- n
  modelo["modelo"] <- "Efectos independientes"
  modelo["var_in"]<-  var_in
  modelo["DIC"]<- out_mod_efc_ind_dic
  modelo["R2"]<- cor( pull(bloqueo_df[var_in]), pull(efc_ind_pred))^2
  
  print(paste ("Efectos independientes: ",var_in))
  print(out_mod_efc_ind_resumen)
  print(ggp)
  
  return(modelo)
  
  
}

############################ EFECTOS INTERCAMBIABLES   #####################
fun_efc_int <- function(var_in, bloqueo_df, var_ini, n) {
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in]+1),"vasab"= var_ini+1 )
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
  

  
  names <- rownames(out_mod_efc_inter_resumen)
  efc_inter_pred <-  as.data.frame(out_mod_efc_inter_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_inter = mean-1) %>% 
    select(efc_inter)
  
  df <- bloqueo_df %>% cbind(efc_inter_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_inter)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos intercambiables: Y vs Y predicha ",var_in) )

  
  # PSEUDO R2

  
  modelo <- list()
  ##modelo["summary"] <-  as.matrix( out_mod_efc_con_resumen)
  modelo["n"] <- n
  modelo["modelo"] <- "Efectos intercambiables"
  modelo["var_in"]<-  var_in
  modelo["DIC"]<- out_mod_efc_inter_dic
  modelo["R2"]<-  cor(pull(bloqueo_df[var_in]),  pull(efc_inter_pred))^2
  
  print(paste ("Efectos intercambiables ",var_in))
  print(out_mod_efc_inter_resumen)
  print(ggp)
  
  return(modelo)

}
 
############################ EFECTOS INDEPENDIENTES CON VARIABLES   #####################
fun_efc_ind_con_var <- function(var_in, bloqueo_df, var_ini, n) {
  data<-list(
    "n"=n,
    "y"=pull(bloqueo_df[var_in]+1),
    "sexo"=bloqueo_df$Sexo,
    "edad"=bloqueo_df$GrupoEdad,
    "cancer"=bloqueo_df$TipoCancer2,
    "imc"=bloqueo_df$GrupoIMC, 
    "vasab" = var_ini+1)
  
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

  
  names <- rownames(out_mod_efc_ind_var_resumen)
  efc_ind_var_pred <-  as.data.frame(out_mod_efc_ind_var_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind_var = mean -1 ) %>% 
    select(efc_ind_var)
  
  df <- bloqueo_df %>% cbind(efc_ind_var_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_ind_var)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos independientes con variables: Y vs Y predicha ",var_in) )

  
  
  modelo <- list()
  modelo["n"] <- n
  modelo["modelo"] <- "Efectos independientes con variables"
  modelo["var_in"]<-  var_in
  modelo["DIC"]<- out_mod_efc_ind_var_dic
  modelo["R2"]<- cor(pull(bloqueo_df[var_in]),  pull(efc_ind_var_pred))^2

  
  print(paste ("Efectos independientes con variables:  ",var_in))
  print(out_mod_efc_ind_var_resumen)
  print(ggp)
  
  return(modelo)
}

############################ ITERACION SOBRE TODOS LOS MODELOS   #####################
vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()

modelos<- function( vect_names,bloqueo) {
  
  modelos_fin <- list()
  
  for (i in 1:length(vect_names)) {
    
    #Parametros de cada modelo  
    bloqueo_df <- funct_base(bloqueo, vect_names, i)
    n <- nrow(bloqueo_df)
    var_ini <- bloqueo_df$VAS_AB
    var_in <-vect_names[i]

    # Modelos
    modelo_con <- fun_efc_ctes(var_in, bloqueo_df, var_ini, n)
    nombre <- paste(modelo_con["modelo"], var_in )
    modelos_fin[[nombre]] <- modelo_con
     
    modelo_ind <- fun_efc_ind(var_in, bloqueo_df, var_ini, n)
    nombre <- paste(modelo_ind["modelo"], var_in )
    modelos_fin[[nombre]] <- modelo_ind
    
    modelo_inter <- fun_efc_int(var_in, bloqueo_df, var_ini, n)
    nombre <- paste(modelo_inter["modelo"], var_in )
    modelos_fin[[nombre]] <- modelo_inter
    
    modelo_ind_var <- fun_efc_ind_con_var(var_in, bloqueo_df, var_ini, n)
    nombre <- paste(modelo_ind_var["modelo"], var_in )
    modelos_fin[[nombre]] <- modelo_ind_var
  
  }
    modelos_fin
}

modelos_todos<- modelos ( vect_names,bloqueo)
#modelos_todos