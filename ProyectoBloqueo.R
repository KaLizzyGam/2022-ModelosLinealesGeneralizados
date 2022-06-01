library(R2jags)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)
library(rlang)



#########################  DESCRIPTIVO TEMPORAL ################################
bloqueo_temp <- bloqueo %>%  
  mutate(
    
    GrupoEdad = if_else(Edad < 50, "Grupo1", "Grupo2"),
    GrupoIMC = case_when(
      IMC < 18.5 ~ "PorDebajo",
      IMC < 24.9 ~ "Saludable",
      T ~ "Sobrepeso"
    ),
    TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")
  ) %>% 
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor) %>% 
  mutate(
    VAS_3M = if_else(is.na(VAS_3M), VAS_1M, VAS_3M),
    VAS_6M = if_else(is.na(VAS_6M), VAS_3M, VAS_6M)
  ) %>% 
  group_by(Sexo, GrupoEdad, TipoCancer2, GrupoIMC) %>% 
  summarise(
    n = n(),
    VAS_AB = mean(VAS_AB, na.rm = T), 
    VAS_24 = mean(VAS_24, na.rm = T),
    VAS_7D = mean(VAS_7D, na.rm = T),
    VAS_1M = mean(VAS_1M, na.rm = T),
    VAS_3M = mean(VAS_3M, na.rm = T),
    VAS_6M = mean(VAS_6M, na.rm = T),
    .groups = "drop"
  ) %>% 
  pivot_longer(cols = VAS_AB:VAS_6M, names_to = "Time", values_to = "VAS") %>% 
  mutate(
    Hrs = case_when(
      Time == "VAS_AB" ~ 0,
      Time == "VAS_24" ~ 24*5,
      Time == "VAS_7D" ~ 24*10,
      Time == "VAS_1M" ~ 24*30,
      Time == "VAS_3M" ~ 24*90,
      Time == "VAS_6M" ~ 24*180
    )
  ) %>% select(-Time) %>% 
  mutate(sample = as.factor(rep(1:16,each=6)))


# Cancer

bloqueo_temp %>% 
  filter(Hrs == 0) %>% 
  group_by(TipoCancer2) %>% 
  summarise(n = sum(n)) %>% 
  ggplot( aes(x = TipoCancer2, y = n, fill = TipoCancer2)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = 2, colour = "white") +
  scale_fill_manual(values = c("#008600", "#41B7C4")) + 
  ggtitle("Número de pacientes por tipo de cáncer")


bloqueo_temp %>%  
  ggplot(aes(x = Hrs, y = VAS, group = sample, color = TipoCancer2)) +
  geom_line() +  geom_point() + 
  scale_color_manual(values = c("#008600", "#41B7C4")) + 
  ggtitle("Escala VAS por tipo de cáncer")

  
  
  
#Sexo

  bloqueo_temp %>% 
    filter(Hrs == 0) %>% 
    group_by(Sexo) %>% 
    summarise(n = sum(n)) %>% 
    ggplot( aes(x = Sexo, y = n, fill = Sexo)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label = n), vjust = 2, colour = "white") +
    scale_fill_manual(values = c("#1E90FF", "#CD6090")) + 
    ggtitle("Número de pacientes por sexo")
  
  
  bloqueo_temp %>%  
    ggplot(aes(x = Hrs, y = VAS, group = sample, color = Sexo)) +
    geom_line() +  geom_point() + 
    scale_color_manual(values = c("#1E90FF", "#CD6090")) + 
    ggtitle("Escala VAS por sexo")
  
  
  
  
#Grupo de edad

  bloqueo_temp %>% 
    filter(Hrs == 0) %>% 
    group_by(GrupoEdad) %>% 
    summarise(n = sum(n)) %>% 
    ggplot( aes(x = GrupoEdad, y = n, fill = GrupoEdad)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label = n), vjust = 2, colour = "white") +
    scale_fill_manual(values = c("#F76D5E", "#66CCFF")) + 
    ggtitle("Número de pacientes por grupo de edad")
  
  bloqueo_temp %>%  
    ggplot(aes(x = Hrs, y = VAS, group = sample, color = GrupoEdad)) +
    geom_line() +  geom_point() + 
    scale_color_manual(values = c("#F76D5E", "#66CCFF")) + 
    ggtitle("Escala VAS por grupo de edad")
  
  
  
#Grupo de IMC

  
  bloqueo_temp %>% 
    filter(Hrs == 0) %>% 
    group_by(GrupoIMC) %>% 
    summarise(n = sum(n)) %>% 
    ggplot( aes(x = GrupoIMC, y = n, fill = GrupoIMC)) +
    geom_bar(stat = "identity")+
    geom_text(aes(label = n), vjust = 2, colour = "white") +
    scale_fill_manual(values = c("#3F0F72", "#D8456C", "#FDB130")) + 
    ggtitle("Número de pacientes por IMC")  
  
  
  bloqueo_temp %>%  
    ggplot(aes(x = Hrs, y = VAS, group = sample, color = GrupoIMC)) +
    geom_line() +  geom_point() + 
    scale_color_manual(values = c("#3F0F72", "#D8456C", "#FDB130")) + 
    ggtitle("Escala VAS por IMC")
  

  
  

  
  
############################ CARGA DE DATOS #####################
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
  ) %>% 
  select(Sexo, GrupoEdad, GrupoIMC, TipoCancer2, starts_with("VAS"), -ends_with("DIS"))


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
  compilado <- as.data.frame(out_mod_efc_inter_resumen) %>% 
    cbind(names) %>% mutate (modelo = paste0 ("EfeInter",var_in))
  
  efc_inter_pred <-  compilado %>% filter(grepl('yf1', names))
  efc_inter_thehas <-  compilado %>% filter(grepl('theta', names))
                                           
                                           
  df <- bloqueo_df %>% cbind(efc_inter_pred) 
  
  ## COMPARACIONES
  df %>%
    ggplot2::ggplot(aes(x=VAS_AB, y=mean-1)) + 
    geom_point() +
    geom_smooth(se=T, size = 0.5, alpha = 0.2) +
    geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
    geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
    geom_abline(linetype = 2, color = "yellow") +
    geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
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






















############################  NUEVO AJUSTE   ###########################################
compilado <- tibble()

vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()
i = 5
bloqueo_df <- funct_base(bloqueo, vect_names, i)
n <- nrow(bloqueo_df)
var_ini <- bloqueo_df$VAS_AB
var_in <-vect_names[i]

######### INTERCAMBIABLES ####

#-Defining data-
data<-list("n"=n,"y"= pull(bloqueo_df[var_in]+1),"vasab"= var_ini+1 )
inits_efc_inter <-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
pars_inter <-c("theta","eta","yf1")

set.seed(1234)

#JAGS
mod_efc_inter_jags <-jags(
  data,
  inits_efc_inter,
  pars_inter,
  model.file="efc_inter_1.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)

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

out_mod_efc_inter_simulaciones <- mod_efc_inter_jags$BUGSoutput$sims.list
out_mod_efc_inter_resumen <- mod_efc_inter_jags$BUGSoutput$summary 
out_mod_efc_inter_dic <- mod_efc_inter_jags$BUGSoutput$DIC; 


names <- rownames(out_mod_efc_inter_resumen)
compilado_model <- as.data.frame(out_mod_efc_inter_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeInter",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_inter_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_inter_thehas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_inter_pred) 

## COMPARACIONES
df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos intercambiables: Y vs Y predicha ",var_in) ) 


efc_inter_thehas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos intercambiables: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_inter_resumen
out_mod_efc_inter_dic
cor(pull(bloqueo_df[var_in]) , pull(efc_inter_pred["mean"]))^2







######### CONSTANTES ####


#-Defining data-
data<-list("n"=n,"y"= pull(bloqueo_df[var_in] +1 ),"vasab"=var_ini +1 )
inits_efc_con <- function(){list(theta=1,yf1=rep(1,n)) }
par_efc_con <-c("theta","yf1")


set.seed(1234)

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
out_mod_efc_con_simulaciones <- mod_efc_con_jags$BUGSoutput$sims.list
out_mod_efc_con_resumen <- mod_efc_con_jags$BUGSoutput$summary 
out_mod_efc_con_dic <- mod_efc_con_jags$BUGSoutput$DIC; 


names <- rownames(out_mod_efc_con_resumen)
compilado_model <- as.data.frame(out_mod_efc_con_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeCon",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_inter_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_inter_thehas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_inter_pred) 

## COMPARACIONES
df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos constantes: Y vs Y predicha ",var_in) ) 


efc_inter_thehas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos constantes: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_con_resumen
out_mod_efc_con_dic
cor(pull(bloqueo_df[var_in]) , pull(efc_inter_pred["mean"]))^2





######### INDEPENDIENTES ####

#-Defining data-
data<-list("n"=n,"y"= pull(bloqueo_df[var_in] + 1)  ,"vasab"= var_ini +1 )
inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
pars_ind<-c("theta","yf1")


set.seed(1234)
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

out_mod_efc_ind_simulaciones <- mod_efc_ind_jags$BUGSoutput$sims.list
out_mod_efc_ind_resumen <- mod_efc_ind_jags$BUGSoutput$summary 
out_mod_efc_ind_dic <- mod_efc_ind_jags$BUGSoutput$DIC ; 


names <- rownames(out_mod_efc_ind_resumen)
compilado_model <- as.data.frame(out_mod_efc_ind_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeInter",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_inter_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_inter_thehas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_inter_pred) 

## COMPARACIONES
df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos independientes: Y vs Y predicha ",var_in) ) 


efc_inter_thehas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos independientes: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_ind_resumen
out_mod_efc_ind_dic
cor(pull(bloqueo_df[var_in]) , pull(efc_inter_pred["mean"]))^2


compilado %>% distinct(modelo)




######### INDEPENDIENTES CON VARIABLES ###########
#-Defining data-
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


set.seed(1234)
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
compilado_model <- as.data.frame(out_mod_efc_ind_var_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeInter",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_inter_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_inter_thehas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_inter_pred) 

## COMPARACIONES
df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos independientes: Y vs Y predicha ",var_in) ) 


efc_inter_thehas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos independientes: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_ind_resumen
out_mod_efc_ind_dic
cor(pull(bloqueo_df[var_in]) , pull(efc_inter_pred["mean"]))^2

