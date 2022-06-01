

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

compilado <- tibble()
medidas <- list()
vect_names <- bloqueo %>% select(starts_with("VAS")) %>% select(-ends_with("DIS"),-VAS_AB) %>% names()



############################  NUEVO AJUSTE   ###########################################


######### CONSTANTES ####
for (i in (1:5)){
  
bloqueo_df <- funct_base(bloqueo, vect_names, i)
n <- nrow(bloqueo_df)
var_ini <- bloqueo_df$VAS_AB
var_in <-vect_names[i]


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
  cbind(names) %>% mutate (modelo = paste0 ("EfeConstantes",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_thetas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_pred) 

## COMPARACIONES
com <- df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos constantes: Y vs Y predicha ",var_in) ) 
print(com)

tt<- efc_thetas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos constantes: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))
print( tt)

out_mod_efc_con_resumen
out_mod_efc_con_dic
out_mod_efc_con_cor <- cor(pull(bloqueo_df[var_in]), pull(efc_pred["mean"]))^2 ; out_mod_efc_con_cor

}

compilado %>% distinct(modelo)


######### INDEPENDIENTES ####
for (i in (1:5)){
  
  bloqueo_df <- funct_base(bloqueo, vect_names, i)
  n <- nrow(bloqueo_df)
  var_ini <- bloqueo_df$VAS_AB
  var_in <-vect_names[i]

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
  cbind(names) %>% mutate (modelo = paste0 ("EfeIndependientes",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_thetas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_pred) 

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


efc_thetas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos independientes: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_ind_resumen
out_mod_efc_ind_dic
out_mod_efc_ind_cor <- cor(pull(bloqueo_df[var_in]), pull(efc_pred["mean"]))^2 ; out_mod_efc_ind_cor


compilado %>% distinct(modelo)

}

compilado %>% distinct(modelo)

######### INDEPENDIENTES CON VARIABLES ###########

for (i in (1:5)){
  
  bloqueo_df <- funct_base(bloqueo, vect_names, i)
  n <- nrow(bloqueo_df)
  var_ini <- bloqueo_df$VAS_AB
  var_in <-vect_names[i]

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
out_mod_efc_ind_var_simulaciones <- mod_efc_ind_var_jags$BUGSoutput$sims.list
out_mod_efc_ind_var_resumen <- mod_efc_ind_var_jags$BUGSoutput$summary 
out_mod_efc_ind_var_dic <- mod_efc_ind_var_jags$BUGSoutput$DIC


names <- rownames(out_mod_efc_ind_var_resumen)
compilado_model <- as.data.frame(out_mod_efc_ind_var_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeIndependientesVariables",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_thetas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_pred) 

## COMPARACIONES
df %>%
  ggplot2::ggplot(aes(x=!!sym(var_in), y=mean-1)) + 
  geom_point() +
  geom_abline(linetype = 2, color = "red") +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`-1),se=F, size = 0.5, alpha = 0.2) +
  #geom_errorbar(aes(ymin=`2.5%` -1, ymax=`97.5%`-1), width=0.05, size=0.25, color="blue") +
  labs(x=var_in, title= paste ("Efectos independientes c/ variables: Y vs Y predicha ",var_in) ) 


efc_thetas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos independientes c/ variables: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))

out_mod_efc_ind_var_resumen
out_mod_efc_ind_var_dic
out_mod_efc_ind_var_cor <- cor(pull(bloqueo_df[var_in]), pull(efc_pred["mean"]))^2 ; out_mod_efc_ind_var_cor


}

compilado %>% distinct(modelo)

######### INTERCAMBIABLES ####

for (i in 1:2) {
bloqueo_df <- funct_base(bloqueo, vect_names, i)
n <- nrow(bloqueo_df)
var_ini <- bloqueo_df$VAS_AB
var_in <-vect_names[i]



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

#mod_efc_inter_jags <-jags(
#  data,
#  inits_efc_inter,
#  pars_inter,
#  model.file="efc_inter.txt",
#  n.iter=50000,
#  n.chains=2,
#  n.burnin=5000,
#  n.thin=1)

# Resultados

out_mod_efc_inter_simulaciones <- mod_efc_inter_jags$BUGSoutput$sims.list
out_mod_efc_inter_resumen <- mod_efc_inter_jags$BUGSoutput$summary 
out_mod_efc_inter_dic <- mod_efc_inter_jags$BUGSoutput$DIC; 


names <- rownames(out_mod_efc_inter_resumen)
compilado_model <- as.data.frame(out_mod_efc_inter_resumen) %>% 
  cbind(names) %>% mutate (modelo = paste0 ("EfeIntercambiables",var_in))

compilado <- compilado %>% bind_rows(compilado_model)

efc_pred <-  compilado_model %>% filter(grepl('yf1', names))
efc_thetas <-  compilado_model %>% filter(grepl('theta', names))


df <- bloqueo_df %>% cbind(efc_pred) 

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


efc_thetas %>% 
  ggplot2::ggplot(aes(x =names,  y = mean)) +
  geom_point()+
  geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
  labs(x=var_in, title= paste ("Efectos intercambiables: Theta's",var_in) ) +
  scale_x_discrete(guide = guide_axis(angle = 90))


out_mod_efc_inter_resumen
out_mod_efc_inter_dic
out_mod_efc_inter_cor <- cor(pull(bloqueo_df[var_in]), pull(efc_pred["mean"]))^2 ; out_mod_efc_inter_cor

}


for (i in 3:5) {
  bloqueo_df <- funct_base(bloqueo, vect_names, i)
  n <- nrow(bloqueo_df)
  var_ini <- bloqueo_df$VAS_AB
  var_in <-vect_names[i]
  
  
  
  #-Defining data-
  data<-list("n"=n,"y"= pull(bloqueo_df[var_in]+1),"vasab"= var_ini+1 )
  inits_efc_inter <-function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
  pars_inter <-c("theta","eta","yf1")
  
  set.seed(1234)
  
  ##JAGS
  #mod_efc_inter_jags <-jags(
  #  data,
  #  inits_efc_inter,
  #  pars_inter,
  #  model.file="efc_inter_1.txt",
  #  n.iter=50000,
  #  n.chains=2,
  #  n.burnin=5000,
  #  n.thin=1)
  
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
    cbind(names) %>% mutate (modelo = paste0 ("EfeIntercambiables",var_in))
  
  compilado <- compilado %>% bind_rows(compilado_model)
  
  efc_pred <-  compilado_model %>% filter(grepl('yf1', names))
  efc_thetas <-  compilado_model %>% filter(grepl('theta', names))
  
  
  df <- bloqueo_df %>% cbind(efc_pred) 
  
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
  
  
  efc_thetas %>% 
    ggplot2::ggplot(aes(x =names,  y = mean)) +
    geom_point()+
    geom_errorbar(aes(ymin=`2.5%` , ymax=`97.5%`), width=0.05, size=0.25, color="blue")+
    labs(x=var_in, title= paste ("Efectos intercambiables: Theta's",var_in) ) +
    scale_x_discrete(guide = guide_axis(angle = 90))
  
  
  out_mod_efc_inter_resumen
  out_mod_efc_inter_dic
  out_mod_efc_inter_cor <- cor(pull(bloqueo_df[var_in]), pull(efc_pred["mean"]))^2 ; out_mod_efc_inter_cor
  
}

compilado %>% distinct(modelo)

########### PARTE FINAL ############


compilado %>% 
  filter(grepl('VAS_6M', modelo),
         grepl('theta', names)) %>% 
  ggplot2::ggplot(aes(x =names,  y = 1-mean, color = modelo )) +
  geom_errorbar(aes(ymin=1-`2.5%` , ymax=1-`97.5%`, size=modelo), size=.7 ) +
  geom_point(aes(size = .5)) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  #scale_y_continuous(limits=c(-0.05, 1), breaks = 10) +
  ylab("1 - theta")+
  ggtitle("Compración de tasas de reducción de VAS")

  











objetos<-list(
  resumen = out_mod_efc_ind_var_resumen,
  predicciones = df ,
  thetas = efc_thetas,
  DIC = out_mod_efc_ind_var_dic, 
  R2 = out_mod_efc_ind_var_cor)

