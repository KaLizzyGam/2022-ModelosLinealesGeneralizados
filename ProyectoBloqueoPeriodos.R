library(R2jags)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)

# Cargando datos
bloqueo <- read_csv("data/BloqueoDatosPeriodo.csv")
bloqueo
View(bloqueo)
# EDA
bloqueo %>% glimpse()
skimr::skim(bloqueo)

# Seleccionando filas sin NA
bloqueo_sin_na <- na.omit(bloqueo)
bloqueo_sin_na
View(bloqueo_sin_na)

# Agrupando datos
bloqueo_sin_na <-bloqueo_sin_na %>%  
  mutate(
    GrupoEdad = if_else(Edad < 50, "Grupo1", "Grupo2"),
    VAS_6M = as.numeric(VAS_6M),
    GrupoIMC = case_when(
      IMC < 18.5 ~ "PorDebajo",
      IMC < 24.9 ~ "Saludable",
      T ~ "Sobrepeso"
    ),
    TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")
  ) %>% 
  mutate_at(vars(Sexo, GrupoEdad, TipoCancer2,GrupoIMC), as.factor)
View(bloqueo_sin_na)
bloqueo_sin_na %>% glimpse()

bloqueo_df <-bloqueo_sin_na %>%  
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
    VAS_24 = mean(VAS_24, na.rm = T),
    VAS_7D = mean(VAS_7D, na.rm = T),
    VAS_7D = mean(VAS_7D, na.rm = T),
    VAS_1M = mean(VAS_1M, na.rm = T),
    VAS_3M = mean(VAS_3M, na.rm = T),
    VAS_6M = mean(VAS_6M, na.rm = T),
    .groups = "drop"
  ) %>% 
  arrange(n)

View(bloqueo_df)

bloqueo_df %>% glimpse()

n<-nrow(bloqueo_df); n

############################ DESCRIPTIVO #########
# bloqueo_df %>% 
#   ggplot(aes(x=VAS_AB)) +
#   geom_histogram(colour = 5, fill = "white", alpha = 0.75,
#                  position = "identity", bins = 25)
# 
# bloqueo %>% 
#   ggplot(aes(x=VAS_AB, y= VAS_1M, color = Sexo)) +
#   geom_point( alpha = 0.75)
# 
# bloqueo %>% 
#   ggplot(aes(x=VAS_AB, y= VAS_1M, color = Sexo, size=GrupoEdad)) +
#   geom_point()
# 
# bloqueo_df$VAS_AB

############################ EFECTOS CONSTANTES   #####################
dic_efc_ctes1 <- c("DIC 1:")
pR2_efc_ctes1 <- c("pR2 1:")

fun_efc_ctes <- function(var_ini, var_fin, n, df, labelx, title_grap) {
  #-Defining data-
  data<-list("n"=n,"y"= var_fin,"vasab"=var_ini)
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
  print("DIC:")
  print(out_mod_efc_con_dic)
  
  dic_efc_ctes1 <<- c(dic_efc_ctes1,out_mod_efc_con_dic)
  
  names <- rownames(out_mod_efc_con_resumen)
  efc_con_pred <-  as.data.frame(out_mod_efc_con_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_con = mean) %>% 
    select(efc_con)
  
  df <- df %>% cbind(efc_con_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=var_fin, y=efc_con)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=labelx, title=title_grap)
  
  out.yf<-out_mod_efc_con_resumen[grep("yf",rownames(out_mod_efc_con_resumen)),]
  plot(var_fin,out.yf[,1],xlab=labelx)
  title("Efectos constantes: Y vs Y predicha")
  R2<-(cor(var_fin,out.yf[,1]))^2
  print("R2")
  print(R2)
  pR2_efc_ctes1 <<- c(pR2_efc_ctes1,R2)
}

# 24 hrs
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos constantes a las 24 hrs.")
# 7 días
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos constantes a los 7 días")
# 1 mes
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos constantes al mes")
# 3 meses
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos constantes a los 3 meses")
# 6 meses
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos constantes a los 6 meses")

var_ini <- bloqueo_df$VAS_AB
df <- bloqueo_df
n <- nrow(bloqueo_df)
dic_efc_ctes2 <- c("DIC 2:")
pR2_efc_ctes2 <- c("pR2 2:")
View(df)

fun_efc_ctes2 <- function(var_in) {
  #-Defining data-
  data<-list("n"=n,"y"= df[[var_in[1]]],"vasab"=var_ini)
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
  print("DIC:")
  print(out_mod_efc_con_dic)
  dic_efc_ctes2 <<- c(dic_efc_ctes2,out_mod_efc_con_dic)
  
  names <- rownames(out_mod_efc_con_resumen)
  efc_con_pred <-  as.data.frame(out_mod_efc_con_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_con = mean) %>% 
    select(efc_con)
  
  df <- df %>% cbind(efc_con_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=df[[var_in[1]]], y=efc_con)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=var_in[1], title=var_in[2])
}


# CON MAP
var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos constantes a las 24 hrs.","Efectos constantes a los 7 días","Efectos constantes al mes","Efectos constantes a los 3 meses","Efectos constantes a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
View(df_param)

apply(X = df_param, MARGIN = 1, FUN = fun_efc_ctes2)

print("Efectos constantes DIC:")
print(dic_efc_ctes1)
print(dic_efc_ctes2)

print("Efectos constantes pR2:")
print(pR2_efc_ctes1)
print(pR2_efc_ctes2)

############################ EFECTOS INDEPENDIENTES  #####################
dic_efc_ind1 <- c("DIC 1:")

fun_efc_ind <- function(var_ini, var_fin, n, df, labelx, title_grap) {
  #-Defining data-
  data<-list("n"=n,"y"= var_fin ,"vasab"= var_ini)
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
  print("DIC:")
  print(out_mod_efc_ind_dic)
  dic_efc_ind1 <<- c(dic_efc_ind1,out_mod_efc_ind_dic)
  
  names <- rownames(out_mod_efc_ind_resumen)
  efc_ind_pred <-  as.data.frame(out_mod_efc_ind_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind = mean) %>% 
    select(efc_ind)
  
  df <- df %>% cbind(efc_ind_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=var_fin, y=efc_ind)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=labelx, title=title_grap)
}


# 24 hrs
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos independientes a las 24 hrs.")
# 7 días
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos independientes a los 7 días")
# 1 mes
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos independientes al mes")
# 3 meses
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos independientes a los 3 meses")
# 6 meses
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos independientes a los 6 meses")

# MAP
dic_efc_ind2 <- c("DIC 2:")

fun_efc_ind2 <- function(var_in) {
  #-Defining data-
  data<-list("n"=n,"y"= df[[var_in[1]]] ,"vasab"= var_ini)
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
  print("DIC:")
  print(out_mod_efc_ind_dic)
  dic_efc_ind2 <<- c(dic_efc_ind2,out_mod_efc_ind_dic)
  
  names <- rownames(out_mod_efc_ind_resumen)
  efc_ind_pred <-  as.data.frame(out_mod_efc_ind_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind = mean) %>% 
    select(efc_ind)
  
  df <- df %>% cbind(efc_ind_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=df[[var_in[1]]], y=efc_ind)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=var_in[1], title=var_in[2])
}


var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos independientes a las 24 hrs.","Efectos independientes a los 7 días","Efectos independientes al mes","Efectos independientes a los 3 meses","Efectos independientes a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
View(df_param)

apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind2)

print("Efectos independientes")
print(dic_efc_ind1)
print(dic_efc_ind2)


############################ EFECTOS INTERCAMBIABLES   #####################
dic_efc_int1 <- c("DIC 1:")

fun_efc_int <- function(var_ini, var_fin, n, df, labelx, title_grap) {
  #-Defining data-
  data<-list("n"=n,"y"= var_fin + 1,"vasab"= var_ini + 1)
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
  print("DIC:")
  print(out_mod_efc_inter_dic)
  dic_efc_int1 <<- c(dic_efc_int1,out_mod_efc_inter_dic)
  
  names <- rownames(out_mod_efc_inter_resumen)
  efc_inter_pred <-  as.data.frame(out_mod_efc_inter_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_inter = mean) %>% 
    select(efc_inter)
  
  df <- df %>% cbind(efc_inter_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=var_fin, y=efc_inter)) + 
    geom_jitter()+
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=labelx, title=title_grap)
}

# 24 hrs
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos intercambiables a las 24 hrs.")
# 7 días
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos intercambiables a los 7 días")
# 1 mes
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos intercambiables al mes")
# 3 meses
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos intercambiables a los 3 meses")
# 6 meses
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos intercambiables a los 6 meses")

# MAP
dic_efc_int2 <- c("DIC 2:")

fun_efc_int2 <- function(var_in) {
  #-Defining data-
  data<-list("n"=n,"y"= df[[var_in[1]]] + 1,"vasab"= var_ini + 1)
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
  print("DIC:")
  print(out_mod_efc_inter_dic)
  dic_efc_int2 <<- c(dic_efc_int2,out_mod_efc_inter_dic)
  
  names <- rownames(out_mod_efc_inter_resumen)
  efc_inter_pred <-  as.data.frame(out_mod_efc_inter_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_inter = mean) %>% 
    select(efc_inter)
  
  df <- df %>% cbind(efc_inter_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=df[[var_in[1]]], y=efc_inter)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=var_in[1], title=var_in[2])
}

var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos intercambiables a las 24 hrs.","Efectos intercambiables a los 7 días","Efectos intercambiables al mes","Efectos intercambiables a los 3 meses","Efectos intercambiables a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
View(df_param)

apply(X = df_param, MARGIN = 1, FUN = fun_efc_int2)

print("Efectos intercambiables")
print(dic_efc_int1)
print(dic_efc_int2)

############################ EFECTOS INDEPENDIENTES CON VARIABLES   #####################
dic_efc_ind_con_var1 <- c("DIC 1:")

fun_efc_ind_con_var <- function(var_ini, var_fin, n, df, labelx, title_grap) {
  data<-list(
    "n"=n,
    "y"=var_fin,
    "sexo"=df$Sexo,
    "edad"=df$GrupoEdad,
    "cancer"=df$TipoCancer2,
    "imc"=df$GrupoIMC, 
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
  print("DIC:")
  print(out_mod_efc_ind_var_dic)
  dic_efc_ind_con_var1 <<- c(dic_efc_ind_con_var1,out_mod_efc_ind_var_dic)
  
  names <- rownames(out_mod_efc_ind_var_resumen)
  efc_ind_var_pred <-  as.data.frame(out_mod_efc_ind_var_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind_var = mean) %>% 
    select(efc_ind_var)
  
  df <- df %>% cbind(efc_ind_var_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=var_fin, y=efc_ind_var)) + 
    geom_jitter()+
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=labelx, title=title_grap)
}

# 24 hrs
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos independientes con variables a las 24 hrs.")
# 7 días
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos independientes con variable a los 7 días")
# 1 mes
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos independientes con variable al mes")
# 3 meses
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos independientes con variable a los 3 meses")
# 6 meses
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos independientes con variable a los 6 meses")

# MAP
dic_efc_ind_con_var2 <- c("DIC 2:")

fun_efc_ind_con_var2 <- function(var_in) {
  data<-list(
    "n"=n,
    "y"=df[[var_in[1]]],
    "sexo"=df$Sexo,
    "edad"=df$GrupoEdad,
    "cancer"=df$TipoCancer2,
    "imc"=df$GrupoIMC, 
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
  print("DIC:")
  print(out_mod_efc_ind_var_dic)
  dic_efc_ind_con_var2 <<- c(dic_efc_ind_con_var2,out_mod_efc_ind_var_dic)
  
  names <- rownames(out_mod_efc_ind_var_resumen)
  efc_ind_var_pred <-  as.data.frame(out_mod_efc_ind_var_resumen) %>% 
    cbind(names)  %>% 
    tibble() %>% 
    select(mean, names) %>% 
    filter(grepl('yf1', names)) %>% 
    mutate(efc_ind_var = mean) %>% 
    select(efc_ind_var)
  
  df <- df %>% cbind(efc_ind_var_pred)
  
  ## COMPARACIONES
  df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=df[[var_in[1]]], y=efc_ind_var)) + 
    geom_point() +
    ylim( y = c(1,5)) +
    xlim(c(1,5)) %>% labs(x=var_in[1], title=var_in[2])
}


var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos independientes con variables a las 24 hrs.",
         "Efectos independientes con variables a los 7 días",
         "Efectos independientes con variables al mes",
         "Efectos independientes con variables a los 3 meses",
         "Efectos independientes con variables a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
View(df_param)

apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind_con_var2)

print("Efectos independientes con variables")
print(dic_efc_ind_con_var1)
print(dic_efc_ind_con_var2)



# CORRIDA COMPLETA
# Efectos constantes ----------------------------------------------------------------------------------------------------------------
dic_efc_ctes1 <- c("DIC 1:")
# 24 hrs
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos constantes a las 24 hrs.")
# 7 días
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos constantes a los 7 días")
# 1 mes
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos constantes al mes")
# 3 meses
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos constantes a los 3 meses")
# 6 meses
fun_efc_ctes(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos constantes a los 6 meses")

# CON MAP
dic_efc_ctes2 <- c("DIC 2:")
var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos constantes a las 24 hrs.","Efectos constantes a los 7 días","Efectos constantes al mes","Efectos constantes a los 3 meses","Efectos constantes a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ctes2)

print("Efectos constantes")
print(dic_efc_ctes1)
print(dic_efc_ctes2)

# Efectos independientes ----------------------------------------------------------------------------------------------------------------
dic_efc_ind1 <- c("DIC 1:")
# 24 hrs
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos independientes a las 24 hrs.")
# 7 días
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos independientes a los 7 días")
# 1 mes
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos independientes al mes")
# 3 meses
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos independientes a los 3 meses")
# 6 meses
fun_efc_ind(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos independientes a los 6 meses")

# CON MAP
dic_efc_ind2 <- c("DIC 2:")
var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos independientes a las 24 hrs.","Efectos independientes a los 7 días","Efectos independientes al mes","Efectos independientes a los 3 meses","Efectos independientes a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind2)

print("Efectos independientes")
print(dic_efc_ind1)
print(dic_efc_ind2)

# Efectos intercambiables ----------------------------------------------------------------------------------------------------------------
dic_efc_int1 <- c("DIC 1:")
# 24 hrs
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos intercambiables a las 24 hrs.")
# 7 días
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos intercambiables a los 7 días")
# 1 mes
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos intercambiables al mes")
# 3 meses
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos intercambiables a los 3 meses")
# 6 meses
fun_efc_int(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos intercambiables a los 6 meses")

# MAP
dic_efc_int2 <- c("DIC 2:")
var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos intercambiables a las 24 hrs.","Efectos intercambiables a los 7 días","Efectos intercambiables al mes","Efectos intercambiables a los 3 meses","Efectos intercambiables a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_int2)

print("Efectos intercambiables")
print(dic_efc_int1)
print(dic_efc_int2)

# Efectos independientes con variables ----------------------------------------------------------------------------------------------------------------
dic_efc_ind_con_var1 <- c("DIC 1:")
# 24 hrs
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_24, n, bloqueo_df, "VAS_24", "Efectos independientes con variables a las 24 hrs.")
# 7 días
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_7D, n, bloqueo_df, "VAS_7D", "Efectos independientes con variable a los 7 días")
# 1 mes
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_1M, n, bloqueo_df, "VAS_1M", "Efectos independientes con variable al mes")
# 3 meses
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_3M, n, bloqueo_df, "VAS_3M", "Efectos independientes con variable a los 3 meses")
# 6 meses
fun_efc_ind_con_var(bloqueo_df$VAS_AB, bloqueo_df$VAS_6M, n, bloqueo_df, "VAS_6M", "Efectos independientes con variable a los 6 meses")

# MAP
dic_efc_ind_con_var2 <- c("DIC 2:")
var_f <- c("VAS_24","VAS_7D","VAS_1M","VAS_3M","VAS_6M")
t_g <- c("Efectos independientes con variables a las 24 hrs.",
         "Efectos independientes con variables a los 7 días",
         "Efectos independientes con variables al mes",
         "Efectos independientes con variables a los 3 meses",
         "Efectos independientes con variables a los 6 meses")

# Data frame
df_param <- data.frame(var_fin = var_f, title_grap = t_g)
apply(X = df_param, MARGIN = 1, FUN = fun_efc_ind_con_var2)

print("Efectos independientes con variables")
print(dic_efc_ind_con_var1)
print(dic_efc_ind_con_var2)

 

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




