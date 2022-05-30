library(R2jags)
library(R2OpenBUGS)
library(readr)
library(skimr)
library(tidyverse)

bloqueo <- read_csv("data/BloqueoDatosPeriodo.csv", na = c(NA, "", "?"))

bloqueo %>% glimpse()
skimr::skim(bloqueo)

#bloqueo <-
bloqueo_h <- bloqueo %>%  
  mutate(TipoCancer2 = if_else(TipoCancer == "CACU", "CACU", "OTRO")) %>% 
  mutate_at("TipoCancer2", as.factor) %>% 
  select(-c(TipoCancer, UbicacionCancer, starts_with("MEDD"),
            ends_with("DIS"), starts_with("Kar"), starts_with("Efe"),
            starts_with("ECO"))) %>% 
  pivot_longer(cols = VAS_24:VAS_6M, names_to = "Time", values_to = "VAS") %>% 
  filter(!is.na(VAS)) %>% 
  mutate(
    Hrs = case_when(
      Time == "VAS_24" ~ 24,
      Time == "VAS_7D" ~ 24*7,
      Time == "VAS_1M" ~ 24*30,
      Time == "VAS_3M" ~ 24*90,
      Time == "VAS_6M" ~ 24*180
    )
  )

n = nrow(bloqueo_h)
y <- bloqueo_h$VAS_AB - bloqueo_h$VAS + 4
s1 <- bloqueo_h$Hrs
s2 <- rnorm(n)
s1f <- seq(24, 4320, 24*30)
m = length(s1f)
s2f <- rnorm(m)
data <- list("n" = n,"y" = y,"s1" = s1,"s2" = s2,"m" = m,"s1f" = s1f,"s2f" = s1f)

#-Defining inits-
inits <- function(){list(alpha = 0,tau = 1,w = rep(0, n),tau.w = 1,phi = 1,yf1 = rep(0, n),wf = rep(0, m),yf2 = rep(0, m))}

#-Selecting parameters to monitor-
parameters <- c("alpha", "tau", "w","tau.w", "phi", "yf1", "yf2")

#-Running code-
#OpenBUGS
ej11.sim<-R2OpenBUGS::bugs(
  data,
  inits,
  parameters,
  model.file="spatial_1.txt",
  n.iter=3000,
  n.chains=2,
  n.burnin=1000,
  n.thin=1
  )





bloqueo_h2 <- bloqueo_h %>% 
  distinct(VAS_AB, VAS, Hrs)
y <- bloqueo_h2$VAS_AB - bloqueo_h2$VAS
n <- nrow(bloqueo_h2)
d <- cbind(bloqueo_h2$Hrs, rnorm(n)) %>% dist() %>% as.matrix()
data <- list("n" = n,"y" = y,"d" = d, "vas_ab"=bloqueo_h2$VAS_AB)
inits <- function(){
  list(alpha = 0,theta=0,tau = 1,w = rep(0, n),tau.w = 1,phi = 1,yf1 = rep(0, n))
  }
parameters <- c("alpha","theta", "tau", "w","tau.w", "phi", "yf1")

  
ej11.sim<-jags(
  data,
  inits,
  parameters,
  model.file="spatial_2.txt",
  n.iter=3000,
  n.chains=2,
  n.burnin=500,
  n.thin=1
  )

saveRDS(ej11.sim, "models/spatial1.rds")
out.sum <- ej11.sim$BUGSoutput$summary
out.yf<-out.sum[grep("yf1",rownames(out.sum)),]
plot(out.yf[,1], bloqueo_h2$VAS)

# Resultado
  mod_efc_ind_jags <- ej11.sim
  
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
  
  df <- bloqueo_h2 %>% cbind(efc_ind_pred)
  
  ## COMPARACIONES
  ggp <- df %>% as_tibble() %>% 
    ggplot2::ggplot(aes(x=!!sym(var_in), y=efc_ind)) + 
    geom_point() +
    coord_equal() %>% 
    labs(x=var_in, title= paste ("Efectos independientes: Y vs Y predicha ",var_in) )

#######################################
#######################################

bloqueo_df <- bloqueo %>%  
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
  pivot_longer(cols = VAS_24:VAS_6M, names_to = "Time", values_to = "VAS") %>% 
  mutate(
    Hrs = case_when(
      Time == "VAS_24" ~ 24,
      Time == "VAS_7D" ~ 24*7,
      Time == "VAS_1M" ~ 24*30,
      Time == "VAS_3M" ~ 24*90,
      Time == "VAS_6M" ~ 24*180
    )
  ) %>% select(-Time)

y <- bloqueo_df$VAS
n <- nrow(bloqueo_df)
d <- cbind(bloqueo_df$Hrs, rnorm(n,0,2)) %>% dist() %>% as.matrix()

s1f<-seq(0, 24*180, 24*3)
m<-length(s1f)
set.seed(13795)
s2f<-rnorm(m)
vas_abf<-rep(7.333, m)
data <- list(
  "n" = n,
  "y" = y,
  #"d" = d, 
  "s1" = bloqueo_df$Hrs, 
  "s2" = rnorm(n,0,2),
  "vas_ab"=bloqueo_df$VAS_AB,
  "vas_abf"=vas_abf,
  "m" = m, "s1f" = s1f, "s2f" = s2f
  )
inits <- function(){
  list(alpha = 0,tau = 1,w = rep(0, n),tau.w = 1,phi = 1,
       yf1 = rep(0, n),yf2 = rep(0, m), wf = rep(0, m))
  }
parameters <- c("alpha","theta", "tau", "w","tau.w", "phi", "yf1", "yf2", "theta_f")
#DIC = 166.9
spatial_model<-bugs(
  data,
  inits,
  parameters,
  model.file="spatial_21.txt",
  n.iter=30000,
  n.chains=4,
  n.burnin=5000,
  n.thin=1
  )

spatial_model %>% saveRDS("models/spatial_model_31_pred.rds")

spatial_model$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,3) == "yf1") %>% 
  bind_cols(bloqueo_df) %>% 
  ggplot(aes(x=mean, y = VAS, colour=VAS_AB)) +
  geom_smooth(se=T, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `2.5%`),se=F, size = 0.5, alpha = 0.2) +
  geom_smooth(aes(y = `97.5%`),se=F, size = 0.5, alpha = 0.2) +
  geom_point() +
  xlab("EVA ajustado") +
  ylab("EVA Real") +
  ggtitle("Ajuste Vs Reales")


interp <- spatial_model$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta_") %>% 
  mutate(sample = 6, Hrs = s1f)
interp[1,2]<-0

spatial_model$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta[") %>% 
  bind_cols(bloqueo_df) %>% 
  mutate(sample = as.factor(rep(1:16,each=5))) %>% 
  ggplot(aes(x = Hrs, y = I(1-mean), group = sample, color = GrupoIMC)) +
  geom_line(data = interp, aes( x = Hrs, y = I(1-mean), group = sample), color = "black") +
  geom_point(data = interp, aes(x = Hrs, y = I(1-mean), group = sample), color = "black") +
  geom_line() +
  geom_point()


sobrepeso
Mujer
OTRO
Grupo2



