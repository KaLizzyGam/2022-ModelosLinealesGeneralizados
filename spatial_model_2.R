library(R2jags)
library(R2OpenBUGS)
library(readr)
library(skimr)
library(tidyverse)

bloqueo <- read_csv("data/BloqueoDatosPeriodo.csv", na = c(NA, "", "?"))

bloqueo %>% glimpse()
skimr::skim(bloqueo)

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


# Variograma
bloqueo_df2<-bloqueo_df
bloqueo_df2$Hrs2 <- rnorm(80)
coordinates(bloqueo_df2)=~Hrs+Hrs2
bloqueo_df_var<-variogram(VAS~Hrs+Hrs2, bloqueo_df2)
plot(bloqueo_df_var)


y <- bloqueo_df$VAS
n <- nrow(bloqueo_df)
d <- cbind(bloqueo_df$Hrs, rnorm(n,0,2)) %>% dist() %>% as.matrix()

s1f<-seq(0, 24*180, 24*3)
m<-length(s1f)
set.seed(13795)
s2f<-rnorm(m)
vas_abf<-rep(7.333, m)

data <- list(
  "n" = n, "y" = y, #"d" = d, 
  "s1" = bloqueo_df$Hrs, 
  "s2" = rnorm(n,0,2),
  "vas_ab"=bloqueo_df$VAS_AB,
  "vas_abf"=vas_abf,
  "sexo"=bloqueo_df$Sexo %>% as.factor() %>% droplevels() %>% as.integer(),
  "sexof"=rep(1, m),
  "edad"=bloqueo_df$GrupoEdad %>% as.factor() %>% droplevels() %>% as.integer(),
  "edadf"=rep(1, m),
  "cancer"=bloqueo_df$TipoCancer2 %>% as.factor() %>% droplevels() %>% as.integer(),
  "cancerf"=rep(1, m),
  "imc"=bloqueo_df$GrupoIMC %>% as.factor() %>% droplevels() %>% as.integer(),
  "imcf"=rep(1, m),
  "m" = m, "s1f" = s1f, "s2f" = s2f
  )

inits <- function(){
  list(
    alpha = 0, tau = 1, w = rep(0, n), tau.w = 1, phi = 1,
    beta_sexo=rep(1,2), beta_edad=rep(1,2), beta_cancer=rep(1,2), beta_imc=rep(1,3),
    yf1 = rep(0, n), yf2 = rep(0, m), wf = rep(0, m)
    )
  }
parameters <- c("alpha","theta", "tau", "w","tau.w", "phi", "yf1", "yf2", "theta_f")
# DIC 174.6
spartial_model_32 <- bugs(
  data,
  inits,
  parameters,
  model.file="spatial_23.txt",
  n.iter=30000,
  n.chains=4,
  n.burnin=5000,
  n.thin=1
  )

spartial_model_32 %>% saveRDS("models/spatial_model_3_pred_cov.rds")

spartial_model_32$summary %>% 
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

spartial_model_32$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta[") %>% 
  bind_cols(bloqueo_df) %>% 
  mutate(sample = as.factor(rep(1:16,each=5))) %>% 
  ggplot(aes(x = Hrs, y = mean, group = sample, color = GrupoIMC)) +
  geom_line() +
  geom_point()

spartial_model_32$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta[") %>% 
  bind_cols(bloqueo_df)

spartial_model_32$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta_") %>% 
  bind_cols(Hrs = s1f) %>% 
  #mutate(sample = as.factor(rep(1:16,each=5))) %>% 
  ggplot(aes(x = Hrs, y = I(1-mean))) +
  geom_line() +
  geom_point()






interp <- spartial_model_32$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta_") %>% 
  mutate(sample = 6, Hrs = s1f)
interp[1,2]<-0

spartial_model_32$summary %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "parameter") %>% 
  as_tibble() %>% 
  filter(str_sub(parameter,1,6) == "theta[") %>% 
  bind_cols(bloqueo_df) %>% 
  mutate(sample = as.factor(rep(1:16,each=5))) %>% 
  ggplot(aes(x = Hrs, y = I(1-mean), group = sample, color = GrupoIMC)) +
  geom_line() +
  geom_point() +
  geom_line(data = interp, aes( x = Hrs, y = I(1-mean), group = sample), color = "black") +
  geom_point(data = interp, aes(x = Hrs, y = I(1-mean), group = sample), color = "black")





