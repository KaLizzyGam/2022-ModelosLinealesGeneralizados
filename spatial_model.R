library(R2jags)
library(R2OpenBUGS)
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(ggplot2)

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
s1f <- seq(24, 4320, 24)
s2f <- rnorm(m)
m = length(s1f)
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





d <- cbind(bloqueo_h$Hrs, rep(0,n)) %>% 
  dist() %>% as.matrix()

data <- list(
  "n" = n,
  "y" = bloqueo_h$VAS,
  "H" = d,
  "m" = m,
  "s1f" = s1f,
  "s2f" = rep(0, m)
  )
  
ej11.sim<-jags(
  data,
  inits,
  parameters,
  model.file="spatial_2.txt",
  n.iter=5000,
  n.chains=2,
  n.burnin=1000,
  n.thin=1
  )




















