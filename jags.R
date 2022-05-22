library(readr)
library(dplyr)
library(tidyr)
library(R2jags)
library(skimr)

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




############################ EFECTOS CONSTANTES   #####################

#-Defining data-
n<-nrow(bloqueo_df)

data<-list("n"=n,"y"=bloqueo_df$VAS_1M + 1,"vasab"= bloqueo_df$VAS_AB + 1)
data<-list("n"=n,"y"=bloqueo_df$VAS_1M,"sexo"=bloqueo_df$Sexo,"edad"=bloqueo_df$GrupoEdad,
            "cancer"=bloqueo_df$TipoCancer2,"imc"=bloqueo_df$GrupoIMC, "vasab" = bloqueo_df$VAS_AB)

#-Defining inits-
inits_efc_con <- function(){list(theta=1,yf1=rep(1,n) )}

#-Selecting parameters to monitor-
par_efc_con <-c("theta","yf1")
parsc<-c("theta","eta","yf1")
parsd<-c("alpha.adj","beta.adj","gama.adj","delta.adj","yf1")

#-Running code-
#OpenBUGS
mod_efc_con_bugs <- bugs(
  data,
  inits_efc_con,
  par_efc_con,
  model.file="modelcode/bloqueo_efc_con.txt",
  n.iter=50000,
  n.chains=2
  ,n.burnin=5000)

#JAGS
mod_efc_con_jags <-jags(
  data,
  inits_efc_con,
  par_efc_con,
  model.file="modelcode/bloqueo_efc_con.txt",
  n.iter=50000,
  n.chains=2,
  n.burnin=5000,
  n.thin=1)



# Resultado
mod_efc_con_jags

traceplot(mod_efc_con_jags)

out_mod_efc_con_simulaciones<- mod_efc_con_jags$BUGSoutput$sims.list


#JAGS
outa.sum<-mod_efc_con_jags$BUGSoutput$summary


#JAGS
outa.dic<-mod_efc_con_jags$BUGSoutput$DIC


print(outa.dic)

#Estimaciones
outa.p<-outa.sum[grep("theta",rownames(outa.sum)),]

#x vs. y
xmin<-0
xmax<-10
ymin<-0
ymax<-5
par(mfrow=c(1,1))
plot(leucemia$Obs/leucemia$Pops*10000,type="p",col="grey50",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
#
#Predictions
out.yf<-outa.sum[grep("^yf$",rownames(outa.sum)),]
y<- bloqueo_df$VAS_1M
or<-order(y)
ymin<-min(y,out.yf[,c(1,3,7)])
ymax<-max(y,out.yf[,c(1,3,7)])
par(mfrow=c(1,1))
plot(y[or],ylim=c(ymin,ymax))
lines(out.yf[or,1],lwd=2,col=2)
lines(out.yf[or,3],lty=2,col=2)
lines(out.yf[or,7],lty=2,col=2)




############################ EFECTOS    #####################


inits_efc_ind <- function(){list(theta=rep(1,n),yf1=rep(1,n))}
inits_efc_int <- function(){list(theta=rep(1,n),a=1,b=1,yf1=rep(1,n))}
inits_var_exp <- function(){list(alpha=0,beta=rep(0,2),gama=rep(0,2),delta=rep(0,2),yf1=rep(1,n))}













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












 