################################################################################
############# Análise estatástica do modelo de dados longitudinal ##############
################################################################################
## Resumo: O estudo consite uma linha de base com in??cio na semana 0 até a 6 semana
## fazendo meditódologia de eletroenfacelograma computado em uma matriz 14 por 45 numa 
## escala de frequência de theta (0-7Hz) e alpha (8-14Hz).
## Assim,como o estudo é clínico aleatorizado, teremos que alanalisar 
## evolução do comportamento ao longo do tempo, Obejtivos (1) comparar os grupos
## placebo e controle, (2) avaliar o comportamento temporal
.libPaths("/home/alex/R") #quando n??o conseguir a baixar pacotes no R
rm(list=ls(all=TRUE)) # limpar a console
## Biblioteca para ser usada do projeto
library(nlme)
library(tidyverse)
library(dplyr)
library(plyr)
# Abaixando a base de dados no Rstudio
load("/home/alex/Downloads/Early.RData")
# Variaveis que será usado nas an??lises do modelos de dados longitudinal
n               # (número de pacientes)
y               # variável resposta
eeg_tnsr        # array que contém os EEG (14 EEG para cada paciente registrados em 45 frequ??ncias de 16 a 60Hz)
scalar_all      # data frame que contém todas as variáveis e covariáveis escalares
cov             # cont??m somente as covariáveis de sexo e cronicidade

# Descrição geral do banco de dados 
dados =  scalar_all %>% as_tibble() %>%
  mutate(semana1=hamd_17_score_eval-hamd_17_score_wk1,
         semana2=hamd_17_score_eval-hamd_17_score_wk2,
         semana3=hamd_17_score_eval-hamd_17_score_wk3,
         semana4=hamd_17_score_eval-hamd_17_score_wk4,
         semana6=hamd_17_score_eval-hamd_17_score_wk6,
         semana8=hamd_17_score_eval-hamd_17_score_wk8,
         ID=1:96) %>%
  replace(is.na(.),0)

dadoscalcudado <- dados %>% 
  transmute(semana1=hamd_17_score_eval-hamd_17_score_wk1,
            semana2=hamd_17_score_eval-hamd_17_score_wk2,
            semana3=hamd_17_score_eval-hamd_17_score_wk3,
            semana4=hamd_17_score_eval-hamd_17_score_wk4,
            semana6=hamd_17_score_eval-hamd_17_score_wk6,
            semana8=hamd_17_score_eval-hamd_17_score_wk8) %>%
  replace(is.na(.),0)
  
summary(dadoscalcudado$semana1)
summary(dadoscalcudado$semana2)
summary(dadoscalcudado$semana3)
summary(dadoscalcudado$semana4)
summary(dadoscalcudado$semana6)
summary(dadoscalcudado$semana8)
# Boxplot para cada semana 
# como o boxlplot n??o considera a estrutur longitudinal por idividuo
# teremos que tomar cuidade nesta an??lise.
boxplot(dadoscalcudado$semana1,dadoscalcudado$semana2,
        dadoscalcudado$semana3,dadoscalcudado$semana4,
        dadoscalcudado$semana6,dadoscalcudado$semana8,ylab="Teste clínico",xlab="Semanas")
axis(1, 1:6, c(1,2,3,4,5,6))
# Descrição por grupo
summary(subset(dados, TreatmentCode=="PLA")[,26:26])
summary(subset(dados, TreatmentCode=="SER/CIT")[,26:26])
#       Explorando a Estrutura de Variancia
#=======================================================================
#
# Avaliacao da correlacao geral e por grupo no formato largo
# matriz de correlacao dos dados
round(cor(dadoscalcudado),3)
round(cov(dadoscalcudado),3)
#
# Existe uma indicacao de similar correlacao em cada grupo, e maior no grupo Placebo.

######################### O tempo está ok ####################################################
# Para cada grupo de 6 temos uma individuo  que é percorrido o i, sendo Individuo Y_i, onde i=1,...,
individuo<- rep(c(1:96),1) %>% as_tibble()
individuo 
individ <- data.frame(rep(1:6,96),rep(1:6,96),rep(1:6,96),rep(1:6,96),rep(1:6,96),rep(1:6,96)) %>% 
  as_tibble() %>%
  pivot_longer(cols = c( rep.1.6..96. ,
                         rep.1.6..96..1,
                         rep.1.6..96..2 ,
                         rep.1.6..96..3,
                         rep.1.6..96..4 ,
                         rep.1.6..96..5), names_to= "Individuos",
               values_to = "Valores") %>% select(Valores)%>%
  rename(Id= Valores)


#individ <- data.frame(rep(1:6,1),rep(1:6,1),rep(1:6,1),rep(1:6,1),rep(1:6,1),rep(1:6,1)) %>% 
#  as_tibble() %>%
#  for (i in 1:length(trat)) {
#    indi= rep(trat[,1],96)
#    return(data.frame(indi))
#  }
#indi
individ

## t
ti<-rep(1:6,96) %>% as_tibble() %>% rename(tjj=value)
ti
#---------------------------------------------------------------------------------------------

############# O tratamento está ok ############################################################
# Para cada 6 sequência, os tratamenos irá corresponder um individuo, trat_i, onde i=1,...,6
trat1=dados %>% transmute(TreatmentCode= ifelse(TreatmentCode=="PLA",1,0)) 
trat=trat1[1]
trat
for (i in 1:length(trat)) {
  tratm= rep(trat[,1],6)
  return(data.frame(tratm))
}
tratm1=tratm %>% data.frame() %>% as_tibble() %>%
  transmute(TreatmentCode1=TreatmentCode,
            TreatmentCode2=TreatmentCode.1,
            TreatmentCode3=TreatmentCode.2,
            TreatmentCode4=TreatmentCode.3,
            TreatmentCode5=TreatmentCode.4,
            TreatmentCode6=TreatmentCode.5)%>%
  pivot_longer(cols = c(TreatmentCode1,TreatmentCode2,
                        TreatmentCode3,TreatmentCode4,
                        TreatmentCode5,TreatmentCode6), names_to= "Tratamento",
               values_to = "Valores") %>%
  rename(tratm=Valores) %>% select(tratm)
tratm1

#-------------------------------------------------------------------------------------------


######################### Sexo está ok. ####################################################
# Para cada grupo de 6 é um identificado como indivíduo
sexo11=dados %>% transmute(sex) 
sexo1=sexo11[1]
sexo1
for (i in 1:length(sexo1)) {
  sx= rep(sexo1[,1],6)
  return(data.frame(sexo1))
}

sexo=sx %>% data.frame() %>% as_tibble()%>%
  pivot_longer(cols = c(sex,sex.1,
                        sex.2,sex.3,
                        sex.4,sex.5), names_to= "sexo",
               values_to = "Valores")%>%
  rename(sx_i = Valores) %>% select(sx_i)
sexo
#--------------------------------------------------------------------------------------

##### Cranicidade 
cranicida <- dados %>% transmute(chronicity)
cranicida[,1]
for (i in 1:length(cranicida)) {
  cronic= rep(cranicida[,1],6)
  return(data.frame(cranicida))
}
cronic=cronic %>% data.frame() %>% as_tibble() %>%
  pivot_longer(cols = c(chronicity,chronicity.1,
                        chronicity.2,chronicity.3,
                        chronicity.4, chronicity.5), names_to= "sexo",
               values_to = "Valores")%>%
  transmute(cronic_i=Valores)
cronic


###### Idade 
idade <- dados %>% transmute(demo_educa_years1)
idade
for (i in 1:length(idade)) {
  idade1= rep(idade[,1],6)
  return(data.frame(idade))
}
idade1=idade1 %>% data.frame() %>% as_tibble() %>%
  transmute(idade1=demo_educa_years1,
            idade2=demo_educa_years1.1,
            idade3=demo_educa_years1.2,
            idade4=demo_educa_years1.3,
            idade5=demo_educa_years1.4,
            idade6=demo_educa_years1.5)%>%
  pivot_longer(cols = c(idade1,idade2,idade3, idade4,idade5,idade6), names_to="idade_individuos",
               values_to = "idade") %>%
  transmute(idade_i=idade)
idade1
###### variável t de cada semana percorrendo os t_ij 
t <- dados %>% transmute(semana1,semana2,semana3,semana4,semana6,semana8)
t
for (i in 1:length(t)) {
  tempo= rep(t[1:6],1)
  return(data.frame(t))
}
tempo %>% data.frame()
Yij=tempo %>% data.frame() %>% as_tibble() %>%
  pivot_longer(cols = c(semana1,semana2,semana3,
                        semana4,semana6,semana8), names_to="tij",
               values_to = "valores")%>%
  transmute(Yijk=valores)
Yij
####################################################################################################
# Biblioteca que será usando nas análises 
library(nlme)
library(lme4)
library(buildmer)
library(tidyverse)
library(stevemisc) 
library(stevedata)
library(fitdistrplus)
library('car')
library(lmtest)
library(boot)
library(nortest)
library(xtable)
library(boot)
# Informação das colunas do banco de dados
#Id=Individuo, tjj=Tempo da semana 1 até a 6, tratm= medicamento=0, placebo=1,
#sx_i= onde i é igual 0 é homem e 1 quando mulhaer,
#cronic_i= onde i = 1 alta, 0 abaixa, idade_i=i vai de cada idade do inividuo,
#Yijk= quando o i=1 tem o resultado  na primeira, 
#i=1,...,6 semana j o a semana sendo j = 1,..., 6 e  k são os blocos de cada semana sendo k=1,...,96
######################### Construindo o data.fram final ###########################################
tabfnl <- data.frame(individ, ti, tratm1,sexo, cronic,idade1, Yij) %>% as_tibble() 
tabelaamotra= tabfnl[1:10, c("Id", "tjj","tratm","sx_i","cronic_i","idade_i","Yijk")]
xtable(tabelaamotra) # Pegando as 10 preimeiras linhas do conjunto de dodos modificado
attach(tabfnl)
##################################################################################################
################ Usando o modelo de dados longitudinal ###########################################
##################################################################################################
############### Ajustando Modelos Lineares Generalizados de Efeitos Mistos ######################
###### Esse modelo que está no relatório. ##############
## Esse modelo preciso arrumar ainda
ajuste.0 <-  glmer(formula =Yijk ~ sx_i + tratm + cronic_i+idade_i+((1+tjj)|Id) , 
                   (family=gaussian(link = "identity")),
                  data = tabfnl)
ajuste.0
majustato=summary(ajuste.0)
majustato
anovaM=anova(ajuste.0)
anovaM
# Análise de resíduos ####
fit01 = fitted(ajuste.0)
fit01
plot(fit01)
#criando objeto com os coeficientes do modelo (efeitos fixos)
cof <- fixef(ajuste.0)
cof
# Estudo do diagnostico do modelo

residuo <-majustato$residuals
# Testes de Normalidade para o modelo ajustado
shapiro.test(residuo)
lillie.test(residuo)
qqnorm(residuo)
qqline(residuo,col=2)
cvm.test(residuo)
ad.test(residuo)
# Análisando o residuos
ard1btn = ls.diag(majustato)
respadron = ard1btn$std.res #Resíduo padronizado
hist(residuo, main = "Histograma dos Resíduos", ylab = "Frequência", col = "gray", 
     sub="Figura 9:Distribuição do histograma dos resíduos padronizado") 








###########################################################################################
############ Ajusto do Modelo Linear de Efeitos Mistos ####################################
###########################################################################################
# Outras análises. 
ajuste.1 <-  lmer(formula =Yijk ~ sx_i + tratm + cronic_i+idade_i+((1+tjj)|Id) , 
                  data = tabfnl)

ajuste.1
majustado1 <-summary(ajuste.1)
majustado1
anovaM1<-anova(ajuste.1)

shapiro.test(majustado1$residuals)

plot(ajuste.1)
hist(ajuste.1)
# Testes de Normalidade para o modelo ajustado
residuo1 <-majustado1$residuals
shapiro.test(residuo1)
lillie.test(residuo1)
qqnorm(residuo1)
qqline(residuo1,col=2)
cvm.test(residuo1)
ad.test(residuo)









