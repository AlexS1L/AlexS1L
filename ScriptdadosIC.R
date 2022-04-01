#################################################### 
########### Scrip do Relátorio de IC ###############
####### Alex dos Santos Lima RA: 230560 ############
####################################################
# Limpando o Environmon is empty
rm(list = ls()) 
## Biblioteca para ser usada do projeto
library(leaps)
library(car) 
library(MASS)
library(magrittr)
library(magick)
library(rsvg)
library(stringr)
library(knitr)
library(lmtest)
library(boot)
library(nortest)
library(Ecdat)
library(dplyr)
library(tidyverse)
library(readr)
library(nortest)
library(reshape2)
library(ggpubr)
library("scales")
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(lubridate)
library(cowplot)
library(nycflights13)

# Abaixando a base de dados no Rstudio

load("/home/alex/Downloads/Early.RData")
dados <- "/home/alex/Downloads/Early.RData"

# Informação sobre a base dedos
      
n
y
eeg_tnsr
scalar_all
cov

## Fazendo uma análise exploratoria dos dodos

length(scalar_all$id)

# Arrumando os dados

Tfinal = scalar_all %>%
  mutate(Resposta = y)

# Gráfico de Histograma e Dispersão... Esse gráfico ficou bom. 

par(mfrow=c(1,2))
hist(Tfinal$Resposta, br=seq(-10,25, by=1.5),
     xlab = "Resultado da primeira semana",
     ylab = "Frequências",
     main = "Histograma",
     col = "white",
     prob=TRUE)
lines(density(Tfinal$Resposta), col="blue")
plot(Tfinal$Resposta,
     main = "Dispersão",
     xlab="Pontos de dispersão na escala HAM-D",
     ylab="Resultado da primeira semana")
abline(h=0)

Tfinal %>% 
  ggplot(aes(x=Tfinal$Resposta))+
  geom_histogram( colour="black",
                 aes(y=..density.., fill = ..count..))+
  geom_density()+
  stat_function(fun = dnorm,
                color= "red",
                args = list(mean=mean(Tfinal$Resposta),
                            sd= sd(Tfinal$Resposta)))+
  labs(x= "Resultado clínico da primeira semana",y="Densidade")

## Primeiro, temos que arrumar os dados 

tabela=scalar_all %>% as_tibble()%>%
  transmute(hamd_17_score_wk1=ifelse(hamd_17_score_wk1=="NA",0,hamd_17_score_wk1),
            hamd_17_score_wk2=ifelse(hamd_17_score_wk2=="NA",0,hamd_17_score_wk2),
            hamd_17_score_wk3=ifelse(hamd_17_score_wk3=="NA",0,hamd_17_score_wk3),
            hamd_17_score_wk4=ifelse(hamd_17_score_wk4=="NA",0,hamd_17_score_wk4),
            hamd_17_score_wk6=ifelse(hamd_17_score_wk6=="NA",0,hamd_17_score_wk6),
            hamd_17_score_wk8=ifelse(hamd_17_score_wk8=="NA",0,hamd_17_score_wk8),
            hamd_17_score_eval=hamd_17_score_eval) %>%
  drop_na()

newdata = tabela %>%
  summarise(Semana1 = hamd_17_score_eval- hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8)%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1",
               values_to = "Valores")

# Gráfico dos histograma em cada semana ....Esse gráfico ficou bom. 

newdata %>%
  group_by(Semana1)%>%
  ggplot()+
  theme_bw()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        strip.placement ="outside",
        strip.background =element_rect(fill="white"),
        axis.title = element_blank(), 
        strip.text.y = element_text(size = 7))+
  geom_histogram(aes(x=Valores), color="black", fill="lightblue") +
  facet_wrap(~Semana1) +
  ylab("Frequência")+
  labs(caption = "Figura 1: Gráficos das seguintes semanas",
       x="",y="",
       title = "Gráfico de histograma de cada semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


## Primeiro, temos que arrumar os dados 

tabela=scalar_all %>% as_tibble()%>%
  transmute(hamd_17_score_wk1=ifelse(hamd_17_score_wk1=="NA",0, hamd_17_score_wk1),
            hamd_17_score_wk2=ifelse(hamd_17_score_wk2=="NA", 0,hamd_17_score_wk2),
            hamd_17_score_wk3=ifelse(hamd_17_score_wk3=="NA", 0,hamd_17_score_wk3),
            hamd_17_score_wk4=ifelse(hamd_17_score_wk4=="NA", 0 , hamd_17_score_wk4),
            hamd_17_score_wk6=ifelse(hamd_17_score_wk6=="NA",0,hamd_17_score_wk6),
            hamd_17_score_wk8=ifelse(hamd_17_score_wk8=="NA",0,hamd_17_score_wk8),
            hamd_17_score_eval=hamd_17_score_eval,
            TreatmentCode=TreatmentCode)%>%
  drop_na()
newdata = tabela %>% as_tibble()%>%
  summarise(Semana1 = hamd_17_score_eval-hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=TreatmentCode)%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7,
                TreatmentCode=TreatmentCode)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1", values_to = "Valores")



# Gráfico da distribuição do histograma em cada semana... Esse gráfico ficou bom.

newdata %>%
  group_by(Semana1)%>%
  ggplot()+
  theme_bw()+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        strip.placement ="outside",
        strip.background =element_rect(fill="white"),
        axis.title = element_blank(), 
        strip.text.y = element_text(size = 7))+
  geom_histogram(aes(x=Valores), color="black", fill="lightblue",bins = 30) +
  facet_grid(rows = vars(Semana1),space = "free") +
  ylab("Frequência")+
  labs(caption = "Figura 2: Gráficos dos Histogramas das semanas",
       x="",y="",
       title = "Gráfico do histograma de cada distribuição na Escala  Avaliação de Depressão de Hamilton")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



## Primeiro, temos que arrumar os dados

newdata1 =Tfinal %>% as_tibble()%>%
  summarise(Semana1 = hamd_17_score_eval-hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8)%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7)%>%
  drop_na(Semana1,Semana2,Semana3,Semana4,
          Semana6,Semana7)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1", values_to = "Valores")


  
 


# Gráfico de linha das semanas para comparar com os grupos...ok
# Gráfico de dispersão entre cada semana
# Na Avalisacao na Escala Himilton  
# O gráfico não ficou bom.

newdata1 %>% as_tibble()%>%
  drop_na()%>%
  ggplot(aes(Semana1, Valores))+
  geom_boxplot()+
  facet_wrap(~Semana1)+
  theme_bw()+
  labs(x= "Comportamento da evolução",
       y="Tramento placebo e comtrole",
       title = "Gráfico da evolução conforme o tempo passa",
       caption = "Figura 3: Evolução em cada semana")+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        strip.placement ="outside",
        strip.background =element_rect(fill="white"),
        axis.title = element_blank(), 
        strip.text.y = element_text(size = 7))+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Gráfico de dispersão para os dois grupos nas 2 primeira semana ....ok
# Semana 1

Tfinal %>% as_tibble()%>%
  transmute(Resposta= Resposta,
            Data = dmy(as.Date(Week.1,format="%Y/%m/%d")),
            `Tratamento Controle`=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"),
            demo_educa_years1=as.integer(demo_educa_years1))%>%
  ggplot(aes(demo_educa_years1,Resposta, colour =`Tratamento Controle`))+
  geom_point()+
  facet_wrap(~`Tratamento Controle`)+
  theme_bw()+
  labs(x= "Resultado do teste clínico na primeira semana",
       y="Idade dos pacientes",
       title = "Gráfico dos dois grupos placebo e controle na primeira semana")+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        strip.placement ="outside",
        strip.background =element_rect(fill="white"),
        axis.title = element_blank(), 
        strip.text.y = element_text(size = 7))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Semana 2

Tfinal %>% as_tibble()%>%
  mutate(semana2=hamd_17_score_eval-hamd_17_score_wk2)%>%
  transmute(`Tratamento Controle`=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"),
            demo_educa_years1=demo_educa_years1,
            semana2=semana2)%>%
  drop_na()%>%
  ggplot(aes(semana2,demo_educa_years1, colour =`Tratamento Controle`))+
  geom_point()+
  facet_wrap(~`Tratamento Controle`)+
  theme_bw()+
  labs(x= "Resultado do teste clínico na segunda semana ",
       y="Idade dos pacientes",
       title = "Gráfico dos dois grupos placebos e controle na segunda semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_line(aes(y = 16), size = 0.5)+
  geom_line(aes(y = 12), size = 0.5)

# Tabela das médias em cada semana fazendo por grupo Placebo e Tratamento ...ok

Tfinal %>% as_tibble() %>%
  mutate(sex=ifelse(sex=="1","Masculino","Feminino"),
         TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  group_by(TreatmentCode,sex)%>%
  summarise(`Semana 1` = mean(hamd_17_score_eval-hamd_17_score_wk1,na.rm=TRUE),
            `Semana 2` = mean(hamd_17_score_eval-hamd_17_score_wk2,na.rm=TRUE),
            `Semana 3` = mean(hamd_17_score_eval-hamd_17_score_wk3,na.rm=TRUE),
            `Semana 4` = mean(hamd_17_score_eval-hamd_17_score_wk4,na.rm=TRUE),
            `Semana 5` = mean(hamd_17_score_eval-hamd_17_score_wk6,na.rm=TRUE),
            `Semana 6` = mean(hamd_17_score_eval-hamd_17_score_wk8,na.rm=TRUE), .groups ='drop')%>%
  rename(`Tratamento`=TreatmentCode,
         `Sexo`=sex)%>%
  knitr::kable(digits=2, caption = "Tabela das médias dos grupos em cada semana")

# Gráfico do histograma das idade dos pacientes ....ok

hist(Tfinal$demo_educa_years1,
     main =  "Histogrma das Idades dos pacientes",
     xlab =  "Idades dos pacientes",
     ylab = "Frequências",
     col = "lightblue")

# Gráfico dos boxplot para cada sexo e cranicidade...ok


tb1=y %>% as_tibble()  # Variável resposta
tb2=cov %>% 
  as_tibble() %>%
  mutate(V2= ifelse(V2=="1", "Masculino","Feminino"),
         V1= ifelse(V1=="1", "Sim","Não"))

# contém somente as covariáveis de sexo e cronicidade

par(mfrow=c(1,2))
boxplot(tb1$value~tb2$V1,
        col = c("blue","lightblue"),
        xlab = "Você tem ou já teve essa doença?",
        ylab = "Resposta experada",
        main= "Cronicidade") # Cronicidade
boxplot(tb1$value~tb2$V2,
        col = c("blue","lightblue"),
        xlab = "Pacientes participantes",
        ylab = "Resposta experada",
        main= "Covariáveis de Sexo") #variável resposta e sexo


## Primeiro, temos que arrumar os dados para ficar de acordo.
tabela=scalar_all %>% as_tibble()%>%
  mutate(hamd_17_score_wk1=ifelse(hamd_17_score_wk1=="NA",0,hamd_17_score_wk1),
         hamd_17_score_wk2=ifelse(hamd_17_score_wk2=="NA",0,hamd_17_score_wk2),
         hamd_17_score_wk3=ifelse(hamd_17_score_wk3=="NA",0,hamd_17_score_wk3),
         hamd_17_score_wk4=ifelse(hamd_17_score_wk4=="NA",0,hamd_17_score_wk4),
         hamd_17_score_wk6=ifelse(hamd_17_score_wk6=="NA",0,hamd_17_score_wk6),
         hamd_17_score_wk8=ifelse(hamd_17_score_wk8=="NA",0,hamd_17_score_wk8),
         hamd_17_score_eval=hamd_17_score_eval,
         demo_educa_years1=demo_educa_years1,
         TreatmentCode=TreatmentCode,
         chronicity=chronicity) 

newdata = tabela %>% 
  summarise(Semana1 = hamd_17_score_eval- hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8,
            demo_educa_years1=demo_educa_years1,
            TreatmentCode=TreatmentCode,
            chronicity=chronicity)%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7,
                demo_educa_years1=demo_educa_years1,
                TreatmentCode=TreatmentCode,
                chronicity=chronicity)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1",
               values_to = "Valores")

## Realizando  o gráfico do histograma por idade relação a resposta
# Boxplot  para cada grupo Placebo e Tratamento controle versos com as idade 

newdata %>%
  mutate(demo_educa_years1=as.integer(demo_educa_years1),
         TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  group_by(Semana1)%>%
  ggplot(aes(x=demo_educa_years1,y=Valores, fill= `Tratamento Controle`))+
  theme_bw()+
  geom_boxplot() +
  facet_wrap(~Semana1) +
  ylab("Frequência")+
  labs(caption = "Figura 7: Gráfico do BoxPlot com as idade dos pacientes e o resultado na Escala de (HAM-D)",
       x="Idades dos Pacientes",y="Resultado na Escala de (HAM-D)",
       title = "Gráfico do BoxPlot para o Tratamento Controle")+
  scale_fill_manual(labels = c("Placebo","Tratamento"), values = c("Wheat", "SkyBlue"))+
  scale_colour_manual(values = "black")+
  theme(strip.placement ="outside",
        strip.background =element_rect(fill="white"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
## Gráfico do boxplo separado para tratamento controle e placebo vesos semanas

newdata %>% ggplot(aes(x=Semana1, y = Valores, fill= TreatmentCode))+
  geom_boxplot()+
  #stat_summary(geom = "point", fun.y = mean) + 
  # adiciona uma linha na média geral
  labs(x = "Evolução em cada semana",
       y = "Resultado do teste clínico",
       title = "Gráfico de BoxPlot para cada semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
## Gráfico do boxplot geral
newdata %>% replace(is.na(.),0)%>%
  ggplot(aes(x=Semana1, y = Valores))+
  geom_boxplot(width = 0.5, col = "black", fill = "SkyBlue")+
  #stat_summary(fun.y = "mean", geom = "point")+
  #geom_hline(yintercept =mean(), linetype = "dashed")+ 
  # adiciona uma linha na média geral
  labs(x = "Evolução em cada semana",
       y = "Resultado do teste clínico",
       title = "Gráfico de BoxPlot para cada semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+  
  scale_colour_manual(values = "black")+
  theme(strip.placement ="outside",
        strip.background =element_rect(fill="white"))


plot_grid(box1, box2, labels = c("a)", "b)"))
## Gráfico de boxplot para cada semana em comparação com a cranicidade
newdata %>%
  mutate(demo_educa_years1=as.integer(demo_educa_years1),
         chronicity=ifelse(chronicity=="1","Sim","Não"))%>%
  rename( `Cranicidade`=chronicity)%>%
  group_by(Semana1)%>%
  ggplot(aes(x=demo_educa_years1,y=Valores, fill= `Cranicidade`))+
  theme_bw()+
  geom_boxplot() +
  facet_wrap(~Semana1) +
  ylab("Frequência")+
  labs(caption = "Figura 7: Gráfico de BoxPlot para Cranicidade",
       x="Idades dos Pacientes",y="Resultado na Escala de (HAM-D)",
       title = "Gráfico do BoxPlot para Cranicidade com as idades dos pacientes")+
  scale_fill_manual(labels = c("Sim","Não"), values = c("Wheat", "SkyBlue"))+
  scale_colour_manual(values = "black")+
  theme(strip.placement ="outside",
        strip.background =element_rect(fill="white"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
## Na posição vertical
newdata %>% as_tibble()%>%
  mutate(TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  ggplot(aes(Semana1, Valores,colour =`Tratamento Controle`))+
  geom_boxplot()+
  facet_wrap(~Semana1)+
  theme_bw()+
  labs(x= "Comportamento da evolução",
       y="Pontuação na Escala de Hamilton",
       title = "Gráfico de BoxPlot para Placebo e Tratamento",
       caption = "Figura 3: Evolução em cada semana")+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.title = element_text(hjust = 0.5),
        strip.placement ="outside",
        strip.background =element_rect(fill="white"),
        axis.title = element_blank(), 
        strip.text.y = element_text(size = 7))
#----------------------------------------------------------------------------------
# Novo Gráfico, idade dos participantes em relação as pontuação na escala de Hamilton
# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na primeira semana

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk1=hamd_17_score_wk1,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana1=hamd_17_score_eval-hamd_17_score_wk1,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana1, color=`Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  facet_wrap(~`Tratamento Controle`)+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Primeira semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na segunda semana

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk2=hamd_17_score_wk2,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana2=hamd_17_score_eval-hamd_17_score_wk2,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana2, color=`Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Segunda semana")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na terceira semana

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk3=hamd_17_score_wk3,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana3=hamd_17_score_eval-hamd_17_score_wk3,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana3, colour= `Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Terceira semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na quarta semana

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk4=hamd_17_score_wk4,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana3=hamd_17_score_eval-hamd_17_score_wk4,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana3, colour= `Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Quarta semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_grid(f1,f2,f3,f4, labels = c("a)", "b)","c)","d)"),ncol =2, nrow = 2)
## gráfico da quinta semana 

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk6=hamd_17_score_wk6,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana6=hamd_17_score_eval-hamd_17_score_wk6,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana6, colour= `Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Quinta semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Gráfico da sexta semana 
Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk8=hamd_17_score_wk8,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana8=hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana8, colour= `Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Sexta semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk8=hamd_17_score_wk8,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana8=hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana8))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Sexta semana")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# =================================================================================
# Novo Gráfico, idade dos participantes em relação as pontuação na escala de Hamilton
# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na primeira semana
f1=Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk1=hamd_17_score_wk1,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana1=hamd_17_score_eval-hamd_17_score_wk1,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana1, color=`Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se= F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Primeira semana separado por grupo")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na segunda semana
f2=Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk1=hamd_17_score_wk1,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana2=hamd_17_score_eval-hamd_17_score_wk1,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana2))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Primeira semana no dois grupos")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na terceira semana
f3=Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk3=hamd_17_score_wk2,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana3=hamd_17_score_eval-hamd_17_score_wk3,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana3, colour= `Tratamento Controle`))+
  geom_point()+
  geom_smooth(method = "lm", se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Segunda semana separado por grupos")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# Gráfico de Dispersão da idades com as pontuações de escala de Hamilton na quarta semana
f4=Tfinal%>% as_tibble()%>%
  transmute(hamd_17_score_wk3=hamd_17_score_wk2,
            TreatmentCode=TreatmentCode, 
            idade=as.integer(demo_educa_years1),
            samana3=hamd_17_score_eval-hamd_17_score_wk3,
            TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  drop_na()%>%
  arrange(idade)%>%
  ggplot(aes(idade, samana3))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_bw()+
  labs(x= "Idades dos pacientes",
       y="Eslaca de Hamilton",
       title = "Segunda semana separado por grupo")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
plot_grid(f1,f2,f3,f4,labels = c("a)", "b)","c)","d)"), ncol =2, nrow = 2)
# Gráfico de densidade separado por sexo com a categoria do grupo cronicidade bixa e alta
# Novo gráfico, Densidade de cronicidade
Tfinal %>%
  mutate(demo_educa_years1=as.integer(demo_educa_years1),
         chronicity=ifelse(chronicity=="1","Alta","Baixa"),
         sexo=ifelse(sex==1,"Mulher","Homens"))%>%
  rename(`Cronicidade`=chronicity)%>%
  ggplot(aes(Resposta, colour =`Cronicidade`))+
  geom_density()+
  labs(caption = "Figura 8: Gráficos da densidade entre cada grupo",
       x="",y="Densidade",
       title = "Gráfico da densidade")+
  theme_bw()+
  facet_wrap(~sexo) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))
# Gráfico de densidade separado por sexo com a categoria do grupo controle e tratamento
##### Dencidade de cronicidade  nos dois grupos
Tfinal %>%
  mutate(TreatmentCode=ifelse(TreatmentCode=="PLA","Placebo","Tratamento"),
         sexo=ifelse(sex==1,"Mulher","Homens"))%>%
  rename(`Tratamento Controle`=TreatmentCode)%>%
  ggplot(aes(Resposta, colour =`Tratamento Controle`))+
  geom_density()+
  labs(caption = "Figura 8: Gráficos da densidade entre cada grupo",
       x="",y="Densidade",
       title = "Gráfico da densidade")+
  theme_bw()+
  facet_wrap(~sexo) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background =element_rect(fill="white"))

#---------------------------------------------------------------------------------
## Gráfico de cadad paciente em relação a semana e resultado do teste clínico 
# Nestes gráficos foram feito todos os pontos para cada pacientes


tabela=scalar_all %>% as_tibble()%>% replace(is.na(.),0)%>%
  mutate(paciente = 1:96)
tabela
newdata = tabela %>% as_tibble()%>%
  summarise(Semana1 = hamd_17_score_eval-hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=TreatmentCode,
            paciente= factor(paciente))%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7,
                TreatmentCode=TreatmentCode,
                paciente=paciente)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1", values_to = "Valores")
newdata
## Separado por cada grupo tratamento controle e placebo
newdata %>%
  ggplot(aes(Semana1, Valores,colour =TreatmentCode))+
  geom_point()+
  facet_wrap(~paciente)+
  theme_bw()+
  labs(x= "Comportamento da evolução em cada semana",
       y="Pontuação na Escala de Hamilton",
       title = "",
       caption = "")+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(strip.background =element_rect(fill="white"))
## Fazendo uma análise dos grupo geral
newdata %>%
  ggplot(aes(Semana1, Valores))+
  geom_point()+
  facet_wrap(~paciente)+
  theme_bw()+
  labs(x= "Comportamento da evolução em cada semana",
       y="Pontuação na Escala de Hamilton",
       title = "",
       caption = "")+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(strip.background =element_rect(fill="white"))
## Separando o banco de dados em dois grupo

grupoplacebo = subset(scalar_all, TreatmentCode=="PLA") %>% as_tibble()%>% replace(is.na(.),0)%>%
  mutate(paciente = 1:50)
nw1 <- grupoplacebo %>% as_tibble()%>%
  summarise(Semana1 = hamd_17_score_eval-hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=TreatmentCode,
            paciente= factor(paciente))%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7,
                TreatmentCode=TreatmentCode,
                paciente=paciente)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1", values_to = "Valores")
nw1
nw1 %>%
  group_by(Semana1)%>%
  ggplot(aes(Semana1, Valores, fill = TreatmentCode))+
  geom_point()+
  geom_line()+
  facet_wrap(~paciente)+
  theme_bw()+
  labs(x= "Comportamento da evolução em cada semana",
       y="Pontuação na Escala de Hamilton",
       title = "Gráfico do grupo placebo",
       caption = "")+
  scale_fill_manual(labels = c("PLA"), values = c("Wheat"))+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(strip.background =element_rect(fill="white"))
#-----------------------------------------------------------
## Gráfico do grupo controle
grupocontrole =subset(scalar_all, TreatmentCode=="SER/CIT") %>% as_tibble()%>% replace(is.na(.),0)%>%
  mutate(paciente = 1:46)
nw2 <- grupocontrole %>% as_tibble()%>%
  summarise(Semana1 = hamd_17_score_eval-hamd_17_score_wk1,
            Semana2 = hamd_17_score_eval-hamd_17_score_wk2,
            Semana3 = hamd_17_score_eval-hamd_17_score_wk3,
            Semana4 = hamd_17_score_eval-hamd_17_score_wk4,
            Semana6 = hamd_17_score_eval-hamd_17_score_wk6,
            Semana7 = hamd_17_score_eval-hamd_17_score_wk8,
            TreatmentCode=TreatmentCode,
            paciente= factor(paciente))%>%
  dplyr::select(Semana1 = Semana1,
                Semana2 = Semana2,
                Semana3 = Semana3,
                Semana4 = Semana4,
                Semana6 = Semana6,
                Semana7 = Semana7,
                TreatmentCode=TreatmentCode,
                paciente=paciente)%>%
  pivot_longer(cols = c(Semana1,Semana2,Semana3,Semana4,Semana6,Semana7), names_to= "Semana1", values_to = "Valores")
nw2 %>%
  ggplot(aes(Semana1, Valores, fill = TreatmentCode))+
  geom_point()+
  #geom_boxplot()+
  facet_wrap(~paciente)+
  theme_bw()+
  labs(x= "Comportamento da evolução em cada semana",
       y="Pontuação na Escala de Hamilton",
       title = "Gráfico do grupo tratamento controle",
       caption = "")+
  scale_fill_manual(labels = c("SER/CIT"), values = c("SkyBlue"))+
  theme(axis.text.x  = element_text(angle =60, hjust = 1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(strip.background =element_rect(fill="white"))














    