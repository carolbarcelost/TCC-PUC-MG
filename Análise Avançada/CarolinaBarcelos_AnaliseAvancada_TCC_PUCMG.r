
#setwd("C:/Users/carol/OneDrive/Área de Trabalho/Pós-Graduação PUC Minas/TCC/TCC Carolina/Análise Avançada")

rm(list = ls(all=T))
options(OutDec=",",max.print=10000)

##### Pacotes ##### 

library(dplyr)
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}

##### Leitura do banco de dados ##### 

dados <- read.csv("SINAN-VIOL-2017-2019.csv", header = TRUE)
head(dados)

##### Arrumando o banco de dados ##### 

dados <- dados %>%     
  mutate(SEXO_cod = case_when(
    CS_SEXO == "F" ~ "Feminino",
    CS_SEXO == "M" ~ "Masculino",
    CS_SEXO == "I" ~ "Ignorado",
    is.na(CS_SEXO) ~ NA_character_),
    
    LES_AUTOP_cod = case_when(
      LES_AUTOP == "1" ~ "Sim",
      LES_AUTOP == "2" ~ "Nao",
      LES_AUTOP == "9" ~ "Ignorado",
      LES_AUTOP == "0" ~ NA_character_,
      is.na(LES_AUTOP) ~ NA_character_),
    
    AG_FOGO_cod = case_when(
      AG_FOGO == "1" ~ "Sim",
      AG_FOGO == "2" ~ "Nao",
      AG_FOGO == "9" ~ "Ignorado",
      is.na(AG_FOGO) ~ NA_character_))

head(dados)
summary(dados)

##### Criacao do dataframe ##### 

df <- data.frame(Sexo = dados$SEXO_cod, Lesao_Autoprov = dados$LES_AUTOP_cod, Ag_armafogo = dados$AG_FOGO_cod)
head(df)
summary(df)


##### Tabela de contingencia 1 ##### 

tab_cont <- table(df$Sexo, df$Lesao_Autoprov)
View(tab_cont)

##### Teste Qui Quadrado 1 ##### 

chisq.test(tab_cont)

##### Tabela de contingencia 2 ##### 

tab_cont2 <- table(df$Ag_armafogo, df$Lesao_Autoprov)
View(tab_cont2)

##### Teste Qui Quadrado 1 ##### 

chisq.test(tab_cont2)
