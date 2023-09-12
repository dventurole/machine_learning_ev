getwd()
setwd("C:/FCD/bigdatarazure/Projetos-1-2v2")

#Objetivo
#Criar modelo de Machine Learning capaz de prever o 
#consumo de energia de veículos elétricos

#Bibliotecas que serão utilizadas no projeto
library(dplyr)
library(readxl)
library(h2o)
library(corrplot)
library(caret)

#Baixando o arquivo
dados <- read_excel("C:/FCD/bigdatarazure/Projetos-1-2v2/FEV-data-Excel.xlsx")
View(dados)

#Remover variáveis que não impactam no consumo do
#automóvel:
#1 - Preço
#2 - Número de portas/assentos (podem influir no peso,
#mas isto já está considerado na variável Minimal 
#Empty Weigth)
#3 - Tipo de freio
#4 - Nome, Modelo e Fabricante do carro
#5 - Tipo de tração

dados$`Minimal price (gross) [PLN]`= NULL
dados$`Number of seats` = NULL
dados$`Number of doors`= NULL
dados$`Drive type` = NULL
dados$`Type of brakes`= NULL
dados$Model = NULL
dados$`Car full name`= NULL
dados$Make = NULL


#renomear títulos de cada coluna
nomes <- c("potencia", "torque", "capacidade_bateria",
           "autonomia","entre_eixos","comprimento",
           "largura","altura","peso_min","peso_max",
           "carga_max","tam_pneu","veloc_max",
           "porta_malas","aceleracao", "carga_DC",
           "consumo_medio")
names(dados) <- nomes

#Início da análise exploratória

str(dados)
sum(is.na(dados))
summary(dados)
linhas_completas <- sum(complete.cases(dados))
linhas_incompletas <- sum(!complete.cases(dados))
perc_incompleto <- (linhas_incompletas/(linhas_incompletas + linhas_completas))*100
perc_incompleto 

#aprox. 21% das linhas do dataframe possui dados NA
#considero esse valor muito relevante para descartar as
#observações, então farei um modelo com e sem 
#descarte para comparar o desempenho


hist(dados$consumo_medio)



#Realizar a normalização dos dados
dados_norm <- as.data.frame(scale(dados))
View(dados_norm)


#Alternativa 1 - Modelo preditivo eliminando as 
#variáveis que contem NA's

alt1 <- dados_norm
View(alt1)
alt1 <- na.omit(alt1)

sum(is.na(alt1))

cor <- as.data.frame(cor(alt1))
cor_consumo <- as.data.frame(cor$consumo_medio)
View(cor_consumo)

#os valores de correlação indicam que potência, 
#torque, capacidade_bateria, peso_min, peso_max,
# e porta_malas são as mais relevantes

#criando os dados de treino e teste
split <- createDataPartition(alt1$consumo_medio, p=0.7, list = FALSE)
dados_treino <- alt1[split,]
dados_teste <- alt1[-split,]

#criando os modelos
modelo1 <- train(consumo_medio ~.,dados_treino, method = "lm")

modelo2 <- train(consumo_medio ~ potencia + torque + 
                capacidade_bateria + peso_min + 
                peso_max + porta_malas, dados_treino, method = "lm")

#verificando a precisão dos modelos
summary(modelo1) #apresentou 95,88%
summary(modelo2) #apresentou 83,17%

#realizando as previsões
previsao1 <- predict(modelo1, dados_teste)
previsao2 <- predict(modelo2, dados_teste)

#Plot das previsões
plot(dados_teste$consumo_medio, previsao1)
plot(dados_teste$consumo_medio, previsao2)

plot(varImp(modelo1))
plot(varImp(modelo2))



#Alternativa 2 - Modelo preditivo substituindo os
#valores NA

#Na variável target, atribuir a média de valores da coluna
#aos valores NA

alt2 <- dados
summary(alt2)
media_consumo <- round(mean(alt2$consumo_medio, na.rm = TRUE),2)
media_peso_max <- round(mean(alt2$peso_max, na.rm = TRUE),2)
media_carga_max <- round(mean(alt2$carga_max, na.rm = TRUE),2)
media_porta_malas <- round(mean(alt2$porta_malas, na.rm = TRUE),2)
media_aceleracao <- round(mean(alt2$aceleracao, na.rm = TRUE),2)

alt2$consumo_medio[is.na(alt2$consumo_medio)] <- media_consumo
alt2$peso_max[is.na(alt2$peso_max)] <- media_peso_max
alt2$carga_max[is.na(alt2$carga_max)] <- media_carga_max
alt2$porta_malas[is.na(alt2$porta_malas)] <- media_porta_malas
alt2$aceleracao[is.na(alt2$aceleracao)] <- media_aceleracao

sum(is.na(alt2))

#Realizar a normalização dos dados
alt2_norm <- as.data.frame(scale(alt2))
View(alt2_norm)

cor(alt2_norm)
pairs(alt2_norm)
#principais variáveis: capacidade_bateria, entre_eixos, 
#comprimento, peso_min, peso_max, carga_max, porta_malas

split2 <- createDataPartition(alt2_norm$consumo_medio, p=0.7, list = FALSE)
dados_treino2 <- alt2_norm[split2,]
dados_teste2 <- alt2_norm[-split2,]

#criando os modelos
modelo1v2 <- train(consumo_medio ~.,dados_treino2, method = "lm")

modelo2v2 <- train(consumo_medio ~ potencia + torque + 
                   capacidade_bateria + peso_min + 
                   peso_max + porta_malas, dados_treino2, method = "lm")

#verificando a precisão dos modelos
summary(modelo1v2) #apresentou 93,51%
summary(modelo2v2) #apresentou 83,23%

#realizando as previsões
previsao1v2 <- predict(modelo1v2, dados_teste2)
previsao2v2 <- predict(modelo2v2, dados_teste2)

#Plot das previsões
plot(dados_teste2$consumo_medio, previsao1v2)
plot(dados_teste2$consumo_medio, previsao2v2)

plot(varImp(modelo1v2))
plot(varImp(modelo2v2))

residuals(modelo1v2)


#adicionalmente, eu tentei criar essa função para
#tratar NAs da coluna consumo_medio, mas não
#consegui fazer funcionar, podem me orientar?
trata_na <- function(x){
  for (i in 1:length(alt2)){
    if (is.na(alt2[17,i] == TRUE)){
      alt2$consumo_medio = mean(alt2$consumo_medio)
    }
    else alt2$consumo_medio = alt2$consumo_medio
  }
}
trata_na(alt2)

