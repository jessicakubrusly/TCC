# CRIAÇÃO DA REDE NEURAL

# Carregando pacotes
library(tidytext)
library(tidyverse)
library(wordcloud)
library(yarrr)
library(dplyr)
library(pROC)
library(caret)

# Carregando matrizes termo-documento
load('MatrizesTermoDocumento.RData')

# Transformando as matrizes em data.frame
termo_documento_treino = data.frame(matriz_termo_docuemnto)
termo_documento_bigram_treino = data.frame(matriz_termo_documento_bigram)
termo_documento_teste = data.frame(matriz_termo_documento_teste)
termo_documento_bigram_teste = data.frame(matriz_termo_documento_bigram_teste)

termo_documento_treino = termo_documento_treino %>% mutate(id = row.names(matriz_termo_docuemnto))
termo_documento_bigram_treino = termo_documento_bigram_treino %>% 
  mutate(id = row.names(matriz_termo_documento_bigram),
         V102 = NULL)
termo_documento_teste = termo_documento_teste %>% 
  mutate(id = row.names(matriz_termo_documento_teste))
termo_documento_bigram_teste = termo_documento_bigram_teste %>%
  mutate(id = row.names(matriz_termo_documento_bigram_teste),
         V102 = NULL)

# Filtrando valores NA na variável target
termo_documento_treino = termo_documento_treino %>% filter(!is.na(target))
termo_documento_bigram_treino = termo_documento_bigram_treino %>% filter(!is.na(target))
termo_documento_teste = termo_documento_teste %>% filter(!is.na(target))
termo_documento_bigram_teste = termo_documento_bigram_teste %>% filter(!is.na(target))

# Rede Neural

#################################### Unigram ####################################

######## nenhuma camada oculta
modelo1 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = 0, rep = 2)
summary(modelo1)
plot(modelo1)

# erro
modelo1$result.matrix

# curva roc
output = predict(modelo1, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
#plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred1 = ifelse(output > threshold[[1]][1], 1, 0)
tab1 = table(pred1, termo_documento_treino$target)
confusionMatrix(tab1, positive = '1', mode = 'everything')

?confusionMatrix
# predicao nos dados de teste
output_teste = predict(modelo1, termo_documento_teste[,-c(101,102)])
pred1_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab1_teste = table(pred1_teste, termo_documento_teste$target)
confusionMatrix(tab1_teste, positive = '1', mode = "everything")

# Perfomance ruim

######### 1 camada oculta com 3 neurônios

modelo2 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = 3, rep = 2)
summary(modelo2)
plot(modelo2)

# erro
modelo2$result.matrix

# curva roc
output = predict(modelo2, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
#plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred2 = ifelse(output > threshold[[1]][1], 1, 0)
tab2 = table(pred2, termo_documento_treino$target)
confusionMatrix(tab2, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo2, termo_documento_teste[,-c(101,102)])
pred2_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab2_teste = table(pred2_teste, termo_documento_teste$target)
confusionMatrix(tab2_teste, positive = '1', mode = 'everything')

# Perfomance melhor, porém houve overfitting

######### 1 camada oculta com 4 neurônios

modelo3 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = 4, rep = 2)
summary(modelo3)
plot(modelo3)

# erro
modelo3$result.matrix

# curva roc
output = predict(modelo3, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred3 = ifelse(output > threshold[[1]][1], 1, 0)
tab3 = table(pred3, termo_documento_treino$target)
confusionMatrix(tab3, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo3, termo_documento_teste[,-c(101,102)])
pred3_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab3_teste = table(pred3_teste, termo_documento_teste$target)
confusionMatrix(tab3_teste, positive = '1', mode = 'everything')

# Overfitting

######### 1 camada oculta com 5 neurônios

modelo4 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = 5, rep = 2)
summary(modelo4)
plot(modelo4)

# erro
modelo4$result.matrix

# curva roc
output = predict(modelo4, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred4 = ifelse(output > threshold[[1]][1], 1, 0)
tab4 = table(pred4, termo_documento_treino$target)
confusionMatrix(tab4, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo4, termo_documento_teste[,-c(101,102)])
pred4_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab4_teste = table(pred4_teste, termo_documento_teste$target)
confusionMatrix(tab4_teste, positive = '1', mode = 'everything')

# Overfitting

######### 2 camadas ocultas com 2 e 1 neurônios

modelo5 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = c(2,1), rep = 2)
summary(modelo5)
plot(modelo5)

# erro
modelo5$result.matrix

# curva roc
output = predict(modelo5, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred5 = ifelse(output > threshold[[1]][1], 1, 0)
tab5 = table(pred5, termo_documento_treino$target)
confusionMatrix(tab5, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo5, termo_documento_teste[,-c(101,102)])
pred5_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab5_teste = table(pred5_teste, termo_documento_teste$target)
confusionMatrix(tab5_teste, positive = '1', mode = 'everything')

# Ruim

######### 2 camadas ocultas com 1 e 2 neurônios

modelo6 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = c(1,2), rep = 2)
summary(modelo6)
plot(modelo6)

# erro
modelo6$result.matrix

# curva roc
output = predict(modelo6, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred6 = ifelse(output > threshold[[1]][1], 1, 0)
tab6 = table(pred6, termo_documento_treino$target)
confusionMatrix(tab6, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo6, termo_documento_teste[,-c(101,102)])
pred6_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab6_teste = table(pred6_teste, termo_documento_teste$target)
confusionMatrix(tab6_teste, positive = '1', mode = 'everything')

# O melhorzinho, mas acurácia e sensibilidade ainda ruim (65% e 57%)

######### 2 camadas ocultas com 2 e 2 neurônios

modelo7 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = c(2,2), rep = 2)
summary(modelo7)
plot(modelo7)

# erro
modelo7$result.matrix

# curva roc
output = predict(modelo7, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred7 = ifelse(output > threshold[[1]][1], 1, 0)
tab7 = table(pred7, termo_documento_treino$target)
confusionMatrix(tab7, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo7, termo_documento_teste[,-c(101,102)])
pred7_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab7_teste = table(pred7_teste, termo_documento_teste$target)
confusionMatrix(tab7_teste, positive = '1', mode = 'everything')

# Pior que o anterior

######### 2 camadas ocultas com 2 e 3 neurônios

modelo8 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = c(2,3), rep = 2)
summary(modelo8)
plot(modelo8)

# erro
modelo8$result.matrix

# curva roc
output = predict(modelo8, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred8 = ifelse(output > threshold[[1]][1], 1, 0)
tab8 = table(pred8, termo_documento_treino$target)
confusionMatrix(tab8, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo8, termo_documento_teste[,-c(101,102)])
pred8_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab8_teste = table(pred8_teste, termo_documento_teste$target)
confusionMatrix(tab8_teste, positive = '1', mode = 'everything')

# Ruim (Overfitting)

######### 2 camadas ocultas com 3 e 2 neurônios

modelo9 = neuralnet::neuralnet(target~.-id, data = termo_documento_treino,
                               hidden = c(3,2), rep = 2)
summary(modelo9)
plot(modelo9)

# erro
modelo9$result.matrix

# curva roc
output = predict(modelo9, termo_documento_treino[,-c(101,102)])
head(output)
roc = roc(response = termo_documento_treino$target, predictor = output)
roc
plot(roc,xlim=c(0,1))
threshold <- coords(roc, "best", ret = "threshold")
threshold[[1]][1]

# predicao
pred9 = ifelse(output > threshold[[1]][1], 1, 0)
tab9 = table(pred9, termo_documento_treino$target)
confusionMatrix(tab9, positive = '1', mode = 'everything')

# predicao nos dados de teste
output_teste = predict(modelo9, termo_documento_teste[,-c(101,102)])
pred9_teste = ifelse(output_teste > threshold[[1]][1],1,0)
tab9_teste = table(pred9_teste, termo_documento_teste$target)
confusionMatrix(tab9_teste, positive = '1', mode = 'everything')

# Ruim (Overfitting)

#################################### Bigram ####################################


modelo2 = neuralnet::neuralnet(target~.-id, data = termo_documento_bigram_treino,
                               hidden = 5, rep = 2)

summary(modelo2)
plot(modelo2)

# erro
modelo2$result.matrix

# curva roc
output_bigram = predict(modelo2, termo_documento_bigram_treino[,-c(101,102)])
head(output)
roc2 = roc(response = termo_documento_bigram_treino$target, predictor = output_bigram)
roc2
plot(roc2)
threshold2 = coords(roc, 'best', ret = 'threshold')
threshold2[[1]][1]

# predicao
pred2 = ifelse(output_bigram > threshold2[[1]][1], 1, 0)
tab2 = table(pred2, termo_documento_bigram_treino$target)
confusionMatrix(tab2, positive = '1')

# predicao nos dados de teste
output_bigram_teste = predict(modelo2, termo_documento_bigram_teste[,-c(101,102)])
pred2_teste = ifelse(output_bigram_teste > threshold2[[1]][1],1,0)
tab2_teste = table(pred2_teste, termo_documento_bigram_teste$target)
confusionMatrix(tab2_teste, positive = '1')

# OVERFITTING (nos dados de teste, previu todos como classe 1)







summary(modelo6)
plot(modelo6)

























































