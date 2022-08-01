# Seleção de termos e matriz termo-documento

library(tidytext)
library(tidyverse)
library(wordcloud)
library(yarrr)
library(dplyr)


load('BancosTreinoTesteStem.RData')


View(token_treino_stem)
View(bigram_treino_stem)

token_treino_stem = token_treino_stem %>% mutate(target = ifelse(target == 'yes',1,0))
bigram_treino_stem = bigram_treino_stem %>% mutate(target = ifelse(target == 'yes',1,0))
token_teste_stem = token_teste_stem %>% mutate(target = ifelse(target == 'yes',1,0))
bigram_teste_stem = bigram_teste_stem %>% mutate(target = ifelse(target == 'yes',1,0))

# Selecionando os termos mais frequentes

freq=token_treino_stem %>%
  count(target, token) #calcular porcentagens

freq%>% arrange(desc(token))
freq%>% arrange(desc(n))

freq = freq %>% mutate(R1 = ifelse(target == 1,n,0),
                       R0 = n - R1)

freq%>% arrange(desc(token))
freq%>% arrange(desc(n))

freq = freq %>% 
  group_by(token) %>% 
  summarise(across(everything(), sum))%>% 
  select(token,n,R1,R0)

freq%>% arrange(desc(n))
freq%>% arrange(desc(R1))
freq%>% arrange(desc(R0))

freq = freq %>% mutate(perc = R1 / n)


# Entre os 10 termos mais frequentes, eliminar aqueles que possuem freq. relativa
# entre 0.4 e 0.6


(freq%>%arrange(desc(n)))$token[1:10]
(freq%>%arrange(desc(R1)))$token[1:10]
(freq%>%arrange(desc(R0)))$token[1:10]
dim(matriz_termo_docuemnto)

#vamos ficar com os 100 termos mais frequentes
(freq%>%arrange(desc(n)))[1:100,]
freq = (freq %>% arrange(desc(n)))[1:100,]
dim(freq)
M = 100
cte = M/sum(freq$n)
cte

dim(freq)
termos = freq$token
M = length(termos)
M
termos
tab=table(token_treino_stem[[1]])
tabteste = table(token_teste_stem[[1]])
tab[1:10]
N = length(tab)
Nteste = length(tabteste)

matriz_termo_docuemnto = matrix(0,N,M)
matriz_termo_documento_teste = matrix(0,Nteste,M)
colnames(matriz_termo_docuemnto) <- termos
rownames(matriz_termo_docuemnto) <- names(tab)
colnames(matriz_termo_documento_teste) <- termos
rownames(matriz_termo_documento_teste) <- names(tabteste)

rotulos = matrix(NA,N,1)#vai ser util jaja, vou aproveitar esse for
rotulosteste = matrix(NA,Nteste,1)
rownames(rotulos) <- names(tab)
rownames(rotulosteste) = names(tabteste)

for(i in 1:nrow(token_treino_stem)){
  teste_termos = (termos == token_treino_stem[[3]][i])
  if(sum(teste_termos) == 1){
    j = 1
    while(!teste_termos[j]){
      j = j + 1
    }
    #j guarda a coluna da matriz onde vamos incrementar o valor
    k = token_treino_stem[[1]][i]  
    matriz_termo_docuemnto[as.character(k),j] = 1
      #matriz_termo_docuemnto[as.character(k),j] + 1
    
    #guardar o rotulo desse documento aproveitando o indice
    rotulos[as.character(k),1] = token_treino_stem[[2]][i]
  }
}

for(i in 1:nrow(token_teste_stem)){
  teste_termos = (termos == token_teste_stem[[3]][i])
  if(sum(teste_termos) == 1){
    j = 1
    while(!teste_termos[j]){
      j = j + 1
    }
    #j guarda a coluna da matriz onde vamos incrementar o valor
    k = token_teste_stem[[1]][i]  
    matriz_termo_documento_teste[as.character(k),j] = 1
    #matriz_termo_docuemnto[as.character(k),j] + 1
    
    #guardar o rotulo desse documento aproveitando o indice
    rotulosteste[as.character(k),1] = token_teste_stem[[2]][i]
  }
}

matriz_termo_docuemnto = matriz_termo_docuemnto %>%
  cbind(rotulos)
colnames(matriz_termo_docuemnto)[101] = 'target'

matriz_termo_documento_teste = matriz_termo_documento_teste %>%
  cbind(rotulosteste)
colnames(matriz_termo_documento_teste)[101] = 'target'

View(matriz_termo_docuemnto)
View(matriz_termo_documento_teste)

nuvem = token_treino_stem %>% count(token, sort = TRUE)
wordcloud(words = nuvem$token, freq = nuvem$n,
          min.freq = 1,max.words=200,
          random.order=FALSE,rot.per=0.35,
          colors=brewer.pal(8, "Dark2"),
          scale = c(4,0.5))

wordcloud2::wordcloud2(data=nuvem, size = 1.6)

### Matriz termo-documento bigram


# Selecionando os termos mais frequentes

freq_bigram=bigram_treino_stem %>%
  count(target, token) #calcular porcentagens

freq_bigram%>% arrange(desc(token))
freq_bigram%>% arrange(desc(n))

freq_bigram = freq_bigram %>% mutate(R1 = ifelse(target == 1,n,0),
                       R0 = n - R1)

freq_bigram%>% arrange(desc(token))
freq_bigram%>% arrange(desc(n))

freq_bigram = freq_bigram %>% 
  group_by(token) %>% 
  summarise(across(everything(), sum))%>% 
  select(token,n,R1,R0)

freq_bigram%>% arrange(desc(token))
freq_bigram%>% arrange(desc(n))

(freq_bigram%>%arrange(desc(n)))$token[1:10]
(freq_bigram%>%arrange(desc(R1)))$token[1:10]
(freq_bigram%>%arrange(desc(R0)))$token[1:10]

#vamos ficar com os 100 termos mais frequentes
(freq_bigram%>%arrange(desc(n)))[1:100,]
freq_bigram = (freq_bigram %>% arrange(desc(n)))[1:100,]
dim(freq_bigram)
M = 100
cte = M/sum(freq_bigram$n)
cte

dim(freq_bigram)
termos_bigram = freq_bigram$token
M = length(termos_bigram)
M

tab=table(bigram_treino_stem[[1]])
tabteste=table(bigram_teste_stem[[1]])
tab[1:10]
N = length(tab)
Nteste = length(tabteste)

matriz_termo_documento_bigram = matrix(0,N,M)
matriz_termo_documento_bigram_teste = matrix(0,Nteste,M)
colnames(matriz_termo_documento_bigram) <- termos_bigram
rownames(matriz_termo_documento_bigram) <- names(tab)
colnames(matriz_termo_documento_bigram_teste) <- termos_bigram
rownames(matriz_termo_documento_bigram_teste) <- names(tabteste)

rotulos = matrix(NA,N,1) #vai ser util jaja, vou aproveitar esse for
rotulosteste = matrix(NA,Nteste,1)
rownames(rotulos) <- names(tab)
rownames(rotulosteste) = names(tabteste)

for(i in 1:nrow(bigram_treino_stem)){
  teste_termos = (termos_bigram == bigram_treino_stem[[3]][i])
  if(sum(teste_termos) == 1){
    j = 1
    while(!teste_termos[j]){
      j = j + 1
    }
    #j guarda a coluna da matriz onde vamos incrementar o valor
    k = bigram_treino_stem[[1]][i]  
    matriz_termo_documento_bigram[as.character(k),j] = 1
      #matriz_termo_documento_bigram[as.character(k),j] + 1
    
    #guardar o rotulo desse documento aproveitando o indice
    if(is.na(rotulos[as.character(k),1])){
    rotulos[as.character(k),1] = bigram_treino_stem[[2]][i]
    }
  }
}

for(i in 1:nrow(bigram_teste_stem)){
  teste_termos = (termos_bigram == bigram_teste_stem[[3]][i])
  if(sum(teste_termos) == 1){
    j = 1
    while(!teste_termos[j]){
      j = j + 1
    }
    #j guarda a coluna da matriz onde vamos incrementar o valor
    k = bigram_teste_stem[[1]][i]  
    matriz_termo_documento_bigram_teste[as.character(k),j] = 1
    #matriz_termo_documento_bigram[as.character(k),j] + 1
    
    #guardar o rotulo desse documento aproveitando o indice
    if(is.na(rotulosteste[as.character(k),1])){
      rotulosteste[as.character(k),1] = bigram_teste_stem[[2]][i]
    }
  }
}

matriz_termo_documento_bigram = matriz_termo_documento_bigram %>%
  cbind(rotulos)
colnames(matriz_termo_documento_bigram)[101] = 'target'

matriz_termo_documento_bigram_teste = matriz_termo_documento_bigram_teste %>%
  cbind(rotulosteste)
colnames(matriz_termo_documento_bigram_teste)[101] = 'target'

nuvem2 = bigram_treino_stem %>% count(token, sort = T)
wordcloud2::wordcloud2(data=nuvem2, size = 1.2)

View(matriz_termo_documento_bigram)
View(matriz_termo_documento_bigram_teste)

# Salvando as matrizes termo-documento
save(matriz_termo_docuemnto,matriz_termo_documento_bigram,
     matriz_termo_documento_teste, matriz_termo_documento_bigram_teste,
     file = 'MatrizesTermoDocumento.RData')
