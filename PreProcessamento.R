# TCC - Thiago Lessa da Costa

# Lista de stopwords em português: 
#https://gist.githubusercontent.com/alopes/5358189/raw/2107d809cca6b83ce3d8e04dbd9463283025284f/stopwords.txt


# Carregando pacotes
library(tidytext)
library(readr)
library(tidyverse)
library(textstem)
#library(casr)
library(installr)
library(spelling)
library(tm)
library(hunspell)
library(igraph)
library(ggraph)
library(wordcloud)
library(lexicon)
library(caret)

# Carregando os dados
base = read_delim('baseg1.txt', delim = ',')
View(base)

# Criando uma variável índice
base$id = 1:length(base$texto)
base = base %>% select(id,texto,target)

base %>% group_by(target) %>% summarize(n())


# Dividindo os dados em treino e teste
#inTrain = createDataPartition(base$target, p=0.8, list = F)
#treino = base[inTrain,]
#teste = base[-inTrain,]

# Pra manter a proporção de 80/20 da classe 'yes' no treino/teste e mantê-los balançeados,
# vamos escolher para o banco de treino, 672 elementos (336 yes/ 336 no)
# e o banco de teste vai ter 168 elementos (83 yes/ 85 no)

set.seed(218054095)

n_treino_1 = 336
n_treino_0 = 336
n_teste_1 = 83
n_teste_0 = 85

sub_base_1 = base %>% filter(target == 'yes')
sub_base_0 = base %>% filter(target == 'no')

inTrain_1 =  sample(sub_base_1$id, n_treino_1)
inTrain_0 = sample(sub_base_0$id, n_treino_0)

treino = base %>% filter(id %in% c(inTrain_0,inTrain_1))

sub_base_0 = sub_base_0 %>% filter(!id %in% inTrain_0)
sub_base_1 = sub_base_1 %>% filter(!id %in% inTrain_1)

inTest_1 = sample(sub_base_1$id, n_teste_1)
inTest_0 = sample(sub_base_0$id, n_teste_0)

teste = base %>% filter(id %in% c(inTest_0, inTest_1))

treino %>% group_by(target) %>% summarise(n())
teste %>% group_by(target) %>% summarise(n())

treino$id %in% teste$id %>% sum()

save(treino,teste, file = 'BancosTreinoTeste.RData')


# Carregando pacotes
library(tidytext)
library(readr)
library(tidyverse)
library(textstem)
#library(casr)
library(installr)
library(spelling)
library(tm)
library(hunspell)
library(igraph)
library(ggraph)
library(wordcloud)
library(lexicon)
library(caret)
install.packages("hashmap")
library(hashmap)
devtools::install_github("nathan-russell/hashmap")


# Carregar bancos de treino e teste
load('BancosTreinoTeste.RData')

## Pré-processamento

# Tokenização - unigrama
token_treino = treino %>% unnest_tokens(token,texto)
token_teste = teste %>% unnest_tokens(token,texto)

# Tokenização - bigrama
bigram_treino = treino %>% unnest_tokens(token,texto, token = 'ngrams', n = 2)
bigram_teste = teste %>% unnest_tokens(token,texto, token = 'ngrams', n = 2)

# Remoção de stopwords
sw = read_delim('stopwords_pt.txt',delim = ',')
sw$stopwords = sw$stopwords %>% str_trim

token_treino  =  token_treino %>% anti_join(sw, by = c('token' = 'stopwords'))
token_teste = token_teste %>% anti_join(sw, by = c('token' = 'stopwords'))

bigram_treino = tidyr::separate(bigram_treino, 'token',c('token1','token2'), sep = ' ', remove = F)
bigram_treino = bigram_treino %>% filter((!token1 %in% sw$stopwords) & (!token2 %in% sw$stopwords))
bigram_teste = tidyr::separate(bigram_teste, 'token',c('token1','token2'), sep = ' ', remove = F)
bigram_teste = bigram_teste %>% filter((!token1 %in% sw$stopwords) & (!token2 %in% sw$stopwords))


#### NORMALIZAÇÃO ####

install.packages("rslp")
library(rslp)

# Normalização - unigrama
tokesn_stem = rslp(token_treino$token)
token_treino_stem = token_treino %>% mutate(token = tokesn_stem)

tokesn_stem_teste = rslp(token_teste$token)
token_teste_stem = token_teste %>% mutate(token = tokesn_stem_teste)

View(token_treino_stem)
View(token_teste_stem)

# Normalização - bigrama
bigram_treino = bigram_treino %>% filter(!is.na(token1) & !is.na(token2))
bigram_teste = bigram_teste %>% filter(!is.na(token1) & !is.na(token2))

bigram_treino_stem = bigram_treino %>% mutate(token1 = rslp(token1),
                                              token2 = rslp(token2))

bigram_teste_stem = bigram_teste %>% mutate(token1 = rslp(token1),
                                            token2 = rslp(token2))

bigram_treino_stem = bigram_treino_stem %>% mutate(token = paste(token1, token2, sep = ' '))
bigram_teste_stem = bigram_teste_stem %>% mutate(token = paste(token1, token2, sep = ' '))

# Salvar o banco

save(token_treino_stem, bigram_treino_stem,token_teste_stem,
     bigram_teste_stem, file = 'BancosTreinoTesteStem.RData')
