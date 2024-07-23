library(randomForest)
library(dplyr)
library(caret)

data(iris) #LEITURA DE UM DATAFRAME ALEATÓRIO NO R
# supondo que a "verdade" é coluna iris... 
                          # mudar se necessário
df <- iris

# Divisão dos dados em treinamento e teste
set.seed(123)  # Para reprodutibilidade
split <- caret::createDataPartition(iris$Species, p = 0.8, list = FALSE)
train_data <- iris[split, ]
test_data <- iris[-split, ]


#MODELO RF  (NESSE CASO 500 ÁRVORES = 500 VOTOS)
rf_model <- randomForest(Species ~ ., data = train_data, ntree = 500, importance = TRUE)

#PREDIÇÃO (CLASSIFICAÇÃO)
predictions <- predict(rf_model, test_data) # ou a image inteira, no caso.
df$Predictions <- predictions

#PORCENTAGEM DE VOTOS
votes <- predict(rf_model, data, type = "vote")

votes_df <- as.data.frame(votes)

# df <- cbind(df, votes_df) # CASO QUEIRA JUNTAR NO ORIGINAL

View(votes_df)
