library(raster)
library(sf)
library(randomForest)
library(dplyr)
library(caret)
library(devtools)
devtools::install_github("vdlucas-queiroz/imageTools")

# Importanto imagem 
# image_path <- 'G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/imgSentinel2_RF.tif'
image_path <- 'SER-413 PDI/Dados_Entrada/imgSentinel2_RF.tif'
image <- brick(image_path)# /(10^-11.5)
# names(image) <- c('HH','HV')
image_df <- imageTools::img2df(image,names(image)) #imageTools - transformando imagens para dataframe

# # Importando vetor (PONTOS)
# vector_path <- 'G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/amostra_preliminar_3.shp'
vector_path <- 'SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp'
vectors <-  st_read(vector_path)

# Extraindo os atributos do RasterBrick baseado na localização espacial do vetor
vector_attributes <- raster::extract(x= image, y=vectors)


#TEM QUE SER MULTIPOLYGON e no mesmo SCR do raster... mas tá filé!
df_samples <- do.call(rbind, lapply(seq_along(vector_attributes), function(i) {
  if (nrow(vector_attributes[[i]]) > 0) {  # Verifica se há valores para evitar o problema de dimensão
    # Cria um data.frame com a classe e os valores de pixels, preservando os nomes das colunas originais
    data.frame(Class = rep(vectors$Classe[i], nrow(vector_attributes[[i]])), 
               vector_attributes[[i]])
  }
}))

classes <- unique(df_samples$Class)

df_samples$Class <- as.factor(df_samples$Class)

#--------------------------------------------------

# Divisão dos dados em treinamento e teste
set.seed(123)  # Para reprodutibilidade
split <- caret::createDataPartition(df_samples$Class, p = 0.7, list = FALSE)
train_data <- df_samples[split, ]
test_data  <- df_samples[-split, ] #usar apenas para a avaliação do modelo

# Criando modelo random forest
#MODELO RF  (NESSE CASO 500 ÁRVORES = 500 VOTOS)
rf_model <- randomForest(Class ~ ., data = train_data, ntree = 252, importance = TRUE)
print(rf_model) # Avaliação do MODELO RF.


# Classificação da imagem
predictions <- predict(rf_model, image_df)
image_df$prediction <- predictions
str(image_df) #conferindo estrutura

# Computando votos de cada pixel da imagem como estimativa de probabilidade
votes <- predict(rf_model, image_df , type = "vote")
votes_df <- as.data.frame(votes) #transformação para dataframe

# Imagens de procentagem de votos por classe
image_voting <- imageTools::df2img(votes_df,image) #imageTools
# write_path <- "G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/imagem_porcentagem_votos.TIF"
write_path <- "SER-413 PDI/Dados_Saida/imagem_porcentagem_votos.TIF"
writeRaster(image_voting, filename = write_path, format = "GTiff",overwrite=TRUE)

# Classificação RF (Cobertura)
image_classification <- imageTools::df2img(as.data.frame(image_df$prediction),image) #imageTools
# write_path <- "G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/imagem_classificada_RF.TIF"
write_path <- 'SER-413 PDI/Dados_Saida/imagem_classificada_RF.TIF'
writeRaster(image_classification, filename = write_path, format = "GTiff",overwrite=TRUE)


# Avaliação da classificação (TEM QUE IMPORTAR OUTRO CONJUNTO COMO TESTE...
# NO SPLIT ESTAMOS DIVIDINDO O CONJUNTO TODO EM TREINO E TESTE E  ISSO PODE DAR PROBLEMA DE AUTOCORRELAÇÃO ESPACIAL)
# SUGESTÃO: FAZER UM CONJUNTO DE REFERÊNCIA ANTES E INDEPENDENTE DO PRIMEIRO. TREINA COM O PRIMEIRO COMPLETO (NAO PRECISA DO SPLIT)
# E TESTA COM O SEGUNDO COMPLETO

predictions <- predict(rf_model, test_data)
test_data$prediction <- predictions
test_data$prediction <- as.factor(test_data$prediction) #PREDIÇÃO E VERDADE DEVEM ESTAR EM FATOR


confusion_matrix <- caret::confusionMatrix(test_data$Class,test_data$prediction)
imageTools::metrics_calculation(confusion_matrix$table)

print(conf_matrix)

# Extrair a acurácia global
accuracy <- conf_matrix$overall['Accuracy']
print(accuracy)


