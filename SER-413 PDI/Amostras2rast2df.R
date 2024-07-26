# r <- rast(xmin=0, ncols=18, nrows=18)
# 
# # generate points
# set.seed(1)
# p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)
# 
# # rasterize points as a matrix
# x <- rasterize(p, r, fun=sum)
# y <- rasterize(p, r, value=1:nrow(p), fun=max)
# 
# # rasterize points as a SpatVector
# pv <- vect(p)
# 
# xv <- rasterize(pv, r, fun=sum)
# 
# # Polygons
# f <- system.file("ex/lux.shp", package="terra")
# # f <- sf::read_sf('SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')
# v <- vect(f)
# # v <- vect(f)
# r <- rast(v, ncols=75, nrows=100)
# # r <- rast(v, ncols=929, nrows=877)
# z <- rasterize(v, r, "NAME_2")
# # z <- rasterize(v, r, 'ID_Classe')
# plot(z)
# lines(v)



r <- terra::rast('SER-413 PDI/Dados_Entrada/imgSentinel2_RF.tif')
# generate points
set.seed(2)
p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)

# Polygons
f <- sf::read_sf('SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')

# Reprojetar amostras para o CRS do raster, se necessário
if (st_crs(f) != crs(r)) {
  f <- sf::st_transform(f, crs(r))
}

v <- vect(f)
rv <- rast(v, ncols=877, nrows=929)

z <- rasterize(v, rv, 'ID_Classe')
plot(z)
lines(v)

# --------------------------------------------------------------------------------
# Ensure that z has the same dimensions and CRS as r
# Resample z to match r's resolution and extent
z_resampled <- resample(z, r, method="bilinear")

# Add z as a new band to r
r_combined <- c(r, z_resampled)

# Plot to check the result
plot(r_combined$ID_Classe)


# # Renomear a nova banda, se necessário
# names(r_combined)[nlyr(r_combined)] <- "ID_Classe"
# 
# # Verificar os resultados
# plot(r_combined$ID_Classe, main = "Raster com Nova Banda de Classes")

# -------- CONVERTENDO RASTERS EM DATAFRAME
# Função para converter imagem em df [Vinicius] -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
img2df <- function(image,band_names){
  nrow <- dim(image)[1]
  ncol <- dim(image)[2]
  nband <- dim(image)[3]
  
  data_vec <- vector("list", nband)
  names(data_vec) <- band_names
  
  for(i in 1:nband){
    data_vec[[i]] <- as.vector(image[[i]])
  }
  
  df_img <- as.data.frame(data_vec)
  
  return(df_img)
} 

# Obtendo nomes das bandas do raster
nomes_bandas <- names(r_combined)

# Convertendo raster r_combined em df
amostras_rast_df <- img2df(r_combined, band_names = nomes_bandas)
amostras_rast_df


# ================================ RANDOM FOREST ===========================================================================
# Dividindo os Pixels Amostrados em Conjuntos de Treinamento e Validação ---------------------------------------------------
# Configurar uma semente para reprodutibilidade 
set.seed(111)

# # Convertendo ID_Classe para 'fator'
# # Garante que o randomForest() entenda os valores de 'ID_Classe' como valores discretos e não contínuos
# pixels_amostrados$ID_Classe <- as.factor(pixels_amostrados$ID_Classe)
# S2_df$ID_Classe <- as.factor(S2_df$ID_Classe)
pixels_amostrados <- amostras_rast_df

# Carregar pacotes necessários
library(caret)

# Filtrar linhas onde 'ID_Classe' não é NaN
filtered_df <- pixels_amostrados[!is.na(pixels_amostrados$ID_Classe), ]

# Configurar a proporção de treinamento e teste
set.seed(123) # Definir a semente para reprodutibilidade
train_index <- createDataPartition(filtered_df$ID_Classe, p = 0.7, list = FALSE)

# Dividir os dados em conjuntos de treinamento e teste
train_data <- filtered_df[train_index, ]
test_data <- filtered_df[-train_index, ]


# Definindo bandas que serão utilizadas no modelo RF -----------------------------------------------------------------------
bandasRF <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'TCI', 'EVI','ID_Classe')

# Selecionar as colunas (bandas) desejadas para o dataframe de validação, treinamento e do raster original -----------------
train_data_bandasRF <- train_data %>%
  dplyr::select(all_of(bandasRF))

test_data_bandasRF <- test_data %>%
  dplyr::select(all_of(bandasRF))

S2_df_bandasRF <- pixels_amostrados %>%               
  dplyr::select(all_of(bandasRF))

# Treinamento, predição e avaliação do modelo Random Forest ----------------------------------------------------------------
# Treinar o modelo Random Forest
rf_model <- randomForest(ID_Classe ~ ., data = train_data_bandasRF, ntree = 100, importance = TRUE)

# Fazer previsões (classificar)
# predictions <- predict(rf_model, test_data_bandasRF)       # Aplica para df de teste
# predictions <- predict(rf_model, S2_df_bandasRF)                    # Aplica para imagem inteira
predictions <- predict(r_combined, rf_model)                    # Aplica para imagem inteira (troca imagem pelo modelo)
predictions_raster <-raster::predict(rf_model, r_combined, progress = "text", type = "response")

# Adicionar previsões ao dataframe
# test_data_bandasRF$Predictions <- predictions
S2_df_bandasRF$Predictions <- predictions

# Avaliar o modelo
# conf_matrix <- confusionMatrix(test_data_bandasRF$Predictions, test_data_bandasRF$ID_Classe)
# conf_matrix <- confusionMatrix(data = S2_df_bandasRF$Predictions, reference = S2_df_bandasRF$ID_Classe)
conf_matrix <- confusionMatrix(data = predictions, reference = r_combined$ID_Classe)

print(conf_matrix)



