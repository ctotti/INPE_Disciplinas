# ======================= CLASSIFICAÇÃO RANDOM FOREST DE USO E COBERTURA DA TERRA =========================================
# -------------- PREDIÇÃO E PREPARAÇÃO PARA APLICAÇÃO COM COMPOUND MAXIMUM A PRIORI (CMAP) --------------------------------

# 1 - IMPORTANDO E PREPARANDO DADOS
# 2 - PROCESSANDO RASTER (SpatRaster) 
# 3 - PROCESSANDO SHAPEFILE (Simple feature collection)
# 4 - APLICANDO O RANDOM FOREST

# Carregar os pacotes necessários ----------------------------------------------------------------------------------------
library(sf)                               # arquivos vetoriais
library(terra)                            # arquivos matriciais
library(magrittr)
library(tidyverse)
library(dplyr)
library(mapview)
library(randomForest)
library(caret)
library(ggplot2)
# library(MetBrewer)
# library(colorspace)
# library(rayshader)
# library(crayon)

# ============================== IMPORTANDO E PREPARANDO DADOS ============================================================
# Importar os raster e shapefile  -----------------------------------------------------------------------------------------
S2        <- terra::rast('SER-413 PDI/Dados_Entrada/imgSentinel2_RF.tif')
amostras  <- sf::read_sf('SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')

# Reprojetar amostras para o CRS do raster, se necessário
if (st_crs(amostras) != crs(S2)) {
  amostras <- sf::st_transform(amostras, crs(S2))
}

plotRGB(S2, 4,3,2, stretch='lin')                                  # plota o raster com composição verdadeira

# =============================== PROCESSANDO RASTER ======================================================================
# Transformando raster em df ----------------------------------------------------------------------------------------------
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
nomes_bandas <- names(S2)

# Convertendo raster S2 em df
S2_df <- img2df(S2, band_names = nomes_bandas)

# Adicionando colunas vazias 'ID' e 'ID_Classe' ao dataframe 'S2_df'
# para correspondencia com df dos pixels que serão amostrados
S2_df <- S2_df %>%
  mutate(ID = numeric(n()),       # Adiciona coluna numérica vazia
         ID_Classe = factor(NA))  # Adiciona coluna fator vazia


# =============================== PROCESSANDO SHAPEFILE ====================================================================
# Separar MULTIPOLYGON em POLYGON ------------------------------------------------------------------------------------------
amostras_separadas <- amostras %>%
  st_cast("POLYGON")

# Função para extrair pixels dentro de um polígono -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
extrair_pix <- function(poligono, raster) {
  # Extrair valores de pixels dentro do polígono
  valores <- terra::extract(raster, poligono, df = TRUE)
  
  # Adicionar ID_Classe ao dataframe resultante
  valores$ID_Classe <- poligono$ID_Classe
  return(valores)
} 

# Aplicar a função a cada polígono e combinar os resultados em um único dataframe
# pixels_amostrados <- do.call(rbind, lapply(1:nrow(amostras), function(i) extrair_pix(amostras[i, ], S2)))
pixels_amostrados <- do.call(rbind, lapply(1:nrow(amostras_separadas), function(i) extrair_pix(amostras_separadas[i, ], S2)))


# Calcular a área de cada polígono em m², km² e % --------------------------------------------------------------------------
amostras <- amostras %>%
  mutate(area_m2 = st_area(geometry)) %>%
  mutate(area_km2 = as.numeric(area_m2) / 1e6) %>%
  mutate(area_percent = (area_km2 / sum(area_km2) * 100))

# ================================ RANDOM FOREST ===========================================================================
# Dividindo os Pixels Amostrados em Conjuntos de Treinamento e Validação ----------------------------------------------------
# Configurar uma semente para reprodutibilidade 
set.seed(123)

# Convertendo ID_Classe para 'fator'
# Garante que o randomForest() entenda os valores de 'ID_Classe' como valores discretos e não contínuos
pixels_amostrados$ID_Classe <- as.factor(pixels_amostrados$ID_Classe)
S2_df$ID_Classe <- as.factor(S2_df$ID_Classe)

# Criar uma partição estratificada
trainIndex <- createDataPartition(pixels_amostrados$ID_Classe, 
                                  p = 0.7,                            # p define a porcentagem dos dados para treinamento
                                  list = FALSE)

# Dividir os dados em treinamento e validação
train_data <- pixels_amostrados[trainIndex, ]
test_data <- pixels_amostrados[-trainIndex, ]

# Definindo bandas que serão utilizadas no modelo RF -----------------------------------------------------------------------
bandasRF <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'TCI', 'EVI','ID_Classe')

# Selecionar as colunas (bandas) desejadas para o dataframe de validação, treinamento e do raster original -----------------
train_data_bandasRF <- train_data %>%
  dplyr::select(all_of(bandasRF))

test_data_bandasRF <- test_data %>%
  dplyr::select(all_of(bandasRF))

S2_df_bandasRF <- S2_df %>%               
  dplyr::select(all_of(bandasRF))

# Treinamento, predição e avaliação do modelo Random Forest ----------------------------------------------------------------
# Treinar o modelo Random Forest
rf_model <- randomForest(ID_Classe ~ ., data = train_data_bandasRF, ntree = 100, importance = TRUE)

# Fazer previsões (classificar)
# predictions <- predict(rf_model, test_data_bandasRF)       # Aplica para df de teste
predictions <- predict(rf_model, S2_df_bandasRF)                    # Aplica para imagem inteira

# Adicionar previsões ao dataframe
# test_data_bandasRF$Predictions <- predictions
S2_df_bandasRF$Predictions <- predictions

# Avaliar o modelo
# conf_matrix <- confusionMatrix(test_data_bandasRF$Predictions, test_data_bandasRF$ID_Classe)
conf_matrix <- confusionMatrix(S2_df_bandasRF$Predictions, S2_df_bandasRF$ID_Classe)

print(conf_matrix)

# Obtendo porcentagem de votos de cada árvore para cada classe ------------------------------------------------------------
# PORCENTAGEM DE VOTOS [Vinicius]
votes <- predict(rf_model, test_data_bandasRF, type = "vote")
votes_df <- as.data.frame(votes)

head(votes_df)

# ==================== CONVERTENDO DATAFRAME CLASSIFICADO PARA O FORMATO RASTER ===========================================
# Função de conversão de dataframe para raster [Vinícius] -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
df2img <- function(df_img, image) {
  # Extrair dimensões da imagem de referência
  nrows <- dim(image)[1]
  ncols <- dim(image)[2]
  nbands <- ncol(df_img)  # Número de colunas no dataframe indica o número de bandas
  
  # Verificar se temos apenas uma banda
  if (nbands == 1) {
    data_vector <- as.vector(as.numeric(df_img[[1]]))
    
    # Criar o RasterLayer
    img_f <- raster(nrows=nrows, ncols=ncols)  # Inicializa um RasterLayer vazio
    values(img_f) <- data_vector               # Preenche o RasterLayer com valores
    extent(img_f) <- extent(image)             # Define a mesma extensão da imagem de referência
    projection(img_f) <- crs(image)            # Define o mesmo CRS da imagem de referência
    
  } else {
    # Tratar como array tridimensional para múltiplas bandas
    array_t <- array(0, dim = c(nrows, ncols, nbands))
    # Preencher o array com os dados de cada banda
    for (i in 1:nbands) {
      array_t[,,i] <- matrix(df_img[[i]], nrow = nrows, ncol = ncols, byrow = TRUE)
    }
    # Criar um RasterBrick
    img_f <- brick(array_t)
    projection(img_f) <- crs(image)
    extent(img_f) <- extent(image)
    crs(img_f) <- crs(image)
  }
  
  return(img_f)
}

# Convertendo dataframe para raster
votes_img <- df2img(votes_df, image = S2)
votes_img
