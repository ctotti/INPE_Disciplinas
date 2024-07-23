# ======================= CLASSIFICAÇÃO RANDOM FOREST DE USO E COBERTURA DA TERRA =========================================
# 1 - IMPORTANDO E PREPARANDO DADOS
# 2 - PROCESSANDO RASTER (SpatRaster) 
# 3 - PROCESSANDO SHAPEFILE (Simple feature collection)
# 4 - APLICANDO O RANDOM FOREST

# Carregar os pacotes necessários ----------------------------------------------------------------------------------------
library(sf)                               # arquivos vetoriais
library(terra)                            # arquivos matriciais
library(magrittr)
library(tidyverse)
library(mapview)
library(randomForest)
library(caret)
# library(stars)
# library(MetBrewer)
# library(colorspace)
# library(rayshader)
# library(crayon)

# ============================== IMPORTANDO E PREPARANDO DADOS ============================================================
# Importar os raster e shapefile  -----------------------------------------------------------------------------------------
S2        <- terra::rast('Dados_Entrada/imgSentinel2_RF.tif')
amostras  <- sf::read_sf('Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')

# Reprojetar amostras para o CRS do raster, se necessário
if (st_crs(amostras) != crs(S2)) {
  amostras <- sf::st_transform(amostras, crs(S2))
}

# plotRGB(S2, 4,3,2, stretch='lin')

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

# =============================== PROCESSANDO SHAPEFILE ====================================================================
# Separar MULTIPOLYGON em POLYGON ------------------------------------------------------------------------------------------
amostras_separadas <- amostras %>%
  st_cast("POLYGON")

# Função para extrair pixels dentro de um polígono -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-
extrair_pix <- function(poligono, raster) {
  # Extrair valores de pixels dentro do polígono
  valores <- terra::extract(raster, poligono, df = TRUE)
  
  # Adicionar ID_Classe ao datafrae resultante
  valores$ID_Classe <- poligono$ID_Classe
  return(valores)
} 

# Aplicar a função a cada polígono e combinar os resultados em um único dataframe
pixels_amostrados <- do.call(rbind, lapply(1:nrow(amostras), function(i) extrair_pix(amostras[i, ], S2)))

# Calcular a área de cada polígono em m², km² e % --------------------------------------------------------------------------
amostras <- amostras %>%
  mutate(area_m2 = st_area(geometry)) %>%
  mutate(area_km2 = as.numeric(area_m2) / 1e6) %>%
  mutate(area_percent = (area_km2 / sum(area_km2) * 100))

# ================================ RANDOM FOREST ===========================================================================
# Dividindo os Pixels Amostrados em Conjuntos de Treinamento e Validação ----------------------------------------------------
# Configurar uma semente para reprodutibilidade 
set.seed(123)

# Convertendo ID_Classe para fator
pixels_amostrados$ID_Classe <- as.factor(pixels_amostrados$ID_Classe)

# Criar uma partição estratificada
trainIndex <- createDataPartition(pixels_amostrados$ID_Classe, p = 0.7, list = FALSE)

# Dividir os dados em treinamento e validação
train_data <- pixels_amostrados[trainIndex, ]
test_data <- pixels_amostrados[-trainIndex, ]

# Definindo bandas que serão utilizadas no modelo RF -----------------------------------------------------------------------
bandasRF <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'TCI', 'EVI','ID_Classe')

# Selecionar as colunas desejadas para o dataframe de validação e treinamento ----------------------------------------------
train_data_bandasRF <- train_data %>%
  select(all_of(bandasRF))

test_data_bandasRF <- test_data %>%
  select(all_of(bandasRF))

# Treinar o modelo Random Forest -------------------------------------------------------------------------------------------
rf_model <- randomForest(ID_Classe ~ ., data = train_data_bandasRF, ntree = 100, importance = TRUE)

# Fazer previsões (classificar)
predictions <- predict(rf_model, test_data_bandasRF)

# Adicionar previsões ao dataframe de teste
test_data_bandasRF$Predictions <- predictions

# Avaliar o modelo
conf_matrix <- confusionMatrix(test_data_bandasRF$Predictions, test_data_bandasRF$ID_Classe)
print(conf_matrix)


#PORCENTAGEM DE VOTOS
votes <- predict(rf_model, test_data_bandasRF, type = "vote")
votes_df <- as.data.frame(votes)

# df <- cbind(df, votes_df) # CASO QUEIRA JUNTAR NO ORIGINAL

head(votes_df)
