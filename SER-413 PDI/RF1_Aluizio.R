# 1 - Carregar bibliotecas

neededPackages = c("raster", "stats", "sf", "ggplot2", "sp", "dplyr", "tidyr", "sp", "sf","ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", "tidyselect" )
pkgTest = function(x) { if (x %in% rownames(installed.packages()) == FALSE) { install.packages(x, dependencies= TRUE) }; library(x, character.only = TRUE) }
for (package in neededPackages) { pkgTest(package) }

library(terra)
library(cluster)
library(parallel)
library(terra)
library(raster) 
library(sf)


# 2 -  ==== FUNÇÕES ====


# 2.0  Ajustar a resolução, extensão e projeção dos rasters
adjust_raster <- function(raster_path, reference, mask) {
  cat("Processando..:", raster_path, "\n")
  r <- rast(raster_path)
  
  cat("  Verificando CRS\n")
  if (!compareCRS(r, reference)) {
    cat("  Projeções diferentes. Reprojetando...\n")
    r <- project(r, crs(reference))
  }
  
  cat("  Reamostrando.\n")
  r <- resample(r, reference, method = "bilinear") #consultar se o método de reamostragem é o ideal para o uso
  
  cat("  Verificando extensão\n")
  if (ext(r) != ext(reference)) {
    cat("  Extensões diferentes. Recortando...\n")
    r <- crop(r, ext(reference))
  }
  
  cat("  Verificando extensão da máscara\n")
  if (ext(r) != ext(mask)) {
    cat("  Extensões da máscara e do raster não coincidem. Ajustando máscara...\n")
    mask <- crop(mask, ext(r))
  }
  
  cat("  Aplicando máscara de nuvens.\n")
  r <- mask(r, mask, maskvalue = 1)
  
  cat("  Verificando valores\n")
  if (any(is.na(values(r)))) {
    cat("  Máscara aplicada \n")
  } else {
    cat("  Erro ao aplicar a máscara...\n")
  }
  
  return(r)
}

# 2.1 Entropia
shannon.entropy = function (prob_RF, raster.ref, path_entropy, samples_area, path_samples) {
  
  # Calcula a entropia do raster
  prob_RF.df <- as.data.frame(prob_RF)
  entropy <- -((prob_RF.df$`1` * log2(prob_RF.df$`1`)) + (prob_RF.df$`2` * log2(prob_RF.df$`2`)))
  entropy.raster = raster(raster.ref) #Raster reference 
  entropy.raster = setValues(entropy.raster, entropy) #Set entropy values
  entropy.raster[is.na(entropy.raster)] = 0.000
  valores_invalidos <- is.na(entropy.raster) | !is.numeric(entropy.raster)
  if (any(valores_invalidos)) {
    message("Aviso: Existem valores NA ou não numéricos no raster de entropia")
    entropy.raster[valores_invalidos] <- 0
  
  # Plota histograma de raster
  writeRaster(entropy.raster, filename = path_entropy, format="GTiff", overwrite = TRUE)
  plot (hist(entropy.raster), main = paste("Histograma entropia", name_area, sep = ' '),xlab = "Values")
  
  amostras.entropy = raster::extract(entropy.raster, samples_area)
  amostras.entropy.df = as.data.frame(amostras.entropy)
  amostras.entropy.sp = SpatialPointsDataFrame(samples_area@coords, amostras.entropy.df)
  crs.ref = crs(samples_area) #set reference projection 
  crs(amostras.entropy.sp) = crs.ref #set projection
  names(amostras.entropy.sp) = c("entropia")
  writeOGR(amostras.entropy.sp, dsn = path_samples, driver = "ESRI Shapefile", layer = path_samples, morphToESRI = TRUE, overwrite = TRUE)
}

# salva um .shp com os valores da entropia para as amostras 
amostras.entropy = raster::extract(entropy.raster, amostras_atributos)
amostras.entropy.df = as.data.frame(amostras.entropy)
amostras.entropy.sp = SpatialPointsDataFrame(amostras_atributos@coords, amostras.entropy.df)}
                                   

# 2.2 Gera um raster com a proporcao de vezes que um pixel foi classificado como "Agua" considerando todas as arvores do RF.
raster.ref <- (rf.class)
proportion.agua = function (raster.ref, prob_RF, class.raster, path) {
  prob.agua = raster(raster.ref) #Empty raster created from another raster reference 
  prob.agua = setValues(prob.agua, prob_RF[,2] ) #the second column is relative to wetland class 
  prob.agua [class.raster == 0] = NA #mask the values only to wetland class 
  writeRaster(prob.agua, filename = path, format="GTiff")
} 
  
  ------------------------------------------------------------
    
# 3 -  ==== ATRIBUTOS ==== 
  
  # Ler pasta com os atributos
Atributos <- list.files(path = "D:\\Dissertação\\R\\RF1\\Atributos", pattern = ".tif", full.names = T)
  
  # Criar stack com os atributos
Atributos <- stack(Atributos)

  #Carregar rasters
path_to_rasters <- "D:\\Dissertação\\R\\RF1\\Atributos"
  
# Caminho da máscara de nuvens e sombras
cloud_mask_path <- "D:\\Dissertação\\Nuvens e Sombras\\Árvore de decisão\\Deteccao_de_nuvens_sombras_arvore\\Class_rpart_Todos_atbA2MEDIA.tif"
  
# Listar todos os arquivos .tif 
input_files <- list.files(path = path_to_rasters, pattern = ".tif$", full.names = TRUE)
  

num_files <- length(input_files)

if (num_files == 0) {
  stop("Nenhum arquivo .tif encontrado")
} else {
  cat("Número de arquivos .tif encontrados:", num_files, "\n")
}
  
# Carregar o raster de referência para os ajustes
  reference_raster <- rast(input_files[12])
  plot(reference_raster)
  
# Carregar a máscara de nuvens/sombras
  cloud_mask <- rast(cloud_mask_path)
  plot(cloud_mask)
  
# Ajustar a máscara de nuvens à resolução e extensão do raster de referência
  adjusted_cloud_mask <- resample(cloud_mask, reference_raster, method = "near")
  adjusted_cloud_mask <- crop(adjusted_cloud_mask, ext(reference_raster))
  
# Ajustar todos os rasters à mesma resolução, extensão e projeção
  adjusted_rasters <- list()
  for (i in seq_along(input_files)) {
    cat("Processando raster", i, "de", length(input_files), "\n")
    try({
      adjusted_raster <- adjust_raster(input_files[i], reference_raster, adjusted_cloud_mask)
      if (!is.null(adjusted_raster)) {
        adjusted_rasters[[length(adjusted_rasters) + 1]] <- adjusted_raster
      } else {
        cat("  Raster ajustado é n\n")
      }
    }, silent = FALSE)
  }
  
  # Verificar 
  if (length(adjusted_rasters) == 0) {
    stop(" raster não foi ajustado corretamente.")
  }
  
  # Empilhar os rasters ajustados
  stack <- rast(adjusted_rasters)
  Atributos <- stack
  # Ler como DataFrame
  
  Atributos.df <- data.frame(values(Atributos)) 
  
  
------------------------------------------------------------
#   4 -    ==== AMOSTRAGEM ==== 
    
# um shp para cada classe na mesma pasta
  amostras = list.files("D:\\Dissertação\\R\\RF1\\Amostragem", pattern = ".shp", full.names = T)
  amostras = lapply(amostras, st_read) #junta os .shp em uma lista
  amostras[[2]] <- st_transform(amostras[[2]], st_crs(amostras[[1]])) # Converter o CRS do segundo conjunto de dados para o CRS do primeiro
  amostras_f = rbind(amostras[[1]], amostras[[2]]) #combina em um ?nico vetor
  
  #Extrair valores dos raster para as amostras
  atributos_amostras = raster::extract(Atributos, amostras_f)
  
  # Acessar apenas os dados em amostras_f
  dados_amostras <- st_drop_geometry(amostras_f)
  
  # Unir a informacao da classe da amostra com os valores dos atributos
  amostras_atributos <- data.frame(Classe = dados_amostras$id, atributos_amostras)
  
  
-----------------------------------------------------------------------------------
# 4 -  MODELO RANDOM FOREST
  
# mudar para fator a fim do modelo entender como classificação e não regressão
amostras_atributos$Classe = as.factor(amostras_atributos$Classe)  
  
# Amostras de Treino e Teste
set.seed(12) 

split.treino <- sample.split(amostras_atributos$Classe, SplitRatio = 0.7)

treino.rf <-amostras_atributos[split.treino,]

teste.rf <- amostras_atributos[split.treino == F, ] 


# modelo
RF_modelo = randomForest(Classe~., ntree = 500, data = treino.rf, classProbs = TRUE)

print(RF_modelo)

#Tune Mtry e plotar OOB - tentar entender melhor

tuneRF(treino.rf   %>% select(-Classe), treino.rf$Classe, ntreeTry = 500, stepFactor = 2, improve = 0.001, trace <- TRUE, plot <- TRUE, doBest = F) #REVISAR

RF_modelo_mtry <- randomForest(Classe~., ntree = 500, mtry = 1, data = treino.rf, importance = T, classProbs = TRUE, localImp = TRUE)

# Convertendo os dados para um data frame
oob_error <- as.data.frame(RF_modelo_mtry$err.rate) %>% 
  mutate(ntree = as.numeric(row.names(.)))

# Criando o gráfico
ggplot(oob_error, aes(x = ntree, y = OOB)) +
  geom_line() + 
  theme_bw() + 
  labs(x = "ntree", y = "erro OOB") +
  theme(
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),)
    
plot(oob_error)

# Salvar o modelo RF

#saveRDS(RF_modelo_mtry, file = "./caminho, ascii = T)

-----------------------------------------------------------------------------------
  # 5 - PREDICT - Processo de Classificação

# Calcular as previsões do modelo nos dados de teste (vai servir pra métricas de desempenho depois)
rf.class_fator <-raster::predict(RF_modelo_mtry, teste.rf, progress = "text", type = "response")


# Classificar toda uma imagem


#Dá errado rf.class <- predict(RF_modelo_mtry, raster, progress = "text", type = "response")
rf.class <- predict(Atributos, RF_modelo_mtry, progress = "text", type = "response")
prob_RF <- stats::predict(RF_modelo_mtry, Atributos.df, type = "prob") #probabilidade de pixel

# Escrever o objeto raster em um arquivo
writeRaster(rf.class, "Classificacao_teste_rf.tif", overwrite = TRUE)
writeRaster(prob_RF, overwrite = TRUE)

------------------------------------------------------------------------------------
  # 8 - Avaliação
  
MatrizConf.RF <- confusionMatrix(data = rf.class_fator, reference = teste.rf$Classe)
print(MatrizConf.RF)


precisão <- MatrizConf.RF$byClass["Precision"]
sensibilidade <- MatrizConf.RF$byClass["Recall"]
especificidade <- MatrizConf.RF$byClass["Specificity"] 
print(precisão)
print(sensibilidade)
print(especificidade)

f1_score <- MatrizConf.RF$byClass["F1"]


# Calcula entropia

name_area <- "Teste"
path_entropy <- "D:/Dissertação/Bahia/R/entropia_teste.tif"
path_samples <- "D:/Dissertação//Bahia/R/amostras_entropia" 
samples_area <- (amostras)


 




    
    
  