library(terra)
library(sf)

# Importar o raster e o shapefile
S2 <- terra::rast('SER-413 PDI/Dados_Entrada/imgSentinel2_RF.tif')
amostras <- sf::read_sf('SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')

# Reprojetar amostras para o CRS do raster, se necessário
if (sf::st_crs(amostras) != terra::crs(S2)) {
  amostras <- sf::st_transform(amostras, crs(S2))
}

# Criar uma máscara rasterizada com base nos valores de ID_Classe
mask_raster <- terra::rasterize(amostras, S2[[1]], field = "ID_Classe", update = TRUE)

plot(mask_raster$B1)

# Adicionar a máscara como uma nova banda ao raster original
S2_classified <- c(S2, mask_raster)

# Adicionar nome correto à nova banda
names(S2_classified) <- c(names(S2), "ID_Classe")

# Verificar os nomes finais das bandas
print(names(S2_classified))

# Verificar o resultado final
plot(S2_classified$ID_Classe, main = "Raster Original com Banda de Classes")
plot(S2_classified)

# Salvar o raster classificado, se necessário
# terra::writeRaster(S2_classified, 'SER-413 PDI/Dados_Entrada/S2_classified.tif', overwrite = TRUE)

# Função para converter imagem em dataframe
img2df <- function(image, band_names) {
  nband <- nlyr(image)
  
  data_vec <- vector("list", nband)
  names(data_vec) <- band_names
  
  for (i in 1:nband) {
    data_vec[[i]] <- as.vector(terra::values(image[[i]]))
  }
  
  df_img <- as.data.frame(data_vec)
  
  return(df_img)
}

# Obtendo nomes das bandas do raster
nomes_bandas <- names(S2_classified)

# Convertendo raster S2_classified em dataframe
S2_classified_df <- img2df(S2_classified, band_names = nomes_bandas)

# Verificar as primeiras linhas do dataframe resultante
head(S2_classified_df)









