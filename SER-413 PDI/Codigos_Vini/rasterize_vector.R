# Código para rasterizar amostras 
library(sf)
library(terra)
library(dplyr)

# Função para rasterizar um vetor usando um raster de referência
rasterize_vector <- function(vector_path, column_name, template_raster_path, fill_value = -9999) {
  # Carregar o shapefile
  shape <- st_read(vector_path)
  
  # Verificar se a coluna existe no shapefile
  if (!column_name %in% colnames(shape)) {
    stop("Erro: A coluna especificada não existe no shapefile.")
  }
  
  # Verificar se a coluna é do tipo fator, caso contrário, converter
  if (!is.factor(shape[[column_name]])) {
    shape[[column_name]] <- as.factor(shape[[column_name]])
  }
  
  # Carregar o raster de template
  template <- rast(template_raster_path)
  
  # Verificar e alinhar CRS do shapefile e do raster de template
  shape_crs <- st_crs(shape)
  template_crs <- crs(template)
  
  if (shape_crs != template_crs) {
    message("Os sistemas de referência de coordenadas são diferentes. Convertendo o shapefile para o CRS do raster de template.")
    shape <- st_transform(shape, crs(template))
  }
  
  # Criar um raster base com a mesma extensão, resolução e projeção do raster de template
  raster_out <- rast(ncol = ncol(template), nrow = nrow(template), ext = ext(template), crs = crs(template))
  
  # Converter o shapefile para o formato adequado para rasterização
  shape_vect <- vect(shape)
  
  # Rasterizar o shapefile
  raster_out <- rasterize(shape_vect, raster_out, field = column_name, background = fill_value)
  
  # Substituir os valores NA pelo fill_value
  raster_out[is.na(raster_out)] <- fill_value
  
  return(raster_out)
}


image_path <- 'G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/imgSentinel2_RF.tif'
vector_path <- 'G:/Meu Drive/INPE/projeto_dissertacao/9_artigo_pdi/amostra_preliminar_3.shp'



raster_out <- rasterize_vector(vector_path, column_name = 'Classe', template_raster= image_path)
plot(raster_out)


# Definição da função
raster_samples_2_df <- function(image, samples) {
  
  # Verificar se as dimensões são iguais
  if (ncol(image) == ncol(samples) && nrow(image) == nrow(samples)) {
    message("A imagem e as amostras possuem a mesma quantidade de linhas e colunas")
  } else {
    stop("Erro: A imagem e as amostras não possuem as mesmas dimensões.")
  }
  
  img_bands <- nlyr(image)
  
  # Converter as amostras para um array bidimensional
  samples_array <- as.array(samples)
  
  # Selecionar a posição das amostras no espaço da imagem
  sample_selection <- lapply(1:img_bands, function(band) {
    band_array <- as.array(image[[band]])
    band_array[samples_array > 0]
  })
  
  # Converter para data frame
  X <- as.data.frame(do.call(cbind, sample_selection))
  colnames(X) <- names(image)
  
  return(X)
}

image = rast(image_path)

X <- raster_samples_2_df(image, raster_out)

