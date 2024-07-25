
library(terra)
library(sf)
library(caret)

r <- terra::rast('SER-413 PDI/Dados_Entrada/imgSentinel2_RF.tif')
f <- sf::read_sf('SER-413 PDI/Dados_Entrada/amostra_preliminar_3/amostra_preliminar_3.shp')

if (st_crs(f) != crs(r)) {
  f <- sf::st_transform(f, crs(r))
}

# Obtendo amostras de pontos para raster
set.seed(2)
p <- spatSample(r, 1000, xy=TRUE, replace=TRUE)
# Convertendo sf to SpatVector
v <- vect(f)
# Criando raster com infos das amostras nas dimensões do raster original
rv <- rast(v, ncols=877, nrows=929)
z <- rasterize(v, rv, 'ID_Classe')
plot(z)
lines(v)

# reamostrando raster das amostras para ter mesma res, extensão e dim que o raster original
z_resampled <- resample(z, r, method="bilinear")

# Adicionando raster das amostras como uma nova banda 'ID_Classe' ao raster original
r_combined <- c(r, z_resampled)
plot(r_combined$ID_Classe)