# =====================================================================================================================
#                  SCRIPT 2 - CÁLCULO DO PROCESSO DE ANÁLISE HIERÁRQUICA (AHP) A PARTIR DAS
#                     VARIÁVEIS INDICADORAS NORMALIZADAS EM UMA ESCALA DE PESO DE 1 A 4
#                                         NO SCRIPT 'DADOS.R'
# =====================================================================================================================

library(sf)
library(dplyr)
library('ggplot2')
library(purrr)
library(terra)

# ====================================================================================================================
# O FLUXO GERAL PARA O CÁLCULO DA AHP ENVOLVE:
#   1° Obtenção das variáveis indicadoras e reclassificação para uma escala de peso comum,
#      nesse caso de 1 a 4. Realizada no Script 1 ('Dados.R').
#   2° Geração da Matriz de Comparação Pareada, com verificação da Razão de Consistência.
#      A melhor combinação de pesos, respeitando a hierarquia das variáveis definida pelo conhecimento
#      de especialistas e estabelecido na literatura, foi definida no script em Python: https://colab.research.google.com/drive/1OpTDllYD1ZJVh_I_-3NoDze75oSQiY_x?usp=sharing. 
#      E a visualização desses valores em formato tabular está disponível aqui: https://docs.google.com/spreadsheets/d/16AlU1qUzUdnwCl2oI2E1gAW_4pQ5pMvovAY8SDRKFnc/edit?usp=sharing.
#   3° Obtenção dos pesos de cada variável (disponível também em formato tabular no link acima)
#   4° Cálculo de bandas a partir dos pesos obtidos para cada variável indicadora


# ESTE SCRIPT CORRESPONDE À QUARTA ETAPA DO PROCESSO DE ANÁLISE HIERÁRQUICA
#   (CÁLCULO DE BANDAS A PARTIR DOS PESOS DE CADA VARIÁVEL INDICADORA)


# O SCRIPT ESTÁ ORGANIZADO DA SEGUINTE MANEIRA:
#   1° - IMPORTAÇÃO DA AREA OF INTEREST (AOI) - ASSURINI
#   2° - IMPORTAÇÃO DAS VARIÁVEIS INDICADORAS
#   3° - VERIFICAÇÕES DE PADRONIZAÇÃO E COMPATIBILIDADE
#   4° - CORREÇÕES DE COMPATIBILIDADE
#   5° - CHECAGENS PÓS CORREÇÃO DE COMPATIBILIDADE
#   6° - CÁLCULO DE BANDAS
#   7° - RECLASSIFICAÇÃO AHP
#   8° - EXPORTAÇÃO RESULTADOS AHP


# 1° IMPORTANDO E PREPARANDO AOI ------------------------------------------------------------------------------------
# AOI dissolvido
AOI_diss <- st_read('Dados/AOI/PAs_Assurini_dissolved.gpkg') |> 
  st_set_crs(4674) |> 
  st_transform(31982)

plot(st_geometry(AOI_diss))         # Viz

AOI_diss_vect <- vect(AOI_diss)     # Converter para formato SpatVector

# 2° IMPORTANDO VARIÁVEIS INDICADORAS ------------------------------------------------------------------------------
# Definir o diretório onde estão os arquivos
dir_rasters <- "Dados_R/VIs/"

# Listar todos os arquivos .tif
arquivos <- list.files(path = dir_rasters, 
                       pattern = "\\.tif$", 
                       full.names = TRUE)

# Função para extrair o nome da variável (antes do "__")
extrair_nome_variavel <- function(caminho) {
  nome_arquivo <- basename(caminho)        # Pega apenas o nome do arquivo
  strsplit(nome_arquivo, "__")[[1]][1]     # Divide no "__" e pega a primeira parte
}

# Importar cada raster e atribuir a uma variável no ambiente global
walk(arquivos, ~ {
  nome_var <- extrair_nome_variavel(.x)
  raster_obj <- rast(.x)                   # Usa terra::rast() (ou raster::raster())
  assign(nome_var, raster_obj, envir = .GlobalEnv)
})

# Verificar os objetos criados
ls()

# 3° VERIFICAÇÕES DE PADRONIZAÇÃO E COMPATIBILIDADE ------------------------------------------------------------------------------
rasters <- list(CONC_ESTAB, DECLIVIDADE, ESTRADAS, LULC, TAM_IMOVEIS, TEXTURA)

# Verificar min e max dos rasters (devem estar entre 1 e 4)
for (r in rasters) {
  cat("Raster:", names(r), "\n")
  cat("  Min:", minmax(r)[1], "\n")
  cat("  Max:", minmax(r)[2], "\n\n")
}

# Checar extensão
lapply(rasters, ext)

# Checar resoluções
lapply(rasters, res)

# Checar CRS
sapply(rasters, st_crs)

# 4° CORREÇÕES DE COMPATIBILIDADE --------------------------------------------------------------------------------------------------
# Função para recortar e alinhar os rasters
processar_raster <- function(raster, nome, template, mascara) {
  # Skip compareGeom and always resample
  raster <- resample(raster, template, method = ifelse(nome == "LULC", "near", "bilinear"))
  
  # Then mask and crop
  raster_recortado <- mask(raster, mascara)
  raster_recortado <- crop(raster_recortado, mascara, mask = TRUE)
  
  return(raster_recortado)
}

# Definir template base (usando o extent do AOI e resolução de 30m)
template <- rast(ext(AOI_diss_vect), resolution = 30, crs = "EPSG:31982")

# Lista de rasters originais (substitua pelos seus objetos)
rasters_orig <- list(
  LULC = LULC,
  ESTRADAS = ESTRADAS,
  CONC_ESTAB = CONC_ESTAB,
  TEXTURA = TEXTURA,
  TAM_IMOVEIS = TAM_IMOVEIS,
  DECLIVIDADE = DECLIVIDADE
)

# Processar cada raster
rasters_processados <- map2(
  rasters_orig, names(rasters_orig),
  ~ processar_raster(.x, .y, template, AOI_diss_vect)
)

# 5° CHECAGENS PÓS CORREÇÃO DE COMPATIBILIDADE --------------------------------------------------------------------------------------
# Checar extenções
lapply(rasters_processados, ext)

# Checar resoluções
lapply(rasters_processados, res)

# Extrair para variáveis individuais
list2env(lapply(rasters_processados, function(x) x), envir = .GlobalEnv)

# Verificar se todos têm mesma resolução, extensão e CRS
all(sapply(rasters_processados, function(x) all(compareGeom(x, template))))

# 6° CÁLCULO DE BANDAS ---------------------------------------------------------------------------------------------------
# Pesos fornecidos
pesos <- c(
            LULC        = 0.345,
            ESTRADAS    = 0.283,
            CONC_ESTAB  = 0.175,
            TEXTURA     = 0.112,
            TAM_IMOVEIS = 0.054,
            DECLIVIDADE = 0.032
            )

# Garantir que a ordem dos rasters corresponde aos pesos
rasters_ordenados <- rasters_processados[names(pesos)]

# Calcular a soma ponderada
AHP <- Reduce(`+`, Map(`*`, rasters_ordenados, pesos)) # Versão programática reduzida


# Plotar resultado
plot(AHP, main = "Resultado da Análise Ponderada")
minmax(AHP)

# 7° RECLASSIFICAÇÃO AHP ------------------------------------------------------------------------------------------------------------
matriz_reclass <- matrix(
  c(-1,    1.63, 1,
    1.63, 2.19, 2,
    2.19, 2.7,  3,
    2.7,  3.32, 4,
    3.32, 4,    5),
  ncol = 3, byrow = TRUE
)

AHP_reclass <- classify(AHP, 
                        rcl = matriz_reclass,
                        include.lowest = TRUE)

plot(AHP_reclass, 
     col = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
     levels = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta")
     )

# 8°PREENCHIMENTO DE NO DATAS DENTRO DA AOI --------------------------------------------------------
# Preencher NAs (áreas brancas) com interpolação focal
AHP_reclass_filled <- terra::focal(
  AHP_reclass,
  w = 5,                     # Tamanho da janela (3x3, 5x5, etc.)
  fun = modal,
  na.policy = "only",
  na.rm = TRUE
  )

plot(AHP_reclass_filled, 
     col = c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"),
     levels = c("Baixa", "Média-Baixa", "Média", "Média-Alta", "Alta")
)

# 9° EXPORTANDO RESULTADOS AHP ------------------------------------------------------------------------------------------------------
writeRaster(AHP, "Dados_R/AHP/AHP__Assurini_R_31982.tif", overwrite = TRUE, datatype = "INT1U")
writeRaster(AHP_reclass, "Dados_R/AHP/AHP__Assurini_31982_R_Reclass.tif", overwrite = TRUE, datatype = "INT1U")
writeRaster(AHP_reclass_filled, "Dados_R/AHP/AHP__Assurini_31982_R_Reclass_Filled.tif", overwrite = TRUE, datatype = "INT1U")
