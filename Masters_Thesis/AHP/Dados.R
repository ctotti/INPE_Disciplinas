# =====================================================================================================================
#                  SCRIPT 1 - IMPORTAÇÃO DOS DADOS E CONVERSÃO EM VARIÁVEIS INDICADORAS
#                               NORMALIZADAS EM UMA ESCALA DE PESO DE 1 A 4
# =====================================================================================================================

library(sf)
library(terra)
library(spatstat)
library(spatstat.geom)
library(exactextractr)
library(raster)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library('ggplot2')
library(RColorBrewer)
library(tmap)
library(osmdata)
library(beepr)

# ====================================================================================================================
# O FLUXO GERAL DE TRABALHO DOS DADOS ENVOLVE:
#   1° Importação e reprojeção dos dados
#   2° Conversão para raster no caso de dados vetoriais
#   3° Reclassificação em uma escala de peso de 1 a 4
#   4° Exportação da Variável Independente normalizada para Dados_R\VIs
# Podendo contar com processamentos intermediários caso necessário.

# Note que os dados de entrada já devem estar recortados para a área de estudo !

# O SCRIPT ESTÁ ORGANIZADO DA SEGUINTE MANEIRA:
# 1° - IMPORTAÇÃO DA AREA OF INTEREST (AOI) - Assurini
#   1.1 Importação do .gpkg do Assurini com os Projetos de Assentamento
#   1.2 Importação do .ggkg do Assurini apenas com limite da área total

# 2° - IMPORTAÇÃO DOS DADOS E PREPARAÇÃO DAS VARIÁVEIS INDICADORAS
#   2.1 TEXTURA DO SOLO - PEDOLOGIA IBGE 2023 - ASSURINI
#     2.1.1 Importação e reprojeção
#     2.1.2 Reclassificação (1 a 4)
#     2.1.3 Rasterizar
#     2.1.4 Exportar
#   2.2 USO E COBERTURA DO SOLO (LULC) - TERRACLASS (TC) 2022 - ASSURINI
#     2.2.1 Importação e reprojeção
#     2.2.2 Definição de estilo e atribuição da paleta de cores às classes de LULC
#     2.2.3 Reclassificação (1 a 4)
#     2.2.4 Rasterizar
#     2.2.5 Exportar
#   2.3 COPDEM - 30m - ASSURINI
#     2.3.1 Importação e reprojeção
#     2.3.2 Extração da declividade
#     2.3.3 Reclassificação (1 a 4)
#     2.3.4 Exportar
#   2.4 OPENSTREETMAPS (OSM) - ASSURINI
#     2.4.1 Importação e reprojeção
#     2.4.2 Rasterizar
#     2.4.3 Interpolar
#     2.4.4 Reclassificação (1 a 4)
#     2.4.5 Exportar
#   2.5 CADASTRO AMBIENTAL RURAL (CAR) - ASSURINI
#     2.5.1 Importação e reprojeção
#     2.5.2 Reclassificação (1 a 4)
#     2.5.3 Rasterizar
#     2.5.4 Exportar
#   2.6 CNEFE - ASSURINI
#     2.6.1 Importação e reprojeção
#     2.6.2 Mapa de Calor (Estimativa de Densidade Kernel)
#     2.6.3 Reclassificação (1 a 4)
#     2.6.4 Exportar


# ----------------------------------- IMPORTANDO E PREPARANDO AOI ----------------------------------------------
AOI <- st_read('Dados/AOI/PAs_Assurini.gpkg')

st_crs(AOI)
AOI <- st_set_crs(AOI, 4674)
AOI <- st_transform(AOI, 31982)
st_crs(AOI)

plot(st_geometry(AOI))  # Visualizar

# AOI dissolvido ----------------------------------------------------------
AOI_diss <- st_read('Dados/AOI/PAs_Assurini_dissolved.gpkg')

st_crs(AOI_diss)
AOI_diss <- st_set_crs(AOI_diss, 4674)
AOI_diss <- st_transform(AOI_diss, 31982)
st_crs(AOI_diss)

plot(st_geometry(AOI_diss))

# ----------------------------------- IMPORTANDO DADOS E PREPARANDO VARIÁVEIS INDICADORAS -----------------------------------
# ============================================================================================================================
# 2.1 TEXTURA DO SOLO - PEDOLOGIA IBGE 2023 - ASSURINI ===========================================================================
# 2.1.1 Importação e reprojeção
TEXTURA <- st_read('Dados/Pedologia_IBGE_2023/pedo_area_Assurini.gpkg')

crs(TEXTURA)
TEXTURA <- st_set_crs(TEXTURA, 4674)
TEXTURA <- st_transform(TEXTURA, 31982)
st_crs(TEXTURA)

ggplot(TEXTURA) +
  geom_sf(aes(fill = textura)) +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Textura") +
  theme_minimal()

# 2.1.2 TEXTURA DO SOLO - RECLASSIFICANDO --------------------------------------------------------------------------------------------------
# Criando a nova coluna 'textura_reclass' baseada na coluna 'textura'
TEXTURA <-
  TEXTURA %>%
  mutate(textura_reclass = case_when(
    # Ordem de importância: média > média/argilosa > argilosa > muito argilosa
    textura == "média/argilosa e argilosa" ~ 3,           # Caso exato primeiro
    str_detect(textura, "média/argilosa") ~ 4,           # Demais casos com "média/argilosa"
    str_detect(textura, "argilosa") & !str_detect(textura, "muito") ~ 2,
    str_detect(textura, "muito argilosa") ~ 1,                                    # Mínima importância
    textura == "indiscriminada" ~ 1,
    is.na(textura) ~ 1,
    TRUE ~ NA_real_  # Fallback (não deve ocorrer)
  ))

TEXTURA %>%
  dplyr::select(textura, textura_reclass) %>%
  arrange(textura_reclass) # Ordenar para visualizar

ggplot(TEXTURA) +
  geom_sf(aes(fill = textura_reclass)) +  # Mapeia valores numéricos
  scale_fill_gradientn(
    colors = rev(brewer.pal(4, "RdYlGn")),  # Escala de cores invertida (verde=importante)
    breaks = c(1, 2, 3, 4),                 # Pontos de quebra
    labels = c("Muito Argilosa (1)", "Argilosa (2)", "Média/Argilosa (3)", "Média (4)"),  # Rótulos
    name = "Importância da Textura"         # Título da legenda
  ) +
  theme_minimal()

# 2.1.3 RASTERIZANDO ---------------------------------------------------------------------------------------------------------------
# Crie um raster de referencia com a resolução e extensão desejadas
raster_ref <- rast(ext(TEXTURA), resolution = 30, crs = "EPSG:31982")

# Converta para vetor do pacote terra (necessário para rasterizar)
TEXTURA_vect <- vect(TEXTURA)

# Rasterize usando o campo 'textura_reclass'
TEXTURA_r <- rasterize(TEXTURA_vect, 
                       raster_ref, 
                       field = "textura_reclass")

# Visualização rápida
plot(TEXTURA_r, col = rev(terrain.colors(4)))

# 2.1.4 EXPORTANDO -----------------------------------------------------------------------------------------------------------------
writeRaster(TEXTURA_r, "Dados_R/VIs/TEXTURA__Textura_Reclass_Assurini_R_31982.tif", overwrite = TRUE)

# ============================================================================================================================
# 2.2 USO E COBERTURA DO SOLO (LULC) - TERRACLASS (TC) 2022 - ASSURINI ===========================================================
# 2.2.1 Importação e reprojeção
LULC <- st_read('Dados/TerraClass2022/Dado/TC_Assurini_2022_Reclass.gpkg')

crs(LULC)
LULC <- st_set_crs(LULC, 4674)
LULC <- st_transform(LULC, 31982)
st_crs(LULC)
  
LULC <- LULC %>%
    rename(id = Classes) 

# 2.2.2 ESTILO E PALETA DE CORES  ------------------------------
LULC_ESTILO <- read.table('Dados/TerraClass2022/Dado/TC_Assurini_2022_Reclass_Estilo.clr', 
                          header = FALSE, sep = "", fill = TRUE)

LULC_ESTILO <-
  LULC_ESTILO %>%
  unite("Classe", V6, V7, V8, V9, sep = " ", remove = TRUE) %>%
  # slice(-c(9, 11)) %>%
  # mutate(Classe = replace(Classe, 8, "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO"),
  #        Classe = replace(Classe, 9, "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO")) %>%
  rename(id = V1, R = V2, G = V3, B = V4, alpha = V5, Classe = Classe) %>%
  mutate(id = as.numeric(id)) %>% 
  mutate(hex = rgb(R, G, B, maxColorValue = 255))  %>%
  relocate(hex, .after = alpha) %>%
  mutate(Classe = str_to_title(str_to_lower(Classe))) %>%
  mutate(Classe = str_trim(Classe)
  )

LULC_ESTILO

# ACRESCENTANDO INFORMAÇÕES DE ESTILO AO DADO DE LULC -------------------------------------
LULC <- LULC %>%
  left_join(LULC_ESTILO %>% 
      dplyr::select(id, hex, Classe), by = "id")

# PLOTANDO MAPA DE LULC COM ESTILO 
ggplot(LULC) +
  geom_sf(aes(fill = Classe), color = NA) +
  scale_fill_manual(
    values = setNames(LULC_ESTILO$hex, LULC_ESTILO$Classe),
    na.value = "gray"  # Cor para IDs sem correspondência
  ) +
  labs(title = "Uso e Cobertura da Terra", fill = "Classe") +
  theme_minimal() +
  theme(legend.position = "right")


tm_shape(LULC) +
  tm_polygons(
    "Classe",
    palette = setNames(LULC_ESTILO$hex, LULC_ESTILO$Classe),
    title = "Uso e Cobertura da Terra",
    border.alpha = 0,
    lwd = 0
  ) +
  tm_graticules() +
  # labs(x = "Longitude", y = "Latitude") +
  # tm_scale_bar(position = c("left", "bottom")) +  # Barra de escala
  # tm_compass(type = "arrow", position = c("right", "top")) +  # Rosa dos ventos
  tm_layout(main.title = "Mapa LULC - 2022",
            legend.outside = TRUE)


LULC %>%
  distinct(Classe)

# 2.2.3 LULC - RECLASSIFICANDO --------------------------------------------------------------------------------------------------
LULC <- LULC %>%
  mutate(Classe_cod = case_when(
    Classe == "Cultura Agricola Perene" ~ 4,
    Classe %in% c("Pastagem Herbacea", "Pastagem Arbustiva/Arborea") ~ 3,
    Classe %in% c("Vegetacao Natural Florestal Secundaria", "Desflorestamento No Ano") ~ 2,
    Classe %in% c("Corpo Dagua", "Vegetacao Natural Florestal Primaria", 
                  "Mineracao", "Urbanizada", "Natural Nao Florestal") ~ 1,
    TRUE ~ NA_integer_  # para quaisquer outros casos não especificados
  ))

LULC %>%
  distinct(hex, Classe, Classe_cod) %>%
  arrange(Classe_cod)

# 2.2.4 RASTERIZANDO -----------------------------------------------------------------------------------------------------
cores_cod <- c(
  "4" = "#67000d", "3" = "#ea372a", "2" = "#fca486", "1" = "#fff5f0"
  )

# Rasterizar o vetor (usando Classe_cod como valor)
LULC_r <- rasterize(
              vect(LULC),       # Converter sf para SpatVector
              raster_ref,
              field = "Classe_cod"  # Campo numérico para rasterizar
)

plot(
  LULC_r,
  col = rev(cores_cod),
  main = "LULC Reclassificado (Cores Personalizadas)"
)

# 2.2.5 EXPORTAR --------------------------------------------------------------------------------------------------------
# Salvar o raster em GeoTIFF
writeRaster(LULC_r, "Dados_R/VIs/LULC__TC_Assurini_2022_Reclass_1a4_R_31982.tif", overwrite = TRUE, datatype = "INT1U")

# Criar um arquivo de atributos (.vat.csv) com as cores e exportar
vat_data <- data.frame(
  ID = c(1, 2, 3, 4),
  Classe = c("Outros", "Vegetação Secundária", "Pastagem", "Cultura Agrícola Perene"),
  Cor = c("#fff5f0", "#fca486", "#ea372a", "#67000d")
)

write.csv(vat_data, "Dados_R/VIs/LULC__TC_Assurini_2022_Reclass_1a4_R_31982.vat.csv", row.names = FALSE)

# Exportar também em .grd (formato nativo do R que suporta tabela de cores) ------------------------
LULC_r_r <- raster(LULC_r)
colortable(LULC_r_r) <- cores_cod  # Definir a paleta de cores

# Salvar em .grd (formato nativo do R que preserva cores)
writeRaster(LULC_r_r, "Dados_R/VIs/LULC__TC_Assurini_2022_Reclass_1a4_R_31982.grd",
  format = "raster",  # Formato .grd
  overwrite = TRUE
)

# ============================================================================================================================
# 2.3 COPDEM - 30m - ASSURINI ==========================================================================================
# 2.3.1 Importação e reprojeção
RELEVO <- rast('Dados/COPDEM/COP30_Topography_Assurini.tif')
plot(RELEVO)

if (is.na(crs(RELEVO))) {     # Verificar CRS atual do raster (se não estiver definido, defina como 4674 - SIRGAS 2000)
  crs(RELEVO) <- "EPSG:4674"  # SIRGAS 2000 (mesmo que LULC original)
}

RELEVO <- project(RELEVO, "EPSG:31982") # Reprojetar para UTM 22S (EPSG:31982)

# 2.3.2 COPDEM - EXTRAINDO DECLIVIDADE -------------------------------------------------------------------------
# Calcular declividade em graus
declividade_graus <- terrain(RELEVO, v = "slope", unit = "degrees")

# Converter graus para porcentagem (%)
declividade_percent <- tan(declividade_graus * pi / 180) * 100

# Visualizar
plot(declividade_percent, main = "Declividade (%)")                                 # Converte para porcentagem

# 2.3.3 COPDEM - DECLIVIDADE - RECLASSIFICANDO --------------------------------------------------------------------------------------------------
COPDEM_Table_reclass <- 
  matrix(
  c(
    0,  5, 4,   # 0-5% → valor 4
    5,  9, 3,   # 5-9% → valor 3
    9, 13, 2,   # 9-13% → valor 2
    13, Inf, 1  # >13% → valor 1
  ),
  ncol = 3,
  byrow = TRUE
)

# Reclassificar o raster
DECLIVIDADE_Reclass <- classify(declividade_percent, COPDEM_Table_reclass, include.lowest = TRUE)

# Visualizar o resultado
cores <- colorRampPalette(c("#FFFFFC", "#FFCCCC", "#FF6666", "#990000"))(4)
plot(
  DECLIVIDADE_Reclass,
  col = cores,
  main = "Declividade Reclassificada",
  plg = list(  # Personalizar a legenda
    title = "Classes",
    legend = c("0-5% (4)", "5-9% (3)", "9-13% (2)", ">13% (1)"),
    title.cex = 0.8,
    cex = 0.8
  )
  )

# 2.3.4 EXPORTAR --------------------------------------------------------------------------------------------------------
# Salvar o raster em GeoTIFF
writeRaster(DECLIVIDADE_Reclass, "Dados_R/VIs/DECLIVIDADE__COP30_Declividade_Reclass_Assurini_R_31982.tif", overwrite = TRUE, datatype = "INT1U")

# ============================================================================================================================
# 2.4 OSM - ASSURINI =========================================================================================================
# 2.4.1 Importação e reprojeção
ESTRADAS <- st_read('Dados/OSM/OSM_HighWay_Assurini.gpkg') %>%
  dplyr::select(where(~!all(is.na(.))))

crs(ESTRADAS)
ESTRADAS <- st_set_crs(ESTRADAS, 4674)
ESTRADAS <- st_transform(ESTRADAS, 31982)
st_crs(ESTRADAS)

plot(ESTRADAS)

ESTRADAS

# 2.4.2 ESTRADAS - RASTERIZAR -----------------------------------------------------------------------------------
# Adicionar coluna com valor 1
ESTRADAS$valor <- 1

# Criando raster vazio
raster_ref <- rast(ext(DECLIVIDADE_Reclass), resolution = 30, crs = st_crs(LULC)$wkt)

# Converter ESTRADAS para formato SpatVector (exigido pelo terra)
estradas_vect <- vect(ESTRADAS)

# Rasterizar (usando a coluna 'valor' como input)
ESTRADAS_raster <- rasterize(estradas_vect, raster_ref, field = "valor")

# Preencher áreas sem estradas com 0 (opcional)
ESTRADAS_raster[is.na(ESTRADAS_raster)] <- 0  # Se quiser 0 em vez de NA

plot(ESTRADAS_raster, col = c("white", "red"), main = "Estradas (1 = presente, 0 = ausente)")

# 2.4.3 ESTRADAS RASTERIZADAS - INTERPOLAR ----------------------------------------------------------------------
# Calcular distância euclidiana (em metros)
ESTRADAS_dist <- gridDist(ESTRADAS_raster, target = 1, scale = 30)
plot(ESTRADAS_dist)

# Aplicar máscara para restringir à AOI
AOI_diss_vect <- vect(AOI_diss)                           # Converter sf para SpatVector
ESTRADAS_dist <- mask(ESTRADAS_dist, AOI_diss_vect)
minmax(ESTRADAS_dist)

cores <- colorRampPalette(c("red2", "#F7F7F7", "lightblue"))(100)  # 100 níveis de gradiente
plot(ESTRADAS_dist, col = cores, main = "Distância Euclidiana das Estradas (metros)")

# 2.4.4 DISTANCIA A ESTRADAS - RECLASSIFICANDO ------------------------------------------------------------------
max_dist <- 207.5341
limites <- c(
  0,             # Mínimo
  0.05 * max_dist,  # ~45.6575 (22%)
  0.12 * max_dist,  # ~105.8424 (51%)
  0.23 * max_dist,  # ~176.4039 (85%)
  Inf            # Máximo
)
limites

estradas_matriz_reclass <- 
  matrix(
  c(
    0, limites[2], 4,
    limites[2], limites[3], 3,
    limites[3], limites[4], 2,
    limites[4], Inf, 1
  ),
  ncol = 3,
  byrow = TRUE
)
estradas_matriz_reclass

ESTRADAS_dist_reclass <- classify(ESTRADAS_dist, estradas_matriz_reclass, include.lowest = TRUE)
# Plotar com cores intuitivas (vermelho = perto, azul = longe)
cores <- colorRampPalette(c("#67000d", "#ea372a","#fca486", "#fff5f0"))(4)  # 100 níveis de cor
plot(
  ESTRADAS_dist_reclass,
  col = rev(cores))  # Inverte a paleta para vermelho = perto

# 2.4.5 EXPORTAR --------------------------------------------------------------------------------------------------------
# Salvar o raster em GeoTIFF
writeRaster(ESTRADAS_dist_reclass, "Dados_R/VIs/ESTRADAS__OSM_HighWay_Distancia_Reclass_Assurini_R_31982.tif", overwrite = TRUE, datatype = "INT1U")

# ============================================================================================================================
# 2.5 CAR - ASSURINI ==========================================================================================
# 2.5.1 Importação e reprojeção
TAM_IMOVEIS <- st_read('Dados/CAR_SICAR/CAR_SICAR_Assurini_Corrigido.shp')

crs(TAM_IMOVEIS)
TAM_IMOVEIS <- st_set_crs(TAM_IMOVEIS, 4674)
TAM_IMOVEIS <- st_transform(TAM_IMOVEIS, 31982)
st_crs(TAM_IMOVEIS)

TAM_IMOVEIS <-
  TAM_IMOVEIS %>%
  dplyr::select(
    fid, cod_imovel, mod_fiscal, municipio, SITUACAO, TIPO_IMOVE, AREA_HA, CLASS, geometry
  )

TAM_IMOVEIS
# Criar o plot
ggplot(data = TAM_IMOVEIS) +
  geom_sf(aes(fill = CLASS), color = NA) +  
  scale_fill_discrete(name = "Classe") +
  # labs(title = "Imóveis por Classe de Tamanho") +
  theme_minimal()

# 2.5.2 TAMANHO IMÓVEIS - RECLASSIFICAR -----------------------------------------------------------------------------
TAM_IMOVEIS <-
  TAM_IMOVEIS %>%
  mutate(CLASS_COD = case_when(
    CLASS == "MINIFUNDIO" ~ 4,
    CLASS == "PEQUENO" ~ 4,
    CLASS == "MEDIO" ~ 3,
    CLASS == "GRANDE" ~ 2,
    is.na(CLASS) ~ 1,              # Caso haja valores NA
    TRUE ~ 1                       # Caso haja outros valores não previstos
  ))

ggplot(data = TAM_IMOVEIS) +
  geom_sf(aes(fill = CLASS_COD), color = NA) +  
  scale_fill_continuous(name = "Classe") +
  # labs(title = "Imóveis por Classe de Tamanho") +
  theme_minimal()

# 2.5.3 TAMANHO IMÓVEIS - RASTERIZAR --------------------------------------------------------------------------------
# Converter  para formato SpatVector (exigido pelo terra)
TAM_IMOVEIS_vect <- vect(TAM_IMOVEIS)

# Rasterizar (usando a coluna 'valor' como input)
TAM_IMOVEIS_raster <- rasterize(TAM_IMOVEIS_vect, raster_ref, field = "CLASS_COD")

# Preencher áreas vazias com 1
TAM_IMOVEIS_raster[is.na(TAM_IMOVEIS_raster)] <- 1  # Se quiser 1 em vez de NA

cores <- colorRampPalette(c("#67000d", "#ea372a","#fca486", "#fff5f0"))(4)
plot(TAM_IMOVEIS_raster, col = rev(cores))

# 2.5.4 EXPORTAR --------------------------------------------------------------------------------------------------------
# Salvar o raster em GeoTIFF
writeRaster(TAM_IMOVEIS_raster, "Dados_R/VIs/TAM_IMOVEIS__CAR_SICAR_Assurini_Reclass_R_31982.tif", overwrite = TRUE, datatype = "INT1U")

# ============================================================================================================================
# 2.6 CNEFE - ASSURINI =========================================================================================
# 2.6.1 Importação e reprojeção
CONC_ESTAB <- st_read('Dados/CNEFE_2022/CNEFE_2022_Assurini_BoundingBox_31982.gpkg')
crs(CONC_ESTAB)
plot(CONC_ESTAB)

# 2.6.2 CONC_ESTAB - MAPA DE CALOR (ESTIMATIVA DE DENSIDADE KERNEL) ----------------------------------------------
# Criar objeto spatstat para KDE
coords <- st_coordinates(CONC_ESTAB)
janela <- as.owin(st_bbox(CONC_ESTAB))
CONC_ESTAB_ppp <- ppp(x = coords[,1], y = coords[,2], window = janela)

# Estimar densidade KDE
dens <- density.ppp(CONC_ESTAB_ppp, sigma = 800, eps = 30, kernel = "quartic", edge = FALSE)
beep(sound = 2)

# Converter para raster (terra) E reprojetar
CONC_ESTAB_kde <- rast(dens)

crs(CONC_ESTAB_kde) <- "EPSG:31982"

# Recortar raster pela AOI
CONC_ESTAB_kde_crop <- crop(CONC_ESTAB_kde, vect(AOI_diss))
CONC_ESTAB_kde_mask <- mask(CONC_ESTAB_kde_crop, vect(AOI_diss))

plot(CONC_ESTAB_kde_mask, main = "Mapa de Calor KDE (recortado pela AOI)", col = hcl.colors(20, "YlOrRd", rev = TRUE))
plot(st_geometry(AOI), add = TRUE, border = "blue", lwd = 2)
beep(sound = 2)

# 2.6.3 CONC_ESTAB - RECLASSIFICAR -------------------------------------------------------------------------------
minmax(CONC_ESTAB_kde_mask)
CONC_ESTAB_kde_mask_norm <- round(CONC_ESTAB_kde_mask * 10000000, 14)
minmax(CONC_ESTAB_kde_mask_norm)

# Extrair o valor máximo do raster (ignorando NAs)
max_dens <- max(values(CONC_ESTAB_kde_mask_norm), na.rm = TRUE)

# Definir os limites das classes (como no seu exemplo)
limites <- c(
  -1,                      # Mínimo (0%)
  max_dens * 0.015,        # Classe 1 (10% do máximo)
  max_dens * 0.03,        # Classe 2 (25% do máximo)
  max_dens * 0.07,        # Classe 3 (50% do máximo)
  max_dens * 0.11,        # Classe 4 (75% do máximo)
  max_dens * 0.37,        # Classe 5 (90% do máximo)
  max_dens * 1.001        # Máximo (100%)
)

# Arredondar para melhor visualização (opcional)
limites <- round(limites, 2)
limites  # Verificar os valores

paleta_cores <- colorRampPalette(c(
  "#67000d",  # Vermelho escuro (mais concentrado)
  "#c5161c",  # Vermelho
  "#f44d38",  # Vermelho claro
  "#fc8f6f",  # Rosa
  "#fdccb8",  # Rosa claro
  "#fff5f0"# Branco (menos concentrado)
))(6)

# Criar matriz de reclassificação (limites inferiores e superiores)
matriz_reclass <- matrix(
  c(
    limites[1], limites[2], 1,  # Classe 1: 0-10%
    limites[2], limites[3], 1.5,  # Classe 2: 10-25%
    limites[3], limites[4], 2,  # Classe 3: 25-50%
    limites[4], limites[5], 3,  # Classe 4: 50-75%
    limites[5], limites[6], 3.5,  # Classe 5: 75-90%
    limites[6], limites[7], 4   # Classe 6: 90-100%
  ),
  ncol = 3,
  byrow = TRUE
)

# Aplicar a reclassificação
CONC_ESTAB_classificado <- classify(CONC_ESTAB_kde_mask_norm, matriz_reclass)

# Plotar o mapa
plot(
  CONC_ESTAB_classificado,
  col = rev(paleta_cores),
  main = "Densidade Kernel (6 Classes: Vermelho → Branco)"
)

plot(st_geometry(AOI_diss), 
     add = TRUE,          # Adiciona ao plot existente
     col = NA,            # Sem preenchimento
     border = "orange",    # Cor da borda
     lwd = 1)             # Espessura da linha

# 2.6.4 EXPORTAR --------------------------------------------------------------------------------------------------------
# Salvar o raster em GeoTIFF
writeRaster(CONC_ESTAB_classificado, "Dados_R/VIs/CONC_ESTAB__CNEFE_2022_Assurini_31982_MapaCalor_sigma800_R_Reclass.tif", overwrite = TRUE, datatype = "INT1U")
