# =====================================================================================================================
#                  SCRIPT 3 - CÁLCULO DE ÁREAS DO DADO RECLASSIFICADO RESULTANTE 
#                   DO PROCESSO DE ANÁLISE HIERÁRQUICA (AHP) NO SCRIPT 'AHP.R'
# =====================================================================================================================

library(sf)
library(dplyr)
library('ggplot2')
library(terra)


# IMPORTANDO DADOS
AOI <- st_read('Dados/AOI/PAs_Assurini.gpkg') |> 
  st_set_crs(4674) |> 
  st_transform(31982)

AHP_reclass <- rast('Dados_R/AHP/AHP__Assurini_31982_R_Reclass_Filled.tif')

# CÁLCULO DA ÁREA TOTAL
area_total <- expanse(AHP_reclass, unit="ha")  # pode usar "m" para metros ou "ha" para hectares
area_total   # 207,382 ha

 # CÁLCULO ÁREAS POR CLASSE
areas_classes <- expanse(AHP_reclass, unit = "ha", byValue = TRUE)
areas_classes

# GERANDO TABELA COM ÁREA EM HECTARES (HA) E PORCENTAGEM (%)
AHP_area_df <- 
  data.frame(
  Classe = c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto", "TOTAL"),
  Área_ha = round(c(areas_classes$area, sum(areas_classes$area)), 2),
  Porcentagem = round(c(areas_classes$area/sum(areas_classes$area)*100, 100), 2)
)
AHP_area_df

write.table(AHP_area_df, file = 'Figuras_R/AHP_area_df.txt')
