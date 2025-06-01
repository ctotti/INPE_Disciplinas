# =====================================================================================================================
#                  SCRIPT 4.1 - GERAÇÃO DO MAPA RESULTANTE DA AHP, CALCULADA NO SCRIPT 'AHP.R'
#                                           COM USO DA BIBLIOTECA TMAP
# =====================================================================================================================

library(sf)
library(terra)
library(tmap)
library(showtext)
library(sysfonts)
library(systemfonts)
library(scales)  
library(MetBrewer)

# ====================================================================================================================

# UMA VEZ GERADA A AHP, PODE SER GERADO O PLOT OU A FIGURA QUE SERÁ UTILIZADA PARA REPRESENTAR 
# O RESULTADO FINAL NO DOCUMENTO DE TEXTO

# FLUXO DE TRABALHO:
# 1° IMPORTAÇÃO AOI e RASTER RECLASSIFICADO DA AHP
# 2° PREDEFINIÇÕES DE PLOTAGEM
#     2.1 DEFINIÇÃO DOS NÍVEIS DE FATOR E ROTULAGEM
#     2.2 PALETAS DE CORES
#     2.3 FONTES
# 3° PLOTANDO
# 4° EXPORTANDO

# 1° IMPORTAÇÃO AOI e RASTER RECLASSIFICADO DA AHP ------------------------------------------------------------------
AOI <- st_read('Dados/AOI/PAs_Assurini.gpkg') |> 
  st_set_crs(4674) |> 
  st_transform(31982)


AHP_reclass <- rast('Dados_R/AHP/AHP__Assurini_31982_R_Reclass_Filled.tif')

# 2° PREDEFINIÇÕES DE PLOTAGEM ---------------------------------------------------------------------------------------------
# 2.1 DEFINIÇÃO DOS NÍVEIS DE FATOR E ROTULAGEM
AHP_reclass[] <- factor(AHP_reclass[], levels = c(5, 4, 3, 2, 1))

rotulos <- c("Muito Alto", "Alto", "Moderado", "Baixo", "Muito Baixo")

categorias <- data.frame(
  value = 1:5,
  category = c("Muito Baixo", "Baixo", "Moderado", "Alto", "Muito Alto")
)

AHP_reclass <- as.factor(AHP_reclass)
levels(AHP_reclass) <- categorias

# 2.2 PALETAS DE CORES -----------------------------------------------------------------------------------------
BuRd <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
BuRd_rev <- rev(BuRd) 

Tiepolo    <- met.brewer("Tiepolo", n=5)
Peru2      <- met.brewer("Peru2", n = 5) 
Peru2[]
# "#65150b" "#961f1f" "#c0431f" "#f19425" "#c59349"

Peru3 <- c("#65150b", "#961f1f", "#e85125", "#f19425", "#8a6f0e")
Oranges1 <- c("#370617", "#9D0208", "#DC2F02", "#F48C06", "#f7b267")

# 2.3 FONTES -------------------------------------------------------------------------------------------------------------
windowsFonts(`Times New Roman` = windowsFont("Times New Roman"))
font_add("Gagugi", regular = "C:/WINDOWS/Fonts/gadugi.ttf")


# font_add("RobotoFlex", regular = "FontFamilies/Roboto_Flex/RobotoFlex-VariableFont_GRAD,XOPQ,XTRA,YOPQ,YTAS,YTDE,YTFI,YTLC,YTUC,opsz,slnt,wdth,wght.ttf")
# font_add("SourceSerif4", 
#          regular = "FontFamilies/Source_Serif_4/static/SourceSerif4_48pt-ExtraLight.ttf",
#          bold = "FontFamilies/Source_Serif_4/static/SourceSerif4-Bold.ttf",
#          bolditalic = "FontFamilies/Source_Serif_4/static/SourceSerif4-BoldItalic.ttf",
#          italic = "FontFamilies/Source_Serif_4/static/SourceSerif4-Italic.ttf")
showtext_auto()

# 3° PLOTANDO -----------------------------------------------------------------------------------------------------------
tm_shape(AHP_reclass) +
  tm_raster(
    title = " ",
    palette = Tiepolo,
    labels = rotulos,
    style = "cat",
    alpha = 1  # Transparência
  ) +
  tm_shape(AOI) +                                       # Área de interesse (vetor)
  tm_borders(col = "black", 
             lwd = 1.4) +
  # BARRA DE ESCALA
  tm_scale_bar(
    breaks = c(0, 5, 10, 15),
    # width = 2.5,
    color.dark = "gray20",
    text.size = 2.5,
    text.color = "gray20",
    position = c("right", "bottom")
    ) +
  # SETA NORTE
  tm_compass(
    type = "8star",
    size = 2,
    color.dark = 'gray20',
    text.size = 2.3,
    text.color = 'gray20',
    position = c("right", "top")
    ) +
  # LINHAS DE GRADE
  tm_grid(alpha = 0.1,
          ticks = TRUE,
          labels.size = 2.3,
          labels.format = list(digits = 0,
                               decimal.mark = ',',
                               big.mark = "."),  
          n.x = 4,
          n.y = 6,
          labels.rot = c(0, 90),
          ) +
  # CRÉDITOS
  tm_credits("DATUM SIRGAS 2000", 
             position = c("left", 0.02), size = 2.5, align = "left", 
             fontfamily = "Gagugi") +
  tm_credits("Sistema de Coordenadas Planas", 
             position = c("left", 0.035), size = 2.5, align = "left",
             fontfamily = "Gagugi") +
  tm_credits("Projeção UTM Fuso 22 Sul", 
             position = c("left", 0.05), size = 2.5, align = "left",
             fontfamily = "Gagugi") +
  # LAYOUT
  tm_layout(
    # main.title = " ",
    # main.title.size = 1.2,
    legend.position = c("left", "top"), 
    legend.title.size = 2.8,
    legend.text.size = 2.8,
    # legend.title.fontfamily = "Times New Roman",  # Específico para título da legenda
    legend.text.fontfamily = "Gagugi",   # Específico para texto da legenda
    fontfamily = "Gagugi",
    # fontface = "bold",
    bg.color = "white",
    inner.margins = c(0.05, 0.05, 0.05, 0.05)
  ) -> AHP_Map
AHP_Map


# 4° EXPORTANDO --------------------------------------------------------------------------------------------------------------------
tmap_save(
  tm = AHP_Map,  # ou o nome do seu mapa, se armazenou em uma variável
  filename = "Figuras_R/AHP_Mapa1.png",
  width = 25,        # largura em polegadas
  height = 30,        # altura em polegadas
  dpi = 300,         # qualidade (300 dpi é ideal para impressão)
  units = "cm"       # pode ser "in", "cm", "mm"
)

