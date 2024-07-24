rm(list = ls()) #limpar o o ambiente

memory.size() #checar mem?ria dispon?vel
memory.limit() #checar o limite de mem?ria dispon?vel 
memory.limit(size=56000) #ampliar mem?ria para 64gb


#Carregando um conjunto de pacotes de uma vez 
neededPackages = c("raster", "stats", "rgdal", "ggplot2", "sp", "dplyr", "tidyr", "sp", "sf","ROCR",
                   "reshape2", "randomForest", "caret", "caTools", "geobr", "prettymapr", "tidyselect" )
pkgTest = function(x) { if (x %in% rownames(installed.packages()) == FALSE) { install.packages(x, dependencies= TRUE) }; library(x, character.only = TRUE) }
for (package in neededPackages) { pkgTest(package) }

# ==== Functions ====

#Gera um raster da entropia, plota o histograma e salva um .shp com os valores da entropia para as amostras da ?rea

shannon.entropy = function (prob_RF, raster.ref, path_entropy, name_area, samples_areaf, path_samples) {
  
  prob_RF.df = as.data.frame(prob_RF)
  entropy = - ((prob_RF.df$`0` * log2(prob_RF.df$`0`)) + (prob_RF.df$`1` * log2(prob_RF.df$`1`)))
  entropy.raster = raster(raster.ref) #Raster reference 
  entropy.raster = setValues(entropy.raster, entropy) #Set entropy values
  entropy.raster[is.na(entropy.raster)] = 0.000
  
  writeRaster(entropy.raster, filename = path_entropy, format="GTiff")
  plot (hist(entropy.raster), main = paste("Histograma entropia", name_area, sep = ' '),xlab = "Values")
  
  amostras.entropy = raster::extract(entropy.raster, samples_area)
  amostras.entropy.df = as.data.frame(amostras.entropy)
  amostras.entropy.sp = SpatialPointsDataFrame(samples_area@coords, amostras.entropy.df)
  crs.ref = crs(samples_area) #set reference projection 
  crs(amostras.entropy.sp) = crs.ref #set projection
  names(amostras.entropy.sp) = c("entropia")
  writeOGR(amostras.entropy.sp, dsn = path_samples, driver = "ESRI Shapefile", layer = path_samples, morphToESRI = T)

}

#Gera um raster com a propor??o de vezes que um pixel foi classificado como "?rea alag?vel" considerando todas as ?rvores do RF.
#raster.ref = um raster a ser dado como referencia para criar o arquivo; prob_RF = probabilidade resultante do predict do pacote stats;
#path= caminho onde salvar o arquivo

proportion.wetland = function (raster.ref, prob_RF, class.raster, path) {
  prob.wetland = raster(raster.ref) #Empty raster created from another raster reference 
  prob.wetland = setValues(prob.wetland, prob_RF[,2] ) #the second column is relative to wetland class 
  prob.wetland [class.raster == 0] = NA #mask the values only to wetland class 
  writeRaster(prob.wetland, filename = path, format="GTiff")
} 

# ==== Attributes ====
#Para rodas esse script, todos os atributos devem estar em pastas separadas por ?reas.
#Os nomes dos arquivos devem ser os mesmos, independente da ?rea

# Reading all attributes (files names ended with .tif) for the model 
attributes_A1 = list.files(path = "C:/Users/debor/OneDrive - inpe.br/INPE/mestrado/Dissertacao/Processamento/Atributos/A1/nasadem/full_area/clip/", pattern = ".tif", full.names = T) #AREA 1
attributes_A2 = list.files(path = "C:/Users/debor/OneDrive - inpe.br/INPE/mestrado/Dissertacao/Processamento/Atributos/A2/nasadem/full_area/clip/", pattern = ".tif", full.names = T) #AREA 2
attributes_A3 = list.files(path = "C:/Users/debor/OneDrive - inpe.br/INPE/mestrado/Dissertacao/Processamento/Atributos/A3/nasadem/full_area/clip/", pattern = ".tif", full.names = T) #AREA 3
attributes_A4 = list.files(path = "C:/Users/debor/OneDrive - inpe.br/INPE/mestrado/Dissertacao/Processamento/Atributos/A4/nasadem/full_area/clip/", pattern = ".tif", full.names = T) #AREA 4

# Stack with all attributes
A1attributes = stack(attributes_A1)
A2attributes = stack(attributes_A2)
A3attributes = stack(attributes_A3)
A4attributes = stack(attributes_A4)

#Reading as data frame
A1attributes.df = data.frame(values(A1attributes))
A2attributes.df = data.frame(values(A2attributes))
A3attributes.df = data.frame(values(A3attributes))
A4attributes.df = data.frame(values(A4attributes))


# ===== Samples ====
#As amostras de uma mesma ?rea est?o separadas em dois arquivos, um shp para ?reas alagaveis e outro .shp para n?o alag?vel

#area1
samples.files_A1 = list.files("G:/Meu Drive/INPE/mestrado/Dissertacao/Artigo/Amostras/A1", pattern = ".shp", full.names = T)
samples.list_A1 = lapply(samples.files_A1, readOGR) #junta os .shp em uma lista 
A1.samples = rbind(samples.list_A1[[1]], samples.list_A1[[2]]) #combina em um ?nico vetor

#area2
samples.files_A2 = list.files("G:/Meu Drive/INPE/mestrado/Dissertacao/Artigo/Amostras/A2", pattern = ".shp", full.names = T)
samples.list_A2 = lapply(samples.files_A2, readOGR) #junta os .shp em uma lista 
A2.samples = rbind(samples.list_A2[[1]], samples.list_A2[[2]]) #combina em um ?nico vetor

#area3
samples.files_A3 = list.files("G:/Meu Drive/INPE/mestrado/Dissertacao/Artigo/Amostras/A3", pattern = ".shp", full.names = T)  
samples.list_A3 = lapply(samples.files_A3, readOGR) #junta os .shp em uma lista 
A3.samples = rbind(samples.list_A3[[1]], samples.list_A3[[2]]) #combina em um ?nico vetor

#area4
A4.samples =  readOGR("./Classification/pre_classification/amostragem/area4/area4_amostras_upland.shp")

#Extraindo valores dos raster para as amostras
atributos.df_A1 = raster::extract(A1attributes, A1.samples)
atributos.df_A2 = raster::extract(A2attributes, A2.samples)
atributos.df_A3 = raster::extract(A3attributes, A3.samples)
atributos.df_A4 = raster::extract(A4attributes, A4.samples)

# #Unindo a informacao da classe da amostra com os valores dos atributos e as coordenadas
# amostras.atrib_A1 = data.frame(Lat = A1.samples@coords[,2], Long = A1.samples@coords[,1],Classe = A1.samples@data$B1,atributos.df_A1)
# amostras.atrib_A2 = data.frame(Lat = A2.samples@coords[,2], Long = A2.samples@coords[,1],Classe = A2.samples@data$B1,atributos.df_A2)
# amostras.atrib_A3 = data.frame(Lat = A3.samples@coords[,2], Long = A3.samples@coords[,1],Classe = A3.samples@data$B1,atributos.df_A3)
# amostras.atrib_A4 = data.frame(Lat = A4.samples@coords[,2], Long = A4.samples@coords[,1],Classe = A4.samples@data$B1,atributos.df_A4)

#Unindo a informacao da classe da amostra com os valores dos atributos (sem coordenadas)
amostras.atrib_A1 = data.frame(Classe = A1.samples@data$B1,atributos.df_A1)
amostras.atrib_A2 = data.frame(Classe = A2.samples@data$B1,atributos.df_A2)
amostras.atrib_A3 = data.frame(Classe = A3.samples@data$B1,atributos.df_A3)
amostras.atrib_A4 = data.frame(Classe = A4.samples@data$B1,atributos.df_A4)

#Uni em um unico df, todas as amostras
#amostras.join = rbind(amostras.atrib_A1, amostras.atrib_A2, amostras.atrib_A3, amostras.atrib_A4)
amostras.join = rbind(amostras.atrib_A1, amostras.atrib_A2, amostras.atrib_A3)

#Determina conjunto de atributos que ser?o mantidos. Est? escolha foi feita com base na an?lise de qual atributo de um mesmo 'grupo'
#e.g. todas as curvaturas horizontais, ocupou maior posi??o em rela?a? aos outros do grupo.

#Lista contendo quais atributos morfom?tricos foram pr?-selecionados
keeps = c("nasadem_filter11x11_HorCurv", "nasadem_filter9x9_LongCurv",
          "nasadem_filter13x13_MaxCurv", "nasadem_filter7x7_MinCurv", "nasadem_filter9x9_MeanCurv", 
          "nasadem_filter9x9_VertCurv","slope_envi_kernel7x7", "TPI_r13", "TRI_r9" )

#organiza o df com os atributos morfom?tricos pr?-selecionados e os atributos hidrol?gicos
amostras.join_var.sel = amostras.join %>% select(starts_with("dist"), "expande_log_area_acum_burned",
                                                 starts_with("hand"), keeps, contains("slope_rampa"), starts_with("TWI"))
#adiciona a informa??o da classe
amostras.join_var.sel = amostras.join_var.sel %>% mutate(Classe = amostras.join$Classe)

#Salvando amostras com info dos atributos
write.csv(amostras.join_var.sel, "G:/Meu Drive/INPE/mestrado/Dissertacao/Artigo/Resultados")

# ==== Boxplot - valor das amostras para os atributos por classe ==== 

dados.melt1 = melt(amostras.join_var.sel)

boxplot_33var = ggplot(dados.melt1, aes(Classe, value, fill=Classe)) + 
                geom_boxplot(outlier.size = 0.9) + 
                scale_fill_manual(values = c("snow4", "steelblue4"))+
                theme( panel.grid.major = element_blank())+
                facet_wrap(~variable, scale="free") + 
                theme(panel.grid.major = element_blank(), #element_line(colour = "#d3d3d3"),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      #panel.border = element_rect(colour = "black", fill=NA),
                      panel.background = element_blank(),
                      text=element_text(family = "Arial"),
                      axis.title = element_text(face="bold", size = 10),
                      axis.text.x = element_text(colour="white", size = 0),
                      axis.text.y = element_text(colour="black", size = 10),
                      axis.line = element_line(size=1, colour = "black"))+
                theme(plot.margin = unit(c(1,1,1,1), "lines"))

ggsave(plot = boxplot_33var, filename = "./resultados_dissertacao/amostras_modificadas/RF33/boxplot_33var.png", device = 'png', 
       width = 11.7, height = 8.3, units = 'in', dpi = 600)

# =================================== Random Forest FULL 124 =================================================
#Roda o RF considerando todos os 124 atributos

## a coluna "Classe" tem que ser colocada como FACTOR
amostras.join$Classe = as.factor(amostras.join$Classe)

# Split the samples in training and test
set.seed(135) #garante o mesmo conjunto de numeros aleatorios

split.treino = sample.split(amostras.join$Classe, SplitRatio = .7)

#Training data
train.RF_full_124 = amostras.join[split.treino,]
#write.csv(train.RF_full_124, "./resultados_dissertacao/samples_train_RF_full_124_mod_a1_a2.csv")

#Test data
valid.RF_full_124 = amostras.join[split.treino == F,] 
#write.csv(valid.RF_full_124, "./resultados_dissertacao/samples_valid_RF_full_124_mod_a1_a2.csv")

set.seed(129)
# Random Forest model (~. -> all attributes are used)
RF_full_131 = randomForest(Classe ~., ntree = 1000, data = train.RF_full_124, importance = T, 
                        classProbs = TRUE,localImp = TRUE)
plot(RF_full_124) #see OOB error

#Tune Mtry
set.seed(1357)
tuneRF(train.RF_full_124 %>% select(-Classe), train.RF_full_124$Classe, ntreeTry=1000, stepFactor=2, improve=0.001,
        trace=TRUE, plot=TRUE, doBest=FALSE)

# mtry = seq(10,100,5)
# ntree= 1000
# df.erro = data.frame(mtry = numeric(),
#                      erro = numeric())
# 
# for (i in mtry){
#   set.seed(3578)
#   RF = randomForest(Classe ~., ntree = ntree, mtry = i, data = train.RF_full_124, importance = T, 
#                     classProbs = TRUE,localImp = TRUE)
#   df = data.frame(mtry = i,
#                  erro = RF$err.rate[ntree,1])
#   df.erro = rbind(df.erro, df)
#     
# }

set.seed(1357)
RF_full_124_tunning = randomForest(Classe ~., ntree = 1000, mtry = 22, data = train.RF_full_124, importance = T, 
                                   classProbs = TRUE,localImp = TRUE)
plot(RF_full_124_tunning) #see OOB error

oob_error = as.data.frame(RF_full_124_tunning$err.rate) %>% mutate(ntree = as.numeric(row.names(.)))
#see OOB error
#plot oob x ntree
ggplot(oob_error, aes(x = ntree, y=OOB)) +
  geom_line() + theme_bw() + 
  labs( x = "ntree", y = "erro OOB") +
  theme(
    axis.title.x = element_text(size=12), 
    axis.title.y = element_text(size=12),
    axis.text.x = element_text(size= 11, color = "black"),
    axis.text.y = element_text(size= 11, color = "black"))

importance_full = as.data.frame(importance(RF_full_124_tunning, type=NULL, class=NULL, scale=TRUE))
importance_full$MeanDecreaseAccuracy =  importance_full$MeanDecreaseAccuracy


#salvando o modelo
saveRDS(RF_full_124_tunning, file = "./resultados_dissertacao/RF_full_124_tunning_ntree1000_mtry22_mod_a1_a2.rds", ascii = T)

predict.RF_full1 = predict(RF_full_124, valid.RF_full_124) #classifica
ConfusMatrix.RF_full1 = confusionMatrix(data = predict.RF_full1, reference = valid.RF_full_124$Classe)
ConfusMatrix.RF_full1
rm(predict.RF_full1)

# =================================== Random Forest var_sel 33 =================================================

## a coluna "Classe" tem que ser colocada como FACTOR
amostras.join_var.sel$Classe = as.factor(amostras.join_var.sel$Classe)
# amostras_todas_RFreduced = amostras.join_var.sel %>% select(Lat, Long,Classe,
#                                                        dist_eucli_dren5, dist_eucli_dren6, dist_eucli_dren7, 
#                                                        hand_SWBD_burned, hand4_burned, hand5_burned, hand6_burned, hand7_burned, 
#                                                        slope_rampa_dren7)
# write.csv(amostras_todas_RFreduced, './resultados_dissertacao/metodo_sel_var/amostras_modificadas/RFm2/amostras_todas_RF9var.csv')

# Split the samples in training and test
set.seed(135) #garante o mesmo conjunto de numeros aleatorios

split.treino = sample.split(amostras.join_var.sel$Classe, SplitRatio = .7)

#Training data
train.RF_var.sel = amostras.join_var.sel[split.treino,]
# write.csv(train.RF_var.sel, "./resultados_dissertacao/samples_train_RF_var_sel_33_mod_a1_a2.csv")

# amostras_train_RFreduced = train.RF_var.sel %>% select(Lat, Long,Classe,
#                                              dist_eucli_dren5, dist_eucli_dren6, dist_eucli_dren7, 
#                                              hand_SWBD_burned, hand4_burned, hand5_burned, hand6_burned, hand7_burned, 
#                                              slope_rampa_dren7)
# write.csv(amostras_train_RFreduced, './resultados_dissertacao/metodo_sel_var/amostras_modificadas/RFm2/amostras_treino_RF9var.csv')




#Test data
valid.RF_var.sel = amostras.join_var.sel[split.treino == F,] 
#write.csv(valid.RF_var.sel, "./resultados_dissertacao/samples_valid_RF_var_sel_33_mod_a1_a2.csv")

# shp_amostras_valid = valid.RF_var.sel %>% select(Latitude, Longitude,Classe)
# write.csv(shp_amostras_valid, 'valid_a3_amostras.csv')


set.seed(11357)
# Random Forest model (~. -> all attributes are used)
RF_var.sel = randomForest(Classe ~., ntree = 1000, data = train.RF_var.sel, importance = T, 
                          classProbs = TRUE,localImp = TRUE)

RF_var.sel #see OOB error
#Tune Mtry

set.seed(23)
tuneRF(train.RF_var.sel %>% select(-Classe), train.RF_var.sel$Classe, ntreeTry=1000, stepFactor=2, improve=0.0002,
       trace=TRUE, plot=TRUE, doBest=FALSE)

mtry = seq(4,20,2)
ntree= 1000
df.erro = data.frame(mtry = numeric(),
                     erro = numeric())

for (i in mtry){
  
  set.seed(13579)
  RF = randomForest(Classe ~., ntree = ntree, mtry = i, data = train.RF_var.sel, importance = T, 
                    classProbs = TRUE,localImp = TRUE)
  df = data.frame(mtry = i,
                  erro = RF$err.rate[ntree,1])
  df.erro = rbind(df.erro, df)
  
}

set.seed(11357)
RF_var.sel_33_tunning = randomForest(Classe ~., ntree = 1000, mtry = 5, data = train.RF_var.sel, importance = T, 
                          classProbs = TRUE,localImp = TRUE)

#salvando o modelo
saveRDS(RF_var.sel_33_tunning, file = "./resultados_dissertacao/RF_var_sel_33_tunning_ntree1000_mtry5_mod_a1_a2.rds", ascii = T)

plot(RF_var.sel_33_tunning, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

predict.RF_var.sel = predict(RF_var.sel_33_tunning, valid.RF_var.sel) #classifica
ConfusMatrix.RF_full1 = confusionMatrix(data = predict.RF_var.sel, reference = valid.RF_var.sel$Classe)
ConfusMatrix.RF_full1

#==== ROC AUC ====
#prediction_roc = stats:: predict(RF, valid, type ="prob")
prediction_roc.RF_var.sel = as.numeric(stats:: predict(RF_var.sel, valid.RF_var.sel, type ="response")) 
pred.RF_var.sel =prediction(prediction_roc.RF_var.sel, valid.RF_var.sel$Classe)
perf.RF_var.sel <- performance(pred.RF_var.sel, measure = "tpr", x.measure = "fpr") 
plot(perf.RF_var.sel, col=rainbow(10), main = "Curva ROC RF_var.sel", cex.axis = 1.5, cex.lab = 1.5, cex.main =1.5)
abline(a=0,b=1,lwd=2,lty=2,col="gray") #reta x=y
auc.perf.RF_var.sel = performance(pred.RF_var.sel,measure = "auc")
print(auc.perf.RF_var.sel@y.values)



#====== rfviz=====
# library("rfviz")
# 
# rfprep <- rf_prep(x=amostras.atrib[,2:10], y=amostras.atrib$Classe, seed=23)
# rf_viz(rfprep) 
