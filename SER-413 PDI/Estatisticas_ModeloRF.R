library(randomForest)
library(ggplot2)
library(dplyr)
library(tibble)

# VERIFICANDO OOB DO MODELO RF JÁ TREINADO
# Convertendo os dados para um data frame
oob_error <- as.data.frame(rf_model$err.rate) %>% 
  mutate(ntree = as.numeric(row.names(.)))

# Encontrar o valor de ntree com o menor erro OOB
min_oob <- oob_error[which.min(oob_error$OOB),]

# Criando o gráfico com a marcação
ggplot(oob_error, aes(x = ntree, y = OOB)) +
  geom_line() + 
  geom_vline(xintercept = min_oob$ntree, linetype = "dashed", color = "red") +
  geom_text(aes(x = min_oob$ntree, y = min_oob$OOB, label = paste("ntree =", min_oob$ntree)), 
            vjust = -1, hjust = 1, color = "red") +
  theme_bw() + 
  labs(x = "Número de Árvores (ntree)", y = "Erro OOB") +
  theme(
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black")
  )


plot(oob_error)

min(oob_error)

# -------------------------------------------------------------------
# OBTENDO MEAN DECREASE ACCURACY DO MODELO RF JÁ TREINADO
# Supondo que rf_model é o seu modelo randomForest já treinado

# Calcular a importância das variáveis (Mean Decrease Accuracy)
importance_values <- importance(rf_model, type = 1)

# Converter para data frame e renomear colunas
importance_df <- as.data.frame(importance_values) %>%
  rownames_to_column(var = "Variable") %>%
  rename(MeanDecreaseAccuracy = MeanDecreaseAccuracy)

# Ordenar por importância
importance_df <- importance_df %>% arrange(desc(MeanDecreaseAccuracy))

# Criar o gráfico de barras
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseAccuracy), y = MeanDecreaseAccuracy)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Coloca as barras na horizontal
  theme_bw() +
  labs(x = "Variável", y = "Mean Decrease Accuracy", 
       title = "Importância das Variáveis (Mean Decrease Accuracy)") +
  theme(
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    plot.title = element_text(size = 14, face = "bold")
  )

