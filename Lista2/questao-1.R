# Script Principal para a Questão 1

# Pacotes Utilizados
library(nnet) # Modelo MLP
library(caret) # Avaliação de Desempenho

# Base de Dados Retirada de:
# http://archive.ics.uci.edu/ml/datasets/Iris
#
# Importação da Base de Dados
iris_data <- read.csv("iris_data.csv")

# Configuração e Projeto do Experimento de Avaliação 

# Utilizando Validação Cruzada em 10 Partes Para Treinamento e Validação do Modelo MLP
training_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Treinamento e Validação (Ajuste de Parâmetros) para o Modelo
mlp_model_eval <- train(class ~ ., data = iris_data, method = "nnet", metric = "Accuracy", trControl = training_ctrl)
