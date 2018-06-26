# Script Principal para a Questão 4

# Pacotes Utilizados
library(kernlab) # Modelos SVM
library(caret) # Avaliação de Desempenho

# Base de Dados Retirada de:
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
#
# Importação da Base de Dados
credit_data <- read.csv("credit.csv")

# Configuração e Projeto do Experimento de Avaliação 

# Utilizando Validação Cruzada em 10 Partes Para Treinamento e Validação dos Três Modelos Considerados
training_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Configuração da Análise Fatorial para o Modelo SVM Linear
linear_svm_factor_grid <- expand.grid(.C = c(0.25, 0.5, 1.0))
# Configuração da Análise Fatorial para o Modelo SVM Polinomial
polynomial_svm_factor_grid <- expand.grid(.degree = c(1, 2, 3, 4, 5), .scale = 0.01, .C = c(0.25, 0.5, 1.0))
# Configuração da Análise Fatorial para o Modelo SVM Com Kernel RBF
rbf_svm_factor_grid <- expand.grid(.sigma = c(0.001, 0.018975, 0.1, 1.0, 10.0), .C = c(0.25, 0.5, 1.0))

# Treinamento e Validação (Ajuste de Parâmetros) para o Modelo SVM Linear
linear_svm_model_eval <- train(default ~ ., data = credit_data, method = "svmLinear", metric = "Accuracy", trControl = training_ctrl, tuneGrid = linear_svm_factor_grid)

# Treinamento e Validação (Ajuste de Parâmetros) para o Modelo SVM Polinomial
polynomial_svm_model_eval <- train(default ~ ., data = credit_data, method = "svmPoly", metric = "Accuracy", trControl = training_ctrl, tuneGrid = polynomial_svm_factor_grid)

# Treinamento e Validação (Ajuste de Parâmetros) para o Modelo SVM com Kernel RBF
rbf_svm_model_eval <- train(default ~ ., data = credit_data, method = "svmRadial", metric = "Accuracy", trControl = training_ctrl, tuneGrid = rbf_svm_factor_grid)
