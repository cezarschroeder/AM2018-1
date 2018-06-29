# Script Principal para a Questão 3

# Pacotes Utilizados
library(C50)
library(caret)

# Bases de Dados Retiradas de:
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
# http://archive.ics.uci.edu/ml/datasets/Iris
#
# Importação da Base de Dados
credit_data <- read.csv("credit.csv")
iris_data <- read.csv("iris-data.csv")

# Preparação da Base de Dados - Criação dos Conjuntos de Treinamento (80%) e Teste (20%)

# Base de Dados German Credit Data
# Proporção Original das Classes
prop.table(table(credit_data$default))
# Aleatoriedade na Escolha das Instâncias de Treino e Teste
set.seed(123) # Configuração da Semente para Garantir a Reproducibilidade dos Experimentos
train_sample_indexes <- sample(1000,800)
# Construção dos Conjuntos de Treino e Teste
credit_data_train_set <- credit_data[train_sample_indexes, ]
credit_data_test_set <- credit_data[-train_sample_indexes, ]
# Verificação da Proporção das Classes nos Conjuntos de Treinamento e Teste
prop.table(table(credit_data_train_set$default))
prop.table(table(credit_data_test_set$default))

# Base de Dados Iris
# Proporção Original das Classes
prop.table(table(iris_data$class))
# Aleatoriedade na Escolha das Instâncias de Treino e Teste
set.seed(321) # Configuração da Semente para Garantir a Reproducibilidade dos Experimentos
train_sample_indexes <- sample(1000,800)
# Construção dos Conjuntos de Treino e Teste
iris_data_train_set <- iris_data[train_sample_indexes, ]
iris_data_test_set <- iris_data[-train_sample_indexes, ]
# Verificação da Proporção das Classes nos Conjuntos de Treinamento e Teste
prop.table(table(iris_data_train_set$class))
prop.table(table(iris_data_test_set$class))

# Treinamento e Avaliação do Modelo C5.0 (German Credit Data)

# Geração da Árvore de Decisão Utilizando o Algoritmo C5.0
credit_model_ctrl <- C5.0Control(minCases = 20) # Para reduzir o número de nós e o consequente sobreajuste
credit_tree_model <- C5.0(credit_data_train_set[-17], credit_data_train_set$default, control = credit_model_ctrl)

# Predição e Avaliação da Capacidade de Generalização do Modelo C5.0 Simples
# Verificação da Matriz de Confusão no Conjunto de Treinamento
summary(credit_tree_model)
# Etapa de Predição Usando o Modelo
credit_predicted_class_labels <- predict(credit_tree_model, credit_data_test_set)
credit_predicted_class_probs <- predict(credit_tree_model, credit_data_test_set, type = "prob")
# Avaliação de Desempenho do Modelo no Conjunto de Teste
# Avaliação Utilizando o Pacote CARET
credit_model_performance <- confusionMatrix(credit_predicted_class_labels, credit_data_test_set$default, positive = "yes")

# Treinamento e Avaliação do Modelo C5.0 (Iris)

# Geração da Árvore de Decisão Utilizando o Algoritmo C5.0
iris_tree_model <- C5.0(iris_data_train_set[-5], iris_data_train_set$class)

# Predição e Avaliação da Capacidade de Generalização do Modelo C5.0 Simples
# Verificação da Matriz de Confusão no Conjunto de Treinamento
summary(iris_tree_model)
# Etapa de Predição Usando o Modelo
iris_predicted_class_labels <- predict(iris_tree_model, iris_data_test_set)
iris_predicted_class_probs <- predict(iris_tree_model, iris_data_test_set, type = "prob")
# Avaliação de Desempenho do Modelo no Conjunto de Teste
# Avaliação Utilizando o Pacote CARET
iris_model_performance <- confusionMatrix(iris_predicted_class_labels, iris_data_test_set$class)
