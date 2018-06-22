# Importação de Dependências
# Necessária Versão R >= 3.4
library(gmodels)
library(C50)
library(caret)
library(ROCR)

# Base de Dados Retirada de:
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
#
# Importação da Base de Dados
credit_data <- read.csv("credit.csv")

# Exploração, Preparação e Pré-Processamento dos Dados

# Exibição da Estrutura dos Dados (Atributos e Rótulo)
str(credit_data)

# Estatística Exploratória Para os Atributos Categóricos
# checking_balance
table(credit_data$checking_balance)
prop.table(table(credit_data$checking_balance))
# credit_history
table(credit_data$credit_history)
prop.table(table(credit_data$credit_history))
# purpose
table(credit_data$purpose)
prop.table(table(credit_data$purpose))
# savings_balance
table(credit_data$savings_balance)
prop.table(table(credit_data$savings_balance))
# employment_duration
table(credit_data$employment_duration)
prop.table(table(credit_data$employment_duration))
# other_credit
table(credit_data$other_credit)
prop.table(table(credit_data$other_credit))
# housing
table(credit_data$housing)
prop.table(table(credit_data$housing))
# job
table(credit_data$job)
prop.table(table(credit_data$job))
# phone
table(credit_data$phone)
prop.table(table(credit_data$phone))

# Estatística Exploratória Para os Atributos Numéricos
# month_loan_duration
summary(credit_data$months_loan_duration)
sd(credit_data$months_loan_duration)
hist(credit_data$months_loan_duration, main = "Histogram of Loan Duration", xlab = "Months")
# amount
summary(credit_data$amount)
sd(credit_data$amount)
hist(credit_data$amount, main = "Histogram of Credit Amount", xlab = "Deutsch Marks")
# percent_of_income
summary(credit_data$percent_of_income)
sd(credit_data$percent_of_income)
hist(credit_data$percent_of_income, main = "Histogram of Installment Rate in Percentage of Disposable Income", xlab = "Percent")
# years_at_residence
summary(credit_data$years_at_residence)
sd(credit_data$years_at_residence)
hist(credit_data$years_at_residence, main = "Histogram of Present Residence Since", xlab = "Years")
# age
summary(credit_data$age)
sd(credit_data$age)
hist(credit_data$age, main = "Histogram of Client Age", xlab = "Years")
# existing_loans_count
summary(credit_data$existing_loans_count)
sd(credit_data$existing_loans_count)
hist(credit_data$existing_loans_count, main = "Histogram of Existing Loans Count", xlab = "Number of Loans")
# dependents
summary(credit_data$dependents)
sd(credit_data$dependents)
hist(credit_data$dependents, main = "Histogram of Dependent Count", xlab = "Number of Dependents")

# Preparação da Base de Dados - Criação dos Conjuntos de Treinamento (90%) e Teste (10%)

# Proporção Original das Classes
prop.table(table(credit_data$default))
# Aleatoriedade na Escolha das Instâncias de Treino e Teste
set.seed(123)
train_sample_indexes <- sample(1000,900)
# Construção dos Conjuntos de Treino e Teste
credit_data_train_set <- credit_data[train_sample_indexes, ]
credit_data_test_set <- credit_data[-train_sample_indexes, ]
# Verificação da Proporção das Classes nos Conjuntos de Treinamento e Teste
prop.table(table(credit_data_train_set$default))
prop.table(table(credit_data_test_set$default))

# Treinamento do Modelo C5.0 Simples (Sem Boosting Adaptativo)

# Geração da Árvore de Decisão Utilizando o Algoritmo C5.0
tree_model <- C5.0(credit_data_train_set[-17], credit_data_train_set$default)

# Predição e Avaliação da Capacidade de Generalização do Modelo C5.0 Simples
# Verificação da Matriz de Confusão no Conjunto de Treinamento
summary(tree_model)
# Etapa de Predição Usando o Modelo
m_predicted_class_labels <- predict(tree_model, credit_data_test_set)
m_predicted_class_probs <- predict(tree_model, credit_data_test_set, type = "prob")
# Avaliação de Desempenho do Modelo no Conjunto de Teste
# DEPRECATED: model_performance <- CrossTable(credit_data_test_set$default, predicted_class_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
# Avaliação Utilizando o Pacote CARET
model_performance <- confusionMatrix(predicted_class_labels, credit_data_test_set$default, positive = "yes")
# Avaliação Utilizando o Pacote ROCR
model_rocr_pred <- prediction(predictions = m_predicted_class_probs[,2], labels = credit_data_test_set$default)
model_rocr_perf <- performance(model_rocr_pred, measure = "tpr", x.measure = "fpr")
plot(model_rocr_perf, main = "ROC Curve for C5.0 Without Boosting", col = "blue", lwd = 3)
model_auc <- performance(model_rocr_pred, measure = "auc")
model_auc_value <- unlist(model_auc@y.values)

# Treinamento do Modelo C5.0 com Boosting Adaptativo

# Geração da Floresta de Árvores de Decisão Utilizando o Algoritmo C5.0 (Com 10 Árvores)
tree_model_boosting <- C5.0(credit_data_train_set[-17], credit_data_train_set$default, trials = 10)

# Predição e Avaliação da Capacidade de Generalização do Modelo C5.0 com Boosting Adaptativo
# Verificação da Matriz de Confusão no Conjunto de Treinamento
summary(tree_model_boosting)
# Etapa de Predição Usando o Modelo
mboost_predicted_class_labels <- predict(tree_model_boosting, credit_data_test_set)
mboost_predicted_class_probs <- predict(tree_model_boosting, credit_data_test_set, type = "prob")
# Avaliação de Desempenho do Modelo no Conjunto de Teste
# DEPRECATED: model_boosting_performance <- CrossTable(credit_data_test_set$default, predicted_class_labels, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))
# Avaliação Utilizando o Pacote CARET
model_boosting_performance <- confusionMatrix(predicted_class_labels, credit_data_test_set$default, positive = "yes")
# Avaliação Utilizando o Pacote ROCR
mboost_rocr_pred <- prediction(predictions = mboost_predicted_class_probs[,2], labels = credit_data_test_set$default)
mboost_rocr_perf <- performance(mboost_rocr_pred, measure = "tpr", x.measure = "fpr")
plot(mboost_rocr_perf, main = "ROC Curve for C5.0 With Boosting", col = "blue", lwd = 3)
mboost_auc <- performance(mboost_rocr_pred, measure = "auc")
mboost_auc_value <- unlist(mboost_auc@y.values)

# Treinamento do Modelo Random Forest
# Avaliação de Desempenho do Modelo no Conjunto de Teste

# Experimento Final de Avaliação de Desempenho