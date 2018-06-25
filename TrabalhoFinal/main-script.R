# Importação de Dependências
# Necessária Versão R >= 3.4
library(gmodels)
library(C50)
library(randomForest)
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
set.seed(123) # Configuração da Semente para Garantir a Reproducibilidade dos Experimentos
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
plot(model_rocr_perf, main = "ROC Curve for C5.0 Without Boosting", col = "blue", lwd = 2)
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
par(new = TRUE)
plot(mboost_rocr_perf, main = "ROC Curve for C5.0 With Boosting", col = "blue", lwd = 2)
mboost_auc <- performance(mboost_rocr_pred, measure = "auc")
mboost_auc_value <- unlist(mboost_auc@y.values)

# Treinamento do Modelo Random Forest

# Geração da Floresta Aleatória Utilizando o Algoritmo randomForest
set.seed(300) # Configuração da Semente para Garantir a Reproducibilidade dos Experimentos
random_forest_model <- randomForest(credit_data_train_set[-17], credit_data_train_set$default)

# Predição e Avaliação da Capacidade de Generalização do Modelo Random Forest
# Etapa de Predição Usando o Modelo
rf_predicted_class_labels <- predict(random_forest_model, credit_data_test_set)
rf_predicted_class_probs <- predict(random_forest_model, credit_data_test_set, type = "prob")
# Avaliação de Desempenho do Modelo no Conjunto de Teste
# Avaliação Utilizando o Pacote CARET
rf_performance <- confusionMatrix(rf_predicted_class_labels, credit_data_test_set$default, positive = "yes")
# Avaliação Utilizando o Pacote ROCR
rf_rocr_pred <- prediction(predictions = rf_predicted_class_probs[,2], labels = credit_data_test_set$default)
rf_rocr_perf <- performance(rf_rocr_pred, measure = "tpr", x.measure = "fpr")
par(new = TRUE)
plot(rf_rocr_perf, main = "ROC Curve for Random Forest", col = "blue", lwd = 2)
rf_auc <- performance(rf_rocr_pred, measure = "auc")
rf_auc_value <- unlist(rf_auc@y.values)

# Geração das Três Curvas ROC em um Mesmo Gráfico
plot(model_rocr_perf, main = "ROC Curves for the Models Under Analysis", sub = "C5.0 (Blue), C5.0 w/ Boosting (Green), Random Forest (Red)", col = "blue", lwd = 2)
par(new = TRUE)
plot(mboost_rocr_perf, main = "", col = "green", lwd = 2)
par(new = TRUE)
plot(rf_rocr_perf, main = "", col = "red", lwd = 2)

# Experimento Final de Avaliação de Desempenho Utilizando o Pacote CARET

# Configuração e Projeto do Experimento de Avaliação 
# Utilizando Validação Cruzada em 10 Partes
training_ctrl <- trainControl(method = "repeatedcv", number = 30, repeats = 10)
# Configuração da Análise Fatorial para os Modelos C5.0 sem (trials = 1) e com Boosting (trials > 1)
c50_factor_grid <- expand.grid(.model = "tree", .trials = c(1, 10, 20, 30, 40), .winnow = "FALSE")
# Configuração da Análise Fatorial para o Modelo Random Forest (Número de Atributos a Serem Considerados nas Divisões)
rf_factor_grid <- expand.grid(.mtry = c(2, 4, 8, 16))

# Treinamento e Validação (Ajuste de Parâmetros pela Análise Fatorial) dos Modelos
tree_boost_model_eval <- train(default ~ ., data = credit_data, method = "C5.0", metric = "Kappa", trControl = training_ctrl, tuneGrid = c50_factor_grid)
random_forest_model_eval <- train(default ~ ., data = credit_data, method = "rf", metric = "Kappa", trControl = training_ctrl, tuneGrid = rf_factor_grid)

# Teste Estatístico t Pareado (Acurácia Preditiva) Boosting x Árvore de Decisão Simples
x1 <- rnorm(mean = tree_boost_model_eval$results$Accuracy[1], sd = tree_boost_model_eval$results$AccuracySD[1], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = tree_boost_model_eval$results$Accuracy[5], sd = tree_boost_model_eval$results$AccuracySD[5], n = 300) # Boosting (40 Árvores - Melhor)
boost_vs_simple_acc <- t.test(x1, x2, alternative = "less", paired = TRUE)

# Teste Estatístico t Pareado (Estatística Kappa) Boosting x Árvore de Decisão Simples
x1 <- rnorm(mean = tree_boost_model_eval$results$Kappa[1], sd = tree_boost_model_eval$results$KappaSD[1], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = tree_boost_model_eval$results$Kappa[5], sd = tree_boost_model_eval$results$KappaSD[5], n = 300) # Boosting (40 Árvores - Melhor)
boost_vs_simple_kappa <- t.test(x1, x2, alternative = "less", paired = TRUE)

# Teste Estatístico t Pareado (Acurácia Preditiva) Floresta Aleatória (Bagging, .mtry = 16) x Árvore de Decisão Simples
x1 <- rnorm(mean = tree_boost_model_eval$results$Accuracy[1], sd = tree_boost_model_eval$results$AccuracySD[1], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = random_forest_model_eval$results$Accuracy[4], sd = random_forest_model_eval$results$AccuracySD[4], n = 300) # Floresta Aleatória e Bagging (16 Atributos - Melhor)
rf_bag_vs_simple_acc <- t.test(x1, x2, alternative = "less", paired = TRUE)

# Teste Estatístico t Pareado (Estatística Kappa) Floresta Aleatória (Bagging, .mtry = 16) x Árvore de Decisão Simples
x1 <- rnorm(mean = tree_boost_model_eval$results$Kappa[1], sd = tree_boost_model_eval$results$KappaSD[1], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = random_forest_model_eval$results$Kappa[4], sd = random_forest_model_eval$results$KappaSD[4], n = 300) # Floresta Aleatória e Bagging (16 Atributos - Melhor)
rf_bag_vs_simple_kappa <- t.test(x1, x2, alternative = "less", paired = TRUE)

# Teste Estatístico t Pareado (Acurácia Preditiva) Floresta Aleatória (Bagging, .mtry = 16) x Boosting (40 Árvores)
x1 <- rnorm(mean = tree_boost_model_eval$results$Accuracy[5], sd = tree_boost_model_eval$results$AccuracySD[5], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = random_forest_model_eval$results$Accuracy[4], sd = random_forest_model_eval$results$AccuracySD[4], n = 300) # Floresta Aleatória e Bagging (16 Atributos - Melhor)
rf_bag_vs_simple_acc <- t.test(x1, x2, alternative = "less", paired = TRUE)

# Teste Estatístico t Pareado (Estatística Kappa) Floresta Aleatória (Bagging, .mtry = 16) x Boosting (40 Árvores)
x1 <- rnorm(mean = tree_boost_model_eval$results$Kappa[5], sd = tree_boost_model_eval$results$KappaSD[5], n = 300) # Árvore de Decisão Simples
x2 <- rnorm(mean = random_forest_model_eval$results$Kappa[4], sd = random_forest_model_eval$results$KappaSD[4], n = 300) # Floresta Aleatória e Bagging (16 Atributos - Melhor)
rf_bag_vs_simple_kappa <- t.test(x1, x2, alternative = "less", paired = TRUE)
