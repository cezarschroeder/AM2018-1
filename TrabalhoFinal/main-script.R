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
