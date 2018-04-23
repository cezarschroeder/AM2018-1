# Código-Fonte para a Questão 6

# Origem dos Dados: https://www.kaggle.com/dmvreddy91/usahousing/data

library(MASS) # Pacote necessário para o cálculo de pseudo-inversas

# Aquisição e Preparação dos Dados
dados_brutos <- read.csv("USA_Housing.csv", header = TRUE, stringsAsFactors = FALSE)
dados_regr <- data.frame(dados_brutos$Avg..Area.Income, dados_brutos$Price)
dados_regr$dados_brutos.Avg..Area.Income <- dados_regr$dados_brutos.Avg..Area.Income/10000
dados_regr$dados_brutos.Price <- dados_regr$dados_brutos.Price/100000

# Preparação dos Conjuntos de Treinamento e de Validação
# 3000 Amostras para Treinamento e 2000 Amostras para Validação
dados_regr_treino <- data.frame(dados_regr$dados_brutos.Avg..Area.Income[1:3000], dados_regr$dados_brutos.Price[1:3000])
dados_regr_valid <- data.frame(dados_regr$dados_brutos.Avg..Area.Income[3001:5000], dados_regr$dados_brutos.Price[3001:5000])

# Trabalharemos com Modelos (Polinômios) de Ordem 1 a 5
mserr_valid <- rep(0,15) # Vetor de Erro Quadrático Médio para Cada Ordem de Modelo
D_treino <- matrix(nrow = 3000, ncol = 16)
D_treino[,1] <- 1
D_valid <- matrix(nrow = 2000, ncol = 16)
D_valid[,1] <- 1
# Montagem das Matrizes D para Treino e para Validação
for (i in 2:16)
{
  D_treino[,i] <- (dados_regr_treino$dados_regr.dados_brutos.Avg..Area.Income.1.3000.)^(i-1)
  D_valid[,i] <- (dados_regr_valid$dados_regr.dados_brutos.Avg..Area.Income.3001.5000.)^(i-1)
}
for (ordem in 1:15)
{
  # Cálculo da Pseudo-Inversa de D (ginv) e do Vetor de Coeficientes de Regressão w
  w <- ginv(D_treino[,1:(ordem+1)]) %*% dados_regr_treino$dados_regr.dados_brutos.Price.1.3000.
  # Aplicando o Modelo no Conjunto de Validação
  r_modelo <- D_valid[,1:(ordem+1)] %*% w
  r_modelo <- (r_modelo - dados_regr_valid$dados_regr.dados_brutos.Price.3001.5000.)^2
  mserr_valid[ordem] <- mean(r_modelo)
}
plot(x = 1:15, y = mserr_valid, main = "Erro de Validação x Ordem do Polinômio", xlab = "Ordem do Modelo", ylab = "Erro Quadrático Médio")
