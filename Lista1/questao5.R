# Código-Fonte para a Questão 5

# Aquisição dos Dados de Entrada
dados2 <- read.csv("dados2.csv", stringsAsFactors = FALSE, header = FALSE)

# Item (a)

# Cálculo do Vetor de Médias dos Três Atributos
vetor_medias <- c(mean(dados2$V1), mean(dados2$V2), mean(dados2$V3))

# Obtenção da Matriz de Dados com Atributos de Médias Nulas
matriz_dados <- as.matrix.data.frame(dados2)
matriz_dados[,1] <- matriz_dados[,1] - vetor_medias[1]
matriz_dados[,2] <- matriz_dados[,2] - vetor_medias[2]
matriz_dados[,3] <- matriz_dados[,3] - vetor_medias[3]

# Item (b)

# Cálculo da Matriz de Covariância
matriz_covariancia <- matrix(nrow = 3, ncol = 3)
for (i in 1:3)
{
  for (j in 1:3)
  {
    matriz_covariancia[i,j] <- cov(matriz_dados[,i], matriz_dados[,j])  
  }
}

# Cálculo dos Autovalores e Autovetores da Matriz de Covariância
auto_val_vet <- eigen(matriz_covariancia)

# Item (d)

# Cálculo da Projeção dos Dados Originais em *Duas* Dimensões No Novo Espaço Vetorial
z2d <- matrix(nrow = 1000, ncol = 2)
for (i in 1:1000) # Para Cada Linha da Matriz de Dados (Observação)
{
  z2d[i,] <- t(auto_val_vet$vectors[,1:2]) %*% matriz_dados[i,]
}
# Graficamente
plot(x = z2d[,1], y = z2d[,2], xlab = "Componente Principal 1", ylab = "Componente Principal 2", main = "Projeção dos Dados em Duas Dimensões")

# Projeção dos Dados Originais em *Uma* Dimensão (Primeira Componente Principal)
plot(x = 1:1000, y = z2d[,1], xlab = "Índice da Amostra", ylab = "Componente Principal 1", main = "Projeção dos Dados em Uma Dimensão")