# Leitura dos Dados de Entrada
dados1 <- read.csv("dados1.csv", header = FALSE, stringsAsFactors = FALSE)

# Obtenção das Estatísticas Desejadas Para o Atributo V1

# Média Amostral
media_v1 <- mean(dados1$V1)
# Variância Amostral
var_v1 <- var(dados1$V1)
# Desvio-Padrão Amostral
desvio_v1 <- sd(dados1$V1)

# Obtenção das Estatísticas Desejadas para o Atributo V2

# Média Amostral
media_v2 <- mean(dados1$V2)
# Variância Amostral
var_v2 <- var(dados1$V2)
# Desvio-Padrão Amostral
desvio_v2 <- sd(dados1$V2)

# Cálculo da Covariância Amostral entre os Atributos V1 e V2
covariancia_v1v2 <- cov(dados1[,1:2])[1,2]

# Cálculo do Coeficiente de Correlação Amostral entre os Atributos V1 e V2
r_v1v2 <- covariancia_v1v2 / (desvio_v1 * desvio_v2)
