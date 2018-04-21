# Leitura dos Dados de Entrada
dados1 <- read.csv("dados1.csv", header = FALSE, stringsAsFactors = FALSE)

# Obtenção das Estatísticas Desejadas Para o Atributo 1

# Média Amostral
mean(dados1$V1)
# Variância Amostral
var(dados1$V1)
# Desvio-Padrão Amostral
sd(dados1$V1)

# Obtenção das Estatísticas Desejadas para o Atributo 2

# Média Amostral
mean(dados1$V2)
# Variância Amostral
var(dados1$V1)
# Desvio-Padrão Amostral
sd(dados1$V1)
