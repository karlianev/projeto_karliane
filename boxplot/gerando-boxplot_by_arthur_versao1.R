# Assumindo que todos os folds das acurácias de cada dataset estão no data Frame
# segue o seguinte código
# LEMBRETE: OS BOXPLOTS VÃO FICAR MAIS ARRUMADINHOS PORQUÊ É UMA DISTRIBUIÇÃO
#   GAUSSIANA COM MÉDIA 80 E DESVIO PADRÃO EM 5 PONTOS
#
# ASSUMINDO que o data frame se encontra no seguinte padrão.
#        BASE1 BASE2 BASE3 ··· BASE30 - AQUI SÃO OS PERCENTUAIS INICIALMENTE ROTULADOS
# FOLD1  84.56 65.89 56.89 ··· 89.68  - AQUI SÃO AS BASES E OS FOLDS
# FOLD2  56.98 66.98 98.48 ··· 84.84
# FOLD3  84.60 84.48 95.48 ··· 79.56
# ·        ·     ·     .   ···   ·
# ·        ·     ·     .   ···   ·
# ·        ·     ·     .   ···   ·
# FOLD10 48.78 79.80 99.98 ··· 48.88

#######################
## Código de Exemplo ##
#######################

# Gerando distribuição aleatória 
distribuicao <- rnorm(300, 80, 5)

# Nomes das bases de dados
databases <- c("iris", "bupa", "segment", "waveform-5000", "phishingData",
               "mushroom", "pima", "vehicle", "wilt",
               "kr-vs-kp", "blood-transfusion-service", "cnae-9",
               "connectionist-mines-vs-rocks", "flare",
               "indian-liver-patient", "leukemia-haslinger",
               "mammographic-mass", "mfeat-karhunen", "musk",
               "ozone-onehr", "pendigits", "planning-relax", "seeds",
               "semeion", "spectf-heart", "tic-tac-toe", "twonorm",
               "hill-valley", "balance-scale", "car")

# criando um data frame com a distribuição gerada
dados <- as.data.frame(matrix(data = distribuicao, byrow = T, nrow = 10))


# Extensão que o arquivo vai ser salvo
extensao <- "png"


for (i in 1:length(databases)) {
  # Medidas ajustáveis, estou informando os parâmetros que julgo necessários/
  # pertinentes para a geração deste gráficos
  png(filename = paste(c(databases[i], extensao), collapse = "."),
      width = 640, height = 640)
  
  # Plotando o gráfico de fato
  boxplot(dados[,i], ylab = "Acurácia", main = databases[i],
          ylim = c(min(dados), max(dados)))
  
  # Salvando o arquivo de fato no .png
  dev.off()  
}

# Plotando um boxplot de todas as distribuições
png("geral.png", width = 1024, height = 1024)

boxplot(dados, names = databases, ylab = "Acurácias", main = "Todas as bases",
        at = 1:30, xlim = c(1, 30), ylim = c(min(dados), max(dados)),
        yaxs = "i")

dev.off()
