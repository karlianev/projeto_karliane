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
#distribuicao <- rnorm(300, 80, 5)



# Nomes das bases de dados
# databases <- c("iris", "bupa", "segment", "waveform-5000", "phishingData",
#                "mushroom", "pima", "vehicle", "wilt",
#                "kr-vs-kp", "blood-transfusion-service", "cnae-9",
#                "connectionist-mines-vs-rocks", "flare",
#                "indian-liver-patient", "leukemia-haslinger",
#                "mammographic-mass", "mfeat-karhunen", "musk",
#                "ozone-onehr", "pendigits", "planning-relax", "seeds",
#                "semeion", "spectf-heart", "tic-tac-toe", "twonorm",
#                "hill-valley", "balance-scale", "car")

# criando um data frame com a distribuição gerada
#dados <- as.data.frame(matrix(data = distribuicao, byrow = T, nrow = 10))


# 
# for (i in 2:length(ncol(dados))) {
#   nome_csv_saida <- paste(c(nome_csv_saida_inicio,i),collapse = "_")
#   # Medidas ajustáveis, estou informando os parâmetros que julgo necessários/
#   # pertinentes para a geração deste gráficos
#   png(filename = paste(c(nome_csv_saida, extensao), collapse = "."),
#       width = 640, height = 640)
#   
#   # Plotando o gráfico de fato x = percentuais, y = acuracia
#   boxplot(dados[,i], ylab = "Acurácia", xlab = paste(c((i-1)*5,"%"), collapse=""), main = nome_grafico,
#           ylim = c(min(dados[,i]), max(dados[,i])))
# 
#   # Salvando o arquivo de fato no .png
#   dev.off()  
# }
# 
# dados_geral <- as.data.frame(dados[,2:6])
# # Plotando um boxplot de todas as distribuições
# png("geral.png", width = 1024, height = 1024)
# 
# boxplot(dados_geral, names = "percentuais", ylab = "Acurácias", main = nome_grafico,
#         add=TRUE, xlim = c(1, 5), ylim = c(min(dados[1:300,2:6]), max(dados[1:300,2:6])),
#         yaxs = "i")
# 
# dev.off()
# 

setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 3_gradativo_completo")
nome_arq <- "self_training_naiveBayes_media_metodo_3_5.csv"
nome_arq2 <- "self_training_rpartXse_media_metodo_3_5.csv"
nome_arq3 <- "self_training_JRip_media_metodo_3_5.csv"
nome_arq4 <- "self_training_IBk_media_metodo_3_5.csv"
nome_grafico <- "FlexCon-G"

dados <- read.csv(nome_arq)
dados2 <- read.csv(nome_arq2)
dados3 <- read.csv(nome_arq3)
dados4 <- read.csv(nome_arq4)
nome_csv_saida_inicio <- paste(c("boxsplot",nome_arq),collapse = "_")

# Extensão que o arquivo vai ser salvo
extensao <- "png"

dados_geral <- cbind(dados[,2:6],dados2[,2:6],dados3[,2:6],dados4[,2:6])

png("geral.png", width = 1024, height = 1024)

boxplot(dados_geral, main=nome_grafico, 
        ylab="Acurácia", xlab="Percentuais inicialmente rotulados", 
        names = rep(c("5%", "10%", "15%","20%","25%"),4), 
        col=c("red","red","red","red","red","blue","blue","blue","blue","blue","yellow","yellow","yellow","yellow","yellow","green","green","green","green","green","purple","purple","purple","purple","purple"),
        par(cex=2))

dev.off()

# percent <- (rep(c("5%", "10%", "15%", "20%", "25%"), each=300))
# boxplot(dados_geral,percent)#, main="ST Original com Nayve Bayes", ylab="acurácia", xlab="5%      10%      15%      20%      25%")
#boxplot(dados_geral[,1],dados_geral[,2],dados_geral[,3],dados_geral[,4],dados_geral[,5], main="ST Original com Nayve Bayes", ylab="acurácia", xlab="5%      10%      15%      20%      25%")