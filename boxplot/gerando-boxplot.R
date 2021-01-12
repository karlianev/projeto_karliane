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

# setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 3_gradativo_completo")
setwd("resultados para gerar boxplot co-training/")
dirs <- list.dirs(recursive = F)
nome_graficos <- c("Original CT", "CTFT", "FlexCon-G", "FlexCon(s)",
                   "FlexCon(v)", "FlexCon-C1(s)", "FlexCon-C1(v)", "FlexCon-C2")
nome_arquivo <- c("boxplot_CT_original.png", "boxplot_CT_limiar_fixo.png", "boxplot_CT_gradativo.png", "boxplot_CT_FlexCon_soma.png", "boxplot_CT_FlexCon_voto.png", "boxplot_CT_FlexCon_c1_soma.png", "boxplot_CT_FlexCon_c1_voto.png", "boxplot_CT_FlexCon_c2.png")
for(dir in dirs) {
  files <- list.files(path = dir, pattern = "^co_training")
  nome_arq <- files[3]
  nome_arq2 <- files[4]
  nome_arq3 <- files[2]
  nome_arq4 <- files[1]
  nome_grafico <- nome_graficos[match(dir, dirs)]
  dados <- read.csv(paste(dir, nome_arq, sep = "/"))
  dados2 <- read.csv(paste(dir, nome_arq2, sep = "/"))
  dados3 <- read.csv(paste(dir, nome_arq3, sep = "/"))
  dados4 <- read.csv(paste(dir, nome_arq4, sep = "/"))
  nome_csv_saida_inicio <- paste(c("boxplot",nome_arq),collapse = "_")
  
  extensao <- "png"
  
  dados_geral <- cbind(dados[,2:6],dados2[,2:6],dados3[,2:6],dados4[,2:6])
  
  png(nome_arquivo[match(dir, dirs)], width = 770, height = 480)
  par(mar = c(4, 4, 6, 4), xpd = T, cex = 1)
  boxplot(dados_geral, main=nome_grafico, 
          ylab="Accuracy", xlab="Percentages initially labeled", 
          names = rep(c("5%", "10%", "15%","20%","25%"),4), 
          col=c("red","red","red","red","red","blue","blue","blue","blue","blue","yellow","yellow","yellow","yellow","yellow","green","green","green","green","green","purple","purple","purple","purple","purple"))
  legend("topleft", inset = c(0, -0.2), legend = c("NB", "AD", "Ripper", "k-NN"),
         fill = c("red", "blue", "yellow", "green"), ncol = 2, bty = "n")
  dev.off()
}


# Extensão que o arquivo vai ser salvo

# percent <- (rep(c("5%", "10%", "15%", "20%", "25%"), each=300))
# boxplot(dados_geral,percent)#, main="ST Original com Nayve Bayes", ylab="acurácia", xlab="5%      10%      15%      20%      25%")
#boxplot(dados_geral[,1],dados_geral[,2],dados_geral[,3],dados_geral[,4],dados_geral[,5], main="ST Original com Nayve Bayes", ylab="acurácia", xlab="5%      10%      15%      20%      25%")