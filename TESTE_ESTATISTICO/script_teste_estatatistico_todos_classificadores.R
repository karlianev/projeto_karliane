#  install.packages("BiocManager")
#  BiocManager::install("scmamp")
  
 
  library("scmamp")
  library("PMCMR")
  
  nome_colunas <- c("Original", "Limiar_fixo","FlexCon-G","FlexCon(s)","FlexCon(v)","FlexCon-C1(s)","FlexCon-C1(v)","FlexCon-C2")

  setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\teste estatistico\\arquivos para rodar o teste estatistico\\todos classificadores juntos")
  
  for (j in 2:6){
    nome_arquivo <- paste(c("teste_estatistico_todos_juntos", (j-1)*5, "porc"), collapse = "_")
    
    if (j==2){
      dados <- as.matrix(read.csv("resultado_5_porc.csv"))  
    }else if (j==3){
      dados <- as.matrix(read.csv("resultado_10_porc.csv"))
    }else if (j==4){
      dados <- as.matrix(read.csv("resultado_15_porc.csv"))
    }else if (j==5){
      dados <- as.matrix(read.csv("resultado_20_porc.csv"))
    }else{
      dados <- as.matrix(read.csv("resultado_25_porc.csv"))
    }
  
    #roda o teste
    #gera grafico com resultados
    # plotCD(dados)
    
    png(filename = paste(c("grafico_friedman", nome_arquivo, ".png"), collapse = "."),
        width = 640, height = 640)
    
    # Plotando o gráfico de fato
    plotCD(dados, cex = 1.25)
    
    # Salvando o arquivo de fato no .png
    dev.off()  
    
    # friedman <- friedman.test(dados)
    # #roda pareado
    # friedman_conover <- posthoc.friedman.conover.test(dados)
    # friedman_nemenyi <- posthoc.friedman.nemenyi.test(dados)
    # 
    # write.csv(friedman$p.value, paste("friedman", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
    # write.csv(friedman_conover$p.value, paste("conover", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
    # write.csv(friedman_nemenyi$p.value, paste("nemenyi", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
  }


# fazendo o teste com os arquivos que já foram montados anteriormente
# e retirando a coluna com o original
# dados <- as.matrix(read.csv("resultado_naiveBayes_10__porc_.csv"))
# new_dados <- dados[,-1]
# friedman_nemenyi <- posthoc.friedman.nemenyi.test(new_dados)
# friedman_nemenyi

#outras op??es de teste pareado
# wilcox.test(dados)
# posthoc.durbin.test(dados)
# posthoc.friedman.nemenyi.test(dados)
