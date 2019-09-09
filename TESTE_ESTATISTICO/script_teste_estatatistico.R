#install.packages("PMCMR")
library("PMCMR")
#seta o diretorio local
classifiers <- c("naiveBayes", "rpartXse", "JRip", "IBk")
nome_colunas <- c("original","limiar_fixo","gradativo","flexcon_s","flexcon_v","flexcon_c1_s","flexcon_c1_v","flexcon_c2")

#MONTANDO O CSV NO FORMATO ABAIXO
# MEU CSV COM RESULTADOS 5% ROTULADOS
# 
# ST-ORIGINAL ST-LIMIAR  FLEXCON....
# BD1-FOLD1
# BD1-FOLD2
# ...
# BD1-FOLD10
# BD2-FOLD1
# BD2-FOLD2
# ...
# BD2-FOLD10
# ...
# ...ATÉ
# BD30-FOLD10


for (i in 1:4){ #É NECESSÁRIO COLOCAR OS NOMES DO SELF-TRAINING E DEPOIS DO CO-TRAINING
    switch (i,
      "1" = { #NB
        nome1 <- "co_training_naiveBayes_media_metodo_1_5.csv"
        nome2 <- "co_training_naiveBayes_media_metodo_2_5.csv"
        nome3 <- "co_training_naiveBayes_media_metodo_3_5.csv"
        nome4_soma <- "co_training_naiveBayes_media_metodo_4_5_soma.csv"
        nome4_voto <- "co_training_naiveBayes_media_metodo_4_5_voto.csv"
        nome5 <- "co_training_naiveBayes_media_metodo_5_5.csv"
        nome6 <- "co_training_naiveBayes_media_metodo_6_5.csv"
        nome7 <- "co_training_naiveBayes_media_metodo_7_5.csv"
        
      },
      "2" = { #AD
        nome1 <- "co_training_rpartXse_media_metodo_1_5.csv"
        nome2 <- "co_training_rpartXse_media_metodo_2_5.csv"
        nome3 <- "co_training_rpartXse_media_metodo_3_5.csv"
        nome4_soma <- "co_training_rpartXse_media_metodo_4_5_soma.csv"
        nome4_voto <- "co_training_rpartXse_media_metodo_4_5_voto.csv"
        nome5 <- "co_training_rpartXse_media_metodo_5_5.csv"
        nome6 <- "co_training_rpartXse_media_metodo_6_5.csv"
        nome7 <- "co_training_rpartXse_media_metodo_7_5.csv"
        
      },
      "3" = { #RIP
        nome1 <- "co_training_JRip_media_metodo_1_5.csv"
        nome2 <- "co_training_JRip_media_metodo_2_5.csv"
        nome3 <- "co_training_JRip_media_metodo_3_5.csv"
        nome4_soma <- "co_training_JRip_media_metodo_4_5_soma.csv"
        nome4_voto <- "co_training_JRip_media_metodo_4_5_voto.csv"
        nome5 <- "co_training_JRip_media_metodo_5_5.csv"
        nome6 <- "co_training_JRip_media_metodo_6_5.csv"
        nome7 <- "co_training_JRip_media_metodo_7_5.csv"
        
      },
      "4" = {#KNN
        nome1 <- "co_training_IBk_media_metodo_1_5.csv"
        nome2 <- "co_training_IBk_media_metodo_2_5.csv"
        nome3 <- "co_training_IBk_media_metodo_3_5.csv"
        nome4_soma <- "co_training_IBk_media_metodo_4_5_soma.csv"
        nome4_voto <- "co_training_IBk_media_metodo_4_5_voto.csv"
        nome5 <- "co_training_IBk_media_metodo_5_5.csv"
        nome6 <- "co_training_IBk_media_metodo_6_5.csv"
        nome7 <- "co_training_IBk_media_metodo_7_5.csv"
        
      }     
      
        
    )
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo_1_original_completo")
    result_met1_original <- read.csv(nome1)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo 2_limiar_completo")
    result_met2_lim_fixo <- read.csv(nome2)
    
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo 3_gradativo_completo")
    result_met3_gradativo <- read.csv(nome3)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo 4_flexcon\\soma")
    result_met4_flexcon_soma <- read.csv(nome4_soma)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo 4_flexcon\\voto")
    result_met4_flexcon_voto <- read.csv(nome4_voto)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo_5_flexcon_c1_soma")
    result_met5_flexcon_c1_soma <- read.csv(nome5)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo_6_flexcon_c1_voto")
    result_met6_flexcon_c1_voto <- read.csv(nome6)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo_7_flexcon_c2")
    result_met7_flexcon_c2 <- read.csv(nome7)

    for (j in 2:6){
      csv_j <- c(result_met1_original[j], result_met2_lim_fixo[j], result_met3_gradativo[j], result_met4_flexcon_soma[j], result_met4_flexcon_voto[j], result_met5_flexcon_c1_soma[j],result_met6_flexcon_c1_voto[j], result_met7_flexcon_c2[j])
      #colnames(csv_j) <- nome_colunas
      setwd("C:\\local_R\\projeto_karliane\\TESTE_ESTATISTICO") 
      nome_arquivo <- paste(c("resultado", classifiers[i], (j-1)*5, "_porc"), collapse = "_")
      nome_arquivo_csv <- paste(c(nome_arquivo, ".csv"), collapse = "_")
      # <- paste(c("resultado", classifiers[i], (j-1)*5, "_porc.csv"), collapse = "_")
      
      dados_gravar <- data.frame(csv_j)
      colnames(dados_gravar) <- nome_colunas
      write.csv(dados_gravar, nome_arquivo_csv, row.names = FALSE, col.names=TRUE)

      # RODAR O TESTE ESTATISTICO COM O CSV CRIADO
      
      #atribui os dados a uma matriz
      dados <- as.matrix(read.csv(nome_arquivo_csv))
      
      
      #roda o teste
      
      friedman <- friedman.test(dados)
      #roda pareado
      friedman_conover <- posthoc.friedman.conover.test(dados)
      friedman_nemenyi <- posthoc.friedman.nemenyi.test(dados)
      
      #write.csv(friedman$p.value, paste(c("resultado_friedman", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
      # write.csv(friedman_conover$p.value, paste(c("resultado_friedman_conover", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
      # write.csv(friedman_nemenyi$p.value, paste(c("resultado_friedman_nemenyi", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
      write.csv(friedman$p.value, paste("resultado_friedman", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
      write.csv(friedman_conover$p.value, paste("resultado_friedman_conover", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
      write.csv(friedman_nemenyi$p.value, paste("resultado_friedman_nemenyi", nome_arquivo, ".csv", sep = "_"), row.names = FALSE, col.names = TRUE)
    }
}


#outras op??es de teste pareado
# wilcox.test(dados)
# posthoc.durbin.test(dados)
# posthoc.friedman.nemenyi.test(dados)
