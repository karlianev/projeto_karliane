#install.packages("PMCMR")
library("PMCMR")
#seta o diretorio local
classifiers <- c("naiveBayes", "rpartXse", "JRip", "IBk")


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

nome1 <- "self_training_naiveBayes_media_metodo_1_5.csv"
nome2 <- "self_training_naiveBayes_media_metodo_2_5.csv"
nome3 <- "self_training_naiveBayes_media_metodo_3_5.csv"
nome4_soma <- "self_training_naiveBayes_media_metodo_4_5_soma.csv"
nome4_voto <- "self_training_naiveBayes_media_metodo_4_5_voto.csv"
nome5 <- "self_training_naiveBayes_media_metodo_5_5.csv"
nome6 <- "self_training_naiveBayes_media_metodo_6_5.csv"
nome7 <- "self_training_naiveBayes_media_metodo_7_5.csv"

for (c in 1:4){
      
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo_1_original_completo")
    result_met1_original <- read.csv(nome1)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 2_limiar_completo")
    result_met2_lim_fixo <- read.csv(nome2)
    
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 3_gradativo_completo")
    result_met3_gradativo <- read.csv(nome3)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 4_flexcon\\soma")
    result_met4_flexcon_soma <- read.csv(nome4_soma)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo 4_flexcon\\voto")
    result_met4_flexcon_voto <- read.csv(nome4_voto)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo_5_flexcon_c1_soma")
    result_met5_flexcon_c1_soma <- read.csv(nome5)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo_6_flexcon_c1_voto")
    result_met6_flexcon_c1_voto <- read.csv(nome6)
    
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados self-training\\metodo_7_flexcon_c2")
    result_met7_flexcon_c2 <- read.csv(nome7)
    
    for (j in 2:6){
      csv_j <- c(result_met1_original[j], result_met2_lim_fixo[j], result_met3_gradativo[j], result_met4_flexcon_soma[j], result_met4_flexcon_voto[j], result_met5_flexcon_c1_soma[j],result_met6_flexcon_c1_voto[j], result_met7_flexcon_c2[j])
      
      setwd("C:\\local_R\\projeto_karliane\\TESTE_ESTATISTICO") 
      nome_arquivo <- paste(c("resultado", classifiers[c], (j-1)*5, "_porc.csv"), collapse = "_")
      write.csv(data.frame(csv_j), nome_arquivo , row.names = FALSE)

      # RODAR O TESTE ESTATISTICO COM O CSV CRIADO
      
      #atribui os dados a uma matriz
      dados <- as.matrix(read.csv(nome_arquivo))
      
      
      #roda o teste
      
      friedman <- friedman.test(dados)
      #roda pareado
      friedman_conover <- posthoc.friedman.conover.test(dados)
      friedman_nemenyi <- posthoc.friedman.nemenyi.test(dados)
      
      write.csv(friedman$p.value, paste(c("resultado_friedman", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
      write.csv(friedman_conover$p.value, paste(c("resultado_friedman_conover", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
      write.csv(friedman_nemenyi$p.value, paste(c("resultado_friedman_nemenyi", nome_arquivo, ".csv"), collapse = "_"), row.names = FALSE)
    }
}


#outras op??es de teste pareado
# wilcox.test(dados)
# posthoc.durbin.test(dados)
# posthoc.friedman.nemenyi.test(dados)
