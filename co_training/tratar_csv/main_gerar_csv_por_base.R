#Este script ler o csv com a acuracia media dos dois classificadores para os 10 folds (co-training) e 
#faz a media, maximo e minimo gerando um novo csv onde cada base de dados ter? apenas uma acur?cia

setWorkspaceLocalR <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/self_training/tratar_csv")
    
  } else {
    setwd("C:\\local_R\\projeto_karliane\\self_training\\tratar_csv")
    
  }
}
setWorkspaceLocalR()
source("functions.R")



for (base in 1:30) {
  dados <- dados_classificador <- c()
  for (w in 1:8){
    metodo <- w #1=original, 2=limiar, 3=gradativo 4=FlexCon 5=FlexCon-C1(s) 6=FlexCon-C1(v) 7=FlexCon-C2
    
    if (metodo==1){
      nome_diretorio <- "metodo_1_original_completo"
    }else if (metodo==2){
      nome_diretorio <- "metodo 2_limiar_completo"
    }else if (metodo==3){
      nome_diretorio <- "metodo 3_gradativo_completo"
    }else if (metodo==4){
      nome_diretorio <- "metodo 4_flexcon\\soma"
    }else if (metodo==5){
      nome_diretorio <- "metodo 4_flexcon\\voto"
      w <- 4
    }else if (metodo ==6){
      nome_diretorio <- "metodo_5_flexcon_c1_soma"
      w <- 5
    }else if (metodo ==7){
      nome_diretorio <- "metodo_6_flexcon_c1_voto"
      w <- 6
    }else if (metodo ==8){
      nome_diretorio <- "metodo_7_flexcon_c2"
      w <- 7
    }
    
    setWorkspace(nome_diretorio)
    
    nome_arquivo_NB <- paste("self_training_naiveBayes_media_metodo_", w, "_5.csv", sep = "")
    nome_arquivo_AD <- paste("self_training_rpartXse_media_metodo_", w, "_5.csv", sep = "")
    nome_arquivo_RIP <- paste("self_training_JRip_media_metodo_", w, "_5.csv", sep = "")
    nome_arquivo_KNN <- paste("self_training_IBk_media_metodo_", w, "_5.csv", sep = "")
  
    if (metodo >= 5) {
        w <- w + 1
    }
    
    index <- (base * 10 - 9):(base * 10)
    
    dados_NB <- as.matrix(read.csv(nome_arquivo_NB))
    dados_AD <- as.matrix(read.csv(nome_arquivo_AD))
    dados_RIP <- as.matrix(read.csv(nome_arquivo_RIP))
    dados_KNN <- as.matrix(read.csv(nome_arquivo_KNN))
    
    dados_classificador <- c(dados_classificador, dados_NB[index, 6], dados_AD[index, 6],
                             dados_RIP[index, 6], dados_KNN[index, 6])
  }
  MatrizMedias = matrix(dados_classificador, nrow = 40, ncol = 8)
  nomes <- unique(dados_NB[,1])  
  write.csv(MatrizMedias, paste("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\teste estatistico jhoseph\\", nomes[base], ".csv", sep = ""),
            row.names = FALSE, col.names = FALSE)
}


#gravando arquivo com as acurácias médias por percentual inicialmente rotulado
setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\teste estatistico jhoseph")
nomes <- sort(nomes)
medias <- matrix(rep(0,30), ncol = 8, nrow = 30,
                 dimnames = list(nomes, c("original", "limiar_fixo", "flexconG", "flexcon(s)",
                                          "flexcon(v)", "flexcon1(s)", "flexconc1(v)", "flexconc2")))
for (i in 1:30){ 
  med_parc <- c()
  nomeArquivo <- paste(nomes[i], ".csv", sep = "")
  arquivo <- as.matrix(read.csv(nomeArquivo))
  for (j in 1:8){
    med_parc <- c(med_parc, mean(arquivo[,j]))
  }
  medias[i,] <- med_parc
}
write.table(medias, "media_por_metodo_5_porc.csv", row.names = FALSE)
