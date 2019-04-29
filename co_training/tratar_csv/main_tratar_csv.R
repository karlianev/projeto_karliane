#Este script ler o csv com a acuracia media dos dois classificadores para os 10 folds (co-training) e 
#faz a media, maximo e minimo gerando um novo csv onde cada base de dados terá apenas uma acurácia

setWorkspaceLocalR <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/co_training/tratar_csv")
    
  } else {
    setwd("C:\\local_R\\projeto_karliane\\co_training\\tratar_csv")
    
  }
}
setWorkspaceLocalR()
source("functions.R")
metodo <- 2 #1=original, 2=limiar, 3=gradativo 4=FlexCon 5=FlexCon-C1(s) 6=FlexCon-C1(v) 7=FlexCon-C2

if (metodo==1){
  nome_diretorio <- "metodo_1_original_completo"
}else if (metodo==2){
  nome_diretorio <- "metodo 2_limiar_completo"
}else if (metodo==3){
  nome_diretorio <- "metodo 3_gradativo_completo"
}else if (metodo==4){
  nome_diretorio <- "metodo 4_flexcon"
}else if (metodo ==5){
  nome_diretorio <- "metodo_5_flexcon_c1_soma"
}else if (metodo ==6){
  nome_diretorio <- "metodo_6_flexcon_c1_voto"
}else if (metodo ==7){
  nome_diretorio <- "metodo_7_flexcon_c2"
}


setWorkspace(nome_diretorio)
nome_arquivo <- "co_training_naiveBayes_media_metodo_2_5.CSV"
#é necessario alterar o csv acrescentando um nome de coluna
#atribui os dados a uma matriz
dados <- as.matrix(read.csv(nome_arquivo))

MatrizMedias = matrix(nrow = (nrow(dados)/10), ncol = ncol(dados))
MatrizMaximo = matrix(nrow = (nrow(dados)/10), ncol = ncol(dados))
MatrizMinimo = matrix(nrow = (nrow(dados)/10), ncol = ncol(dados))

pos <- 1
base <- 1
while (pos <= (nrow(dados)/10)) {
  for  (i in 2:6){
    
      MatrizMedias[pos, i] <- media(base, dados, i, pos)  
    
      MatrizMaximo[pos, i] <- maximo(base, dados, i, pos)  
    
      MatrizMinimo[pos, i] <- minimo(base, dados, i, pos)  
    
    
    # if (metodo==1){ #media
    #   MatrizMedias[pos, i] <- media(base, dados, i, pos)  
    # }else if (metodo==2){#maximo e minimo
    #   MatrizMedias[pos, i] <- maximo(base, dados, i, pos)  
    # }else if (metodo==3){#minimo
    #   MatrizMedias[pos, i] <- minimo(base, dados, i, pos)  
    # }#fim if
  

  }#fim for
  MatrizMedias[pos,1] <- dados[base,1]
  MatrizMaximo[pos,1] <- dados[base,1]
  MatrizMinimo[pos,1] <- dados[base,1]
  pos <- pos + 1
  base <- base +10
}#fim while
write.csv(MatrizMedias, paste(c("media_fold", nome_arquivo), sep = "", collapse = "_", row.names(FALSE)))
write.csv(MatrizMaximo, paste(c("maximo_fold", nome_arquivo), sep = "", collapse = "_", row.names(FALSE)))
write.csv(MatrizMinimo, paste(c("minimo_fold", nome_arquivo), sep = "", collapse = "_", row.names(FALSE)))
