setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/co_training")
    
  } else {
    # setwd("C:\\local_R\\projeto_karliane\\co_training\\metodo_1_original_completo")
    setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\resultados co-training\\metodo_2_limiar_completo")
  }
}
setWorkspace()

#atribui os dados a uma matriz
nome_arquivo <- "co_training_IBk_media_metodo1_5.CSV"
dados <- as.matrix(read.csv(nome_arquivo))

MatrizMedias = matrix(nrow = (nrow(dados)/10), ncol = ncol(dados))

pos <- 1
base <- 1
while (pos <= (nrow(dados)/10)) {
  for  (i in 2:6){
    soma <- 0
    media <- 0
    for (j in base:(base+9)){
      soma <- soma + as.double(dados[j, i])
    }
    media <- soma/10
    MatrizMedias[pos, i] <- media
  }
  MatrizMedias[pos,1] <- dados[base,1]
  pos <- pos + 1
  base <- base +10
}
write.csv(MatrizMedias, paste(c("media_fold", nome_arquivo), sep = "", collapse = "_", row.names(FALSE)))
