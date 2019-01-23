setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/co_training")
  } else {
    setwd("C:\\local_R\\projeto_karliane\\co_training")
  }
}
setWorkspace()

#atribui os dados a uma matriz
dados <- as.matrix(read.csv("co_trainingnaiveBayes_5_k_10_porcento.CSV"))

MatrizMedias = matrix(nrow = (nrow(dados)/10), ncol = ncol(dados))

pos <- 1
base <- 1
while (pos <= (nrow(dados)/10)) {
  for  (i in 1:5){
    soma <- 0
    media <- 0
    for (j in base:(base+9)){
      soma <- soma + as.integer(dados[j, i])
    }
    media <- soma/10
    MatrizMedias[pos, i] <- media
  }
  pos <- pos + 1
  base <- base +9
}
  
write.csv(MatrizMedias, "resultado")
