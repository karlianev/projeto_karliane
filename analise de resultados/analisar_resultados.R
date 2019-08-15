appendVectors2 <- function(v1, v2) {
  return (c(v1, v2))
}

appendVectors4 <- function(v1, v2, v3, v4) {
  return (c(v1, v2, v3, v4))
}

appendVectors6 <- function(v1, v2, v3, v4, v5, v6) {
  return (c(v1, v2, v3, v4, v5, v6))
}

appendDataFrame <- function(v1, v2) {
  return (rbind(v1, v2))
}

appendDataFrameColuna <- function(v1, v2) {
  return (cbind(v1, v2))
}

cleanVector <- function(x) {
  x <- c()
  return (x)
}

setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\metodo 2_limiar_completo")
result <- read.csv("resultado_rpartXse_metodo_2_095.csv")
nome_arq_acc <- "co_training_rpartXse_media_metodo_2_5.csv"
acc <- read.csv(nome_arq_acc)
nome_csv_saida <- paste(c("analise",nome_arq_acc, ".csv"),collapse = "_")

qtd_rot <- 0
dados_csv <- cleanVector(dados_csv)
dados <- cleanVector(dados)
base <- cleanVector(base)
perc <- cleanVector(perc)
it <- cleanVector(it)
qtd_rot <- cleanVector(qtd_rot)
base <- "karliane"

for (i in 1:nrow(result)){
  
  #se mudar a base de dados
  if (as.character(base)!=as.character(result[i,1])){
    if (as.character(base) !="karliane"){
      dados <- appendVectors4 (as.character(base), perc,it,qtd_rot)
      dados_csv <- appendDataFrame(dados_csv, dados)
    }
    base <- as.character(result[i,1])
    perc <- result[i,2]
    it <- result[i,3]
    qtd_rot <- result[i,6]
    cat("\n base:", base)
  }else 
    # se NÃO mudar a base e mudar o percentual
    if (perc!=result[i,2]){
      dados <- appendVectors4 (as.character(base), perc,it,qtd_rot)
      dados_csv <- appendDataFrame(dados_csv, dados)
      perc <- result[i,2]
      it <- result[i,3]
      qtd_rot <- result[i,6]
    }else 
      #se NÃO mudar a base, NÃO mudar o percentual e for a primeira iteração  
      if (result[i,3]==1){
        dados <- appendVectors4(as.character(base), perc,it,qtd_rot)
        dados_csv <- appendDataFrame(dados_csv, dados)
        it <- result[i,3]
        qtd_rot <- result[i,6]
      }else
        qtd_rot <- qtd_rot + result[i,6]
        it <- result[i,3]
        if (i==nrow(result)){
          dados <- appendVectors4 (as.character(base), perc,it,qtd_rot)
          dados_csv <- appendDataFrame(dados_csv, dados)
        }
}

dados_acc <- cleanVector(dados_acc)
acc5 <- cleanVector(acc5)
acc10 <- cleanVector(acc10)
acc15 <- cleanVector(acc15)
acc20 <- cleanVector(acc20)
acc25 <- cleanVector(acc25)

base_anterior <- as.character(acc[1,1])
  for (j in 1:nrow(acc)){
    base <- as.character(acc[j,1])
    if (j!=1) {
      base_anterior <- as.character(acc[j-1,1])
    }
    
    if (base == base_anterior){
      acc5 <- appendVectors2(acc5, acc[j,2])
      acc10 <- appendVectors2(acc10,acc[j,3])
      acc15 <- appendVectors2(acc15,acc[j,4])
      acc20 <- appendVectors2(acc20,acc[j,5])
      acc25 <- appendVectors2(acc25,acc[j,6])
    }else{
      dados_acc <- appendVectors6(dados_acc,acc5,acc10,acc15,acc20,acc25)
      acc5 <- acc[j,2]
      acc10 <- acc[j,3]
      acc15 <- acc[j,4]
      acc20 <- acc[j,5]
      acc25 <- acc[j,6]
    }
  }
dados_acc <- appendVectors6(dados_acc,acc5,acc10,acc15,acc20,acc25)
dados_csv_gravar <- appendDataFrameColuna(dados_csv,dados_acc)
write.csv2(dados_csv_gravar,nome_csv_saida,row.names=F,col.names = T,sep = ";",dec = ",")


