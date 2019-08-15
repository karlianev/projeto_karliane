setWorkspace <- function(diretorio) {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    caminho <- paste(c("~/R/karliane/projeto_karliane/co_training\\", diretorio), sep="", collapse = "", row.names(FALSE))
    setwd(caminho)
  } else {
    caminho <- paste(c("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\resultados co-training\\", diretorio), sep="", collapse = "", row.names(FALSE))
    setwd(caminho)
  }
}


media <- function(base, dados, i, pos){
  soma <- 0
  media <- 0
  for (j in base:(base+9)){
    soma <- soma + as.double(dados[j, i])
  }
  media <- soma/10
  return(media)
}

maximo <- function(base, dados, i, pos){
  maximo <- 0
  for (j in base:(base+9)){
    if (maximo < as.double(dados[j, i])){
      maximo <- as.double(dados[j, i])  
    }
  }
  return(maximo)
}

minimo <- function(base, dados, i, pos){
  minimo <- as.double(dados[base, i])
  for (j in base:(base+9)){
    if (minimo > as.double(dados[j, i])){
      minimo <- as.double(dados[j, i])  
    }
  }
  return(minimo)
}