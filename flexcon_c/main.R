#' Função chega o SO utilizado e seta o diretório
#' This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/flexcon_c")
  } else {
    setwd("C:\\local_R\\projeto_karliane\\flexcon_c")
  }
}

args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0) {
  cl <- 1
} else if (length(args) > 1) {
  stop("Just one argument")
} else if ((as.integer(args) == F) || (is.na(as.integer(args))) || 
           (as.integer(args) > 4) || (as.integer(args) < 1)) {
  stop("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
       "\n3 - JRip\n4 - IBk")
} else {
  setWorkspace()
  source("functions.R")
  source("utils.R")
  source("crossValidation.R")
  initGlobalVariables()
  defines()
  medias_c1_s <- cleanVector(medias_c1_s)
  medias_c1_v <- cleanVector(medias_c1_v)
  medias_c2 <- cleanVector(medias_c2)
  cl <- 4
  param <- whichDB(classifiers[cl])
  ini_cr <- param$cr
  ini_bd <- param$bd
  for(i in ini_bd:31) { #Iris
    base_original <- getDatabase(i)
    k_NN <- attKValue(base_original)
    qtd_exem_por_classe <- ddply(base_original, ~class, summarise,
                                 distinct_orders = length(class))
    qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$distinct_orders) * 0.1)
    folds <- crossValidation(base_original, base_original$class)
    for (cr in ini_cr:8) { #2
      for(j in 1:5) { #1
        taxa <- j * 5 # 5%
        acc_c1_s <- cleanVector(acc_c1_s)
        acc_c1_v <- cleanVector(acc_c1_v)
        acc_c2 <- cleanVector(acc_c2)
        for (fold in 1:length(folds)) {
          base_teste <- base_original[folds[[fold]], ]
          base <- base_original[- folds[[fold]], ]
          treinamento <<- base_rotulada_treino <- base
          #sorteando os exemplos que ficarão rotulados inicialmente
          cat("\nBD:", i, "    CL:", cl, "    CR:", cr, "   TX:", j, "   FOLD:",
              fold)
          H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100),
                        mode = "stratified")
          ids_treino_rot <- H2$tr
          base <- newBase(base_rotulada_treino, ids_treino_rot)
          base_rotulados_ini <- base_rotulada_treino[ids_treino_rot, ]
          source('training.R')
        }
        medias_c1_s <- appendVectors(medias_c1_s, acc_c1_s)
        medias_c1_v <- appendVectors(medias_c1_v, acc_c1_v)
        medias_c2 <- appendVectors(medias_c2, acc_c2)
      }
    outputArchive(cr, as.character(classifiers[cl]), medias_c1_s, medias_c1_v,
                   medias_c2)
    medias_c1_s <- cleanVector(medias_c1_s)
    medias_c1_v <- cleanVector(medias_c1_v)
    medias_c2 <- cleanVector(medias_c2)
    }
    if(ini_cr != 2) {
      ini_cr = 2
    }
  }
}