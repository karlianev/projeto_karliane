#' Função chega o SO utilizado e seta o diretório
#' This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/co_training")
  } else {
    setwd("C:\\local_R\\projeto_karliane\\co_training")
  }
}


# args = commandArgs(trailingOnly=TRUE)
# if ((args == "-h") || (args == "--help")) {
#   cat("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
#        "\n3 - JRip\n4 - IBk")
# } else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
#            (as.integer(args) > 4) || (as.integer(args) < 1)) {
#   stop("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
#        "\n3 - JRip\n4 - IBk")
# } else {
  args <- 1 #classificador 1 = naive, 2=rpartxse, 3=ripper, 4=ibk
  
  setWorkspace()
  source("functions.R")
  source("utils.R")
  source("crossValidation.R")
  initGlobalVariables()
  defines()
  medias_c1_s <- cleanVector(medias_c1_s)
  medias_c1_v <- cleanVector(medias_c1_v)
  medias_c2 <- cleanVector(medias_c2)
  medias_self <- cleanVector(medias_self)
  cl <- as.integer(args)
  ini_bd <- whichDB(join(c("co_training", classifiers[cl])))
  for(i in 1:1){#ini_bd:31) { #bases de dados #Iris=1
    base_original <- getDatabase(i)
    k_NN <- attKValue(base_original)
    qtd_exem_por_classe <- ddply(base_original, ~class, summarise,
                                 distinct_orders = length(class))
    qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$distinct_orders) * 0.1)
    folds <- crossValidation(base_original, base_original$class)
    
    visao <- criar_visao(base_original)
    data1 <- visao[[1]]
    data2 <- visao[[2]]
    

    for (cr in 5:5) { #2 change rate
      for(j in 1:5) { #1 taxa de exemplos inicialmente rotulados
        taxa <- j * 5 # 5%
        acc_c1_s <- cleanVector(acc_c1_s)
        acc_c1_v <- cleanVector(acc_c1_v)
        acc_c2 <- cleanVector(acc_c2)
        acc_self <- cleanVector(acc_self)
        for (fold in 1:length(folds)) {
          base_teste1 <- data1[folds[[fold]], ]
          base_teste2 <- data2[folds[[fold]], ]
          base1 <- data1[- folds[[fold]], ]
          base2 <- data2[- folds[[fold]], ]
          
#acho que a variavel treinamento nao esta sendo usada!!!!          
          treinamento1 <- base1
          treinamento2 <- base2
          #sorteando os exemplos que ficarão rotulados inicialmente
          cat("\nBD:", i, "    CL:", cl, "    CR:", cr, "   TX:", j, "   FOLD:",
              fold)
          H2 <- holdout(base1$class, ratio = (taxa / 100),
                        mode = "stratified")
          ids_treino_rot <- H2$tr
          base1 <- newBase(base1, ids_treino_rot)
          base2 <- newBase(base2, ids_treino_rot)
          base_rotulados_ini1 <- base1[ids_treino_rot, ]
          base_rotulados_ini2 <- base2[ids_treino_rot, ]
          # base_rotulados_ini <- cbind(base_rotulados_ini1[,-ncol(base_rotulados_ini1)], base_rotulados_ini2)
          # base_teste <- cbind(base_teste1[,-ncol(base_teste1)], base_teste2)
          
          source('training.R')
        }
        # medias_c1_s <- appendVectors(medias_c1_s, acc_c1_s)
        # medias_c1_v <- appendVectors(medias_c1_v, acc_c1_v)
        # medias_c2 <- appendVectors(medias_c2, acc_c2)
        medias_self <- appendVectors(medias_self, acc_self)
      }
    data_arquivo_o <- data.frame(bd_g_o, tx_g_o, it_g_o, thrConf_g_o,
                                 nr_added_exs_g_o, acertou_g_o)
    outputArchive(cr, as.character(classifiers[cl]), medias_c1_s, medias_c1_v,
                   medias_c2, medias_self)
    write.csv(data_arquivo_o, paste(c("resultado", classifiers[cl], "095.csv"),
                                    collapse = "_"), row.names = FALSE)
    # medias_c1_s <- cleanVector(medias_c1_s)
    # medias_c1_v <- cleanVector(medias_c1_v)
    # medias_c2 <- cleanVector(medias_c2)
    medias_self <- cleanVector(medias_self)
    }
    # if(ini_cr != 2) {
    #   ini_cr = 2
    # }
  }
# }
