
#' Função chega o SO utilizado e seta o diretório
#' This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if (mySystem[[1]] == "Linux") {
    setwd("~/R/karliane/projeto_karliane/self_training")
  } else {
    setwd("C:\\local_R\\projeto_karliane\\self_training")
  }
}

#ESTES COMANDOS SO FUNCIONAM SE FOR NO TERMINAL LINUX, NAO CONSEGUI BOTAR PRA FUNCIONAR NO RSTUDIO
# parametro = commandArgs(trailingOnly=TRUE)
# args = as.integer(parametro[1])
# method = as.integer(parametro[2])
# votacao = as.character(parametro[3])
# 
# if ((args == "-h") || (args == "--help")) {
#   cat("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
#        "\n3 - JRip\n4 - IBk")
# }else if ((as.integer(args) == F) || (is.na(as.integer(args))) ||
#            (as.integer(args) > 4) || (as.integer(args) < 1)) {
#   stop("The arg must be integer between 1-4!\n1 - NaiveBayes\n2 - rpartXse",
#        "\n3 - JRip\n4 - IBk")
# }#else {
# #     args <- args
# # }
# 
# if ((method == "-h") || (method == "--help")) {
#   cat("The method must be integer between 1-5!\n1 - original\n2 - original_felipe",
#       "\n3 - gradativo\n4 - FlexCon\n5 - FlexCon-C")
# }else if ((as.integer(method) == F) || (is.na(as.integer(method))) ||
#           (as.integer(method) > 5) || (as.integer(method) < 1)) {
#   stop("The method must be integer between 1-5!\n1 - original\n2 - original_felipe",
#        "\n3 - gradativo\n4 - FlexCon\n5 - FlexCon-C")
#  }#else {
# #     method <<- method
# # }
# 
# 
# if ((votacao == "-h") || (votacao == "--help")) {
#   cat("The votacao must be character T or F!\T - TRUE\F - FALSE")
# }else if ((as.character(method) != F) || (as.character(method) != T)||
#           (is.na(as.character(method)))) {
#   stop("The votacao must be character T or F")
# }#else {
# #     votacao <<- votacao
# # }
# 
#PARA RODAR NO RSTUDIO COMENTA A PARTE ACIMA E DESCOMENTA A DE BAIXO

  args <- 4 #classificador 1 = naive, 2=rpartxse, 3=ripper, 4=ibk
  method <<- 1 # 1 = co-training original (k=10%)  
               # 2 = co-training baseado no metodo de Felipe (k=limiar)
               # 3 = co-training gradativo (k=limiar que diminui 5% a cada iteracao)
               # 4 = co-training FlexCon SETAR A VARIAVEL VOTACAO
               # 5 = co-training FlexConc1s
               # 6 = co-training FlexConc1v
               # 7 = co-training FlexConc2
  votacao <<- T
  
  
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
  # todas_acc_co_v1 <- cleanVector(todas_acc_co_v1)
  # todas_acc_co_v2 <- cleanVector(todas_acc_co_v2)

  cl <- as.integer(args)
  ini_bd <- whichDB(join(c("self_training", classifiers[cl], "media")))
  
  for(i in ini_bd:30) { #bases de dados #Iris=1
    base_original <- getDatabase(i)
    k_NN <- attKValue(base_original)
    qtd_exem_por_classe <- ddply(base_original, ~class, summarise,
                                 distinct_orders = length(class))
    qtd_exem_menor_classe <<- trunc(min(qtd_exem_por_classe$distinct_orders) * 0.1)
    folds <- crossValidation(base_original, base_original$class)
    
    #No co-training era assim    
    # visao <- criar_visao(base_original)
    # dat1 <- visao[[1]]
    # dat2 <- visao[[2]]
    #No self-training ? assim
    dat1 <- base_original
    

    for (cr in 5:5) { #2 change rate
      for(j in 1:5) { #1 taxa de exemplos inicialmente rotulados
        taxa <- j * 5 # 5%
        acc_c1_s <- cleanVector(acc_c1_s)
        acc_c1_v <- cleanVector(acc_c1_v)
        acc_c2 <- cleanVector(acc_c2)
        acc_self <- cleanVector(acc_self) #acur?cia m?dia dos classificadores das duas visoes
        # acc_co_v1<- cleanVector(acc_co_v1) #acur?cia do classificador da visao 1
        # acc_co_v2<- cleanVector(acc_co_v2) #acur?cia do classificador da visao 2
        for (fold in 1:length(folds)) {
          base_teste1 <- dat1[folds[[fold]], ]
          #base_teste2 <- dat2[folds[[fold]], ]
          base1 <- dat1[- folds[[fold]], ]
          #base2 <- dat2[- folds[[fold]], ]
          
#acho que a variavel treinamento nao esta sendo usada!!!!          
          treinamento1 <- base1
          #treinamento2 <- base2
          #sorteando os exemplos que ficarão rotulados inicialmente
          cat("\nBD:", i, "    CL:", cl, "    CR:", cr, "   TX:", j, "   FOLD:",
              fold)
          H2 <- holdout(base1$class, ratio = (taxa / 100),
                        mode = "stratified")
          ids_treino_rot <- H2$tr
          base1 <- newBase(base1, ids_treino_rot)
          #base2 <- newBase(base2, ids_treino_rot)
          base_rotulados_ini1 <- base1[ids_treino_rot, ]
          #base_rotulados_ini2 <- base2[ids_treino_rot, ]
          # base_rotulados_ini <- cbind(base_rotulados_ini1[,-ncol(base_rotulados_ini1)], base_rotulados_ini2)
          # base_teste <- cbind(base_teste1[,-ncol(base_teste1)], base_teste2)
          
          source('training.R')
        }
        
        medias_c1_s <- appendVectors(medias_c1_s, acc_c1_s)
        medias_c1_v <- appendVectors(medias_c1_v, acc_c1_v)
        medias_c2 <- appendVectors(medias_c2, acc_c2)
        medias_self <- appendVectors(medias_self, acc_self)
        # todas_acc_co_v1 <- appendVectors(todas_acc_co_v1, acc_co_v1)
        # todas_acc_co_v2 <- appendVectors(todas_acc_co_v2, acc_co_v2)
      }
    
      data_arquivo_o <- data.frame(bd_g_o, tx_g_o, it_g_o, thrConf1_g_o, #thrConf2_g_o,
                                   nr_added_exs_g_o)#, acertou_g_o)
      outputArchive(cr, as.character(classifiers[cl]), nome_acc = "media", method=method, medias_c1_s, medias_c1_v,
                     medias_c2, medias_self) 
      # outputArchive(cr, as.character(classifiers[cl]), nome_acc = "visao1", method=method, medias_c1_s, medias_c1_v,
      #                medias_c2, todas_acc_co_v1) 
      # outputArchive(cr, as.character(classifiers[cl]), nome_acc = "visao2", method=method, medias_c1_s, medias_c1_v,
      #               medias_c2, todas_acc_co_v2) 
      
      writeArchive(paste(c("resultado", classifiers[cl],"metodo", method, "095.csv"), collapse = "_"), data_arquivo_o, row = F, col = F)
      
      
      
      # write.csv(data_arquivo_o, paste(c("resultado", classifiers[cl],"metodo", method, "095.csv"),
      #                                 collapse = "_"), row.names = FALSE)
      
      #write.csv2(base2,"base2.csv",row.names=T,col.names = T,sep = ";",dec = ",")
      
      # medias_c1_s <- cleanVector(medias_c1_s)
      # medias_c1_v <- cleanVector(medias_c1_v)
      # medias_c2 <- cleanVector(medias_c2)
      medias_self <- cleanVector(medias_self)
      # todas_acc_co_v1 <- cleanVector(todas_acc_co_v1)
      # todas_acc_co_v2 <- cleanVector(todas_acc_co_v2)
    }
  }
