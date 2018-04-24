


func <- function(m, d){ #NB
  p <- predict(m, d, type = "raw") #col2 armazena a confian?a em cada classe predita pelo classificador (ex: classe 1 = 0.8, classe2 = 0.1, classe 3= 0.1)
  predicao <<- data.frame(p)
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
}

f <- function(m,d) { #AD
  p <- predict(m,d,type='prob') #predicao dos dados (d) de acordo com o modelo (m)
  predicao <<- data.frame(p)
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2)
}

f2 <- function(m,d) { #JRip e KNN
  p <- predict(m,d,type='probability') # l ? uma matriz com a confian?a da predi??o de cada exemplo
  predicao <<- data.frame(p)
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2) #um data frame com 2 colunas: 1) a predi??o de cada exemplo; 2) a classe predita para cada exemplo
}


#funcao criada a partir da funcao selfTrainOriginal
#implementação que usa a variavel COMBINAR para combinar ou não a saída dos classificadores.
coTrainingOriginal <- function (form, data, learner, predFunc, thrConf = 0.9, maxIts = 10, 
                               percFull = 1, verbose = F, combinar = T) 
{
  #roda esses comandos somente quando não chamar a funcao
#  source('C:/local_R/projeto_karliane/co_training/scripts_comuns/configuracoes_co_training.R')
#criar as funções do predfunc
  # setwd("C:\\local_R\\projeto_karliane\\bases")
  # base_original <- read.arff("bupa.arff");
  # bd_nome <- "bupa"
  # taxa <- 5
  # classe <- "class"
  # setwd("C:\\local_R\\projeto_karliane")
  # source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R') 
  # data <- base_treino_self_training
  # form <- as.formula(paste(classe,'~', '.'))
  # learner <- learner("naiveBayes", list(4))
  # predFunc <- 'func'
  # thrConf <- 0.9
  #rodar func, f1 e f2
  #--- até aqui

  N <- NROW(data)  

  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  col <- (ncol(data)-1)/2 #PENSAR COMO FAZER SE O NUMERO DE COLUNAS FOR IMPAR.
  xl <- data[,1:ncol(data)-1] #a base dados iris sem os rotulos
  yl <- data[-(1:ncol(data)-1)] #rotulos da base iris
  view <- partition.matrix(xl, rowsep = nrow(data), colsep = c(col,col))
  data1 <- data.frame(view$`1`$`1`,yl)
  data2 <- data.frame(view$`1`$`2`,yl)
  

  #aqui começa o treinamento igual ao self-training
  it <- 0
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  repeat {
    # acertou <- 0
    it <- it + 1
    model1 <- runLearner(learner, form, data1[sup1, ])
    model2 <- runLearner(learner, form, data2[sup2, ])
    probPreds1 <- do.call(predFunc, list(model1, data1[-sup1,]))
    probPreds2 <- do.call(predFunc, list(model2, data2[-sup2,]))
    if (combinar){
      new1 <- which(as.character(probPreds1[, 1])==as.character(probPreds2[, 1]))  #DANDO PROBLEMA QDO NÃO ROTULA NENHUM EXEMPLO PARA DETERMINADA CLASSE
      new2 <- which(as.character(probPreds1[, 1])==as.character(probPreds2[, 1]))  #EM OUTRAS PALAVRAS, NEW1 E NEW2 NÃO TEM AS MESMAS CLASSES
      produto_confianca <- probPreds1[, 2]*probPreds2[, 2]
      new <- which(produto_confianca > thrConf)
    }else{  
      new1 <- which(probPreds2[, 2] > thrConf) #adiciona a visao 1 os exemplos que a visao 2 rotulou
      new2 <- which(probPreds1[, 2] > thrConf) #adiciona a visao 2 os exemplos que a visao 1 rotulou
      
    }  
    
    if (verbose) {
      #cat("IT.", it, "\t nr. added exs. =", length(new), 
      #   "\n")
      if (combinar){
        cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     
      }else{
        cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. v1 =',length(new1), '\t nr. added exs. v2 =',length(new2),'\n')       
      }
      
      it_g_o <<-c(it_g_o,it)
      bd_g_o <<-c(bd_g_o,bd_nome)
      thrConf_g_o <<-c(thrConf_g_o,thrConf)
      if (combinar){
        nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
      }else{
        nr_added_exs_g_o_v1 <<-c(nr_added_exs_g_o_v1,length(new1))
        nr_added_exs_g_o_v2 <<-c(nr_added_exs_g_o_v2,length(new2))
      }  
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if (length(new)) {

      if (combinar){
        data1[(1:N)[-sup1][new], as.character(form[[2]])] <- probPreds1[new, 1]
        data2[(1:N)[-sup2][new], as.character(form[[2]])] <- probPreds2[new, 1]
        sup1 <- c(sup1, (1:N)[-sup1][new])
        sup2 <- c(sup2, (1:N)[-sup2][new])
        
      }else{
        data1[(1:N)[-sup1][new1], as.character(form[[2]])] <- probPreds1[new1, 1]
        data2[(1:N)[-sup2][new2], as.character(form[[2]])] <- probPreds2[new2, 1]
        sup1 <- c(sup1, (1:N)[-sup1][new1])
        sup2 <- c(sup2, (1:N)[-sup2][new2])
        
      }
      # acertou <- 0
      # acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      # tam_acerto <- NROW(acerto)
      # for (w in 1:tam_acerto){
      #   if (acerto[w] == TRUE)
      #     acertou <- acertou + 1
      # }
      
      # acertou_g_o <<- c(acertou_g_o, acertou)
    }
    else{
      # acertou <- 0
      # acertou_g_o <<- c(acertou_g_o, acertou)
      break
    }
    if (it == maxIts || length(sup1)/N >= percFull || length(sup2)/N >= percFull) 
      break
  }

  model <- list(model1,model2)
  return(model)
}

# #funcao criada a partir da funcao selfTrainOriginal
# #implementação que NÃO COMBINA a saída dos classificadores.
# coTrainingOriginal <- function (form, data, learner, predFunc, thrConf = 0.9, maxIts = 10, 
#                                 percFull = 1, verbose = F) 
# {
#   #roda esses comandos somente quando não chamar a funcao
#   source('C:/local_R/projeto_karliane/co_training/scripts_comuns/configuracoes_co_training.R')
#   setwd("C:\\local_R\\projeto_karliane\\bases")
#   base_original <- read.arff("bupa.arff");
#   bd_nome <- "bupa"
#   taxa <- 5
#   classe <- "class"
#   setwd("C:\\local_R\\projeto_karliane")
#   source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R') 
#   data <- base_treino_self_training
#   form <- as.formula(paste(classe,'~', '.'))
#   learner <- learner("naiveBayes", list(4))
#   predFunc <- 'func'
#   thrConf <- 0.9
#   #rodar func, f1 e f2
#   #--- até aqui
#   N <- NROW(data)  
#   
#   #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
#   col <- (ncol(data)-1)/2 #PENSAR COMO FAZER SE O NUMERO DE COLUNAS FOR IMPAR.
#   xl <- data[,1:ncol(data)-1] #a base dados iris sem os rotulos
#   yl <- data[-(1:ncol(data)-1)] #rotulos da base iris
#   view <- partition.matrix(xl, rowsep = nrow(data), colsep = c(col,col))
#   data1 <- data.frame(view$`1`$`1`,yl)
#   data2 <- data.frame(view$`1`$`2`,yl)
#   
#   
#   #aqui começa o treinamento igual ao self-training
#   it <- 0
#   sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
#   sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
#   repeat {
#     # acertou <- 0
#     it <- it + 1
#     model1 <- runLearner(learner, form, data1[sup1, ])
#     model2 <- runLearner(learner, form, data2[sup2, ])
#     probPreds1 <- do.call(predFunc, list(model1, data1[-sup1,]))
#     probPreds2 <- do.call(predFunc, list(model2, data2[-sup2,]))
#     # new1 <- which(probPreds1[, 1]==probPreds2[, 1])
#     # new2 <- which(probPreds1[, 1]==probPreds2[, 1])
#     # produto_confianca <- probPreds1[, 2]*probPreds2[, 2]
#     # new <- which(produto_confianca > thrConf)
#      new1 <- which(probPreds2[, 2] > thrConf)
#      new2 <- which(probPreds1[, 2] > thrConf)
#     if (verbose) {
#       #cat("IT.", it, "\t nr. added exs. =", length(new), 
#       #   "\n")
#       cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     
#       it_g_o <<-c(it_g_o,it)
#       bd_g_o <<-c(bd_g_o,bd_nome)
#       thrConf_g_o <<-c(thrConf_g_o,thrConf)
#       nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
#       tx_g_o <<- c(tx_g_o, taxa)
#     }
#     if (length(new)) {
#       data1[(1:N)[-sup1][new1], as.character(form[[2]])] <- probPreds1[new1, 1]
#       data2[(1:N)[-sup2][new2], as.character(form[[2]])] <- probPreds2[new2, 1]
#       
#       # acertou <- 0
#       # acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
#       # tam_acerto <- NROW(acerto)
#       # for (w in 1:tam_acerto){
#       #   if (acerto[w] == TRUE)
#       #     acertou <- acertou + 1
#       # }
#       
#       sup1 <- c(sup1, (1:N)[-sup1][new1])
#       sup2 <- c(sup2, (1:N)[-sup2][new2])
#       # acertou_g_o <<- c(acertou_g_o, acertou)
#     }
#     else{
#       # acertou <- 0
#       # acertou_g_o <<- c(acertou_g_o, acertou)
#       break
#     }
#     
#     
#     
#     if (it == maxIts || length(sup)/N >= percFull) 
#       break
#   }
#   #DEFINIR O RETORNO, ACHO Q PRECISA RETORNAR O 1 E O 2
#   
#   return(model)
# }

# sslSelfTrain()
# #xl - os dados cujos rótulos SÃO conhecidos
# #yl - o rótulo dos exemplos rotulados
# #xu - os dados cujos rótulos NÃO SÃO conhecidos
# 
# sslCoTrain_Original <- function (xl, yl, xu, method1 = "nb", method2 = "nb", nrounds1, 
#                                  nrounds2, portion = 0.5, n = 10, seed = 0, ...) 
# {
#   yu <- NULL
#   num.class <- length(unique(yl)) + 1
#   while (dim(xu)[1] != 0) {
#     yl <- as.factor(yl)
#     set.seed(seed)
#     seq <- createDataPartition(y = yl, list = FALSE, p = portion) #seleciona os exemplos para criar as partições 
#     x1 <- xl[seq, ] #exemplos da partição 1
#     y1 <- yl[seq]   #rotulos da partição 1
#     x2 <- xl[-seq, ]#exemplos da partição 2
#     y2 <- yl[-seq]  #rotulos da partição 2
#     num <- min(dim(xu)[1], floor(n/2)) #não sei quem é n, nao e o 0.5 que esta sendo passado por parametro. Desconfio que essa linha indica a quantidade de exemplos que serao incluidos no conjunto dos rotulados
#     xd <- xu[1:num, ] # dados não rotulados de 1 até num
#     xu <- xu[-(1:num), ] # dados não rotulados EXCETO de 1 até num
#     if (method1 == "xgb") { #faz a pedicao caso o metodo seja xgb
#       dtrain <- xgb.DMatrix(data = as.matrix(x1), label = y1) #indica os dados para treinamento, ou seja, os rotulados da particao 1
#       h1 <- xgb.train(data = dtrain, nrounds = nrounds1, 
#                       num_class = num.class, objective = "multi:softmax", 
#                       ...) # gera o modelo para treinamento a partir dos dados rotulados da particao 1
#       pred <- predict(h1, as.matrix(xd)) #faz a predicao de acordo com o modelo gerado na linha anterior
#       pred <- round(pred) #arredonda a predicao, nao sei pra que
#     }
#     else { #faz a predicao para qualquer metodo que nao seja o xgb
#       h1 <- train(x1, y1, method = method1, ...) #cria o modelo a partir dos dados rotulados
#       pred <- predict(h1, xd) #faz a predicao
#     }
#     yu <- c(yu, pred) # rotulos preditos
#     xl <- rbind(xl, xd) #junta ao conjunto dos rotulados os exemplos nao rotulados armazenados em xd
#     yl <- c(yl, pred) #armazena o rotulo dos exemplos que nao estavam rotulados e agora fazem parte dos rotulados
#     num <- min(dim(xu)[1], n - floor(n/2))
#     if (num > 0) {
#       xd <- xu[1:num, ] #dados nao rotulados de 1 ate num
#       xu <- xu[-(1:num), ] #dados nao rotulados exceto de 1 ate num
#       if (method2 == "xgb") {
#         dtrain <- xgb.DMatrix(data = as.matrix(x2), 
#                               label = y2) #dados de treinamento
#         h2 <- xgb.train(data = dtrain, nrounds = nrounds2, 
#                         num_class = num.class, objective = "multi:softmax", 
#                         ...) #cria o modelo de acordo com os dados rotulados
#         pred <- predict(h2, as.matrix(xd)) #predicao
#         pred <- round(pred) #arredonda a predicao, nao sei pra que
#       }
#       else {
#         h2 <- train(x2, y2, method = method2, ...) #cria o modelo a partir dos dados rotulados
#         pred <- predict(h2, xd) #faz a predicao
#       }
#       yu <- c(yu, pred)
#       xl <- rbind(xl, xd)
#       yl <- c(yl, pred)
#     }
#   }
#   return(yu)
# }
