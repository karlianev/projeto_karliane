


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

#funcao que cria duas visoes para serem usadas no treinamento do co-training
criar_visao <- function(dados){
  col <- (ncol(dados)-1)/2
  xl <- dados[,1:ncol(dados)-1] #a base dados sem os rotulos
  yl <- dados[-(1:ncol(dados)-1)] #rotulos da base 
  view <- partition.matrix(xl, rowsep = nrow(dados), colsep = c(col,col))
  data1 <- data.frame(view$`1`$`1`,yl)
  data2 <- data.frame(view$`1`$`2`,yl)
  visoes <- list(data1,data2)
  return(visoes)
}

#Co-Training original
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
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]

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
      produto_confianca <- probPreds1[, 2]*probPreds2[, 2]
      new <- which((produto_confianca > thrConf) & (as.character(probPreds1[, 1])==as.character(probPreds2[, 1])))
    }else{  
      new <- which((probPreds2[, 2] > thrConf)|| (probPreds1[, 2] > thrConf)) #adiciona ao new os exemplos que as visoes 1 e 2 rotularam
    }  
    
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     
      it_g_o <<-c(it_g_o,it)
      bd_g_o <<-c(bd_g_o,bd_nome)
      thrConf_g_o <<-c(thrConf_g_o,thrConf)
      nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
      tx_g_o <<- c(tx_g_o, taxa)
      metodo_g_o <<- c(metodo_g_o, metodo)
    }
    if (length(new)) {
      data1[(1:N)[-sup1][new], as.character(form[[2]])] <- probPreds1[new, 1]
      data2[(1:N)[-sup2][new], as.character(form[[2]])] <- probPreds2[new, 1]
      sup1 <- c(sup1, (1:N)[-sup1][new])
      sup2 <- c(sup2, (1:N)[-sup2][new])
      
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

#FlexCon-G
#fun??o self-training diminuindo a taxa de confian?a para inclus?o em 5 pontos percentuais a cada itera??o
coTrainingGradativo <- function(form,data,
                                   learner,
                                   predFunc,
                                   thrConf=0.9,
                                   maxIts=10,percFull=1,
                                   verbose=F,gradativo=0.05, combinar=T){
  
  
  data
  N <- NROW(data)
  
  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]
  
  it <- 0
  qtd_Exemplos_Rot <- 0
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  repeat {
    acertou <- 0
    it <- it+1
    
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      thrConf <- (thrConf - gradativo)
      if (thrConf <= 0.0) thrConf <- (thrConf + gradativo)
    }
    # soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model1 <- runLearner(learner, form, data1[sup1, ])
    model2 <- runLearner(learner, form, data2[sup2, ])
    probPreds1 <- do.call(predFunc, list(model1, data1[-sup1,]))
    probPreds2 <- do.call(predFunc, list(model2, data2[-sup2,]))
    if (combinar){
      produto_confianca <- probPreds1[, 2]*probPreds2[, 2]
      new <- which((produto_confianca >= thrConf) & (as.character(probPreds1[, 1])==as.character(probPreds2[, 1])))
    }else{  
      new <- which((probPreds2[, 2] > thrConf)|| (probPreds1[, 2] > thrConf)) #adiciona ao new os exemplos que as visoes 1 e 2 rotularam
    }  

    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     

      ##guardando nas variaveis 
      it_g_o <<-c(it_g_o,it)
      bd_g_o <<-c(bd_g_o,bd_nome)
      thrConf_g_o <<-c(thrConf_g_o,thrConf)
      nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
      tx_g_o <<- c(tx_g_o, taxa)
      grad_g<<-c(grad_g,grad)
      metodo_g_o <<-c(metodo_g_o, num_metodo)
    }
    
    if (length(new)) {
      data1[(1:N)[-sup1][new], as.character(form[[2]])] <- probPreds1[new, 1]
      data2[(1:N)[-sup2][new], as.character(form[[2]])] <- probPreds2[new, 1]
      sup1 <- c(sup1, (1:N)[-sup1][new])
      sup2 <- c(sup2, (1:N)[-sup2][new])

      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup1][new],as.character(form[[2]])])
      # totalrot <- totalrot + qtd_Exemplos_Rot
      
      # acertou <- 0
      # acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      # tam_acerto <- NROW(acerto)
      # for (w in 1:tam_acerto){
      #   if (acerto[w] == TRUE)
      #     acertou <- acertou + 1
      # }
      # 
      
    }
    
      # acertou_g_gra <<- c(acertou_g_gra, acertou)
    if(length(new)==0){
      #TEMOS PROBLEMA AQUI PQ PODE SER QUE A CONFIANCA MAXIMA SEJA DIVERGENTE QUANTO AO ROTULO
      #POR ISSO NÃO INCLUI NINGUÉM NOVAMENTE E ENTRA EM LOOP ATÉ A IT=100
      #UMA POSSIVEL SOLUCAO É PEGAR O MAX CUJO ROTULO SEJA CONVERGENTE, BASTA APRENDER COMO IMPLEMENTAR.
      #!!!!!!!!!!!!!!!!NÃO ESTA DANDO CERTO, CONTINUA SEM ROTULAR NINGUÉM!!!!!!!!!!!!!!!!!!!!!!!!!!
      baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !(produto_confianca > thrConf))
      if (length(baixa_conf)==0){
        break
      }else{
        thrConf<-max(produto_confianca[baixa_conf])  
      }
      
    }
    
    if (it == maxIts || length(sup1)/N >= percFull || length(sup2)/N >= percFull) 
      break  
    

  } #fim do repeat
  model <- list(model1,model2)
  return(model)  
} #fim da funcao

#funcao co-training modificado (flexCon), usa uma formula para calcular a nova taxa de confianca.
#inclui no conjunto dos rotulados os exemplos que possuem mesmo rotulo e taxa de conf. >= thrconf
#caso nao exista nenhum exemplo com essa caracteristica, serao incluidos os exemplos que possuem o mesmo
#rotulo e uma das duas confiancas >= thrConf. Se ainda assim nao existir nenhum exemplo, serao
#incluidos os exemplos cujos rotulos sao diferentes, mas uma das duas confiancas seja >= que thrConf

#!!!!!!!!!!!!!!!!!IMPLEMENTAR!!!!!!!!!!!!!!!!!!!
  
coTrainFlexCon <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,
                          votacao = TRUE){
  
  
  
  #N armazena a quantidade de exemplos na base de dados
  N <- NROW(data)
  
  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]
  
  #inicializando vari?veis
  it <- 0 #iteracao
  
  
  # soma_Conf <- 0 #soma da confianca
  conf_media <- 0 #confian?a m?dia da predicao dos exemplos rotulados em cada itera??o
  qtd_Exemplos_Rot <- 0 #quantidade de eemplos rotulados
  totalrot <- 0 #total de exemplos rotulados
  corret <- 0 #corretude
  cobert <- 0 #cobertura
  #sup recebe o indice de todos os exemplos rotulados
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  
  #quantidade de linhas do conjunto de dados retirando os exemplos rotulados, ou seja, a quantidade de exemplos n?o rotulados no conjunto de dados
  N_nao_rot <- NROW(data[-sup,])
  repeat {
    acertou <- 0
    it <- it+1
    #O c?lculo da taxa de confianca (thrConf) ser? realizado a partir da segunda iteracao e se houver exemplos rotulados
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      thrConf <- (thrConf + conf_media + (qtd_Exemplos_Rot/N_nao_rot))/3
    }
    
    # soma_Conf <- 0
    conf_media <- 0
    qtd_Exemplos_Rot <- 0

    
    #model armazena o modelo gerado utilizando o aprendiz learner (AD, NB, KNN OU RIPPER), a base data[sup,] que s?o os dados rotulados e a classe ? passada no par?metro form
    model1 <- runLearner(learner, form, data1[sup1, ])
    model2 <- runLearner(learner, form, data2[sup2, ])
    #a predicao e gerada de acordo com predFunc (func ou f1 ou f2 que foi passado como par?metro)
    probPreds1 <- do.call(predFunc, list(model1, data1[-sup1,]))
    probPreds2 <- do.call(predFunc, list(model2, data2[-sup2,]))
    #transforma as classes em caracter
    probPreds1$cl <- as.character(probPreds1$cl)
    probPreds2$cl <- as.character(probPreds2$cl)
    #    !!!!!!!!!!!!!!!!!!!!!!PAREI AQUI!!!!!!!!!!!!!!!!!!!    
    if(it == 1){
      probPreds_1_it <<- probPreds
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original),unique(base_original$class)))
      new <- which(probPreds[,2] >= thrConf)
      rotulados <- data.frame(id = new,cl = probPreds[new,1]) 
      
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(indices,probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(indices,predicao) # Armazena a soma das classes
      }
      
    }else{
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(indices,probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(indices,predicao) # Armazena a soma das classes
      }
      
      
      rotulados <- checa_classe(probPreds_1_it, probPreds, indices, thrConf, usarModa = FALSE, moda)
      if (length(rotulados$id) == 0){
        rotulados <- checa_confianca(probPreds_1_it, probPreds, indices, thrConf, usarModa = FALSE, moda)
        if (length(rotulados$id) == 0){
          rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, indices, thrConf, moda)
        }
      }
      new <- rotulados$id
    }
    
    if (verbose) {
      #imprime na tela o % de exemplos rotulados inicialmente, a iteracao, a base de dados, a taxa de confianca e a quantidade de exemplos rotulados
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g <<-c(it_g,it)
      bd_g <<-c(bd_g,bd_nome)
      thrConf_g <<-c(thrConf_g,thrConf)
      nr_added_exs_g <<-c(nr_added_exs_g,length(new))
      tx_g <<- c(tx_g, taxa)
    }
    
    #Se existir algum exemplo a ser rotulado, ser?o inseridos no conjunto dos rotulados
    if (length(new)) {
      #quantidade de exemplos n?o rotulados no conjunto de dados
      N_nao_rot <- NROW(data[-sup,])
      
      data[(1:N)[-sup][new],as.character(form[[2]])] <- rotulados[,2]
      # data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      
      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
      conf_media <- mean(probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      
      sup <- c(sup,(1:N)[-sup][new])      
    }
    
    # corret <- (soma_Conf/qtd_Exemplos_Rot)
    corret <- conf_media
    cobert <- (qtd_Exemplos_Rot/N_nao_rot)
    corretude_g <<- c(corretude_g, corret)
    cobertura_g <<- c(cobertura_g, cobert)
    acertou_g <<- c(acertou_g, acertou)
    
    #se n?o existir nenhum exemplo a ser rotulado, atribua a taxa de confianca (thrConf) a maior confian?a na predicao
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) 
      # thrConf<-mean(probPreds[,2])
    }
    
    #termine se chegar ao n?mero m?ximo de itera??es ou se atingir o percentual m?ximo de exemplos rotulados
    if (it == maxIts || length(sup)/N >= percFull) break
    
  }
  
  #retorne o modelo criado pelo classificador  
  return(model)  
}

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
