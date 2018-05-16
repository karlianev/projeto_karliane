func <- function(m, d){ #NB
  p <- predict(m, d, type = "raw") #col2 armazena a confian?a em cada classe predita pelo classificador (ex: classe 1 = 0.8, classe2 = 0.1, classe 3= 0.1)
  predicao <<- data.frame(p, row.names(d))
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max), id=row.names(d))
}

f <- function(m,d) { #AD
  p <- predict(m,d,type='prob') #predicao dos dados (d) de acordo com o modelo (m)
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2, id=row.names(d))
}

f2 <- function(m,d) { #JRip e KNN
  p <- predict(m,d,type='probability') # l ? uma matriz com a confian?a da predi??o de cada exemplo
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2, id=row.names(d)) #um data frame com 2 colunas: 1) a predi??o de cada exemplo; 2) a classe predita para cada exemplo
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

################################
#                              #
# Funcoes para o ST-Modificado #
#                              #
################################
# compara se as classes estao iguais e se as confiancas sao maiores q a da iteracao atual
#checa_classe novo - usado no co-training
checa_classe <- function(data_1_it, data_x_it, thrConf, usarModa=F, moda){
  examples <- c()
  pos <- 0
  xid <- c() # Vetor de id
  ycl <- c()
  if(usarModa){
    for (i in 1:NROW(data_1_it)){
      id1 <- as.character(data_1_it[i,3])
      if (!is.na(data_1_it[i, 1]) && (as.character(data_x_it[i, 1]) == as.character(data_1_it[i, 1]))){
        if ((data_1_it[i, 2]* data_x_it[i, 2]) >= thrConf){
          pos <- pos + 1
          xid[pos] <- i
          ycl[pos] <- pesquisa_classe(id1, moda)
        }#fim if
      }#fim if
    } #fim do for
  }else{
    for (i in 1:NROW(data_1_it)){
      # id1 <- as.character(data_1_it[i,3])
      cl1 <- as.character(data_1_it[i,1])
      if (!is.na(data_1_it[i, 1]) && (as.character(data_x_it[i, 1]) == as.character(data_1_it[i, 1]))){
        if ((data_1_it[i, 2]*data_x_it[i, 2]) >= thrConf){
          pos <- pos + 1
          xid[pos] <- i
          ycl[pos] <- cl1
        }
      }
    }

   
  }
  #cria o data frame com colunas ID (posicao no probpreds) e CLASSE
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
} #fim da funcao

#checa_classe antigo - usado no self-training
# checa_classe <- function(data_1_it, data_x_it, indices, thrConf, usarModa, moda){
#   examples <- c()
#   pos <- 0
#   xid <- c() # Vetor de id
#   ycl <- c()
#   if(usarModa){
#     for (i in indices){
#       if (!is.na(data_1_it[i, 1]) && (data_x_it[i, 1] == data_1_it[i, 1])){
#         if ((data_1_it[i, 2]* data_x_it[i, 2]) >= thrConf){
#         #estava assim no selftraining
#         #if ((data_1_it[i, 2] >= thrConf) && (data_x_it[i, 2] >= thrConf)){
#           pos <- pos + 1
#           xid[pos] <- i
#           ycl[pos] <- pesquisa_classe(i, moda)
#         }
#       }
#     }
#     #cria o data frame com colunas ID e CLASSE
#     examples <- data.frame(id = xid,cl = ycl)
#   }else{
#     for (i in indices){
#       if (!is.na(data_1_it[i, 1]) && (data_x_it[i, 1] == data_1_it[i, 1])){
#         if ((data_1_it[i, 2]*data_x_it[i, 2]) >= thrConf){  
#         #estava assim no self-training
#         #if ((data_1_it[i, 2] >= thrConf) && (data_x_it[i, 2] >= thrConf)){  
#           pos <- pos + 1
#           xid[pos] <- i
#         }
#       }
#     }
#     #cria o data frame com colunas ID e CLASSE
#     examples <- data.frame(id = xid,cl = data_x_it[xid, 1])
#   }
#   return (examples)
# }

# compara se as classes sao iguais e uma das confiacas Ã© maior qua a confianca da iteracao atual
checa_confianca <- function(data_1_it, data_x_it, indices, thrConf, usarModa, moda){
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  if(usarModa){
    for (i in indices){
      if (!is.na(data_1_it[i, 1]) && (data_1_it[i, 1] == data_x_it[i, 1])){
        if((data_1_it[i, 2] >= thrConf) || (data_x_it[i, 2] >= thrConf)){
          pos <- pos + 1
          xid[pos] <- i
          ycl[pos] <- pesquisa_classe(i, moda)
        }
      }
    }
    examples <- data.frame(id = xid,cl = ycl)
  }else{
    for (i in indices){
      if (!is.na(data_1_it[i, 1]) && (data_1_it[i, 1] == data_x_it[i, 1])){
        if((data_1_it[i, 2] >= thrConf) || (data_x_it[i, 2] >= thrConf)){
          pos <- pos + 1
          xid[pos] <- i
        }
      }
    }
    examples <- data.frame(id = xid,cl = data_x_it[xid, 1])
  }
  return (examples)
}


# compara se as classes sao diferentes e o produto das confiancas e maior que a confianca atual
#checa_classe_diferentes novo - usado no co-training
checa_classe_diferentes <- function(data_1_it, data_x_it, thrConf, usarmoda, moda){
  pos <- 0
  xid <- c()
  ycl <- c()
  for (i in 1:nrow(data_1_it)){
    id <- data_1_it[i,3]
    if (!is.na(data_1_it[i, 1]) && (as.character(data_1_it[i, 1]) != as.character(data_x_it[i, 1]))){
      if ((data_1_it[i, 2]*data_x_it[i, 2]) >= thrConf){
        pos <- pos + 1
        # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
        xid[pos] <- i
        ycl[pos] <- pesquisa_classe(id, moda)  
        }#fim if
      }#fim if
    }#fim for
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
}

# #checa_classe_diferentes antigo - usado no self-training
# checa_classe_diferentes <- function(data_1_it, data_x_it, indices, thrConf, usarmoda, moda){
#   pos <- 0
#   xid <- c()
#   ycl <- c()
#   for (i in indices){
#     if (!is.na(data_1_it[i, 1]) && (data_1_it[i, 1] != data_x_it[i, 1])){
#       if ((data_1_it[i, 2]*data_x_it[i, 2]) >= thrConf){
#         pos <- pos + 1
#         # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
#         xid[pos] <- i
#         if (usarmoda) {
#           ycl[pos] <- pesquisa_classe(i, moda)  
#         }else{
#           ycl[pos] <- mais_confiavel(data_1_it, data_x_it, i)  
#         }
#       }
#     }
#   }
#   examples <- data.frame(id = xid,cl = ycl)
#   return (examples)
# }

#retorna a classe do classificador mais confiavel
mais_confiavel <- function(pred1, pred2, i){
  if (pred2[i, 2] > pred1[i, 2]){
    return(pred2[i, 1])
  }else{
    return(pred1[i, 1])
  }
}

pesquisa_classe <- function(i, moda){
  maior <- 0
  classes <- colnames(moda)
  for (j in 1:length(moda[i,])){
    if(moda[i,j] >= maior){
      maior <- moda[i,j] 
      cl <- classes[j] 
    }
  }
  return(cl)
}

#armazena o voto do classificador para cada r?tulo
#guarda_moda novo - usado no co-training
guarda_moda <- function(probPreds){
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:NROW(probPreds)){
    id <- as.character(probPreds[x,ncol(probPreds)])
    for(y in 1:(length(dist_classes))){
      if(probPreds[x,1] == dist_classes[y]){
        moda[id,dist_classes[y]] <- moda[id,dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

#guarda_moda antigo - usado no self-training
# guarda_moda <- function(indices,probPreds){
#   dist_classes <- unique(probPreds[,1]) #pega as classes distintas
#   for (x in indices){
#     for(y in 1:length(dist_classes)){
#       if(probPreds[x,1] == dist_classes[y]){
#         moda[x,dist_classes[y]] <- moda[x,dist_classes[y]] + 1
#         break
#       }
#     }
#   }
#   return (moda)
# }

#guarda_soma novo - usado no co-training
guarda_soma <- function(p){ # p = predicao
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:nrow(p)){
    id <- as.character(p[x,ncol(p)]) #pega o id que é a ultima coluna de p
    for(y in 1:length(dist_classes)){ 
      moda[id,dist_classes[y]] <- moda[id,dist_classes[y]] + p[x,dist_classes[y]]

    }
  }
  return(moda)
}

# guarda_soma antigo - usado no self-training
# guarda_soma <- function(indices,p){ # p = predicao
#   dist_classes <- unique(base_original$class) #pega as classes distintas
#   for (x in indices){
#     # x<-as.factor(x)
#     for(y in 1:(ncol(p)-1)){ # SUBTRAI 1 PQ A ULTIMA COLUNA É O ID DO EXEMPLO
#       moda[x,dist_classes[y]] <- moda[x,dist_classes[y]] + p[x,dist_classes[y]]
#       
#     }
#   }
#   return(moda)
# }

#funcao, chamada em modificado2 e 3, que valida se o conjunto de treinamento pode ser utilizado para calcular a nova taxa de confianca
validar_treino<- function(data,id_conj_treino,N_classes,min_exem_por_classe){
  #cat("entrou if da segunda itera??o", '\n')
  N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))
  
  treino_valido <<- FALSE
  if (NROW(N_instancias_por_classe2)  == N_classes){#TAVA nrow
    
    for (x in 1:NROW(N_instancias_por_classe2)){ #TAVA nrow
      
      if (N_instancias_por_classe2$number_of_distinct_orders[x]>= min_exem_por_classe) #N_classes*5)
        treino_valido <<- TRUE
      else treino_valido <<- FALSE
    }  
  }
} 

#funcao, chamada em modificado2 e 3, que define o conjunto de treinamento a ser classificado e indica se a classificacao e possivel
validar_classificacao<-function(treino_valido_i,id_conj_treino,id_conj_treino_antigo,data, N_classes, min_exem_por_classe){
  #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N?fO)
  if (treino_valido_i){
    #o conjunto de treinamento serao as instancias inclu????das (rotuladas)
    conj_treino <<- data[id_conj_treino,]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
    
  }else if (length(id_conj_treino_antigo)>=1) {
    #o conjunto de treinamento serÃ¡ o anterior + as instancias incluidas (rotuladas)
    conj_treino <<- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
    
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validar_treino(data,id_conj_treino1,N_classes,min_exem_por_classe);
    
    if (treino_valido){
      classificar <- TRUE
    }else{
      classificar <- FALSE
    }
    
  }else classificar <- FALSE #a confian?a permanece a mesma ao inves de parar
  return(classificar)  
}

calcular_acc_local <- function(){
  
  if(c==1){
    classificador <- naiveBayes(as.factor(class) ~ .,conj_treino)
    matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
  }
  else if (c==2){
    #IMPLEMENTAR ARVORE DE DECIS?O
    classificador <- rpartXse(as.factor(class) ~ .,conj_treino)
    matriz <- table(predict(classificador,base_rotulados_ini, type="class"),base_rotulados_ini$class)        
  } else if (c==3){
    #IMPLEMENTAR ripper
    classificador <- JRip(as.factor(class) ~ .,conj_treino)
    matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)        
  } else if (c==4){
    #IMPLEMENTAR IBk
    classificador <- IBk(as.factor(class) ~ .,conj_treino)
    matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
  }
  acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
  return(acc_local)
}

#funcao que calcula a nova confianca de acordo com a acuracia local e o limiar
calcular_confianca<-function(acc_local,limiar,txConf){
  if((acc_local>(limiar + 1)) && (txConf-0.05>0.0)){
    txConf<-txConf-0.05
    
  }else if((acc_local<(limiar - 1)) && (txConf+0.05 <= 1)){
    
    txConf<-txConf+0.05
  } #caso contrario a confianca permanecera a mesma
  return(txConf)
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
      # produto_confianca <- probPreds1[, 2]*probPreds2[, 2]
      # new <- which((produto_confianca > thrConf) & (as.character(probPreds1[, 1])==as.character(probPreds2[, 1])))
      rotulados <- checa_classe(probPreds1, probPreds2, thrConf,FALSE, moda)
      new <- rotulados$id
    }else{  
      #POR ENQUANTO NÃO ESTA SENDO USADO
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
    if ((it == maxIts) || (length(sup1)/N >= percFull) || (length(sup2)/N >= percFull)){
      break
    } 
      
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
                                   verbose=F,gradativo=0.05, 
                                   votacao=F){
  
  
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
    #transforma as classes em caracter
    probPreds1$cl <- as.character(probPreds1$cl)
    probPreds2$cl <- as.character(probPreds2$cl)
    
    if(it == 1){
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original),sort(unique(base_original$class), decreasing = FALSE)))
      
    }  
    
    if (votacao){
      moda <<- guarda_moda(probPreds1) # Armazena a moda das classes
      moda <<- guarda_moda(probPreds2) # Armazena a moda das classes
    }else{
      #variavel predicao e gerado no predfunc
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
    }        
    
    rotulados <- checa_classe(probPreds1, probPreds2, thrConf, FALSE)
    if (length(rotulados$id) == 0){
      # compara se as classes sao diferentes e o produto das confiancas e maior que o limiar
      rotulados <- checa_classe_diferentes(probPreds1, probPreds2, thrConf, usarmoda = TRUE, moda)
    }
    new <- rotulados$id #nao e o id do exemplo e sim a posição no probpreds        

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
    # if(length(new)==0){
    #   baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !(produto_confianca > thrConf))
    #   if (length(baixa_conf)==0){
    #     break
    #   }else{
    #     thrConf<-max(produto_confianca[baixa_conf])  
    #   }
    # }
    
    if(length(new)==0){
            baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
      if (length(baixa_conf)==0){
          break
      }else{
        maior <- 0
        for (i in baixa_conf){
          produto <- (probPreds1[i, 2]*probPreds2[i, 2])
          if (produto > maior){
            maior <- produto
          }
        }
        thrConf <- maior
      }
    }
    
    if ((it == maxIts) || (length(sup1)/N >= percFull) || (length(sup2)/N >= percFull)){
      break  
    } 
  } #fim do repeat
  model <- list(model1,model2)
  return(model)  
} #fim da funcao

#funcao co-training modificado (flexCon), usa uma formula para calcular a nova taxa de confianca.
#inclui no conjunto dos rotulados os exemplos que possuem mesmo rotulo e taxa de conf. >= thrconf
#caso nao exista nenhum exemplo com essa caracteristica, serao incluidos os exemplos que possuem o mesmo
#rotulo e uma das duas confiancas >= thrConf. Se ainda assim nao existir nenhum exemplo, serao
#incluidos os exemplos cujos rotulos sao diferentes, mas uma das duas confiancas seja >= que thrConf

coTrainFlexCon <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,
                          votacao = TRUE,
                          combinar=TRUE){
  
  
  
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
  N_nao_rot <- NROW(data[-sup1,])
  repeat{
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
    produto_confianca <- probPreds1[, 2]*probPreds2[, 2] #NECESSARIO APENAS PARA CALCULAR A CONFIANCA MEDIA
    if(it == 1){
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original), sort(unique(base_original$class), decreasing=FALSE)))
    }
    
    
    if (votacao){
      moda <<- guarda_moda(probPreds1) # Armazena a moda das classes
      moda <<- guarda_moda(probPreds2) # Armazena a moda das classes
    }else{
      #a variavel predicao e gerada pelo predfunc
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
    }        
    
    #exemplos da mesma classe e com o produto das confianças maior do que o limiar
    rotulados <- checa_classe(probPreds1, probPreds2, thrConf, usarModa = TRUE, moda)
      if (length(rotulados$id) == 0){
        # compara se as classes sao diferentes e o produto das confiancas e maior que o limiar
        rotulados <- checa_classe_diferentes(probPreds1, probPreds2, thrConf, usarmoda = TRUE, moda)
      }
      
    new <- rotulados$id
    
    
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
      N_nao_rot <- NROW(data[-sup1,])
      data1[(1:N)[-sup1][new],as.character(form[[2]])] <- rotulados[,2]
      data2[(1:N)[-sup2][new],as.character(form[[2]])] <- rotulados[,2]
      conf_media <- mean(produto_confianca)
      qtd_Exemplos_Rot <- length(data1[(1:N)[-sup1][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      sup1 <- c(sup1,(1:N)[-sup1][new])
      sup2 <- c(sup2,(1:N)[-sup2][new])      
    }
    
    # corret <- (soma_Conf/qtd_Exemplos_Rot)
    corret <- conf_media
    cobert <- (qtd_Exemplos_Rot/N_nao_rot)
    corretude_g <<- c(corretude_g, corret)
    cobertura_g <<- c(cobertura_g, cobert)
    # acertou_g <<- c(acertou_g, acertou)
    
    #se n?o existir nenhum exemplo a ser rotulado, atribua a taxa de confianca (thrConf) a maior confian?a na predicao
    if(length(new)==0){
      baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
      if (length(baixa_conf)==0){
        #Flavius sugeriu que aqui fosse trocado o classificador
        baixa_conf <- which((as.character(probPreds1[, 1])!=as.character(probPreds2[, 1])) & !((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
      }
      maior <- 0
      for (i in baixa_conf){
        produto <- (probPreds1[i, 2]*probPreds2[i, 2])
        if (produto > maior){
          maior <- produto
        }
      }
      thrConf<-maior
    }
    #termine se chegar ao n?mero m?ximo de itera??es ou se atingir o percentual m?ximo de exemplos rotulados
    if ((it == maxIts) || (length(sup1)/N >= percFull) || (length(sup2)/N>= percFull)){
      break
    }
      
  }#fim do repeat
  
  model <- list(model1,model2)  
  #retorne o modelo criado pelo classificador  
  return(model)  
}
#calcula a confianca de acordo com o treinamento do classificador
#so acumula o conjunto de treinamento com o conjunto anterior caso o conjunto de treinamento nao seja valido
#usa votacao/soma para definir o rotulo nos casos em que os classificadores divergem
coTrainFlexCon_C1 <- function(form,data,
                                     learner,
                                     predFunc,
                                     thrConf=0.9,
                                     maxIts=10,percFull=1,
                                     verbose=F,
                                     min_exem_por_classe,
                                     limiar=70,
                                     votacao = TRUE){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
  #substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  
  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]
  
  it <- 0
  #soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  
  #sup recebe o indice de todos os exemplos rotulados
  #está sendo sup = sup1 = sup2
  sup <- which(!is.na(data[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados

  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      validar_treino(data,id_conj_treino,N_classes,min_exem_por_classe);
      classificar <- validar_classificacao(treino_valido,id_conj_treino,id_conj_treino_antigo,data, N_classes, min_exem_por_classe)
      
      if (classificar){
        acc_local <- calcular_acc_local()
        thrConf <- calcular_confianca(acc_local,limiar,thrConf)  
      }  
    }
   # soma_Conf <- 0
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
    
    
    if(it == 1){
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original),sort(unique(base_original$class), decreasing = FALSE)))
      
    }  
    #está sendo indices1 = indices2

    if (votacao){
      moda <<- guarda_moda(probPreds1) # Armazena a moda das classes
      moda <<- guarda_moda(probPreds2) # Armazena a moda das classes
    }else{
      #variavel predicao e gerado no predfunc
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
      moda <<- guarda_soma(predicao) # Armazena a soma das classes
    }        
    #exemplos da mesma classe e com o produto das confianças maior do que o limiar
    rotulados <- checa_classe(probPreds1, probPreds2, thrConf, usarModa = TRUE, moda)
    if (length(rotulados$id) == 0){
      # compara se as classes sao diferentes e o produto das confiancas e maior que o limiar
      rotulados <- checa_classe_diferentes(probPreds1, probPreds2, thrConf, usarmoda = TRUE, moda)
    }
    new <- rotulados$id
  
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g <<-c(it_g,it)
      bd_g <<-c(bd_g,bd_nome)
      thrConf_g <<-c(thrConf_g,thrConf)
      nr_added_exs_g <<-c(nr_added_exs_g,length(new))
      tx_g <<- c(tx_g, taxa)
    }
    
    if (length(new)) {
      #PRECISO ATUALIZAR O DATA POR CAUSA DO CONJ_TREINO
      data[(1:N)[-sup][new],as.character(form[[2]])] <- rotulados[,2]
      data1[(1:N)[-sup1][new],as.character(form[[2]])] <- rotulados[,2]
      data2[(1:N)[-sup2][new],as.character(form[[2]])] <- rotulados[,2]
      
    #  soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      # acertou <- 0
      # acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      # tam_acerto <- NROW(acerto)
      # for (w in 1:tam_acerto){
      #   if (acerto[w] == TRUE)
      #     acertou <- acertou + 1
      # }
      
      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      
      sup <- c(sup,(1:N)[-sup][new])
      sup1 <- c(sup1,(1:N)[-sup1][new])
      sup2 <- c(sup2,(1:N)[-sup2][new])      
    }
    
#    acertou_g <<- c(acertou_g, acertou)    
    if(length(new)==0){
      #baixar a confiança para os exemplos que não coincidem na classe, mas o produto da confiança é maior que o limiar
      baixa_conf <- which((as.character(probPreds1[, 1])!=as.character(probPreds2[, 1])) & ((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
      #baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !(produto_confianca > thrConf))
      if (length(baixa_conf)==0){
        #baixa a confianca para os exemplos cujas classes coincidem e o produto da conficanca nao e maior do que o limiar
        baixa_conf <- which((as.character(probPreds1[, 1])==as.character(probPreds2[, 1])) & !((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
        if (length(baixa_conf)==0){
          #Flavius sugeriu que aqui fosse trocado o classificador
          #baixa a confianca para os exemplos cuja classe não coincide e confianca menor que o limiar
          baixa_conf <- which((as.character(probPreds1[, 1])!=as.character(probPreds2[, 1])) & !((probPreds1[, 2]*probPreds2[, 2]) > thrConf))
          #baixa_conf <- which((as.character(probPreds1[, 1])!=as.character(probPreds2[, 1])) & !(produto_confianca > thrConf))
        }
      }
      #thrConf<-max(produto_confianca[baixa_conf])  
      maior <- 0
      for (i in baixa_conf){
        produto <- (probPreds1[i, 2]*probPreds2[i, 2])
        if (produto > maior){
          maior <- produto
        }
      }
      thrConf <- maior
    }
    
    if (it == maxIts || length(sup)/N >= percFull) break
    
    #FIM DO REPEAT
  }
  model <- list(model1,model2)  
  #retorne o modelo criado pelo classificador  
  return(model)  
  
}

#calcula a confianca de acordo com o treinamento do classificador
# o conjunto de treinamento e cumulativo
#usa O ROTULO DO CLASSIFICADOR SUPERVISIONADO nos casos em que os classificadores divergem
coTrainFlexCon_C2 <- function(form,data,
                               learner,
                               predFunc,
                               thrConf=0.9,
                               maxIts=10,percFull=1,
                               verbose=F,
                               min_exem_por_classe,
                               limiar=70,
                               model_supervisionado){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
  #substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  
  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]
  
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <- c()
  classificar <- TRUE
  add_rot_superv <- FALSE
  
  #sup recebe o indice de todos os exemplos inicialmente rotulados
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) 
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) 
  
  
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      validar_treino(data,id_conj_treino,N_classes,min_exem_por_classe);
      classificar <- validar_classificacao(treino_valido,id_conj_treino,id_conj_treino_antigo,data)
      
      if (classificar){
        acc_local <- calcular_acc_local()
        thrConf <- calcular_confianca(acc_local,limiar,thrConf)  
      }  
    }
    
    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0

    #model armazena o modelo gerado utilizando o aprendiz learner (AD, NB, KNN OU RIPPER), a base data[sup,] que sao os dados rotulados e a classe e passada no parametro form
    model1 <- runLearner(learner, form, data1[sup1, ])
    model2 <- runLearner(learner, form, data2[sup2, ])
    
    #a predicao e gerada de acordo com predFunc (func ou f1 ou f2 que foi passado como parametro)
    probPreds1 <- do.call(predFunc, list(model1, data1[-sup1,]))
    probPreds2 <- do.call(predFunc, list(model2, data2[-sup2,]))
    
    
    #guardar_predicao(predicao, it)
    probPreds_model_superv1 <- do.call(predFunc,list(model_supervisionado,data1[-sup1,]))
    probPreds_model_superv2 <- do.call(predFunc,list(model_supervisionado,data2[-sup2,]))
    
    #transformando os dados dos factors probpreds e probpreds_model_superv em caracter para n?o ter problema quando a quantidade de classes preditas em um factor n?o for a mesma do outro
    z <- sapply(probPreds1, is.factor)
    probPreds1[z] <- lapply(probPreds1[z], as.character)
    z <- sapply(probPreds2, is.factor)
    probPreds2[z] <- lapply(probPreds2[z], as.character)
    z <- sapply(probPreds_model_superv1, is.factor)
    probPreds_model_superv1[z] <- lapply(probPreds_model_superv1[z], as.character)
    z <- sapply(probPreds_model_superv2, is.factor)
    probPreds_model_superv2[z] <- lapply(probPreds_model_superv2[z], as.character)
    
    #!!!!!!!!!!!!!!!!!!!!PAREI AQUI!!!!!!!!!!!!!!!!!!    
    #!!!!!!!!!!!!!PENSAR DIREITINHO COMO SERÁ AQUI!!!!!!!!!!!!!!!!!!!!!!
    
    produto_confianca <- probPreds1[, 2]*probPreds2[, 2]#*probPreds_model_superv1[, 2]*probPreds_model_superv2[, 2]
    #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
    new <- which((produto_confianca[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds1[,1]==probPreds2[,1]) & (probPreds2[,1]==probPreds_model_superv2[,1]) & (probPreds1[,1]==probPreds_model_superv1[,1]))
    if (length(new)==0){
      #adiciona exemplos cuja confian?a de um dos classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
      new <- which((probPreds[,2] >= thrConf) | (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1]==probPreds_model_superv[,1]))

      if (length(new)==0){
        #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv nao seja a mesma
        new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1] != probPreds_model_superv[,1]))
        if (length(new)){
          add_rot_superv <- TRUE
        }
      }
    }
    
    # #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
    # new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1]==probPreds_model_superv[,1]))
    # if (length(new)==0){
    #   #adiciona exemplos cuja confian?a de um dos classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
    #   new <- which((probPreds[,2] >= thrConf) | (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1]==probPreds_model_superv[,1]))  
    #   
    #   if (length(new)==0){
    #     #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv nao seja a mesma
    #     new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1] != probPreds_model_superv[,1]))  
    #     if (length(new)){
    #       add_rot_superv <- TRUE
    #     }
    #   }
    # }
    
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g_3 <<-c(it_g_3,it)
      bd_g_3 <<-c(bd_g_3,bd_nome)
      thrConf_g_3 <<-c(thrConf_g_3,thrConf)
      nr_added_exs_g_3 <<-c(nr_added_exs_g_3,length(new))
      tx_g_3 <<- c(tx_g_3, taxa)
    }
    
    if (length(new)) {
      if (add_rot_superv) {
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds_model_superv[new,1])
      }else{
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      }
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      
      #esse comando pode ser usado para tornar o conj_treino cumulativo
      # conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
      
      sup <- c(sup,(1:N)[-sup][new])
    }
    
    acertou_g_3 <<- c(acertou_g_3, acertou)
    
    cat('acertou',acertou,'\n') 
    
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  } #FIM DO REPEAT
  
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
