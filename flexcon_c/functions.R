guardar_predicao <- function(predic, iterac){
  if (iterac == 1){
    soma <<- predic
    cat("criar vetor com o voto e a soma")  
  }else{
    cat("incrementar o voto e a soma")
  }
}

func <- function(m, d){
  p <- predict(m, d, type = "raw")
  predicao <<- data.frame(p, row.names(d))
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max), id=row.names(d))
}

f <- function(m, d) {
  p <- predict(m, d, type = 'prob')
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
}

f2 <- function(m, d) {
  p <- predict(m, d, type = 'probability')
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
}

################################
#                              #
# Funcoes para o ST-Modificado #
#                              #
################################
checaClasse <- function(data_1_it, data_x_it, thr_conf){
  examples <- c()
  pos <- 0
  xid <- c() # Vetor de id
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if((as.character(data_1_it[lvls[indice], 1]) == as.character(data_x_it[indice, 1]))){
      if ((data_1_it[lvls[indice], 2] >= thr_conf) && (data_x_it[indice, 2] >= thr_conf)){
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice,1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# compara se as classes sao iguais e uma das confiacas é maior qua a confianca da iteracao atual
checaConfianca <- function(data_1_it, data_x_it, thr_conf){
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((data_1_it[lvls[indice], 1] == data_x_it[indice, 1])){
      if ((data_1_it[lvls[indice], 2] >= thr_conf) || (data_x_it[indice, 2] >= thr_conf)){
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}


# compara se as classes sao diferentes e o produto das confiancas e maior que a confianca atual
checaClasseDiferentes <- function(data_1_it, data_x_it, thr_conf, moda){
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))){
      if ((data_1_it[lvls[indice], 2] >= thr_conf) && (data_x_it[indice, 2] >= thr_conf)){
        pos <- pos + 1
        # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
        xid[pos] <- indice
        ycl[pos] <- pesquisaClasse(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# As classes são diferentes, mas uma das confianças é maior que o Limiar, inclui o de maior confiança
checaConfiancasDiferentes <- function(data_1_it, data_x_it, thr_conf){
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))){
      if ((data_1_it[lvls[indice], 2] >= thr_conf) || (data_x_it[indice, 2] >= thr_conf)){
        pos <- pos + 1
        # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
        xid[pos] <- indice
        ycl[pos] <- pesquisaClasse(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

pesquisaClasse <- function(i, moda){
  maior <- 0
  classes <- colnames(moda)
  for (j in 1:length(moda[i, ])){
    if((moda[i, j] >= maior)){
      maior <- moda[i, j]
      cl <- classes[j] 
    }
  }
  return(cl)
}

#armazena o voto do classificador para cada r?tulo
guardaModa <- function(prob_preds){
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:NROW(prob_preds)){
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for(y in 1:(length(dist_classes))){
      if(prob_preds[x, 1] == dist_classes[y]){
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

guardaSoma <- function(p){ # p = predicao
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:nrow(p)){
    id <- as.character(p[x, ncol(p)]) #pega o id que ? a ultima coluna de p
    for(y in 1:length(dist_classes)){ 
      moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + p[x, dist_classes[y]]
    }
  }
  return(moda)
}

################################
#                              #
#            FIM               #
#                              #
################################

#funcao, chamada em modificado2 e 3, que valida se o conjunto de treinamento pode ser utilizado para calcular a nova taxa de confianca
validarTreino <- function(data, id_conj_treino, N_classes, min_exem_por_classe){
  N_instancias_por_classe2 <- ddply(data[id_conj_treino, ], ~class, summarise, number_of_distinct_orders = length(class))
  
  treino_valido <<- FALSE
  if (NROW(N_instancias_por_classe2) == N_classes){#TAVA nrow
    
    for (x in 1:NROW(N_instancias_por_classe2)){ #TAVA nrow
      
      if (N_instancias_por_classe2$number_of_distinct_orders[x] >= min_exem_por_classe){
        treino_valido <<- TRUE
      } else {
        treino_valido <<- FALSE
        break
      }
    }
  }
}

#funcao, chamada em modificado2 e 3, que define o conjunto de treinamento a ser classificado e indica se a classificacao e possivel
validarClassificacao <- function(treino_valido_i, id_conj_treino, id_conj_treino_antigo, data, N_classes, min_exem_por_classe){
  #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N?fO)
  if (treino_valido_i){
    #o conjunto de treinamento serao as instancias inclu????das (rotuladas)
    conj_treino <<- data[id_conj_treino, ]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
    
  } else if (length(id_conj_treino_antigo) >= 1) {
    #o conjunto de treinamento será o anterior + as instancias incluidas (rotuladas)
    conj_treino <<- rbind(data[id_conj_treino, ], data[id_conj_treino_antigo, ])
    
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validarTreino(data, id_conj_treino1, N_classes, min_exem_por_classe)
    
    if (treino_valido){
      classificar <- TRUE
    } else {
      classificar <- FALSE
    }
  } else {
    classificar <- FALSE #a confian?a permanece a mesma ao inves de parar
  }
  return(classificar)  
}

#funcao que faz o treinamento usando o conjunto de treinamento definido na funcao validarClassificacao e
#calcula a acuracia para ser usada na definicao da nova taxa de confianca. Essa acuracia comparada com o 
#limiar indica se a nova taxa sobe ou desce.
calcularAccLocal <- function(){
  
  if(c==1){
    classificador <- naiveBayes(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  }
  else if (c==2){
    #IMPLEMENTAR ARVORE DE DECIS?O
    classificador <- rpartXse(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini, type="class"), base_rotulados_ini$class)        
  } else if (c==3){
    #IMPLEMENTAR ripper
    classificador <- JRip(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)        
  } else if (c==4){
    #IMPLEMENTAR IBk
    classificador <- IBk(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  }
  acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
  return(acc_local)
}


#funcao que calcula a nova confianca de acordo com a acuracia local e o limiar
calcularConfianca <- function(acc_local, limiar, tx_conf){
  cr <- 0.07
  if((acc_local > (limiar + 1)) && ((tx_conf - cr) > 0.0)){
    tx_conf <- tx_conf - cr
    
  }else if((acc_local < (limiar - 1)) && ((tx_conf + cr) <= 1)){
    txConf <- tx_conf + cr
  }
  return(tx_conf)
}


gerarModel <- function(learner, form, data, sup){
  model <- runLearner(learner, form, data[sup,])
  return(model)
}

gerarProbPreds <- function(pred_func, model, data, sup){
  prob_preds <- do.call(pred_func, list(model, data[-sup,]))
  return(prob_preds)
}

flexConC <- function(learner, pred_func, min_exem_por_classe, limiar){
  # Initial setup, this is equal in all methods
  form <- as.formula(paste(classe, '~', '.'))
  data <- base_treino_self_training
  thr_conf <- 0.95
  max_its <- 100
  verbose <- TRUE
  it <- 0
  
  # FlexCon-C1 and FlexCon-C2 
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data, ~class, summarise, number_of_distinct_orders = length(class))
  N_classes <- NROW(N_instancias_por_classe) - 1
  qtd_exemplos_rot <- 0
  total_rot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()

  # FlexCon-C2 only
  soma_Conf <- 0
  add_rot_superv <- FALSE
  
  repeat {
    acertou <- 0
    it = it + 1
    
    if (qtd_exemplos_rot > 0){
      qtd_exemplos_rot = 0
      validartreino(data, id_conj_treino, n_classes, min_exem_por_classe)
      classificar <- validarclassificacao(treino_valido, id_conj_treino, id_conj_treino_antigo, data, n_classes, min_exem_por_classe)
      
      if(classificar){
        acc_local <- calcularacclocal()
        thr_conf <- calcularconfianca(acc_local, limiar, thr_conf)
      }
    }
    
    model <- gerarmodel(learner, form, data, sup)
    prob_preds <- gerarprobpreds(pred_func, model, data, sup)
    
    switch (method,
            "1" = {
              flexconc1s()
            },
            "2" = {
              flexconc1v()
            },
            "3" = {
              prob_preds_model_superv <- gerarprobpreds(pred_func, model_sup, data, sup)
              flexconc2()
            }
    )
    if (length(new)) {
      if (add_rot_superv) {
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds_model_superv[new,1])
      }else{
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      }
      
      qtd_exemplos_rot <- length(data[(1:N)[-sup][new], as.character(form[[2]])])
      total_rot <- total_rot + qtd_exemplos_rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])] == data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE){
          acertou <<- acertou + 1
        }
      }
      id_conj_treino_antigo <- c(id_conj_treino_antigo, id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      
      sup <- c(sup, (1:N)[-sup][new])
      
      if(length(new) == 0){
        thr_conf <- max(probPreds[ ,2])
      }
    }
    if ((it == max_its) || ((length(sup) / N) >= percFull)){
      break
    }
  }
  return(model)
}

flexConC2 <- function(model, prob_preds, prob_preds_model_superv, thr_conf){
  z <- sapply(prob_preds, is.factor)
  prob_preds[z] <- lapply(prob_preds[z], as.character)
  
  z <- sapply(prob_preds_model_superv, is.factor)
  prob_preds_model_superv[z] <- lapply(prob_preds_model_superv[z], as.character)
  
  new <- which((prob_preds[, 2] >= thr_conf) & (prob_preds_model_superv[, 2] >= thr_conf) & (prob_preds[, 1] == prob_preds_model_superv[, 1]))
  if (length(new) == 0){
    new <- which(((prob_preds[, 2] >= thr_conf) | (prob_preds_model_superv[, 2] >= thr_conf)) & (prob_preds[, 1] == prob_preds_model_superv[, 1]))
    
    if (length(new) == 0){
      new <- which((prob_preds[, 2] >= thr_conf) & (prob_preds_model_superv[, 2] >= thr_conf) & (prob_preds[, 1] != prob_preds_model_superv[, 1]))
      if (length(new)){
        add_rot_superv <- TRUE
      }
    }
  }
  
}
