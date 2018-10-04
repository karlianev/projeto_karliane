
training <- function(cl,base,base_original,base_rotulados_ini,base_teste,algorithm) {
    my_learner <- obj[[cl]]
    my_function <- funcs[cl]
    classifier_name <- classifiers[cl]
    limiar <- supAcc(classifier_name, base_rotulados_ini, base_teste)
    n <- getLength(base_teste$class)
    
	partial_acc <- train_algorithm(base,base_original,base_rotulados_ini,base_teste,my_learner,my_function,limiar,n,algorithm)
	return (partial_acc)
}

train_algorithm <- function(base,base_original,base_rotulados_ini,base_teste,my_learner,my_function,limiar,n,algorithm) {
    
    switch (algorithm,
            "1" = { # FlexCon-C1 (s)
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "1")
                name_alg <- "flexcon-c1(s)"
            },
            "2" = { # FlexCon-C1 (v)
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "2")
                name_alg <- "flexcon-c1(v)"
            },
            "3" = { # FlexCon-C2
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "3")
                name_alg <- "flexcon-c2"
            },
            "4" = { # FlexCon-C1S (s)
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "1", stratified = TRUE)
                name_alg <- "flexcon-c1S(s)"
            },
            "5" = { # FlexCon-C1S (v)
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "2", stratified = TRUE)
                name_alg <- "flexcon-c1S(v)"
            },
            "6" = { # FlexCon-C2S
                model <- flexConC(base,base_original,base_rotulados_ini,my_learner, my_function, qtd_exem_menor_classe, limiar, "3", stratified = TRUE)
                name_alg <- "flexcon-c2S"
            }
    )
    
    matrix <- confusionMatrix(model,base_teste)
    partial_acc <- getAcc(matrix, n)
    cat("\n Acerto global ",name_alg," (%) =", partial_acc)
    
    return (partial_acc)
}


storagePred <- function(predic, iterac) {
  if (iterac == 1) {
    soma <<- predic
    cat("criar vetor com o voto e a soma")  
  } else {
    cat("incrementar o voto e a soma")
  }
}

func <- function(m, d) {
  p <- predict(m, d, type = "raw")
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
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

cleanVector <- function(my_vector) {
  my_vector <- c()
  return (my_vector)
}

# Check which samples in data_x_it have equal classes than data_1_it
# Check in both matrixes if the confidence value are higger than thr_conf
classCheck <- function(data_1_it, data_x_it, thr_conf) {
  examples <- cleanVector(examples)
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1]) == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- as.character(data_x_it[indice, 1])
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
confidenceCheck <- function(data_1_it, data_x_it, thr_conf) {
  examples <- cleanVector(examples)
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1]) == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- as.character(data_x_it[indice, 1])
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Check in both matrixes if both confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentClassesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- searchClass(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentConfidencesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- searchClass(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Search in the 'moda' vector the higger value of the sample (sum or vote)
searchClass <- function(i, moda) {
  maior <- 0
  classes <- colnames(moda)
  for (j in 1:length(moda[i, ])) {
    if((moda[i, j] >= maior)) {
      maior <- moda[i, j]
      cl <- classes[j] 
    }
  }
  return (cl)
}

# Storage the vote of the classifier each iteration
storageFashion <- function(base_original, prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for (y in 1:(length(dist_classes))) {
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

# Storage the sum of the confidence for each iteration
storageSum <- function(base_original,prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for (y in 1:length(dist_classes)) { 
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + prob_preds[x, 2]
        break
      }
    }
  }
  return (moda)
}

# Calculate the acc value of the training samples
calcLocalAcc <- function() {
  if (c == 1) {
    classificador <- naiveBayes(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  } else if (c == 2) {
    classificador <- rpartXse(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini, type="class"), base_rotulados_ini$class)        
  } else if (c == 3) {
    classificador <- JRip(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)        
  } else if (c == 4) {
    classificador <- IBk(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  }
  acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
  return(acc_local)
}

# Return the confusion matrix
confusionMatrix <- function(model,base_teste) {
  coluns_names <- colnames(base_teste)
  db_without_class <- match("class", coluns_names)
  test_db <- base_teste[, - db_without_class]
  type <- 'class'
  class_test_bd <- base_teste$class
  confusion <- table(predict(model, test_db, type), class_test_bd)
  return (confusion)
}

# Convert each sample in prob_preds in character
convertProbPreds <- function(prob_preds) {
  aux <- sapply(prob_preds, is.factor)
  prob_preds[aux] <- lapply(prob_preds[aux], as.character)
  return (prob_preds)
}


# Função para definir constantes ao longo do código
# Function to define constants in all code
defines <- function() {
  classifiers <<- c("naiveBayes", "rpartXse", "JRip", "IBk")
  change_rate <<- c(5)
  extention <<- ".csv"
  funcs <<- c('func', 'f', 'f2', 'f2')
  obj <<- c(learner(classifiers[1], list()), learner(classifiers[2], list(se = 0.5)), learner(classifiers[3], list()),
            learner(classifiers[4], list()))
  #control = Weka_control(K = as.integer(sqrt(nrow(base_original))), X = TRUE)
}

attKValue <- function(database){
  listas <- list(control = Weka_control(K = as.integer(sqrt(nrow(database))), X = TRUE))
  obj[4] <<- c(learner(classifiers[4], listas))
}

# -- Stratified--
#Calcula a porcentagem das classes em relacao ao total
porc_classes <- function(classes){
    
    classes_dist <- unique(classes) #Guarda as classes distintas
    qtd_total <- sum(length(classes)) #Guarda o número total de classes
    
    qtd_por_classe <- c(rep(0,length(classes_dist))) #
    porcentagens <- qtd_por_classe                   # Cria os vetores de armazenamento e nomeia as colunas com as classes
    names(qtd_por_classe) <- classes_dist            #
    
    for (i in 1:length(classes_dist)){ #Percorre por classe distinta
        qtd_por_classe[i] <- length(which(classes == classes_dist[i])) #Conta a quantidade por classe e guarda
        porcentagens[i] <- (qtd_por_classe[i]/qtd_total)*100 #Guarda a porcentagem da classe em relação ao todo
    }
    
    cat('    Total: ',qtd_total,'\n')
    cat('    ')
    print(qtd_por_classe)
    cat('    Porcentagem: ',porcentagens,'\n')
    
    return (porcentagens)
}

#
tratar_dados_c1 <- function(rotulados,base_rotulados_ini,probPreds,thrConf,moda,it,taxa_de_perda){
    classes_dist <- unique(base_rotulados_ini$class) # Classes distintas na base inicial
    classes_dist_pp <- unique(probPreds$cl)          # Classes distintas no probPreds
    classes_dist_rot <- unique(rotulados$cl)         # Classes distintas no conjunto dos rotulados
    novos_rotulados <- rotulados
    
	epoch <- 1
	# Enquanto o (((numero de classes do probPreds for maior que dos rotulados) ou (as classes são diferentes) e 
	# (o limiar maior que a confiança mínima do probPreds)) e (esteja dentro do limite de iteracoes))
    while((((length(classes_dist_rot) < length(classes_dist_pp)) || analyzeClasses(classes_dist_pp,classes_dist_rot)) 
           && (thrConf > min(probPreds$p))) && epoch <= 15){ 

        thrConf <- thrConf - taxa_de_perda
        if(thrConf < min(probPreds$p)){
            thrConf <- min(probPreds$p)
        }
        #Rotular novamente
        novos_rotulados <- flexConC1(probPreds, thrConf, moda, it)
        classes_dist_rot <- unique(novos_rotulados$cl)
		epoch <- epoch + 1
    }
    return (novos_rotulados)
}

tratar_dados_c2 <- function(rotulados,base_rotulados_ini,probPreds,prob_preds_superv,thrConf,taxa_de_perda){
    classes_dist <- unique(base_rotulados_ini$class) # Classes distintas na base inicial
    classes_dist_pp <- unique(probPreds$cl)          # Classes distintas no probPreds
    classes_dist_rot <- unique(rotulados$cl)         # Classes distintas no conjunto dos rotulados
    novos_rotulados <- rotulados
    
    epoch <- 1
    # Enquanto o (((numero de classes do probPreds for maior que dos rotulados) ou (as classes são diferentes) e 
    # (o limiar maior que a confiança mínima do probPreds)) e (esteja dentro do limite de iteracoes))
    while((((length(classes_dist_rot) < length(classes_dist_pp)) || analyzeClasses(classes_dist_pp,classes_dist_rot)) 
           && (thrConf > min(probPreds$p))) && (epoch <= 15) ) {
        
        thrConf <- thrConf - taxa_de_perda # Reduzir limiar
        if(thrConf < min(probPreds$p)){
            thrConf <- min(probPreds$p)
        }
        #Rotular novamente
        novos_rotulados <- flexConC2(probPreds, prob_preds_superv, thrConf)
        classes_dist_rot <- unique(novos_rotulados$cl)
		epoch <- epoch + 1
    }
    return (novos_rotulados)
}

#2
prop <- function(rotulados,base_rotulados_ini){
    if(length(rotulados$id)){
        classes_dist_rot <- unique(rotulados$cl)  # Guarda as classes distintas
        qtd_classes_ini <- c(rep(0,length(classes_dist_rot)))
        qtd_classes_rot <- c(rep(0,length(classes_dist_rot)))
        names(qtd_classes_ini) <- classes_dist_rot            # Organiza os titulos de cada 
        names(qtd_classes_rot) <- classes_dist_rot            # posicao por classe
        
        for(cl in classes_dist_rot){                                            # Guarda a quantidade por classe 
            qtd_classes_ini[cl] <- length(which(base_rotulados_ini$class == cl))  # inicialmente rotuladas
            qtd_classes_rot[cl] <- length(which(rotulados$cl == cl)) # guarda a quantidade por classe rotulada
        }
        
        menorCL <- as.character(names(qtd_classes_rot[which(qtd_classes_rot == min(qtd_classes_rot))])) # Classe com menos rotulados
        if(length(menorCL) > 1)
            menorCL <- menorCL[1]
        
        proporcoes <- c(rep(0,length(classes_dist_rot))) # vetor com as proporcoes calculadas
        names(proporcoes) <- as.character(classes_dist_rot) # Organiza os titulos de cada posicao por classe
        
        pos <- 1
        epoch <- 1
        while ((pos <= length(classes_dist_rot)) && (epoch <= 50)){
            cl <- as.character(classes_dist_rot[pos])
            proporcoes[[cl]] <- as.numeric(format((qtd_classes_rot[[menorCL]]*qtd_classes_ini[[cl]])/qtd_classes_ini[[menorCL]],digits = 4)) # Regra de 3
            if(trunc(proporcoes[[cl]]) > trunc(qtd_classes_rot[[cl]])){ #Se o resultado passa do numero disponivel pra rotular, eh pq o anteror
                proporcoes[[cl]] <- as.numeric(format(qtd_classes_rot[[cl]],digits = 4))   #possui mais do que devia
                proporcoes[[menorCL]] <- as.numeric(format(qtd_classes_rot[[cl]]/(qtd_classes_ini[[cl]]/qtd_classes_ini[[menorCL]]),digits = 4))
                qtd_classes_rot[[menorCL]] <- as.numeric(format(qtd_classes_rot[[cl]]/(qtd_classes_ini[[cl]]/qtd_classes_ini[[menorCL]]),digits = 4))
                pos <- 1
            }else
                pos <- pos +1
            epoch <- epoch + 1
        }
        
        # Caso a proporcao para uma base fique entre 0 e 1, esta recebera 1, pois nao se pode adicionar classe pela metade
        for(pos in 1:length(proporcoes))
            if(proporcoes[pos] < 1)
                proporcoes[pos] <- 1
        
        return (trunc(proporcoes)) # O trunc passa a parte inteira do numero
    }
    return (c())
}

#3
estratificar_rot <- function(new,probPreds,proporcoes){
    add_prop <- c()
    if(length(proporcoes)){
        pos <- 1 # para controlar as posicoes dos ids a serem adicionados
        for(x in new){ # percorre os exemplos selecionados
            if(x %in% probPreds[,3]){ 
                indice <- which(probPreds[,3] == x)
                if((as.character(probPreds[indice,1]) %in% names(proporcoes))  && (proporcoes[[as.character(probPreds[indice,1])]] > 0)){
                    add_prop[pos] <- indice #indice do probPreds referente ao exemplo a ser rotulado
                    pos <- pos + 1
                    proporcoes[[as.character(probPreds[indice,1])]] <- proporcoes[[as.character(probPreds[indice,1])]] - 1
                }
            }
        }
    }
    return (add_prop)
}

# verificar se as classes são iguais. Pois pode haver o msm numero de classes, porém diferentes
analyzeClasses <- function(classes_dist_pp,classes_dist_rot){ 
    for(cl in classes_dist_pp){
        if(!(cl %in% classes_dist_rot)) #Caso alguma classe do probPreds não esteja nos rotulados
            return (TRUE) # Retorna TRUE para rotular novamente
    }
    return (FALSE)
}
# -- Stratified --

# FlexCon-C the base algorithm
flexConC <- function(base,base_original,base_rotulados_ini,learner, pred_func, min_exem_por_classe, limiar, method, stratified = FALSE) {
  # Initial setup, this is equal in all methods FlexCon-C1 and FlexCon-C2 
  form <- as.formula(paste("class", '~', '.'))
  data <- base
  thr_conf <- 0.95
  max_its <- 100
  verbose <- TRUE
  it <- 0
  N <- NROW(data)
  n_instancias_por_classe <- ddply(data, ~class, summarise, number_of_distinct_orders = length(class))
  n_classes <- NROW(n_instancias_por_classe) - 1
  qtd_exemplos_rot <- 0
  total_rot <- 0
  conj_treino <<- cleanVector(conj_treino)
  treino_valido <<- FALSE
  classificar <- TRUE
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  id_conj_treino <- cleanVector(id_conj_treino)
  id_conj_treino_antigo <- cleanVector(id_conj_treino_antigo)

  # FlexCon-C1 only
  if ((method == "1") || (method == "2")) {
    moda <- matrix(data = rep(0, getLength(base_original$class)), ncol = length(levels(base_original$class)),
                   nrow = NROW(base_original), byrow = TRUE, dimnames = list(row.names(base_original),
                   sort(levels(base_original$class), decreasing = FALSE)))
  }
  
  # FlexCon-C2 only
  add_rot_superv <- FALSE
  
  repeat {
    new_samples <- cleanVector(new_samples)
    acertou <- 0
    it = it + 1

    if (qtd_exemplos_rot > 0) {
      qtd_exemplos_rot = 0
      treino_valido <- validTraining(data, id_conj_treino, n_classes, min_exem_por_classe)
      classificar <- validClassification(treino_valido, id_conj_treino, id_conj_treino_antigo, data, n_classes, min_exem_por_classe)
      if(classificar) {
        acc_local <- calcLocalAcc()
        thr_conf <- newConfidence(acc_local, limiar, thr_conf)
      }
    }
    model <- generateModel(learner, form, data, sup)
    prob_preds <- generateProbPreds(pred_func, model, data, sup)
    
    switch (method,
            "1" = {
              moda <- storageSum(base_original,prob_preds, moda)
              rotulados <- flexConC1(prob_preds, thr_conf, moda, it)
              new_samples <- rotulados$id
            },
            "2" = {
              moda <- storageFashion(base_original,prob_preds, moda)
              rotulados <- flexConC1(prob_preds, thr_conf, moda, it)
              new_samples <- rotulados$id
            },
            "3" = {
              model_superv <- generateModel(learner, form, data, sup)
              prob_preds_superv <- generateProbPreds(pred_func, model_superv, data, sup)
              rotulados <- flexConC2(prob_preds, prob_preds_superv, thr_conf)
              new_samples <- rotulados$id
            }
    )
    
    if(stratified){
        if(method == "1" || method == "2") {
            rotulados <- tratar_dados_c1(rotulados,base_rotulados_ini,prob_preds,thr_conf,moda,it,taxa_de_perda = 0.2)
        } else {
            rotulados <- tratar_dados_c2(rotulados,base_rotulados_ini,prob_preds,prob_preds_superv,thr_conf,taxa_de_perda = 0.2)
        }
         ratio <- prop(rotulados,base_rotulados_ini) # Encontra as proporções para incluir os rotulos
         new_samples <- estratificar_rot(rotulados$id,prob_preds,ratio) # Selecionar os exemplos para incluir na base
    }
    
    if (length(new_samples)) {
      new_data <- data[(1:N)[-sup][new_samples], as.character(form[2])]
      
      if (add_rot_superv) {
        add_rot_superv <- FALSE
        new_data <- as.character(prob_preds_superv[new_samples, 1])
      } else {
        new_data <- as.character(prob_preds[new_samples, 1])
      }
      
      qtd_exemplos_rot <- getLength(new_data)
      total_rot <- total_rot + qtd_exemplos_rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new_samples], as.character(form[2])] == new_data
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto) {
        if (acerto[w] == TRUE) {
          acertou <<- acertou + 1
        }
      }
      id_conj_treino_antigo <- appendVectors(id_conj_treino_antigo, id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new_samples]
      
      sup <- c(sup, id_conj_treino)
      
    } else {
      thr_conf <- max(prob_preds[ , 2])
    }
    
    if ((it == max_its) || ((length(sup) / N) >= 1)) {
      break
    }
  }
  return(model)
}

flexConC1 <- function(prob_preds, thr_conf, moda, it) {
  if(it == 1) {
    prob_preds_1_it <<- prob_preds
    new_samples <- which(prob_preds[ , 2] >= thr_conf)
    rotulados <- data.frame(id = prob_preds[new_samples, 3], cl = prob_preds[new_samples, 1])
  } else { 
    rotulados <- classCheck(prob_preds_1_it, prob_preds, thr_conf)
    len_rotulados <- getLength(rotulados$id)
    if (len_rotulados == 0) {
      rotulados <- confidenceCheck(prob_preds_1_it, prob_preds, thr_conf)
      len_rotulados <- getLength(rotulados$id)
      if (len_rotulados == 0) {
        rotulados <- differentClassesCheck(prob_preds_1_it, prob_preds, thr_conf, moda)
        len_rotulados <- getLength(rotulados$id)
        if(len_rotulados == 0) {
          rotulados <- differentConfidencesCheck(prob_preds_1_it, prob_preds, thr_conf, moda)
        }
      }
    }
  }
    
    return (rotulados)
}

# FlexCon-C2 funtion
flexConC2 <- function(prob_preds, prob_preds_superv, thr_conf) {
  prob_preds <- convertProbPreds(prob_preds)
  prob_preds_superv <- convertProbPreds(prob_preds_superv)
  
  prob_preds_con <- (prob_preds[, 2] >= thr_conf)
  prob_preds_superv_con <- (prob_preds_superv[, 2] >= thr_conf)
  
  prob_preds_cl <- prob_preds[, 1]
  prob_preds_superv_cl <-  prob_preds_superv[, 1]
  
  new_samples <- which((prob_preds_con & prob_preds_superv_con) & (prob_preds_cl == prob_preds_superv_cl))
  if (length(new_samples) == 0) {
    new_samples <- which((prob_preds_con | prob_preds_superv_con) & (prob_preds_cl == prob_preds_superv_cl))
    if (length(new_samples) == 0) {
      new_samples <- which((prob_preds_con & prob_preds_superv_con) & (prob_preds_cl != prob_preds_superv_cl))
      if (length(new_samples)) {
        add_rot_superv <<- TRUE
      }
    }
  }
  rotulados <- data.frame(id = prob_preds[new_samples,3], cl = prob_preds[new_samples,1])
  return(rotulados)
}

# Calculate the acc and return
getAcc <- function(matrix, all) {
  acc <- ((sum(diag(matrix)) / all) * 100)
  return (acc)
}

# Get the length of the data
getLength <- function(n) {
  return (length(n))
}

# Generate the learner with the supervised samples
generateModel <- function(learner, form, data, sup) {
  model <- runLearner(learner, form, data[sup, ])
  return (model)
}

# Generate a matrix with the sample, class and confidence value
generateProbPreds <- function(pred_func, model, data, sup) {
  prob_preds <- do.call(pred_func, list(model, data[-sup, ]))
  return (prob_preds)
}

# Increment the acc vector with the new value
appendVectors <- function(v1, v2) {
  vector <- c(v1, v2)
  return (vector)
}

# Função void que inicia todas as variáveis globais do código
# Void Function to load all global variables of the code
initGlobalVariables <- function() {
  conj_treino <<- c()
  treinamento <<- c()
  
  # FlexCon-C1 variables
  it_g <<- c()
  bd_g <<- c()
  thrConf_g <<- c()
  nr_added_exs_g <<- c()
  tx_g <<- c()
  acc_g <<- c()
  acertou_g <<- c()
  
  # FlexCon-C2 variables
  it_g_3 <<- c()
  bd_g_3 <<- c()
  thrConf_g_3 <<- c()
  nr_added_exs_g_3 <<- c()
  tx_g_3 <<- c()
  acc_g_3 <<- c()
  acertou_g_3 <<- c()
  
  grad_g <<- c()
  bd <<- c()
  tx <<- c()
}

# Install packages if it not installed and load
installNeedPacks <- function() {
  packages <- c("caret", "ssc", "plyr", "DMwR", "DMwR2", "caTools", "RWeka", "rJava", "rminer", "datasets", "e1071",
                "ggplot2")
  for (pack in packages) {
    if (!require(pack, character.only = TRUE)) {
      install.packages(pack)
    }
    library(pack, character.only = TRUE)
  }
}

setDatabases <- function(){
    base_vector <- c("iris","bupa","segment","waveform-5000","phishingData","haberman","mushroom","pima","vehicle",
                      "wilt","kr-vs-kp","blood-transfusion-service","cnae-9","connectionist-mines-vs-rocks","flare",
                      "indian-liver-patient","leukemia-haslinger","mammographic-mass","mfeat-karhunen","musk",
                      "ozone-onehr","pendigits","planning-relax","seeds","semeion","spectf-heart","tic-tac-toe",
                      "twonorm","hill-valley-with-noise","balance-scale","car")
    return (base_vector)
}

readDatabase <- function(base,format){
    setwd("../bases")
    bd <- paste(base,format, sep = "")
    base_original <- read.arff(bd);  
    setwd("../selftrain_inclusaoProporcional/")
    return (base_original)
}

# Return the new confidence value changed by the cr value
newConfidence <- function(acc_local, limiar, tx_conf) {
  if ((acc_local > (limiar + 1)) && ((tx_conf - cr) > 0.0)) {
    tx_conf <- tx_conf - cr
  } else if ((acc_local < (limiar - 1)) && ((tx_conf + cr) <= 1)) {
    txConf <- tx_conf + cr
  }
  return (tx_conf)
}

# Return the acc of the current db
supAcc <- function(cl, base_rotulados_ini, base_teste){
  std <- supModel(cl, base_rotulados_ini)
  matriz_confusao_supervisionado <- confusionMatrix(std,base_teste)
  acc_sup_3 <- getAcc(matriz_confusao_supervisionado, matriz_confusao_supervisionado)
  return(acc_sup_3)
}

# Return a supervised classifier
supModel <- function(cl, base_rotulados_ini){
  switch (cl,
          "naiveBayes" = std <- naiveBayes(as.formula(paste("class", '~', '.')), base_rotulados_ini),
          "rpartXse" = std <- rpartXse(as.formula(paste("class", '~', '.')), base_rotulados_ini, se = 0.5),
          "JRip" = std <- JRip(as.formula(paste("class", '~', '.')), base_rotulados_ini),
          "IBk" = std <- IBk(as.formula(paste("class", '~', '.')), base_rotulados_ini,
                             control = Weka_control(K = as.integer(sqrt(nrow(base_rotulados_ini))), X = TRUE))
  )
  return(std)
}

# Check if the classification if valid. In this function the data need to have a min samples of each class or i need to join the current samples in data with an old data
validClassification <- function(treino_valido_i, id_conj_treino, id_conj_treino_antigo, data, N_classes, min_exem_por_classe) {
  if (treino_valido_i) {
    conj_treino <<- data[id_conj_treino, ]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
  } else if (length(id_conj_treino_antigo) >= 1) {
    conj_treino <<- rbind(data[id_conj_treino, ], data[id_conj_treino_antigo, ])
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validTraining(data, id_conj_treino1, N_classes, min_exem_por_classe)
    if (treino_valido) {
      classificar <- TRUE
    } else {
      classificar <- FALSE
    }
  } else {
    classificar <- FALSE
  }
  return(classificar)  
}

# Chech if i have a min accetable samples per class
validTraining <- function(data, id_conj_treino, N_classes, min_exem_por_classe) {
  n_instancias_por_classe2 <- ddply(data[id_conj_treino, ], ~class, summarise, number_of_distinct_orders = length(class))
  treino_valido <- FALSE
  if (NROW(n_instancias_por_classe2) == N_classes) {
    for (x in 1:NROW(n_instancias_por_classe2)) {
      if (n_instancias_por_classe2$number_of_distinct_orders[x] >= min_exem_por_classe) {
        treino_valido <- TRUE
      } else {
        treino_valido <- FALSE
        return (treino_valido)
      }
    }
    return (treino_valido)
  }
  return (treino_valido)
}


# Create the base name of the output archives
output_archive <- function(cr, cl, medias_accuracy, algorithm) {

    archive <- c()
    switch (algorithm,
            "1" = {
                # FlexCon-C1 (s)
                archive <- paste("flexcon_c1_S_", cl, "_", cr, extention, sep = "")
            },
            "2" = {
                # FlexCon-C1 (v)
                archive <- paste("flexcon_c1_V_", cl, "_", cr, extention, sep = "")
            },
            "3" = {
                # FlexCon-C2
                archive <- paste("flexcon_c2_", cl, "_", cr, extention, sep = "")
            },
            "4" = {
                # FlexCon-C1S (s)
                archive <- paste("flexcon_c1S_S_", cl, "_", cr, extention, sep = "")
            },
            "5" = {
                # FlexCon-C1S (v)
                archive <- paste("flexcon_c1S_V_", cl, "_", cr, extention, sep = "")
            },
            "6" = {
                # FlexCon-C2S
                archive <- paste("flexcon_c2S_", cl, "_", cr, extention, sep = "")
            }
    )
    
    table_result <- matrix(medias_accuracy, ncol = 5, byrow = TRUE)
    write_archive(archive, table_result)
}

# Write in the output file the content
write_archive <- function(title, content){
  write.csv(content, title, row.names = FALSE)
}
