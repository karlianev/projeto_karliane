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
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1]) == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
confidenceCheck <- function(data_1_it, data_x_it, thr_conf) {
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((data_1_it[lvls[indice], 1] == data_x_it[indice, 1])) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf) || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
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
  xid <- c()
  ycl <- c()
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
differentConfidencesCheck <- function(data_1_it, data_x_it, thr_conf) {
  pos <- 0
  xid <- c()
  ycl <- c()
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
storageFashion <- function(prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for (y in 1:(length(dist_classes))) {
      if (prob_preds[x, 1] == dist_classes[y]) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

# Storage the sum of the confidence for each iteration
storageSum <- function(prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for (y in 1:length(dist_classes)) { 
      if (prob_preds[x, 1] == dist_classes[y]) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + prob_preds[x, 2]
        break
      }
    }
  }
  return (moda)
}

# Calculate the acc value of the training samples
calcLocalAcc <- function() {
  if (c==1) {
    classificador <- naiveBayes(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  } else if (c==2) {
    classificador <- rpartXse(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini, type="class"), base_rotulados_ini$class)        
  } else if (c==3) {
    classificador <- JRip(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)        
  } else if (c==4) {
    classificador <- IBk(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini), base_rotulados_ini$class)
  }
  acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
  return(acc_local)
}

# Return the confusion matrix
confusionMatrix <- function(model) {
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

# FlexCon-C the base algorithm
flexConC <- function(learner, pred_func, min_exem_por_classe, limiar, method) {
  # Initial setup, this is equal in all methods
  form <- as.formula(paste(classe, '~', '.'))
  data <- base_treino_self_training
  thr_conf <- 0.95
  max_its <- 100
  verbose <- TRUE
  it <- 0
  
  # FlexCon-C1 and FlexCon-C2 
  N <- NROW(data)
  n_instancias_por_classe <- ddply(data, ~class, summarise, number_of_distinct_orders = length(class))
  n_classes <- NROW(n_instancias_por_classe) - 1
  qtd_exemplos_rot <- 0
  total_rot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()

  # FlexCon-C1 only
  if ((method == "1") || (method == "2")) {
    moda <- matrix(data = rep(0, length(base_original$class)), ncol = length(unique(base_original$class)),
                   nrow = NROW(base_original), byrow = TRUE, dimnames = list(row.names(base_original),
                   sort(unique(base_original$class), decreasing = FALSE)))
  }
  
  # FlexCon-C2 only
  add_rot_superv <- FALSE
  
  repeat {
    new_samples <- cleanVector(new_samples)
    acertou <- 0
    it = it + 1
    cat("Iteração", it)
    
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
              moda <- storageSum(prob_preds, moda)
              new_samples <- flexConC1(prob_preds, thr_conf, moda, it)
            },
            "2" = {
              moda <- storageFashion(prob_preds, moda)
              new_samples <- flexConC1(prob_preds, thr_conf, moda, it)
            },
            "3" = {
              model_superv <- generateModel(learner, form, data, sup)
              prob_preds_superv <- generateProbPreds(pred_func, model_superv, data, sup)
              new_samples <- flexConC2(prob_preds, prob_preds_superv, thr_conf)
            }
    )
    if (length(new_samples)) {
      new_data <- data[(1:N)[-sup][new_samples], as.character(form[2])]
      
      if (add_rot_superv) {
        add_rot_superv <- FALSE
        new_data <- as.character(prob_preds_superv[new_samples, 1])
      } else {
        new_data <- as.character(prob_preds[new_samples, 1])
      }
      
      qtd_exemplos_rot <- length(new_data)
      total_rot <- total_rot + qtd_exemplos_rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new_samples], as.character(form[2])] == new_data
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto) {
        if (acerto[w] == TRUE) {
          acertou <<- acertou + 1
        }
      }
      id_conj_treino_antigo <- c(id_conj_treino_antigo, id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new_samples]
      
      sup <- c(sup, id_conj_treino)
      
      if(length(new_samples) == 0) {
        thr_conf <- max(prob_preds[ ,2])
      }
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
    new_samples <- which(prob_preds[, 2] >= thr_conf)
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
          rotulados <- differentConfidencesCheck(prob_preds_1_it, prob_preds, thr_conf)
        }
      }
    }
  }
  new_samples <- rotulados$id
  return (new_samples)
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
  return(new_samples)
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
incrementAcc <- function(old_acc, acc) {
  new_acc <- c(old_acc, acc)
  return (new_acc)
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
supAcc <- function(cl){
  std <- supModel(cl)
  matriz_confusao_supervisionado <- confusionMatrix(std)
  acc_sup_3 <- getAcc(matriz_confusao_supervisionado, matriz_confusao_supervisionado)
  return(acc_sup_3)
}

# Return a supervised classifier
supModel <- function(cl){
  switch (cl,
          "naiveBayes" = std <- naiveBayes(as.formula(paste(classe, '~', '.')), base_rotulados_ini),
          "rpartXse" = std <- rpartXse(as.formula(paste(classe, '~', '.')), base_rotulados_ini, se=0.5),
          "JRip" = std <- JRip(as.formula(paste(classe, '~', '.')), base_rotulados_ini),
          "IBk" = std <- IBk(as.formula(paste(classe, '~', '.')), base_rotulados_ini, control = Weka_control(K = 15,
                                                                                                             X = TRUE))
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
output_archive <- function(cr, cl, acc_c1_s, acc_c1_v, acc_c2) {
  flexcon_c1_s <- c()
  flexcon_c1_v <- c()
  flexcon_c2 <- c()
  
  flexcon_c1_s <- paste("flexcon_c1_S_", cl, "_", cr, extention, sep = "")
  flexcon_c1_v <- paste("flexcon_c1_V_", cl, "_", cr, extention, sep = "")
  flexcon_c2 <- paste("flexcon_c2_", cl, "_", cr, extention, sep = "")
  
  acc_flexcon_c1_s <- matrix(acc_c1_s, ncol = 5, byrow = TRUE)
  acc_flexcon_c1_v <- matrix(acc_c1_v, ncol = 5, byrow = TRUE)
  acc_flexcon_c2 <- matrix(acc_c2, ncol = 5, byrow = TRUE)

  write_archive(flexcon_c1_s, acc_flexcon_c1_s)
  write_archive(flexcon_c1_v, acc_flexcon_c1_v)
  write_archive(flexcon_c2, acc_flexcon_c2)
}

# Write in the output file the content
write_archive <- function(title, content){
  write.csv(content, title, row.names = FALSE)
}