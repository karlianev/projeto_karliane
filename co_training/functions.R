#' @description Increment the acc vector with the new value.
#'
#' @param v1 first vector.
#' @param v2 second vector.
#'
#' @return A new vector with the combination of the two orther vectors
#'
appendVectors <- function(v1, v2) {
  return (c(v1, v2))
}

#o valor de K do k-nn igual a raiz da quantidade de exemplos da base de dados
attKValue <- function(database) {
  listas <- list(control = Weka_control(K = as.integer(sqrt(nrow(database))), X = TRUE))
  obj[4] <<- c(learner(classifiers[4], listas))
}

# Calculate the acc value of the training samples
calcLocalAcc <- function() {
  if (c == 1) {
    classificador <- naiveBayes(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini),
                    base_rotulados_ini$class)
  } else if (c == 2) {
    classificador <- rpartXse(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini, type="class"),
                    base_rotulados_ini$class)
  } else if (c == 3) {
    classificador <- JRip(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini),
                    base_rotulados_ini$class)
  } else if (c == 4) {
    classificador <- IBk(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini),
                    base_rotulados_ini$class)
  }
  acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
  return(acc_local)
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
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

#' @description This function provide an easy way to clean the any variable.
#'
#' @usage cleanVector(x)
#'
#' @param x a variable should be clean.
#'
#' @return a clean variable.
#'
#' @examples
#' x <- cleanVector(x)
cleanVector <- function(x) {
  x <- c()
  return (x)
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
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# Return the confusion matrix
confusionMatrix <- function(model, base_teste) {
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

# FunÃ§Ã£o para definir constantes ao longo do cÃ³digo
# Function to define constants in all code
defines <- function() {
  classe <<- "class"
  classifiers <<- c("naiveBayes", "rpartXse", "JRip", "IBk")
  change_rate <<- c(2:8)
  extention <<- ".csv"
  funcs <<- c('func', 'f', 'f2', 'f2')
  obj <<- c(learner(classifiers[1], list()),
            learner(classifiers[2], list(se = 0.5)),
            learner(classifiers[3], list()),
            learner(classifiers[4], list(control = Weka_control(K = 3,
                                                                X = TRUE))))
}

# Check in both matrixes if both confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentClassesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
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
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- searchClass(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

f <- function(m, d) { #arg 2 - árvore de decisão
  p <- predict(m, d, type = 'prob')
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
}

f2 <- function(m, d) { #arg3 e 4 - Jrip e IBK
  p <- predict(m, d, type = 'probability')
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
}

# FlexCon-C the base algorithm
flexConC <- function(learner, pred_func, min_exem_por_classe, limiar, method) {
  # Initial setup, this is equal in all methods FlexCon-C1 and FlexCon-C2
  form <- as.formula(paste(classe, '~', '.'))
  data <- base
  thr_conf <- 0.95
  max_its <- 100
  verbose <- TRUE
  it <- 0
  N <- NROW(data)
  n_instancias_por_classe <- ddply(data, ~class, summarise,
                                   number_of_distinct_orders = length(class))
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
      treino_valido <- validTraining(data, id_conj_treino, n_classes,
                                     min_exem_por_classe)
      classificar <- validClassification(treino_valido, id_conj_treino,
                                         id_conj_treino_antigo, data, n_classes,
                                         min_exem_por_classe)
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
              prob_preds_superv <- generateProbPreds(pred_func, model_superv,
                                                     data, sup)
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
      qtd_exemplos_rot <- getLength(new_data)
      total_rot <- total_rot + qtd_exemplos_rot
      acertou <- 0
      acerto <- (treinamento[(1:N)[-sup][new_samples], as.character(form[2])]
                 == new_data)
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto) {
        if (acerto[w] == TRUE) {
          acertou <<- acertou + 1
        }
      }
      id_conj_treino_antigo <- appendVectors(id_conj_treino_antigo,
                                             id_conj_treino)
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
    rotulados <- data.frame(id = prob_preds[new_samples, 3],
                            cl = prob_preds[new_samples, 1])
  } else {
    rotulados <- classCheck(prob_preds_1_it, prob_preds, thr_conf)
    len_rotulados <- getLength(rotulados$id)
    if (len_rotulados == 0) {
      rotulados <- confidenceCheck(prob_preds_1_it, prob_preds, thr_conf)
      len_rotulados <- getLength(rotulados$id)
      if (len_rotulados == 0) {
        rotulados <- differentClassesCheck(prob_preds_1_it, prob_preds,
                                           thr_conf, moda)
        len_rotulados <- getLength(rotulados$id)
        if(len_rotulados == 0) {
          rotulados <- differentConfidencesCheck(prob_preds_1_it, prob_preds,
                                                 thr_conf, moda)
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
  new_samples <- which((prob_preds_con & prob_preds_superv_con)
                       & (prob_preds_cl == prob_preds_superv_cl))
  if (length(new_samples) == 0) {
    new_samples <- which((prob_preds_con | prob_preds_superv_con)
                         & (prob_preds_cl == prob_preds_superv_cl))
    if (length(new_samples) == 0) {
      new_samples <- which((prob_preds_con & prob_preds_superv_con)
                           & (prob_preds_cl != prob_preds_superv_cl))
      if (length(new_samples)) {
        add_rot_superv <<- TRUE
      }
    }
  }
  return(new_samples)
}

func <- function(m, d) { #arg1 - naive
  p <- predict(m, d, type = "raw")
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p, 1, which.max)]
  col2 <- apply(p, 1, max)
  data.frame(cl = col1, p = col2, id = row.names(d))
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

# Calculate the acc and return
getAcc <- function(matrix, all) {
  acc <- ((sum(diag(matrix)) / all) * 100)
  return (acc)
}

getDatabase <- function(pos) {
  databases <- c("iris", "bupa", "segment", "waveform-5000", "phishingData",
                 "mushroom", "pima", "vehicle", "wilt",
                 "kr-vs-kp", "blood-transfusion-service", "cnae-9",
                 "connectionist-mines-vs-rocks", "flare",
                 "indian-liver-patient", "leukemia-haslinger",
                 "mammographic-mass", "mfeat-karhunen", "musk",
                 "ozone-onehr", "pendigits", "planning-relax", "seeds",
                 "semeion", "spectf-heart", "tic-tac-toe", "twonorm",
                 "hill-valley-with-noise", "balance-scale", "car")

# haberman deixou de rodar e foi retirada
#    databases <- c("iris", "bupa", "segment", "waveform-5000", "phishingData",
#                  "haberman", "mushroom", "pima", "vehicle", "wilt",
#                  "kr-vs-kp", "blood-transfusion-service", "cnae-9",
#                  "connectionist-mines-vs-rocks", "flare",
#                  "indian-liver-patient", "leukemia-haslinger",
#                  "mammographic-mass", "mfeat-karhunen", "musk",
#                  "ozone-onehr", "pendigits", "planning-relax", "seeds",
#                  "semeion", "spectf-heart", "tic-tac-toe", "twonorm",
#                  "hill-valley-with-noise", "balance-scale", "car")
  
  database <- paste(databases[pos], "arff", sep = ".")
  base_original <- read.arff(paste("../bases", database, sep = "/"))
  bd_nome <<- databases[pos]
  return (base_original)
}

# Get the length of the data
getLength <- function(tamanho) {
  return (length(tamanho))
}

#' @description Void Function to load all global variables of the code
#'
initGlobalVariables <- function() {
  conj_treino <<- c()
  treinamento <<- c()
  acc_c1_s <<- c()
  acc_c1_v <<- c()
  acc_c2 <<- c()
  # # FlexCon-C1 variables
  # it_g <<- c()
  # bd_g <<- c()
  # thrConf_g <<- c()
  # nr_added_exs_g <<- c()
  # tx_g <<- c()
  # acc_g <<- c()
  # acertou_g <<- c()
  # # FlexCon-C2 variables
  # it_g_3 <<- c()
  # bd_g_3 <<- c()
  # thrConf_g_3 <<- c()
  # nr_added_exs_g_3 <<- c()
  # tx_g_3 <<- c()
  # acc_g_3 <<- c()
  # acertou_g_3 <<- c()
  # grad_g <<- c()
  # bd <<- c()
  # tx <<- c()
  # Self-Training
  it_g_o <<- c()
  bd_g_o <<- c()
  thrConf_g_o <<- c()
  nr_added_exs_g_o <<- c()
  tx_g_o <<- c()
  acertou_g_o <<- c()
}

#' @description This function set the class atribute to NA without change the
#' class of selected samples
#'
#' @usage newBase(base_rotulada, ids_treino_rot)
#'
#' @param base_rotulada the full dataset without changes
#' @param ids_treino_rot the vector with the selected samples
#'
#' @return a new dataset with some percents of the samples have the NA in class
#' atribute
#'
#' @examples
#' data(iris)
#'
#' H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100),
#' mode = "stratified")
#' base <- newBase(base_rotulada_treino, ids_treino_rot)
#' ids_treino_rot <- H2$tr
#'
#' @seealso rminer.holdout
newBase <- function(base_rotulada, ids_treino_rot){
  base_rotulada[- ids_treino_rot, "class"] <- NA
  return (base_rotulada)
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

#' @description Create the base name of the output archives and call the write
#' function.
#'
#' @param cr current change rate.
#' @param cl current classifier.
#' @param acc_c1_s acc of the Flexcon-c1 (s) method.
#' @param acc_c1_v acc of the Flexcon-c1 (v) method.
#' @param acc_c2 acc of the Flexcon-c2 method.
#' @param acc_self acc of the Self-Training method.
#'
outputArchive <- function(cr, cl, nome_acc, acc_c1_s, acc_c1_v, acc_c2, acc_self) {
  # flexcon_c1_s <- paste("flexcon_c1_S_", cl, "_", cr, extention, sep = "")
  # flexcon_c1_v <- paste("flexcon_c1_V_", cl, "_", cr, extention, sep = "")
  # flexcon_c2 <- paste("flexcon_c2_", cl, "_", cr, extention, sep = "")
  self_training <- paste("co_training_", cl, "_", nome_acc, "_", cr, extention, sep = "")

  # acc_flexcon_c1_s <- matrix(acc_c1_s, ncol = 5, byrow = TRUE)
  # acc_flexcon_c1_v <- matrix(acc_c1_v, ncol = 5, byrow = TRUE)
  # acc_flexcon_c2 <- matrix(acc_c2, ncol = 5, byrow = TRUE)
  acc_self_training <- matrix(acc_self, ncol = 5, byrow = FALSE)

  row <- rep(bd_nome, 10)
  # writeArchive(flexcon_c1_s, acc_flexcon_c1_s)
  # writeArchive(flexcon_c1_v, acc_flexcon_c1_v)
  # writeArchive(flexcon_c2, acc_flexcon_c2)
  writeArchive(self_training, acc_self_training, row = row)
}

#funcao que cria duas visoes para serem usadas no treinamento do co-training
criar_visao <- function(dados){
  col <- round((ncol(dados)-1) / 2)
  col1 <- as.integer((ncol(dados)-1) / 2)
    if ((col + col1) < (ncol(dados)-1)){
    col <- col + 1
  }
  xl <- dados[,1:ncol(dados)-1] #a base dados sem os rotulos
  yl <- dados[-(1:ncol(dados)-1)] #rotulos da base 
  view <- partition.matrix(xl, sep = length(dados), rowsep = nrow(dados), colsep = c(col,col1))
  data1 <- data.frame(view$`1`$`1`,yl)
  data2 <- data.frame(view$`1`$`2`,yl)
  visoes <- list(data1,data2)
  return(visoes)
}

# Function co-Training original (w/ fix threshold)
#@param metodo - 1 = co-training original (k=10%)
#                2 = co-training baseado no metodo de Felipe (k=limiar)
#                3 = co-training gradativo (k=limiar que diminui 5% a cada iteracao)
coTrainingOriginal <- function (learner, predFunc, data1, data2, metodo, k_fixo = T) {
  if (metodo==2){ #original igual ao de felipe
    k_fixo <- F
  }else if (metodo==3){ #gradativo
    k_fixo <- F
    qtd_Exemplos_Rot <- 0
    gradativo <- 0.05
  }
  form <- as.formula(paste(classe,'~', '.'))
  k <- 5
  # k <- 10
  # k <- 0.05
  # k <- 0.1
  thrConf <- 0.95
  maxIts <- 100
  verbose <- T
  N <- NROW(data1)
  it <- 0
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  repeat {
    new_samples1 <- cleanVector(new_samples1)
    new_samples2 <- cleanVector(new_samples2)
    acertou <- 0
    it <- it + 1
    
    if (metodo == 3){ #gradativo
      if ((it>1)&&(qtd_Exemplos_Rot>0)){
        thrConf <- (thrConf - gradativo)
        if (thrConf <= 0.0) thrConf <- (thrConf + gradativo)
      }
      qtd_Exemplos_Rot <- 0
    }
    
    model1 <- generateModel(learner, form, data1, sup1)
    model2 <- generateModel(learner, form, data2, sup2)
    probPreds1 <- generateProbPreds(predFunc, model1, data1, sup1)
    probPreds2 <- generateProbPreds(predFunc, model2, data2, sup2)

    if (k_fixo) { 
      #NAO VAMOS USAR ESSE K
      #quanidade de atributos = ao valor de K definido no inicio da funcao
      # qtd_add <- min(k,nrow(probPreds1)) # tamanho do probpreds1=probpreds2
      #quanidade de atributos = 10% do conjunto nao rotulado      
      qtd_add <- as.integer(nrow(probPreds1)*0.1)
      if ((nrow(probPreds1)>=1) && (qtd_add<1)){
        qtd_add <- 1
      }
    }
    else {
      #co-training adaptado para funcionar igual ao self-training de Felipe
      qtd_add <- min(length(which(probPreds1[, 2] > thrConf)), length(which(probPreds2[, 2] > thrConf)))
    }
    #criando os vetores em ordem decrescente pela confianca
    probPreds1_ordenado <- order(probPreds1$p, decreasing = T)
    probPreds2_ordenado <- order(probPreds2$p, decreasing = T)

    new_samples1 <- probPreds1[probPreds1_ordenado[1:qtd_add], 3]
    new_samples2 <- probPreds2[probPreds2_ordenado[1:qtd_add], 3]

    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf_g_o <<- c(thrConf_g_o, thrConf)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, qtd_add)
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if ((length(new_samples1)) && (length(new_samples2))) {
      new_data1 <- data1[(1:N)[-sup1][new_samples2], as.character(form[[2]])]
      new_data2 <- data2[(1:N)[-sup2][new_samples1], as.character(form[[2]])]
      
      new_data1 <- as.character(probPreds2[probPreds2_ordenado[1:qtd_add], 1])
      new_data2 <- as.character(probPreds1[probPreds1_ordenado[1:qtd_add], 1])
      
    
      #acertou <- 0
      #acerto <- (treinamento[(1:N)[-sup][new_samples], as.character(form[2])] == new_data)
      
      #acertou <- length(which(acerto == T))

      sup1 <- c(sup1, new_samples2)
      sup2 <- c(sup2, new_samples1)
      

      acertou_g_o <<- c(acertou_g_o, acertou)
    }
    else {
      acertou <- 0
      acertou_g_o <<- c(acertou_g_o, acertou)
      break
    }
    
    if (metodo ==3){#gradativo
      if(length(new_samples1)==0){ #se o 1 for zero o 2 tbm será
        thrConf<-min(max(probPreds1[,2]), max(probPreds2[,2]))
      }
      
    }
    if ((it == maxIts) || ((length(sup1) / N) >= 1) || ((length(sup2) / N) >= 1) ) {
      break
    }
  }
  model <- list(model1, model2)
  return (model)
}

# Function co-Training Gradativo (w/ fix threshold)
coTrainingGradativo <- function (learner, predFunc, k_fixo = T) {
  form <- as.formula(paste(classe,'~', '.'))
  k <- 5
  # k <- 10
  # k <- 0.05
  # k <- 0.1
  data <- base
  thrConf <- 0.95
  maxIts <- 100
  verbose <- T
  N <- NROW(data)
  
##dif. origin.  
  qtd_Exemplos_Rot <- 0
  gradativo <- 0.05
##dif. origin.    
  
  #primeiramente se faz necessario particionar os dados, ou seja, criar duas visoes
  visao <- criar_visao(data)
  data1 <- visao[[1]]
  data2 <- visao[[2]]
  
  it <- 0
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  repeat {
    new_samples1 <- cleanVector(new_samples1)
    new_samples2 <- cleanVector(new_samples2)
    acertou <- 0
    it <- it + 1
    
##dif. origin.        
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      thrConf <- (thrConf - gradativo)
      if (thrConf <= 0.0) thrConf <- (thrConf + gradativo)
    }
    qtd_Exemplos_Rot <- 0
##dif. origin.        
    
    model1 <- generateModel(learner, form, data1, sup1)
    model2 <- generateModel(learner, form, data2, sup2)
    probPreds1 <- generateProbPreds(predFunc, model1, data1, sup1)
    probPreds2 <- generateProbPreds(predFunc, model2, data2, sup2)
    
    if (k_fixo) { 
      qtd_add <- k
      # qtd_add <- as.integer(nrow(probPreds1)*0.1)
    }
    else {
      qtd_add <- min(length(which(probPreds1[, 2] > thrConf)), length(which(probPreds2[, 2] > thrConf)))
    }

    #criando os vetores em ordem decrescente pela confian?a
    probPreds1_ordenado <- order(probPreds1$p, decreasing = T)
    probPreds2_ordenado <- order(probPreds2$p, decreasing = T)
    
    new_samples1 <- as.integer(rownames(probPreds1[probPreds1_ordenado[1:qtd_add], ]))
    new_samples2 <- as.integer(rownames(probPreds2[probPreds2_ordenado[1:qtd_add], ]))
    
    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf_g_o <<- c(thrConf_g_o, thrConf)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, qtd_add)
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if ((length(new_samples1)) && (length(new_samples2))) {
      new_data1 <- data1[(1:N)[-sup1][new_samples2], as.character(form[[2]])]
      new_data2 <- data2[(1:N)[-sup2][new_samples1], as.character(form[[2]])]
      
      new_data1 <- as.character( probPreds2[new_samples2, 1])
      new_data2 <- as.character( probPreds1[new_samples1, 1])
      
      #acertou <- 0
      #acerto <- (treinamento[(1:N)[-sup][new_samples], as.character(form[2])] == new_data)
      
      #acertou <- length(which(acerto == T))
      
      sup1 <- c(sup1, (1:N)[-sup1][new_samples2])
      sup2 <- c(sup2, (1:N)[-sup2][new_samples1])
      
      acertou_g_o <<- c(acertou_g_o, acertou)
    }
    else {
      acertou <- 0
      acertou_g_o <<- c(acertou_g_o, acertou)
      break
    }
##dif. origin.
    if(length(new)==0){
      thrConf<-min(max(probPreds1[,2]), max(probPreds2[,2]))
    }
##dif. origin.    
    
    if ((it == maxIts) || ((length(sup1) / N) >= 1) || ((length(sup2) / N) >= 1) ) {
      break
    }
  }
  model <- list(model1, model2)
  return (model)
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
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

storagePred <- function(predic, iterac) {
  if (iterac == 1) {
    soma <<- predic
    cat("criar vetor com o voto e a soma")
  } else {
    cat("incrementar o voto e a soma")
  }
}

# Storage the sum of the confidence for each iteration
storageSum <- function(prob_preds, moda) {
  dist_classes <- unique(base_original$class)
  for (x in 1:NROW(prob_preds)) {
    id <- as.character(prob_preds[x, ncol(prob_preds)])
    for (y in 1:length(dist_classes)) {
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]]
                                      + prob_preds[x, 2]
        break
      }
    }
  }
  return (moda)
}


#' @description Make a supervised model and get the accuracy of this.
#'
#' @param cl the choosen classifier
#' @param base_rotulados_ini the dataset with the initial samples labeled.
#' @param baase_tst base de teste = dados inicialmente rotulados.
#'
#' @return Return the accuracy of the dataset with the initial samples
#' labeled.
#'
supAcc <- function(cl, base_rotulados_ini, base_tst){
  std <- supModel(cl, base_rotulados_ini)
  matriz_confusao_supervisionado <- confusionMatrix(std, base_tst)
  # acc_sup_3 <- getAcc(matriz_confusao_supervisionado, sum(matriz_confusao_supervisionado))
  return (getAcc(matriz_confusao_supervisionado, sum(matriz_confusao_supervisionado)))
}

#' @description A supervised model trained with the initial samples.
#'
#' @param cl the classifier to be used.
#' @param base_rotulados_ini the dataset with the initial samples labeled.
#'
#' @return Return a supervised classifier.
#'
supModel <- function(cl, base_rotulados_ini){
  switch (cl,
          "naiveBayes" = std <- naiveBayes(as.formula(paste(classe, '~', '.')),
                                           base_rotulados_ini),
          "rpartXse" = std <- rpartXse(as.formula(paste(classe, '~', '.')),
                                       base_rotulados_ini, se = 0.5),
          "JRip" = std <- JRip(as.formula(paste(classe, '~', '.')),
                               base_rotulados_ini),
          "IBk" = std <- IBk(as.formula(paste(classe, '~', '.')),
                             base_rotulados_ini,
                             control = Weka_control(K = as.integer(sqrt(
                               nrow(base_rotulados_ini))), X = TRUE))
  )
  return(std)
}

#' @description Check if the classification if valid.
#'
#' @param treino_valido_i boolean for check if it's a valid train.
#' @param in_conj_treino vector with the samples to train.
#' @param id_conj_treino_antigo old vector whit the samples to train.
#' @param data the dataset with all samples.
#' @param N_classes the total of the classes in the dataset.
#' @param min_exem_por_classe the min samples of each class for training.
#'
#' @return a boolean to say if the classification is valid.
#'
validClassification <- function(treino_valido_i, id_conj_treino,
                                id_conj_treino_antigo, data, N_classes,
                                min_exem_por_classe) {
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

#' @description Check if exists a min accetable samples per class.
#'
#' @param data the all dataset.
#' @param id_conj_treino vector with the samples selectedes to train.
#' @param Nclasses the total of the classes in the dataset.
#' @param min_exem_por_classe the min samples of each class for training.
#'
#' @return a boolean to say if the training is valid.
#'
validTraining <- function(data, id_conj_treino, Nclasses, min_exem_por_classe) {
  exemplos_classe <- ddply(data[id_conj_treino, ], ~class, summarise,
                                    number_of_distinct_orders = length(class))
  treino_valido <- FALSE
  if (NROW(exemplos_classe) == Nclasses) {
    for (x in 1:NROW(exemplos_classe)) {
      if (exemplos_classe$number_of_distinct_orders[x] >= min_exem_por_classe) {
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

whichDB <- function(pattern) {
  tryCatch({  
    file <- list.files(pattern = pattern)
    bd <- readFile(file)
    return ((nrow(bd) / 10) + 1)
  }, 
  error = function(setIniBd){return(1)})
}
