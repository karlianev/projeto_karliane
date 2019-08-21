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

appendDataFrame <- function(v1, v2) {
  return (rbind(v1, v2))
}

#o valor de K do k-nn igual a raiz da quantidade de exemplos da base de dados
attKValue <- function(database) {
  listas <- list(control = Weka_control(K = as.integer(sqrt(nrow(database))), X = TRUE))
  obj[4] <<- c(learner(classifiers[4], listas))
}

# Calculate the acc value of the training samples
#add parametro para passar as 2 visoes do co-training
calcLocalAcc <- function(base_rotulados_ini, conj_treino) {
  if (args == 1) { #esta dando erro nessa linha variavel c nao existe
    classificador <- naiveBayes(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini),
                    base_rotulados_ini$class)
  } else if (args == 2) {
    classificador <- rpartXse(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini, type="class"),
                    base_rotulados_ini$class)
  } else if (args == 3) {
    classificador <- JRip(as.factor(class) ~ ., conj_treino)
    matriz <- table(predict(classificador, base_rotulados_ini),
                    base_rotulados_ini$class)
  } else if (args == 4) {
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
  zp <- cleanVector(zp) #confianca na predicao
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(data_x_it[indice, 1])
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
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
  zp <- cleanVector(zp)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         == as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(data_x_it[indice, 1])
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
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

# Função para definir constantes ao longo do código
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
  zp <- cleanVector(zp)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(searchClass(xid[pos], moda))
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
  return (examples)
}

# Check in both matrixes if both confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentClassesCheckC2 <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  zp <- cleanVector(zp)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          && (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(data_1_it[lvls[indice], 1])
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
  return (examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentConfidencesCheck <- function(data_1_it, data_x_it, thr_conf, moda) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  zp <- cleanVector(zp)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(searchClass(xid[pos], moda))
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
  return (examples)
}

# Check in both matrixes if one of confidences values are higger than thr_conf
# The class of this samples is select observing the sum of the confidences or choose the most voted class
differentConfidencesCheckC2 <- function(data_1_it, data_x_it, thr_conf) {
  pos <- 0
  xid <- cleanVector(xid)
  ycl <- cleanVector(ycl)
  zp <- cleanVector(zp)
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)) {
    if ((as.character(data_1_it[lvls[indice], 1])
         != as.character(data_x_it[indice, 1]))) {
      if ((data_1_it[lvls[indice], 2] >= thr_conf)
          || (data_x_it[indice, 2] >= thr_conf)) {
        pos <- pos + 1
        xid[pos] <- data_x_it[indice, 3]
        ycl[pos] <- as.character(data_1_it[lvls[indice], 1])
        zp[pos] <- data_x_it[indice, 2]
      }
    }
  }
  examples <- data.frame(cl = ycl, p = zp, id = xid)
  return (examples)
}

f <- function(m, d) { #arg 2 - ?rvore de decis?o
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
  qtd_exemplos_rot <- 0 #qtd rotulados na interacao
  total_rot <- 0 #qtd soma de todos rotulados
  conj_treino <<- cleanVector(conj_treino) #nao vi utilizacao
  treino_valido <<- FALSE #algum verificador do treinamento
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

flexConC1 <- function(prob_preds_1_it, prob_preds, thr_conf, moda, it) {
  # if(it == 1) {
  #   prob_preds_1_it <<- prob_preds
  #   new_samples <- which(prob_preds[ , 2] >= thr_conf)
  #   rotulados <- data.frame(id = prob_preds[new_samples, 3],
  #                           cl = prob_preds[new_samples, 1])
  # } else {

  # os exemplos possuem a mesma classe na iteracao atual e na primeira iteracao e
  # as taxas de confianca na iteracao atual e na primera iteracao sao maiores que o thrconf
  rotulados <- classCheck(prob_preds_1_it, prob_preds, thr_conf)
  len_rotulados <- getLength(rotulados$id)
  if (len_rotulados == 0) {
    # exemplos possuem as mesmas classes e uma das confiancas é maior que o thrConf
    rotulados <- confidenceCheck(prob_preds_1_it, prob_preds, thr_conf)
    len_rotulados <- getLength(rotulados$id)
    if (len_rotulados == 0) {
      #as classes sao diferentes, mas as duas confiancas sao maiores que thrConf      
      rotulados <- differentClassesCheck(prob_preds_1_it, prob_preds,
                                         thr_conf, moda)
      len_rotulados <- getLength(rotulados$id)
      if(len_rotulados == 0) {
        #as classes sao diferentes, mas pelo menos uma das confiancas sao maiores que thrConf      
        rotulados <- differentConfidencesCheck(prob_preds_1_it, prob_preds,
                                               thr_conf, moda)
      }
    }
  }
  #new_samples <- rotulados$id
  return (rotulados)
}


flexConC2 <- function(prob_preds_1_it, prob_preds, thr_conf) {
  # os exemplos possuem a mesma classe na iteracao atual e na primeira iteracao e
  # as taxas de confianca na iteracao atual e na primera iteracao sao maiores que o thrconf
  rotulados <- classCheck(prob_preds_1_it, prob_preds, thr_conf)
  len_rotulados <- getLength(rotulados$id)
  if (len_rotulados == 0) {
    # exemplos possuem as mesmas classes e uma das confiancas é maior que o thrConf
    rotulados <- confidenceCheck(prob_preds_1_it, prob_preds, thr_conf)
    len_rotulados <- getLength(rotulados$id)
    if (len_rotulados == 0) {
      #as classes sao diferentes, mas as duas confiancas sao maiores que thrConf      
      rotulados <- differentClassesCheckC2(prob_preds_1_it, prob_preds,
                                         thr_conf)
      len_rotulados <- getLength(rotulados)
      if (len_rotulados!=0){
        add_rot_superv <<- TRUE
      } 
        
      if(len_rotulados == 0) {
        #as classes sao diferentes, mas pelo menos uma das confiancas sao maiores que thrConf      
        rotulados <- differentConfidencesCheckC2(prob_preds_1_it, prob_preds,
                                               thr_conf)
        len_rotulados <- getLength(rotulados)
        if (len_rotulados!=0){
          add_rot_superv <<- TRUE
        } 
        
      }
    }
  }
  #new_samples <- rotulados$id
  return (rotulados)
}

#ESSA FUNCAO ESTA ERRADA PQ COMPARA 2 DATA FRAMES DE TAMANHOS DIFERENTES E O WHICH RETORNA
#A QUANTIDADE DE EXEMPLOS DO MAIOR DATA FRAME
# # FlexCon-C2 funtion
# flexConC2 <- function(prob_preds, prob_preds_superv, thr_conf) {
#   prob_preds <- convertProbPreds(prob_preds)
#   prob_preds_superv <- convertProbPreds(prob_preds_superv)
#   prob_preds_con <- (prob_preds[, 2] >= thr_conf)
#   prob_preds_superv_con <- (prob_preds_superv[, 2] >= thr_conf)
#   prob_preds_cl <- prob_preds[, 1]
#   prob_preds_superv_cl <-  prob_preds_superv[, 1]
#   new_samples <- which((prob_preds_con & prob_preds_superv_con)
#                        & (prob_preds_cl == prob_preds_superv_cl))
#   if (length(new_samples) == 0) {
#     new_samples <- which((prob_preds_con | prob_preds_superv_con)
#                          & (prob_preds_cl == prob_preds_superv_cl))
#     if (length(new_samples) == 0) {
#       new_samples <- which((prob_preds_con & prob_preds_superv_con)
#                            & (prob_preds_cl != prob_preds_superv_cl))
#       if (length(new_samples)) {
#         add_rot_superv <<- TRUE
#       }
#     }
#   }
#   return(new_samples)
# }

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

getID <- function(base, sup){ #pegar o ID real no data
  base_local <- base
  base_local$id <- seq(1,nrow(base_local))
  base_local <- base_local[-sup,]
  return(base_local$id)
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
  #thrConf_g_o <<- c()
  thrConf1_g_o <<- c()
  thrConf2_g_o <<- c()
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
#cr=5 nem umas das condiçoes vao ser aceitas
newConfidence <- function(acc_local, limiar, tx_conf) { 
  if ((acc_local > (limiar + 1)) && ((tx_conf - cr/100) > 0.0)) {
    tx_conf <- tx_conf - cr/100
  } else if ((acc_local < (limiar - 1)) && ((tx_conf + cr/100) <= 1)) {
    tx_conf <- tx_conf + cr/100
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
outputArchive <- function(cr, cl, nome_acc, method, acc_c1_s, acc_c1_v, acc_c2, acc_self) {
  # flexcon_c1_s <- paste("flexcon_c1_S_", cl, "_", cr, extention, sep = "")
  # flexcon_c1_v <- paste("flexcon_c1_V_", cl, "_", cr, extention, sep = "")
  # flexcon_c2 <- paste("flexcon_c2_", cl, "_", cr, extention, sep = "")
  self_training <- paste("co_training_", cl, "_", nome_acc, "_metodo_", method, "_", cr, extention, sep = "")

  # acc_flexcon_c1_s <- matrix(acc_c1_s, ncol = 5, byrow = TRUE)
  # acc_flexcon_c1_v <- matrix(acc_c1_v, ncol = 5, byrow = TRUE)
  # acc_flexcon_c2 <- matrix(acc_c2, ncol = 5, byrow = TRUE)
  acc_self_training <- matrix(acc_self, ncol = 5, byrow = FALSE)

  row <- rep(bd_nome, 10)
  # col <- rep("ATT", 5)
  # writeArchive(flexcon_c1_s, acc_flexcon_c1_s)
  # writeArchive(flexcon_c1_v, acc_flexcon_c1_v)
  # writeArchive(flexcon_c2, acc_flexcon_c2)
  if (bd_nome == "iris"){
    writeArchive(self_training, acc_self_training, row = row, col = T)
  }else{
    writeArchive(self_training, acc_self_training, row = row, col = F)
  }
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
    qtd_add <- 0
    gradativo <- 0.05
  }
  form <- as.formula(paste(classe,'~', '.'))
  k <- 5
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
      if ((it>1)&&(qtd_add>0)){
        thrConf <- (thrConf - gradativo)
        if (thrConf <= 0.0) thrConf <- (thrConf + gradativo)
      }
      qtd_add <- 0
    }
    
    model1 <- generateModel(learner, form, data1, sup1)
    model2 <- generateModel(learner, form, data2, sup2)
    probPreds1 <- generateProbPreds(predFunc, model1, data1, sup2)
    probPreds2 <- generateProbPreds(predFunc, model2, data2, sup1)
    
    id_data1 <- getID(data1,sup2)
    id_data2 <- getID(data2,sup1)
    probPreds1$id <- id_data1
    probPreds2$id <- id_data2
    
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
      qtd_add <- min(length(which(probPreds1[, 2] >= thrConf)), length(which(probPreds2[, 2] >= thrConf)))
    }
    #criando os vetores em ordem decrescente pela confianca
    probPreds1_ordenado <- order(probPreds1$p, decreasing = T)
    probPreds2_ordenado <- order(probPreds2$p, decreasing = T)

    if (qtd_add > 0) {
      new_samples1 <- probPreds1[probPreds1_ordenado[1:qtd_add], -2]
      new_samples2 <- probPreds2[probPreds2_ordenado[1:qtd_add], -2]
      data1[(1:N)[new_samples2$id], as.character(form[[2]])] <- new_samples2$cl
      data2[(1:N)[new_samples1$id], as.character(form[[2]])] <- new_samples1$cl
      sup1 <- c(sup1, new_samples2$id)
      sup2 <- c(sup2, new_samples1$id)
      
    } else {
      new_samples1 <- cleanVector(new_samples1)
      new_samples2 <- cleanVector(new_samples2)
    }

    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf1_g_o <<- c(thrConf1_g_o, thrConf)
      thrConf2_g_o <<- c(thrConf2_g_o, thrConf)
      # thrConf_g_o <<- c(thrConf_g_o, thrConf)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, qtd_add)
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if (metodo ==3){#gradativo
      if(qtd_add==0){ #se o 1 for zero o 2 tbm ser?
        thrConf<-min(max(probPreds1[,2]), max(probPreds2[,2]))
      }
    }

    if ((it == maxIts) || ((length(sup1) / N) >= 1) || ((length(sup2) / N) >= 1) ) {
      break
    }else{
      if ((method ==2) & (qtd_add == 0)){
          break
      }
    }
  }
  model <- list(model1, model2)
  return (model)
}

# Function co-Training FlexCon
#@param metodo - 1 = co-training FlexCon soma
#                2 = co-training FlexCon Voto
coTrainingFlexCon <- function (learner, predFunc, data1, data2, votacao = T) {
  conf_media <- 0
  form <- as.formula(paste(classe,'~', '.'))
  thrConf1 <- 0.95
  thrConf2 <- 0.95
  maxIts <- 100
  verbose <- T
  N <- NROW(data1)
  it <- 0
  cobertura <- 0
  #POSICAO DOS EXEMPLOS NA BASE DE TREINAMENTO
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados
  
  moda1 <- matrix(data = rep(0,length(data1$class)),ncol = length(unique(base_original$class)), nrow = NROW(data1), byrow = TRUE, 
                  dimnames = list(seq(1,nrow(data1)),unique(base_original$class)))
  moda2 <- matrix(data = rep(0,length(data2$class)),ncol = length(unique(base_original$class)), nrow = NROW(data2), byrow = TRUE, 
                  dimnames = list(seq(1,nrow(data2)),unique(base_original$class)))
  
  repeat {
    new_samples1 <- cleanVector(new_samples1)
    new_samples2 <- cleanVector(new_samples2)
    acertou <- 0
    it <- it + 1

    if ((it>1)&&(qtd_add>0)){
      #foi acrescentado a virgula na parte data1[-sup1,] nas linhas abaixo
      thrConf1 <- (thrConf1 + conf_media1 + cobertura)/3
      thrConf2 <- (thrConf2 + conf_media2 + cobertura)/3
    }
    
    conf_media <- 0
    
    model1 <- generateModel(learner, form, data1, sup1)
    model2 <- generateModel(learner, form, data2, sup2)
    probPreds1 <- generateProbPreds(predFunc, model1, data1, sup2)
    probPreds2 <- generateProbPreds(predFunc, model2, data2, sup1)
    
    id_data1 <- getID(data1,sup2)
    id_data2 <- getID(data2,sup1)
    probPreds1$id <- id_data1
    probPreds2$id <- id_data2
    
    if (votacao){
      moda1 <- storageFashion(probPreds1, moda1) # Armazena a moda das classes
      moda2 <- storageFashion(probPreds2, moda2) # Armazena a moda das classes
    }else{
      moda1 <- storageSum(probPreds1, moda1) # Armazena a soma das classes
      moda2 <- storageSum(probPreds2, moda2) # Armazena a soma das classes
    }
#LEMBRAR DE ADD ESSA PARTE NO FLEXCON-C
    if(it == 1) {
      prob_preds1_1_it <<- probPreds1
      prob_preds2_1_it <<- probPreds2
      novos1 <- which(probPreds1[ , 2] >= thrConf1)
      novos2 <- which(probPreds2[ , 2] >= thrConf2)
      new_samples1 <- probPreds1[novos1,]
      new_samples2 <- probPreds2[novos2,]
    } else {
      #retorna a posicao do exemplo no probpreds e a classe a ser atribuida
      new_samples1 <- flexConC1(prob_preds1_1_it, probPreds1, thrConf1, moda1, it)
      new_samples2 <- flexConC1(prob_preds2_1_it, probPreds2, thrConf2, moda2, it)
    }
    
    
    #co-training adaptado para funcionar igual ao self-training de Felipe
    qtd_add <- min(nrow(new_samples1), nrow(new_samples2))
    if (qtd_add > 0) {
      probPreds1_ordenado <- order(new_samples1$p, decreasing = T)
      probPreds2_ordenado <- order(new_samples2$p, decreasing = T)
      new_samples1 <- new_samples1[probPreds1_ordenado[1:qtd_add], ] #id da base de treinamento
      new_samples2 <- new_samples2[probPreds2_ordenado[1:qtd_add], ] #id da base de treinamento
      data1[(1:N)[new_samples2$id], as.character(form[[2]])] <- new_samples2$cl
      data2[(1:N)[new_samples1$id], as.character(form[[2]])] <- new_samples1$cl
      cobertura <- qtd_add/nrow(data1[-sup1,]) #é igual para as duas visoes

      sup1 <- c(sup1, new_samples2$id)
      sup2 <- c(sup2, new_samples1$id)
      conf_media1 <- mean(probPreds1$p)
      conf_media2 <- mean(probPreds2$p)
    } else {
      
      #podemos comentar o código que atribui ao limiar a maior taxa de confiança na predição
      #para parar no momento em que a qtdade de exemplos incluidos for zero
      #isso justificará o limiar fixo ser o melhor do co-training
      #it <- maxIts
      
      if (nrow(new_samples1) == 0) { #se o 1 for zero o 2 tbm ser?
        thrConf1 <- max(probPreds1[,2])
      }
      if (nrow(new_samples2) == 0) { #se o 1 for zero o 2 tbm ser?
        thrConf2 <- max(probPreds2[,2])
      }
      new_samples1 <- cleanVector(new_samples1)
      new_samples2 <- cleanVector(new_samples2)
    }
    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf1_g_o <<- c(thrConf1_g_o, thrConf1)
      thrConf2_g_o <<- c(thrConf2_g_o, thrConf2)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, qtd_add)
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if ((it == maxIts) || ((length(sup1) / N) >= 1)) {
      break
    }
  }
  model <- list(model1, model2)
  return (model)
}

# Function co-Training FlexConC
#@param metodo  5- FlexConC1s
#               6- FlexConC1v
#               7- FlexConC2
coTrainingFlexConC <- function(learner, predFunc, data1, data2, limiar1, limiar2, method, min_exem_por_classe){#inicializar vatacao?
  #inicializacao das variaveis
  conf_media <- 0
  form <- as.formula(paste(classe,'~', '.'))
  #taxa de confianca inicial das duas visao
  thrConf1 <- 0.95
  thrConf2 <- 0.95
  #numero maximo de interacao
  maxIts <- 100 
  verbose <- T #não sei o pq
  #num de linhas da base a ser treinada
  N <- NROW(data1)
  it <- 0 #iniciando a variavel da interacao
  conj_treino <<- cleanVector(conj_treino) 
  conj_treino_antigo <<- cleanVector(conj_treino) 
  conj_treino_local1 <- cleanVector(conj_treino_local1)
  conj_treino_antigo1 <- cleanVector(conj_treino_antigo1)
  conj_treino_local2 <- cleanVector(conj_treino_local2)
  conj_treino_antigo2 <- cleanVector(conj_treino_antigo2)
  
  #sup1 = posicao do exemplo em data1
  sup1 <- which(!is.na(data1[, as.character(form[[2]])])) #exemplos inicialmente rotulados (posição no vetor)
  sup2 <- which(!is.na(data2[, as.character(form[[2]])])) #exemplos inicialmente rotulados (posição no vetor)
  #FlexConC1
  if ((method == "5") || (method == "6")) {
    moda1 <- matrix(data = rep(0,length(data1$class)),ncol = length(unique(base_original$class)), nrow = NROW(data1), byrow = TRUE, 
                    dimnames = list(seq(1,nrow(data1)),unique(base_original$class)))
    moda2 <- matrix(data = rep(0,length(data2$class)),ncol = length(unique(base_original$class)), nrow = NROW(data2), byrow = TRUE, 
                    dimnames = list(seq(1,nrow(data1)),unique(base_original$class)))
  }
  
  # add_rot_superv <- FALSE #so utilizada pelo FlexConC2
  #variaveis de uma verificacao 
  
  #so basta uma base pois ambas tem a mesma qtd de instancias por classe
  n_instancias_por_classe <- ddply(data1, ~class, summarise,
                                   number_of_distinct_orders = length(class))
  n_classes <- NROW(n_instancias_por_classe) - 1
  
  treino_valido <<- FALSE 

  classificar <- TRUE

  mudou_conj_treino <<- FALSE  
  #laco de repeticao igual ao da funcao coTrainingFlexCon
  repeat {
    new_samples1 <- cleanVector(new_samples1)
    new_samples2 <- cleanVector(new_samples2)
    acertou <- 0
    it <- it + 1
    #cat("IT", it, "\n")
    
    #condicao para calcular a nova taxa de confianca
    if ((it>1)&&(qtd_add>0)){
      qtd_add = 0
      
      #gerando nova taxa de confiança para data1  
      treino_valido <- validTraining(conj_treino_local1, n_classes, min_exem_por_classe)
      classificar <- validClassification(treino_valido, conj_treino_antigo1, 
                                          conj_treino_local1, n_classes, min_exem_por_classe)

      if (mudou_conj_treino){
        conj_treino_local1 <- conj_treino
        conj_treino_antigo1 <- conj_treino_antigo
        conj_treino <<- cleanVector(conj_treino)
        conj_treino_antigo <<- cleanVector(conj_treino_antigo)
      }
      mudou_conj_treino <<- FALSE
      
      if(classificar) {
          #caculo para nava taxa de confianca
          #limiar1 = acur?cia de um classificador treinado e testado com os dados inicialmente rotulados
          acc_local1 <- calcLocalAcc(base_rotulados_ini1,conj_treino_local1)
          thrConf1 <- newConfidence(acc_local1, limiar1, thrConf1)
          
      }
      
      
      #gerando nova taxa de confiança para data2
      treino_valido <- validTraining(conj_treino_local2, n_classes, min_exem_por_classe)
      classificar <- validClassification(treino_valido, conj_treino_antigo2, 
                                         conj_treino_local2, n_classes, min_exem_por_classe)
      
      if (mudou_conj_treino){
        conj_treino_local2 <- conj_treino
        conj_treino_antigo2 <- conj_treino_antigo
        conj_treino <<- cleanVector(conj_treino)
        conj_treino_antigo <<- cleanVector(conj_treino_antigo)
      }
      mudou_conj_treino <<- FALSE
      
      if(classificar) {
        #caculo para nava taxa de confianca
        acc_local2 <- calcLocalAcc(base_rotulados_ini2,conj_treino_local2)
        thrConf2 <- newConfidence(acc_local2, limiar2, thrConf2)
        
      }
    }
    
    # conf_media <- 0
    
    model1 <- generateModel(learner, form, data1, sup1)
    model2 <- generateModel(learner, form, data2, sup2)
    probPreds1 <- generateProbPreds(predFunc, model1, data1, sup2)
    probPreds2 <- generateProbPreds(predFunc, model2, data2, sup1)
    
    id_data1 <- getID(data1,sup2)
    id_data2 <- getID(data2,sup1)
    #O id no probpreds passa a ser a posicao em data, ou seja, na base de treinamento completa (karliane)
    probPreds1$id <- id_data1
    probPreds2$id <- id_data2
    
    #Switch para verificar qual metodo vai ser utilizado
    if(it == 1) {
      prob_preds1_1_it <<- probPreds1
      prob_preds2_1_it <<- probPreds2
      novos1 <- which(probPreds1[ , 2] >= thrConf1)
      novos2 <- which(probPreds2[ , 2] >= thrConf2)
      new_samples1 <- probPreds1[novos1,]
      new_samples2 <- probPreds2[novos2,]
      
    } else {
      if ((method==5) || (method==6)){
        if(method == 5){
            moda1 <- storageSum(probPreds1, moda1)
            moda2 <- storageSum(probPreds2, moda2)
        }else if(method == 6){
            moda1 <- storageFashion(probPreds1, moda1)
            moda2 <- storageFashion(probPreds2, moda2)
        }  
          # retorna a posicao do exemplo no probpreds
        new_samples1  <- flexConC1(prob_preds1_1_it, probPreds1, thrConf1, moda1, it)
        new_samples2 <- flexConC1(prob_preds2_1_it, probPreds2, thrConf2, moda2, it)
      }else if(method == 7){
        new_samples1 <- flexConC2(prob_preds1_1_it, probPreds1, thrConf1)
        new_samples2 <- flexConC2(prob_preds2_1_it, probPreds2, thrConf2)

      }
    }
    # qtd_add <- min(length(new_samples1), length(new_samples2))
    qtd_add <- min(nrow(new_samples1), nrow(new_samples2))
    
    if (qtd_add>0){
      probPreds1_ordenado <- order(new_samples1$p, decreasing = T)
      probPreds2_ordenado <- order(new_samples2$p, decreasing = T)
      new_samples1 <- new_samples1[probPreds1_ordenado[1:qtd_add], ] #id da base de treinamento
      new_samples2 <- new_samples2[probPreds2_ordenado[1:qtd_add], ] #id da base de treinamento
      data1[(1:N)[new_samples2$id], as.character(form[[2]])] <- new_samples2$cl
      data2[(1:N)[new_samples1$id], as.character(form[[2]])] <- new_samples1$cl

      conj_treino_antigo1 <- appendDataFrame(conj_treino_antigo1, conj_treino_local1)
      conj_treino_local1 <- data1[(1:N)[new_samples2$id],]

      
      conj_treino_antigo2 <- appendDataFrame(conj_treino_antigo2, conj_treino_local2)
      conj_treino_local2 <- data2[(1:N)[new_samples1$id],]

      
      sup1 <- c(sup1, new_samples2$id)
      sup2 <- c(sup2, new_samples1$id)

    } else {
      if (nrow(new_samples1) == 0) { #se o 1 for zero o 2 tbm ser?
        thrConf1 <- max(probPreds1[,2])
      }
      if (nrow(new_samples2) == 0) { #se o 1 for zero o 2 tbm ser?
        thrConf2 <- max(probPreds2[,2])
      }
      
      new_samples1 <- cleanVector(new_samples1)
      new_samples2 <- cleanVector(new_samples2)
    }
    
    
    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf1_g_o <<- c(thrConf1_g_o, thrConf1)
      thrConf2_g_o <<- c(thrConf2_g_o, thrConf2)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, qtd_add)
      tx_g_o <<- c(tx_g_o, taxa)
    }
    #condicao de para do repeat
    if ((it == maxIts) || ((length(sup1) / N) >= 1)) {
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
    id <- as.numeric(prob_preds[x, ncol(prob_preds)])
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
    id <- as.numeric(prob_preds[x, ncol(prob_preds)])
    for (y in 1:length(dist_classes)) {
      if (as.character(prob_preds[x, 1]) == as.character(dist_classes[y])) {
        moda[id, dist_classes[y]] <- moda[id, dist_classes[y]] + prob_preds[x, 2]
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
#' se o treino for válido, a função apenas atribui o conj de treinamento antigo
#' ao novo conj. de treinamento e limpa o conj. antigo.
#' se o treino não for válido, a função junta o conj de treinamento antigo com o
#' novo e chama a funcao validTraining para validar se os dois conjuntos juntos 
#' podem ser treinados.
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
validClassificationAntigo <- function(treino_valido_i, id_conj_treino,
                                id_conj_treino_antigo, data, N_classes,
                                min_exem_por_classe) {
  if (treino_valido_i) {
    conj_treino <<- data[id_conj_treino, ]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
    mudou_conj_treino <<- TRUE
  } else if (length(id_conj_treino_antigo) >= 1) {
    conj_treino <<- rbind(data[id_conj_treino, ], data[id_conj_treino_antigo, ])
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validTraining(data, id_conj_treino1, N_classes, min_exem_por_classe)
    mudou_conj_treino <<- TRUE
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

validClassification <- function(treino_valido_i, conj_treino_antigo_local,
                                conj_treino_local, N_classes, min_exem_por_classe) {
  if (treino_valido_i) {
    conj_treino <<- conj_treino_local
    conj_treino_antigo <<- c()
    classificar <- TRUE
    mudou_conj_treino <<- TRUE
  } else if (!is.null(nrow(conj_treino_antigo_local))) {
    conj_treino <<- rbind(conj_treino_local, conj_treino_antigo_local)
    treino_valido_i <- validTraining(conj_treino, N_classes, min_exem_por_classe)
    mudou_conj_treino <<- TRUE
    if (treino_valido_i) {
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
#' o treino só é válido se todas as classes estiverem representadas no conj.
#' de treimento e se a quantidade de exemplos de cada classe for no mínimo (a qtdade
#' de exemplos da classe com menor representação no conj. ini. rot.?)
#'
#' @param data the all dataset.
#' @param id_conj_treino vector with the samples selectedes to train.
#' @param Nclasses the total of the classes in the dataset.
#' @param min_exem_por_classe the min samples of each class for training.
#'
#' @return a boolean to say if the training is valid.
#'
validTrainingAntigo <- function(data, id_conj_treino, Nclasses, min_exem_por_classe) {
  exemplos_classe <- ddply(data[id_conj_treino, ], ~class, summarise,number_of_distinct_orders = length(class))
  
  treino_valido <- FALSE
  if ((NROW(exemplos_classe)-1) == Nclasses) {
    for (x in 1:(NROW(exemplos_classe)-1)) {
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

validTraining <- function(data, Nclasses, min_exem_por_classe) {
  exemplos_classe <- ddply(data, ~class, summarise,number_of_distinct_orders = length(class))
  
  treino_valido <- FALSE
  if (NROW(exemplos_classe) == Nclasses) {
    for (x in 1:(NROW(exemplos_classe))) {
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
    return (as.integer((nrow(bd) / 10) + 1))
  }, 
  error = function(setIniBd){return(1)})
}
