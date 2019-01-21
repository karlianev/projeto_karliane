source("flexcon_c/functions.R")

#funcao self-training padrao (original)
SelfTrainOriginal <- function (learner, predFunc) {
  form <- as.formula(paste(classe,'~', '.'))
  data <- base
  thrConf <- 0.95
  maxIts <- 100
  percFull <- 1
  verbose <- T
  N <- NROW(data)
  it <- 0
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  repeat {
    acertou <- 0
    it <- it + 1
    model <- generate
      
      runLearner(learner, form, data[sup, ])
    probPreds <- do.call(predFunc, list(model, data[-sup, ]))
    new <- which(probPreds[, 2] > thrConf)
    if (verbose) {
      it_g_o <<- c(it_g_o, it)
      bd_g_o <<- c(bd_g_o, bd_nome)
      thrConf_g_o <<- c(thrConf_g_o, thrConf)
      nr_added_exs_g_o <<- c(nr_added_exs_g_o, length(new))
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if (length(new)) {
      data[(1:N)[-sup][new], as.character(form[[2]])] <- probPreds[new, 1]
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])] == data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto) {
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      sup <- c(sup, (1:N)[-sup][new])
      acertou_g_o <<- c(acertou_g_o, acertou)
    }
    else {
      acertou <- 0
      acertou_g_o <<- c(acertou_g_o, acertou)
      break
    }
    if (it == maxIts || length(sup)/N >= percFull) 
      break
  }
  return(model)
}