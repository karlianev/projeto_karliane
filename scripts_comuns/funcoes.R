func <- function(m, d){
  p <- predict(m, d, type = "raw")
  data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
}
f <- function(m,d) {
  l <- predict(m,d,type='class')
  c <- apply(predict(m,d),1,max)
  data.frame(cl=l,p=c)
}



funcSelfTrain <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F){
  
  data
  N <- NROW(data)
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  repeat {
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0))thrConf <- (thrConf + (soma_Conf/qtd_Exemplos_Rot) + (qtd_Exemplos_Rot/N))/3
    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
  
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    new <- which(probPreds[,2] >= thrConf)
    
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
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      sup <- c(sup,(1:N)[-sup][new])
    }
    if(length(new)==0){
        thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A MÉDIA DAS PREDIÇÕES.
        #thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  }
  
  return(model)  
}

SelfTrainOriginal <- function (form, data, learner, predFunc, thrConf = 0.9, maxIts = 10, 
          percFull = 1, verbose = F) 
{
  N <- NROW(data)
  it <- 0
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  repeat {
    it <- it + 1
    model <- runLearner(learner, form, data[sup, ])
    probPreds <- do.call(predFunc, list(model, data[-sup, 
                                                    ]))
    new <- which(probPreds[, 2] > thrConf)
    if (verbose) {
      #cat("IT.", it, "\t nr. added exs. =", length(new), 
       #   "\n")
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     
      it_g_o <<-c(it_g_o,it)
      bd_g_o <<-c(bd_g_o,bd_nome)
      thrConf_g_o <<-c(thrConf_g_o,thrConf)
      nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if (length(new)) {
      data[(1:N)[-sup][new], as.character(form[[2]])] <- probPreds[new, 
                                                                   1]
      sup <- c(sup, (1:N)[-sup][new])
    }
    else break
    if (it == maxIts || length(sup)/N >= percFull) 
      break
  }
  return(model)
}


#NÃO ESTÁ IMPLEMENTADO, ESSA IMPLEMENTACAO É A DO SELFTRAIN MODIFICADO
funcSelfTrainGradativo <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,gradativo=0.05){
  
  data
  N <- NROW(data)
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  repeat {
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0))thrConf <- (thrConf - gradativo)

    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    new <- which(probPreds[,2] >= thrConf)
    
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g_gra <<-c(it_g_gra,it)
      bd_g_gra <<-c(bd_g_gra,bd_nome)
      thrConf_g_gra <<-c(thrConf_g_gra,thrConf)
      nr_added_exs_g_gra <<-c(nr_added_exs_g_gra,length(new))
      tx_g_gra <<- c(tx_g_gra, taxa)
      grad_g<<-c(grad_g,grad)
      
      }
    
    if (length(new)) {
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      sup <- c(sup,(1:N)[-sup][new])
    }
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A MÉDIA DAS PREDIÇÕES.
      #thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  }
  
  return(model)  
}
