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
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      #data[sup,] corresponde os que possuem rotulos
      if(k==1){
        classificador <- naiveBayes(as.factor(class) ~ .,data[new,])
      }
      else{
        classificador <- naiveBayes(as.factor(class) ~ .,data[new,])
        
      }
     
    
      matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
      acc_local <- ((sum(diag(matriz)) / n) * 100)
      if(acc_local>=80){
        thrConf<-thrConf-0.05
      }else{
        thrConf<-thrConf+0.05
      }
    }
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

