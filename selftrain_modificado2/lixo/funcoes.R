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
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n„o rotulados
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <- c()
  
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
#      N_instancias_por_classe2 <- ddply(data[new,],~class,summarise,number_of_distinct_orders=length(class))
      N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))
      treino_valido <- FALSE
      # teste <<- N_c
      for (x in 1:nrow(N_instancias_por_classe2)){

        if (N_instancias_por_classe2$number_of_distinct_orders[x]>= N_classes*5)
          treino_valido <- TRUE
        else treino_valido <- FALSE

      }
      
      
      #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N√ÉO)
      if (treino_valido){
      # if (nrow(data[new,])>=N_classes*5){
        #o conjunto de treinamento serao as instancias inclu√???das (rotuladas)
        conj_treino <- data[id_conj_treino,]
        id_conj_treino_antigo <- c()
        #conj_treino <- data[new,]
      }else if (length(conj_treino)>=1) {
        #o conjunto de treinamento ser√° o anterior + as instancias incluidas (rotuladas)
        #conj_treino <- rbind(data[new,],conj_treino)
        conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
       # id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
        cat("juntou", nrow(conj_treino), "\n")
      }else break
      
      if(k==1){
        classificador <- naiveBayes(as.factor(class) ~ .,conj_treino)
        matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
      }
      else{
        #IMPLEMENTAR ARVORE DE DECIS√O
        classificador <- rpartXse(as.factor(class) ~ .,conj_treino)
        matriz <- table(predict(classificador,base_rotulados_ini, type="vector"),base_rotulados_ini$class)        
      }

      acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
      if(acc_local>=50){
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
      
      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      sup <- c(sup,(1:N)[-sup][new])
    }
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      #thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  } #FIM DO REPEAT
  
  return(model)  
}