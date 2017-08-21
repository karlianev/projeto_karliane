#setando parametros do selftrain


#classes da base de dados
form <- as.formula(paste(classe,'~', '.'))
data <- base_treino_self_training	#base de dados
learn <- learner('rpartXse',list(se=0.5))
#aprendiz naive = 
#learn <- learner('naiveBayes',list())
predFunc <- 'f'   			#Uma string com o nome de uma fun??o que ir? realizar as tarefas de classifica??o probabil?stica que ser?o necess?rias durante o processo de self-training
thrConf=0.9       			#taxa de confian?a dos exemplos a serem incluidos no conjunto de rotulados
maxIts=10					#n?mero m?ximo de itera??es
percFull=1					#Um n?mero entre 0 e 1. Se a porcentagem de exemplos rotulados atingir esse valor o processo de self-training ? parado
verbose=TRUE				#Um booleano indicando o n?vel de verbosidade?? (verbosity??) da fun??o

#adapta??o da implementa??o do selftrain
data
N <- NROW(data)
it <- 0


soma_Conf <- 0
qtd_Exemplos_Rot <- 0
totalrot <- 0

sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
repeat {
  
  it <- it+1
  
  
  if (it>1) thrConf <- (thrConf + (soma_Conf/qtd_Exemplos_Rot) + (qtd_Exemplos_Rot/N))/3
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  
  
  model <- runLearner(learn,form,data[sup,])
  probPreds <- do.call(predFunc,list(model,data[-sup,]))
  
  
  new <- which(probPreds[,2] > thrConf)
  
  
  
  
  
  if (verbose) {
    cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
    ##guardando nas variaveis 
    it_g <-c(it_g,it)
    bd_g <-c(bd_g,i)
    thrConf_g<-c(thrConf_g,thrConf)
    nr_added_exs_g<-c(nr_added_exs_g,length(new))
    tx_g <- c(tx_g, taxa)
    #    acc_g <- c(acc_g, acc)
    ##resultado <-  c(it,",",i,",",thrConf,",",length(new))
    ##write(resultado, file = "result")
    
    
  }
  
  
  if (length(new)) {
    data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
    
    soma_Conf <- sum(soma_Conf, probPreds[new,2])
    qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
    totalrot <- totalrot + qtd_Exemplos_Rot
    #   	      cat('dentro do self training', '\n limiar confian?a(thrConf).',thrConf,'\n soma Confian?a rotulados. =',soma_Conf, '\n quantidade rotulados. =',qtd_Exemplos_Rot,'\n','\n total rotulados. =',totalrot,'\n')
    
    sup <- c(sup,(1:N)[-sup][new])
  } else break
  if (it == maxIts || length(sup)/N >= percFull) break
}

#matriz de confusao do selftraining
#N?O EST? FUNCIONANDO PARA BASE DE DADOS 2, A MATRIZ N?O APARECE COM A MESMA QUANTIDADE DE LINHAS E COLUNAS  
#if (i==1){
# matriz_confusao1 = table(predict(model,base_teste,type='class'),base_teste$class)
#n <- length(base_teste$class)
#}else if (i==2){
# matriz_confusao1 = table(predict(model,base_teste,type='class'),base_teste$class)
#n <- length(base_teste$class)
#}

#os if´s acima foram substituídos pelas linhas abaixo (karliane)
matriz_confusao1 = table(predict(model,base_teste,type='class'),base_teste$class)
n <- length(base_teste$class)



cat("\n Acerto (%) = \n", levels(base_original[, classe]), "\n", diag(matriz_confusao1) / colSums(matriz_confusao1) * 100)

acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_g <- c(acc_g, acc)
bd <- c(bd, i)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)


#predicted predict(model, newdata = base_teste)
cat('FIM', '\t base de dados ', i, '\n', 'total rotulados: ', totalrot, '\n')
