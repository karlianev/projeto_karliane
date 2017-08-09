#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive
if(k==1){
  if (t==1){
    nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
   
  }else if (t==2){
    nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
   
  }
  matriz_confusao1<-table(predict(nbST, base_teste), base_teste$class)

}
if(k==2){
  if (t==1){
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
   
  }else if (t==2){    
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
    
  }  
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
}

n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)

acc_g <- c(acc_g, acc)



bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)


cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



