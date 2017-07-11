#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive
if(k==1){
  nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
  nbST_o<- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
  matriz_confusao1<-table(predict(nbST, base_teste), base_teste$class)
  matriz_confusao_o<-table(predict(nbST_o, base_teste), base_teste$class)
}
if(k==2){
  ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
  ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  matriz_confusao_o=table(predict(ST_O,base_teste,type='class'),base_teste$class)
  
  
}

n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_o<-((sum(diag(matriz_confusao_o)) / n) * 100)

acc_g <- c(acc_g, acc)
acc_g_o <- c(acc_g_o, acc_o)

bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)
cat("\n Acerto global original (%) =", acc_o)

cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



