#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive
if(c==1){
  if (t==1){
    # o ripper é JRip. acho que o SVM é svm, mas não deu certo
    
    RipST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
    RipST_o <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
    RipST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
              
    # nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
    # nbST_o<- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
    # nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
  }else if (t==2){

    RipST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
    RipST_o <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
    RipST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
    
    # nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
    # nbST_o<- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
    # nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
  }
  matriz_confusao1<-table(predict(RipST, base_teste), base_teste$class)
  matriz_confusao_o<-table(predict(RipST_o, base_teste), base_teste$class)
  matriz_confusao_gra<-table(predict(RipST_gra, base_teste), base_teste$class)
}
# if(c==2){
#   if (t==1){
#     ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
#     ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
#     ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
#   }else if (t==2){
#     ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
#     ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
#     ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
#   }  
#   matriz_confusao_o=table(predict(ST_O,base_teste,type='class'),base_teste$class)
#   matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
#   matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
# }

n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_o<-((sum(diag(matriz_confusao_o)) / n) * 100)
acc_gra<-((sum(diag(matriz_confusao_gra)) / n) * 100)

acc_g <- c(acc_g, acc)
acc_g_o <- c(acc_g_o, acc_o)
acc_g_gra <- c(acc_g_gra, acc_gra)

bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)
cat("\n Acerto global original (%) =", acc_o)
cat("\n Acerto global original (%) =", acc_gra)

cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



