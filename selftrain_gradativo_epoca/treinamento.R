#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive
if(k==1){
  if (t==1){
  
    nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE,grad)
  }else if (t==2){
   
    nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE,grad)
  }
 
  matriz_confusao_gra<-table(predict(nbST_gra, base_teste), base_teste$class)
}
if(k==2){
  if (t==1){
   
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE,grad)
  }else if (t==2){    
    
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,grad)
  }  
  
  matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}

n <- length(base_teste$class)


acc_gra<-((sum(diag(matriz_confusao_gra)) / n) * 100)

acc_g_gra <- c(acc_g_gra, acc_gra)
grad_g_acc<-c(grad_g_acc,grad)

bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global original (%) =", acc_gra)

cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



