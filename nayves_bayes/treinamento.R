#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")

if(c==1){ #NAIVE BAYES
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo modificado usando naive
    nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
    #chamada da funcao que implementa o metodo original usando naive
    nbST_o<- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
    #chamada da funcao que implementa o metodo gradativo usando naive
    nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
  }else if (t==2){ #TAXA INICIAL 0.95
    nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
    nbST_o<- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
    nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
  }
  #não testei o supervisionado com naive, as 2 linhas abaixo são apenas para não dar erro na execução do código
  matriz_confusao_supervisionado <- c()
  matriz_confusao_tot_supervisionado <- c()
  
  #criando a matriz de confusão para o método modificado
  matriz_confusao1<-table(predict(nbST, base_teste, type = 'class'), base_teste$class) #antes estava sem o type='class', karliane acrescentou apenas para ficar tudo igual
  #criando a matriz de confusão para o método original
  matriz_confusao_o<-table(predict(nbST_o, base_teste, type = 'class'), base_teste$class)
  #criando a matriz de confusão para o método modificado gradativo
  matriz_confusao_gra<-table(predict(nbST_gra, base_teste, type = 'class'), base_teste$class)
}
if(c==2){ #ARVORE DE DECISAO
  #fazendo teste com classificador supervisionado
  #chamada da arvore de decisão usando os exemplos inicialmente rotulados
  stdTree <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulados_ini,se=0.5)
  #chamada da arvore de decisão usando todos os exemplos de treinamento rotulados
  stdTree_tot <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulada_treino,se=0.5)
  
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo modificado usando arvore de decisão
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
    #chamada da funcao que implementa o metodo original usando arvore de decisão
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
    #chamada da funcao que implementa o metodo gradativo usando arvore de decisão
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
  }else if (t==2){ #TAXA INICIAL 0.95
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
  }  
  
  #fazendo teste com classificador supervisionado
  #criando a matriz de confusão para o modelo gerado pela árvore de decisão usando os exemplos inicialmente rotulados
  matriz_confusao_supervisionado <- table(predict(stdTree,base_teste,type='class'),base_teste$class)
  #criando a matriz de confusão para o modelo gerado pela árvore de decisão usando todos os exemplos de treinamento rotulados
  matriz_confusao_tot_supervisionado <- table(predict(stdTree_tot,base_teste,type='class'),base_teste$class)
  #criando a matriz de confusão para o modelo gerado pelo método original com árvore de decisão
  matriz_confusao_o=table(predict(ST_O,base_teste,type='class'),base_teste$class)
  #criando a matriz de confusão para o modelo gerado pelo método modificado com árvore de decisão
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  #criando a matriz de confusão para o modelo gerado pelo método gradativo com árvore de decisão
  matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}
if(c==3){ #RIPPER
  
  if (t==1){
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
  }else if (t==2){
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
  }  
  matriz_confusao_supervisionado <- c()
  matriz_confusao_tot_supervisionado <- c()
  
  matriz_confusao_o=table(predict(ST_O,base_teste,type='class'),base_teste$class)
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}
if(c==4){ #IBK
  if (t==1){
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE)
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE)
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE)
  }else if (t==2){
    ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE)
    ST_O <- SelfTrainOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE)
    ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE)
  }  
  matriz_confusao_supervisionado <- c()
  matriz_confusao_tot_supervisionado <- c()
  
  matriz_confusao_o=table(predict(ST_O,base_teste,type='class'),base_teste$class)
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}




#n <- length(base_teste$class)



# acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
# acc_o<-((sum(diag(matriz_confusao_o)) / n) * 100)
# acc_gra<-((sum(diag(matriz_confusao_gra)) / n) * 100)

#CALCULANDO A ACURACIA (ATÉ LINHA 109)
#fazendo teste com classificador supervisionado
acc_sup <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
acc_sup_tot <- ((sum(diag(matriz_confusao_tot_supervisionado)) / sum(matriz_confusao_tot_supervisionado)) * 100)

acc <- ((sum(diag(matriz_confusao1)) / sum(matriz_confusao1)) * 100)
acc_o<-((sum(diag(matriz_confusao_o)) / sum(matriz_confusao_o)) * 100)
acc_gra<-((sum(diag(matriz_confusao_gra)) / sum(matriz_confusao_gra)) * 100)

#ARMAZENANDO A ACURACIA EM UMA VARIAVEL (ATE LINHA 118)
#fazendo teste com classificador supervisionado
acc_g_sup <- c(acc_g_sup, acc_sup)
acc_g_sup_tot <- c(acc_g_sup, acc_sup)

acc_g <- c(acc_g, acc)
acc_g_o <- c(acc_g_o, acc_o)
acc_g_gra <- c(acc_g_gra, acc_gra)

#ARMAZENANDO EM UMA VARIAVEL O NOME DA BASE E O % DE EXEMPLOS ROTULADOS INICIALMENTE
bd <- c(bd, bd_nome)
tx <- c(tx, taxa)

#imprimindo a acuracia na tela 
cat("\n Acerto global modificado (%) =", acc)
cat("\n Acerto global original (%) =", acc_o)
cat("\n Acerto global gradativo (%) =", acc_gra)

#fazendo teste com classificador supervisionado
cat("\n Acerto global supervisionado (%) =", acc_sup)
cat("\n Acerto global totalmente supervisionado (%) =", acc_sup_tot)

cat("\n FIM") #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



