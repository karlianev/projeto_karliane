#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive


if(c==1){ #NB
  stdNB <- naiveBayes(as.formula(paste(classe,'~', '.')),base_rotulados_ini)
  matriz_confusao_supervisionado <- table(predict(stdNB,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    nbST<- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    nbST_3<- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdNB)
  }else if (t==2){ #0.95
    nbST<- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    nbST_3<- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdNB)
  }
  matriz_confusao1<-table(predict(nbST, base_teste), base_teste$class)
  matriz_confusao3<-table(predict(nbST_3, base_teste), base_teste$class)
}
if(c==2){ #AD
  stdTree <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulados_ini,se=0.5)
  matriz_confusao_supervisionado <- table(predict(stdTree,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdTree)
   
  }else if (t==2){ #0.95
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdTree)
  }  
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste,type='class'),base_teste$class)
  
}

if(c==3){ #RIPPER
  stdJRip <- JRip(as.formula(paste(classe,'~', '.')),base_rotulados_ini)
  matriz_confusao_supervisionado <- table(predict(stdJRip,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdJRip)
    
  }else if (t==2){ #0.95
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdJRip)
  }  
  matriz_confusao1 = table(predict(ST,base_teste),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste),base_teste$class)
}

if(c==4){ #IBK
  stdIBK <- IBk(as.formula(paste(classe,'~', '.')),base_rotulados_ini, control = Weka_control(K=15, X=TRUE))
  matriz_confusao_supervisionado <- table(predict(stdIBK,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdIBK)
    
  }else if (t==2){ #0.95
    ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3)
    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdIBK)
  }  
  matriz_confusao1 = table(predict(ST,base_teste),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste),base_teste$class)
}


n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_3 <- ((sum(diag(matriz_confusao3)) / n) * 100)

acc_g <- c(acc_g, acc)
acc_g_3 <- c(acc_g_3, acc_3)



bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global modif_2 (%) =", acc)
cat("\n Acerto global modif_3 (%) =", acc_3)


cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



