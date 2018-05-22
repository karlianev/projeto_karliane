#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
#naive

#COMENTEI  MODIFICADO3 PARA PODER RODAR S? O MODIFICADO2 COM SOMA
#N?O COMENTEI O SUPERVISIONADO PQ A ACURACIA ? USADA COMO LIMIAR
#LEMBRAR DE RETIRAR O COMENTARIO
#AL?M DISSO COLOQUEI AS  MATRIZES DE CONFUSAO RECEBENDO VAZIO


if(c==1){ #NB
  stdNB <- naiveBayes(as.formula(paste(classe,'~', '.')),base_rotulados_ini)
  matriz_confusao_supervisionado <- table(predict(stdNB,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
     nbST_IP<- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    nbST<- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    nbST_3<- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdNB)
  }else if (t==2){ #0.95
    nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE, votacao = FALSE)
    nbST_IP<- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list(4)),'func',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
   # nbST<- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
   #nbST_3<- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdNB)
  }
  matriz_confusaoIP<- table(predict(nbST_IP, base_teste), base_teste$class)
  matriz_confusao1<- table(predict(nbST, base_teste), base_teste$class)
}
if(c==2){ #AD
  stdTree <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulados_ini,se=0.5)
  matriz_confusao_supervisionado <- table(predict(stdTree,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdTree)
   
  }else if (t==2){ #0.95
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdTree)
  }  
  matriz_confusao1 = table(predict(ST_IP,base_teste,type='class'),base_teste$class)
  matriz_confusao3 = c()#table(predict(ST_3,base_teste,type='class'),base_teste$class)
  
}

if(c==3){ #RIPPER
  stdJRip <- JRip(as.formula(paste(classe,'~', '.')),base_rotulados_ini)
  matriz_confusao_supervisionado <- table(predict(stdJRip,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdJRip)
    
  }else if (t==2){ #0.95
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdJRip)
  }  
  matriz_confusao1 = table(predict(ST_IP,base_teste),base_teste$class)
  matriz_confusao3 = c()#table(predict(ST_3,base_teste),base_teste$class)
}

if(c==4){ #IBK
  stdIBK <- IBk(as.formula(paste(classe,'~', '.')),base_rotulados_ini, control = Weka_control(K=15, X=TRUE))
  matriz_confusao_supervisionado <- table(predict(stdIBK,base_rotulados_ini,type='class'),base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  
  if (t==1){ #0.9
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdIBK)
    
  }else if (t==2){ #0.95
    ST_IP <- funcSelfTrainInclusaoProp(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
#    ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdIBK)
  }  
  matriz_confusaoIP = table(predict(ST_IP,base_teste),base_teste$class)
  matriz_confusao1 = c()#table(predict(ST_3,base_teste),base_teste$class)
}


n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusaoIP)) / n) * 100)
acc_3 <- ((sum(diag(matriz_confusao1)) / n) * 100)


acc_g <- c(acc_g, acc)
acc_g_3 <- c(acc_g_3, acc_3)


#fazendo teste com classificador supervisionado
acc_g_sup <- c(acc_g_sup, acc_sup_3)


bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global modifInclusãoProp (%) =", acc)
cat("\n Acerto global modif_1 (%) =", acc_3)
cat("\n Acerto global supervisionado (%) =", acc_sup_3)


cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')



