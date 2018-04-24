
#ESSE SCRIPT CONTÉM TODOS OS MÉTODOS JUNTOS, TÁ A MAIOR MISCELÂNEA IGUAL AO TREINAMENTO DO SELFTRAINING

print("Iniciando Treinamento")

if(c==1){ #NAIVE BAYES
  # #classificador supervisionado
  # stdNaive <- naiveBayes(as.formula(paste(classe,'~', '.')), base_rotulados_ini)
  # stdNaive_tot <- naiveBayes(as.formula(paste(classe,'~', '.')), base_rotulada_treino)
  
  
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo original usando naive
    nbCT_o<- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE, combinar=TRUE)

    # #chamada da funcao que implementa o metodo modificado usando naive
    # nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE, votacao = FALSE)
    # #chamada da funcao que implementa o metodo gradativo usando naive
    # nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE)
  }else if (t==2){ #TAXA INICIAL 0.95
    nbCT_o<- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE, combinar = TRUE)

    # nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE, votacao = FALSE)
    # nbST_gra<- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE)
  }
  #criando a matriz de confus?o para o m?todo original
  matriz_confusao_o_v1<-table(predict(nbCT_o[[1]], base_teste, type = 'class'), base_teste$class)
  matriz_confusao_o_v2<-table(predict(nbCT_o[[2]], base_teste, type = 'class'), base_teste$class)
  
  # #n?o testei o supervisionado com naive, as 2 linhas abaixo s?o apenas para n?o dar erro na execu??o do c?digo
  # matriz_confusao_supervisionado <- table(predict(stdNaive, base_teste, type = "class"), base_teste$class)
  # matriz_confusao_tot_supervisionado <- table(predict(stdNaive_tot, base_teste, type = "class"), base_teste$class)
  # #criando a matriz de confus?o para o m?todo modificado
  # matriz_confusao1<-table(predict(nbST, base_teste, type = 'class'), base_teste$class) #antes estava sem o type='class', karliane acrescentou apenas para ficar tudo igual
  #criando a matriz de confus?o para o m?todo modificado gradativo
  #matriz_confusao_gra<-table(predict(nbST_gra, base_teste, type = 'class'), base_teste$class)
}
if(c==2){ #ARVORE DE DECISAO
  #fazendo teste com classificador supervisionado
  #chamada da arvore de decis?o usando os exemplos inicialmente rotulados
  # stdTree <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulados_ini,se=0.5)
  # # #chamada da arvore de decis?o usando todos os exemplos de treinamento rotulados
  # stdTree_tot <- rpartXse(as.formula(paste(classe,'~', '.')),base_rotulada_treino,se=0.5)
  
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo original usando arvore de decis?o
    CT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE, combinar = TRUE)
    
    # #chamada da funcao que implementa o metodo modificado usando arvore de decis?o
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE, votacao = FALSE)
    # #chamada da funcao que implementa o metodo gradativo usando arvore de decis?o
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE)
  }else if (t==2){ #TAXA INICIAL 0.95
    cT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE, combinar = TRUE)
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE, votacao = FALSE)
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE)
  }  
  #criando a matriz de confus?o para o modelo gerado pelo m?todo original com ?rvore de decis?o
  matriz_confusao_o_v1=table(predict(CT_O[[1]],base_teste,type='class'),base_teste$class)
  matriz_confusao_o_v2=table(predict(CT_O[[2]],base_teste,type='class'),base_teste$class)
  
  
  # #fazendo teste com classificador supervisionado
  # #criando a matriz de confus?o para o modelo gerado pela ?rvore de decis?o usando os exemplos inicialmente rotulados
  # matriz_confusao_supervisionado <- table(predict(stdTree,base_teste,type='class'),base_teste$class)
  # #criando a matriz de confus?o para o modelo gerado pela ?rvore de decis?o usando todos os exemplos de treinamento rotulados
  # matriz_confusao_tot_supervisionado <- table(predict(stdTree_tot,base_teste,type='class'),base_teste$class)
  # #criando a matriz de confus?o para o modelo gerado pelo m?todo modificado com ?rvore de decis?o
  # matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  # #criando a matriz de confus?o para o modelo gerado pelo m?todo gradativo com ?rvore de decis?o
  # matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}
if(c==3){ #RIPPER
  # stdJRip <- JRip(as.formula(paste(classe,'~', '.')),base_rotulados_ini)
  # stdJRip_tot <- JRip(as.formula(paste(classe,'~', '.')),base_rotulada_treino)
  
  if (t==1){
    CT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE, combinar = TRUE)
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE, votacao = FALSE)
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE)
  }else if (t==2){
    CT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE, combinar = TRUE)
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE, votacao = FALSE)
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE)
  }  
  matriz_confusao_o_v1=table(predict(CT_O[[1]],base_teste,type='class'),base_teste$class)
  matriz_confusao_o_v2=table(predict(CT_O[[2]],base_teste,type='class'),base_teste$class)
  
  # matriz_confusao_supervisionado <- table(predict(stdJRip,base_teste,type='class'),base_teste$class)
  # matriz_confusao_tot_supervisionado <- table(predict(stdJRip_tot,base_teste,type='class'),base_teste$class)
  # matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  # matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}
if(c==4){ #IBK
  # stdIBK <- IBk(as.formula(paste(classe,'~', '.')),base_rotulados_ini, control = Weka_control(K=15, X=TRUE))
  # #chamada da arvore de decis?o usando todos os exemplos de treinamento rotulados (BASE COMPLETA, N?O REFLETE A REALIDADE DOS PROBLEMAS DO MUNDO REAL)
  # stdIBK_tot <- IBk(as.formula(paste(classe,'~', '.')),base_rotulada_treino, control = Weka_control(K=15, X=TRUE))
  
  if (t==1){
    CT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE, combinar = TRUE)
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE, votacao = FALSE)
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE)
  }else if (t==2){
    CT_O <- coTrainingOriginal(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE, combinar = TRUE)
    # ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE, votacao = FALSE)
    # ST_gra <- funcSelfTrainGradativo(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE)
  }  

  # #criando a matriz de confus?o para o modelo gerado pelo IBK usando os exemplos inicialmente rotulados
  matriz_confusao_o_v1=table(predict(CT_O[[1]],base_teste,type='class'),base_teste$class)
  matriz_confusao_o_v2=table(predict(CT_O[[2]],base_teste,type='class'),base_teste$class)
  
  # matriz_confusao_supervisionado <- table(predict(stdIBK,base_teste,type='class'),base_teste$class)
  # #criando a matriz de confus?o para o modelo gerado pela ?rvore de decis?o usando todos os exemplos de treinamento rotulados
  # matriz_confusao_tot_supervisionado <- table(predict(stdIBK_tot,base_teste,type='class'),base_teste$class)
  # matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  # matriz_confusao_gra=table(predict(ST_gra,base_teste,type='class'),base_teste$class)
}

acc_o_v1<-((sum(diag(matriz_confusao_o_v1)) / sum(matriz_confusao_o_v1)) * 100)
acc_o_v2<-((sum(diag(matriz_confusao_o_v2)) / sum(matriz_confusao_o_v2)) * 100)
acc_o <- (acc_o_v1 + acc_o_v2)/2
acc_g_o <- c(acc_g_o, acc_o)


# #fazendo teste com classificador supervisionado
# acc_sup <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
# acc_sup_tot <- ((sum(diag(matriz_confusao_tot_supervisionado)) / sum(matriz_confusao_tot_supervisionado)) * 100)
# 
# acc <- ((sum(diag(matriz_confusao1)) / sum(matriz_confusao1)) * 100)

# acc_gra<-((sum(diag(matriz_confusao_gra)) / sum(matriz_confusao_gra)) * 100)

# #ARMAZENANDO A ACURACIA EM UMA VARIAVEL (ATE LINHA 118)
# #fazendo teste com classificador supervisionado
# acc_g_sup <- c(acc_g_sup, acc_sup)
# acc_g_sup_tot <- c(acc_g_sup, acc_sup)
# acc_g <- c(acc_g, acc)
# acc_g_gra <- c(acc_g_gra, acc_gra)

# acc_g_o <- c(acc_g_o, acc_o)
#ARMAZENANDO EM UMA VARIAVEL O NOME DA BASE E O % DE EXEMPLOS ROTULADOS INICIALMENTE
bd <- c(bd, bd_nome)
tx <- c(tx, taxa)

#imprimindo a acuracia na tela 
cat("\n Acerto global original (%) =", acc_o)
# cat("\n Acerto global modificado (%) =", acc)
# cat("\n Acerto global gradativo (%) =", acc_gra)

#fazendo teste com classificador supervisionado
# cat("\n Acerto global supervisionado (%) =", acc_sup)
# cat("\n Acerto global totalmente supervisionado (%) =", acc_sup_tot)

cat("\n FIM") #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')
