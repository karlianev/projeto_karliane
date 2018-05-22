#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")
if (num_metodo == 3){
  vot <- FALSE
}else if (num_metodo == 4){
  vot <- TRUE
}else{
  print("ERRO!!! AO SETAR VOTACAO TRUE OU FALSE, NUMERO DO METODO NAO CONFERE")
  break
}

if(c==1){ #NAIVE BAYES
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo modificado usando naive
    CT<- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,100,1,TRUE, votacao = TRUE)
  }else if (t==2){ #TAXA INICIAL 0.95
    CT<- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.95,100,1,TRUE, votacao = TRUE)
  }
}
if(c==2){ #ARVORE DE DECISAO
  if (t==1){ #TAXA INICIAL 0.9
    #chamada da funcao que implementa o metodo modificado usando arvore de decis?o
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('J48', list()),'f',0.9,100,1,TRUE, votacao = TRUE)
    #CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,100,1,TRUE, votacao = FALSE)
  }else if (t==2){ #TAXA INICIAL 0.95
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('J48', list()),'f',0.95,100,1,TRUE, votacao = TRUE)
    # CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE, votacao = FALSE)
  }  
}
if(c==3){ #RIPPER
  if (t==1){
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.9,100,1,TRUE, votacao = TRUE)
  }else if (t==2){
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE, votacao = TRUE)
  }  
}
if(c==4){ #IBK
  if (t==1){
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.9,100,1,TRUE, votacao = TRUE)
  }else if (t==2){
    CT <- coTrainFlexCon(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE, votacao = TRUE)
  }  
}

matriz_confusao_v1<-table(predict(CT[[1]], base_teste, type = 'class'), base_teste$class)
matriz_confusao_v2<-table(predict(CT[[2]], base_teste, type = 'class'), base_teste$class)

#CALCULANDO A ACURACIA (AT? LINHA 109)
acc_o_v1<-((sum(diag(matriz_confusao_v1)) / sum(matriz_confusao_v1)) * 100)
acc_o_v2<-((sum(diag(matriz_confusao_v2)) / sum(matriz_confusao_v2)) * 100)
acc_o <- (acc_o_v1 + acc_o_v2)/2
acc_g_o <- c(acc_g_o, acc_o)


#ARMAZENANDO EM UMA VARIAVEL O NOME DA BASE E O % DE EXEMPLOS ROTULADOS INICIALMENTE
bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
metodo <- c(metodo, num_metodo)

#imprimindo a acuracia na tela 
cat("\n Acerto global original (%) =", acc_o)
cat("\n FIM") #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')
