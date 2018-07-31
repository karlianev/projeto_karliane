classifiers <<- c("naiveBayes", "rpartXse", "JRip", "IBk")
obj <- c(learner(classifiers[1], list()), learner(classifiers[2], list(se=0.5)), learner(classifiers[3], list()),
         learner(classifiers[4], list(control = Weka_control(K = 15, X = TRUE))))
funcs <- c("func", "f", "f2", "f2")

supModel <- function(cl){
  switch (cl,
          "naiveBayes" = std <- naiveBayes(as.formula(paste(classe, '~', '.')), base_rotulados_ini),
          "rpartXse" = std <- rpartXse(as.formula(paste(classe, '~', '.')), base_rotulados_ini, se=0.5),
          "JRip" = std <- JRip(as.formula(paste(classe, '~', '.')), base_rotulados_ini),
          "IBk" = std <- IBk(as.formula(paste(classe, '~', '.')), base_rotulados_ini, control = Weka_control(K = 15, X = TRUE))
  )
  return(std)
}

supAcc <- function(cl){
  supModel(cl)
  matriz_confusao_supervisionado <- table(predict(std, base_rotulados_ini, type = 'class'), base_rotulados_ini$class)
  acc_sup_3 <- ((sum(diag(matriz_confusao_supervisionado)) / sum(matriz_confusao_supervisionado)) * 100)
  return(acc_sup_3)
}

for(c in 1:length(classifiers)){
  flex_con_c1_s <- funcSelfTrainModificado2(obj[[c]], funcs[c], 0.95, 100, 1, TRUE, qtd_exem_menor_classe, limiar = supAcc(classifiers[c]))
  
  flex_con_c1_v <- funcSelfTrainModificado2(obj[[c]], funcs[c], 0.95, 100, 1, TRUE, qtd_exem_menor_classe, limiar = supAcc(classifiers[c]))
  
  flex_con_c2 <- funcSelfTrainModificado3(obj[[c]], funcs[c], 0.95, 100, 1, TRUE, qtd_exem_menor_classe, limiar = supAcc(classifiers[c]))
  
  matriz_confusao1 <- table(predict(ST_s, base_teste), base_teste$class)
  matriz_confusao2 <- table(predict(ST_v, base_teste), base_teste$class)
  matriz_confusao3 <- table(predict(ST_3, base_teste), base_teste$class)
}
if(c==2){ #AD

  ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
  ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdTree)
  
  matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste,type='class'),base_teste$class)
  
}

if(c==3){ #RIPPER

  ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
  ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('JRip',list()),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdJRip)
  
  matriz_confusao1 = table(predict(ST,base_teste),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste),base_teste$class)
}

if(c==4){ #IBK
  
  ST <- funcSelfTrainModificado2(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, votacao = FALSE)
  ST_3 <- funcSelfTrainModificado3(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('IBk',list(control = Weka_control(K=15, X=TRUE))),'f2',0.95,100,1,TRUE,qtd_exem_menor_classe, limiar = acc_sup_3, stdIBK)

  matriz_confusao1 = table(predict(ST,base_teste),base_teste$class)
  matriz_confusao3 = table(predict(ST_3,base_teste),base_teste$class)
}


n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_3 <- ((sum(diag(matriz_confusao3)) / n) * 100)


acc_g <- c(acc_g, acc)
acc_g_3 <- c(acc_g_3, acc_3)


#fazendo teste com classificador supervisionado
acc_g_sup <- c(acc_g_sup, acc_sup_3)


bd <- c(bd, bd_nome)
tx <- c(tx, taxa)
cat("\n Acerto global modif_2 (%) =", acc)
cat("\n Acerto global modif_3 (%) =", acc_3)
cat("\n Acerto global supervisionado (%) =", acc_sup_3)