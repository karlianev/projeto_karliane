#chamando a funcao selfTrain adaptada
print("Iniciando Treinamento")

nbST<- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'func',0.9,10,1,TRUE)

matriz_confusao1<-table(predict(nbST, base_teste), base_teste$class)



n <- length(base_teste$class)



acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_g <- c(acc_g, acc)
bd <- c(bd, i)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)
cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')
