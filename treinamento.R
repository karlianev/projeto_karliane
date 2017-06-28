#chamando a funcao selfTrain adaptada
ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner('rpartXse',list(se=0.5)),'f',0.9,10,1,TRUE)
#ST <- funcSelfTrain(as.formula(paste(classe,'~', '.')), base_treino_self_training,learner("naiveBayes", list()),'f',0.9,10,1,TRUE)

nbST <- SelfTrain(class ~ ., base_treino_self_training, learner("naiveBayes", list()), "func")
table(predict(nbST, base_teste), base_teste$class)



matriz_confusao1 = table(predict(ST,base_teste,type='class'),base_teste$class)
n <- length(base_teste$class)

cat("\n Acerto (%) = \n", levels(base_original[, classe]), "\n", diag(matriz_confusao1) / colSums(matriz_confusao1) * 100)

acc <- ((sum(diag(matriz_confusao1)) / n) * 100)
acc_g <- c(acc_g, acc)
bd <- c(bd, i)
tx <- c(tx, taxa)
cat("\n Acerto global (%) =", acc)
cat('FIM') #, '\t base de dados ', i, '\n', 'total rotulados: ', total_rotulados, '\n')
