print("organizando os dados")

set.seed(214)# garante que o conjunto de dados escolhido para treinamento ser? sempre o mesmo - n?o sei se preciso dessa garantia

exemplos = nrow(base_original)



H <- holdout(base_original$class, ratio = 0.75, mode="stratified")
base <- base_original[H$tr,]
base_teste <- base_original[H$ts,]



ids_treino_rot<-sample(nrow(base),nrow(base)*(taxa/100))

base[-ids_treino_rot,"class"] <- NA
base_treino_self_training<-base
base_rotulados_ini <-base[ids_treino_rot,]

