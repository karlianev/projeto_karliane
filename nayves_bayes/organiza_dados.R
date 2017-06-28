print("organizando os dados")

set.seed(214)# garante que o conjunto de dados escolhido para treinamento ser? sempre o mesmo - n?o sei se preciso dessa garantia

exemplos = nrow(base_original)

#sorteio de ids para treinamento
indice_treinamento <- sample(exemplos,exemplos*0.75, replace=FALSE)

base<-base_original[indice_treinamento,]
base_teste<-base_original[-indice_treinamento,]

ids_treino_rot<-sample(nrow(base),nrow(base)*(taxa/100))

base[-ids_treino_rot,"class"] <- NA
base_treino_self_training<-base