print("organizando os dados")

set.seed(214)# garante que o conjunto de dados escolhido para treinamento ser? sempre o mesmo - n?o sei se preciso dessa garantia

#Quantidade de Exemplos
exemplos = nrow(base)
#NÃO DEU CERTO, ACHO Q TÁ PEGANDO EXEMPLOS VAZIOS
#ACHO Q O ERRO NÃO É AQUI PQ PEGA OS EXEMPLOS DIREITINHO ATÉ O SAMPLE, MAS NA HORA 
#DE PEGAR OS DADOS DA BASE USANDO ESSES INDICES AÍ ELE PEGA DE ACORDO COM A POSIÇÃO
#NO VETOR BASE, AO INVES DE PEGAR PELO INDICE
#exemplos <- indice_treinamento

#taxa inicial de exemplos rotulados erm percentual
#taxa = 10 a taxa é setada no script_main
#taxa_inicial = exemplos*taxa/100
taxa_inicial = exemplos*(taxa/100)




#sorteio de ids para treinamento
ids_treino_rot <- sample(exemplos,taxa_inicial, replace=FALSE)


#base de treinamento
base_treino_rot <- base[ids_treino_rot,]
base_treino_sem_rot <- base[-ids_treino_rot,]

base_treino_sem_rot$class <- NA #para base IRIS


#base de treinamento rotulada
base_treino_self_training_rot <- base_treino_rot
base_treino_self_training_sem_rot <- base_treino_sem_rot
dfs <- list(base_treino_self_training_rot, base_treino_self_training_sem_rot)
base_treino_self_training <- join_all(dfs, type="full")

