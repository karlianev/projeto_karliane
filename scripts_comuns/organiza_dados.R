print("organizando os dados")

set.seed(214)# garante que o conjunto de dados escolhido para treinamento ser? sempre o mesmo - n?o sei se preciso dessa garantia

exemplos = nrow(base_original)

#comando que retorna a quantidade de exemplos em cada uma das classes
qtd_exem_por_classe <- ddply(base_original,~class,summarise,number_of_distinct_orders=length(class))
#comando que retorna 10% da quantidade de exemplos da classe com menor número de instâncias
qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$number_of_distinct_orders)*0.1)

#sorteio de ids para treinamento
#indice_treinamento <- sample(exemplos,exemplos*0.75, replace=FALSE)
#base<-base_original[indice_treinamento,]
#base_teste<-base_original[-indice_treinamento,]

H <- holdout(base_original$class, ratio = 0.75, mode="stratified")
base <- base_original[H$tr,]
base_teste <- base_original[H$ts,]


H2 <- holdout(base$class, ratio = (taxa/100), mode="stratified")
ids_treino_rot <- H2$tr #ids dos exemplos que iniciarão rotulados

#ids_treino_rot<-sample(nrow(base),nrow(base)*(taxa/100))
##VER SE REALMENTE ESTÁ PEGANDO 5% (PEGAR UMA QTDE PEQ DE EXEMPLOS PARA VALIDAR)

base[-ids_treino_rot,"class"] <- NA
base_treino_self_training<-base
base_rotulados_ini <-base[ids_treino_rot,]