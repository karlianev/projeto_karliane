print("organizando os dados")

set.seed(214)# garante que o conjunto de dados escolhido para treinamento ser? sempre o mesmo - n?o sei se preciso dessa garantia

#conta a quantidade de exemplos da base de dados completa
exemplos = NROW(base_original) #TAVA nrow

#comando que retorna a quantidade de exemplos em cada uma das classes
qtd_exem_por_classe <- ddply(base_original,~class,summarise,number_of_distinct_orders=length(class))
#comando que retorna 10% da quantidade de exemplos da classe com menor n�mero de inst�ncias
qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$number_of_distinct_orders)*0.1)

#sorteio de ids para treinamento
#indice_treinamento <- sample(exemplos,exemplos*0.75, replace=FALSE)
#base<-base_original[indice_treinamento,]
#base_teste<-base_original[-indice_treinamento,]

#cria��o dos conjuntos de treinamento e teste
#o holdout cria dois conjuntos: 1) H$tr com o id de 75% dos exemplos para treinamento e 2) H$ts com id de 25% dos eemplos para teste
H <- holdout(base_original$class, ratio = 0.75, mode="stratified") 
base <- base_original[H$tr,] 
base_teste <- base_original[H$ts,]

#vari�vel que armazena os exemplos inicialmente routlados, ser� usado para treinamento supervisionado
treinamento <<- base_rotulada_treino <- base_original[H$tr,]


#sorteando os exemplos que ficar�o rotulados inicialmente
H2 <- holdout(base$class, ratio = (taxa/100), mode="stratified")
ids_treino_rot <- H2$tr #ids dos exemplos que iniciar�o rotulados


#ids_treino_rot<-sample(nrow(base),nrow(base)*(taxa/100))
##VER SE REALMENTE EST� PEGANDO 5% (PEGAR UMA QTDE PEQ DE EXEMPLOS PARA VALIDAR)

#retirando o r�tulo dos exemplos que iniciar�o o treinamento sem r�tulo
base[-ids_treino_rot,"class"] <- NA
#atribuindo a vari�vel base_treino_self_training a base de dados semissupervisionada
base_treino_self_training<-base
#guardando os exemplos inicialmente rotulados
base_rotulados_ini <-base[ids_treino_rot,]
