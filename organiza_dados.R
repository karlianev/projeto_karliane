print("organizando os dados")

if (i==1) {
  #base de dados IRIS
  base_original <- getdata("iris")
  classe <- "Species"
}else if (i==2){
  #base de dados ECOLI
  base_original <- read.arff("ecoli.arff")
  classe <- "class"
}else if(i==3){
  base_original <- read.arff("bupa.arff");
  classe <- "selector"
  
}else if(i==4){
  base_original <- read.arff("glass.arff")
  classe <- "Type"
  
}else if(i==5){
  base_original <- read.arff("haberman.arff")
  classe <-"Survival_status"
}else if(i==6){
  base_original <-read.arff("pima.arff")
  classe <- "class"
  
}else if(i==7){
  base_original <-read.arff("cleveland.arff")
  classe <- "num"
  
}

#tentando usar filtro do weka para transformar dados nominais em binarios
#nombi <- make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary") # creates an R interface to the WEKA filter
#datbin <- nombi(AT1 ~., data=base, control =Weka_control(N=TRUE, A=TRUE)) # Fehlermeldung
#datbin


#N?O EST? CERTO ASSIM, POIS ALGUNS EXEMPLOS N?O EST?O SENDO USADOS NO TREINAMENTO NUNCA E OUTROS EST?O APARECENDO MAIS DE UMA VEZ
set.seed(100)
if (i==1){
  indice_treinamento <- createDataPartition(base_original$Species, p=0.75, list=FALSE)
}else if (i==2){
  indice_treinamento <- createDataPartition(base_original$class, p=0.75, list=FALSE)
}else if (i==3){
  indice_treinamento <- createDataPartition(base_original$selector, p=0.75, list=FALSE)
}else if (i==4){
  indice_treinamento <- createDataPartition(base_original$Type, p=0.75, list=FALSE)
}else if (i==5){
  indice_treinamento <- createDataPartition(base_original$Survival_status, p=0.75, list=FALSE)
}else if (i==6){
  indice_treinamento <- createDataPartition(base_original$class, p=0.75, list=FALSE)
}else if (i==7){
  indice_treinamento <- createDataPartition(base_original$num, p=0.75, list=FALSE)
}



base <- base_original[indice_treinamento,]
base_teste <- base_original[-indice_treinamento,]
#PRECISO RENUMERAR OS INDICES, TANTO DE TREINAMENTO QUANTO DE TESTE


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
taxa_inicial = nrow(base)*taxa/100




#sorteio de ids para treinamento
ids_treino_rot <- sample(exemplos,taxa_inicial, replace=FALSE)


#base de treinamento
base_treino_rot <- base[ids_treino_rot,]
base_treino_sem_rot <- base[-ids_treino_rot,]


if (i==1) {
  base_treino_sem_rot$Species <- NA #para base IRIS
}else if (i==2){
  base_treino_sem_rot$class <- NA #para base ECOLI
}else if(i==3){
  base_treino_sem_rot$selector <- NA # para base BUPA
}else if(i==4){
  base_treino_sem_rot$Type <- NA # para base glass
}else if(i==5){
  base_treino_sem_rot$Survival_status<- NA # para base haberman
}else if (i==6){
  base_treino_sem_rot$class <- NA #para base pima
}else if (i==7) base_treino_sem_rot$num <- NA #para base cleveland

#base de treinamento rotulada
base_treino_self_training_rot <- base_treino_rot
base_treino_self_training_sem_rot <- base_treino_sem_rot
dfs <- list(base_treino_self_training_rot, base_treino_self_training_sem_rot)
base_treino_self_training <- join_all(dfs, type="full")

