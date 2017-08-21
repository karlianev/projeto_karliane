print("carregando os dados")
setwd("C:\\local_R\\projeto_karliane\\bases")

if (i==1) {
  #base de dados IRIS
  base_original <- read.arff("iris.arff")
  bd_nome <- "iris"
}else if (i==2){
  base_original <- read.arff("phishingData.arff")
  bd_nome <- "phishing"
}else if (i==3){
  base_original <- read.arff("letter.arff")
  bd_nome <- "letter"
}else if(i==4){
  base_original <- read.arff("bupa.arff");
  bd_nome <- "bupa"
}else if(i==5){
  base_original <- read.arff("segment.arff")
  bd_nome <- "segment"
}else if(i==6){
  base_original <- read.arff("haberman.arff")
  bd_nome <- "haberman"
}else if(i==7){
  base_original <-read.arff("pima.arff")
  bd_nome <- "pima"
}else if(i==8){
  base_original <-read.arff("cleveland.arff")
  bd_nome <- "cleveland"
}
classe <- "class"


#tentando usar filtro do weka para transformar dados nominais em binarios
#nombi <- make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary") # creates an R interface to the WEKA filter
#datbin <- nombi(AT1 ~., data=base, control =Weka_control(N=TRUE, A=TRUE)) # Fehlermeldung
#datbin


#N?O EST? CERTO ASSIM, POIS ALGUNS EXEMPLOS N?O EST?O SENDO USADOS NO TREINAMENTO NUNCA E OUTROS EST?O APARECENDO MAIS DE UMA VEZ
set.seed(100)

indice_treinamento <- createDataPartition(base_original$class, p=0.75, list=FALSE)



base <- base_original[indice_treinamento,]
base_teste <- base_original[-indice_treinamento,]
#PRECISO RENUMERAR OS INDICES, TANTO DE TREINAMENTO QUANTO DE TESTE
setwd("C:\\local_R\\projeto_karliane")

