print("carregando os dados")

if (i==1) {
  #base de dados IRIS
  base_original <- read.arff("iris.arff")
}else if (i==2){
  #base de dados ECOLI
  base_original <- read.arff("letter.arff")
}else if(i==3){
  base_original <- read.arff("bupa.arff");

}else if(i==4){
  base_original <- read.arff("segment.arff")

}else if(i==5){
  base_original <- read.arff("haberman.arff")
}else if(i==6){
  base_original <-read.arff("pima.arff")

}else if(i==7){
  base_original <-read.arff("cleveland.arff")

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

