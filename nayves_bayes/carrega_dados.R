print("carregando os dados")

if (i==1) {
  #base de dados IRIS
  base_original <- read.arff("iris.arff")
  bd_nome <- "iris"
}else if (i==2){
  #base de dados ECOLI
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


