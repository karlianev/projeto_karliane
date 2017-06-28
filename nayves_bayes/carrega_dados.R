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


