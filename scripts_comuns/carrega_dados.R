print("carregando os dados")
#setando o diret?rio local para ser a pasta onde est?o as bases
setwd("C:\\local_R\\projeto_karliane\\bases")
# setwd("~/R/karliane/projeto_karliane/bases")


if (i==1) {
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
  base_original <- read.arff("waveform-5000.arff")
  bd_nome <- "waveform"
  
}else if(i==7){
  base_original <-read.arff("pima.arff")
  bd_nome <- "pima"
}else if(i==8){
  base_original <-read.arff("vehicle.arff")
  bd_nome <- "vehicle"
}else if(i==9){
  base_original <-read.arff("wilt.arff")
  bd_nome <- "wilt"
}else if(i==10){
  base_original <-read.arff("balance-scale.arff")
  bd_nome <- "balance-scale"
}else if(i==11){
  base_original <-read.arff("car.arff")
  bd_nome <- "car"
}else if(i==12){  
  base_original <-read.arff("kr-vs-kp.arff")
  bd_nome <- "kr-vs-kp"
}else if(i==13){  
  base_original <-read.arff("haberman.arff")
  bd_nome <- "haberman"
}else if(i==14){
  base_original <-read.arff("mushroom.arff")
  bd_nome <- "mushroom"
  #a partir daqui todas estão dando problemas

#bases para serem incluidas no WCCI (IJCNN) - selecionadas por Alan
}else if(i==15){
  base_original <-read.arff("abalone.arff")
  bd_nome <- "abalone"
}else if(i==16){  
  base_original <-read.arff("blogger.arff")
  bd_nome <- "blogger"
}else if(i==17){  
  base_original <-read.arff("blood-transfusion-service.arff")
  bd_nome <- "blood-transfusion-service"
}else if(i==18){  
  base_original <-read.arff("flare.arff")
  bd_nome <- "flare"
}else if(i==19){  
  base_original <-read.arff("leukemia-haslinger.arff")
  bd_nome <- "leukemia-haslinger"
}else if(i==20){  
  base_original <-read.arff("parkinsons.arff")
  bd_nome <- "parkinsons"
}else if(i==21){  
  base_original <-read.arff("pendigits.arff")
  bd_nome <- "pendigits"
}else if(i==22){  
  base_original <-read.arff("planning-relax.arff")
  bd_nome <- "planning-relax"
}else if(i==23){  
  base_original <-read.arff("twonorm.arff")
  bd_nome <- "twonorm.arff"
}else if(i==24){  
  base_original <-read.arff("indian-liver-patient.arff")
  bd_nome <- "indian-liver-patient"
}else if(i==25){  
  base_original <-read.arff("ozone-onehr.arff")
  bd_nome <- "ozone-onehr"

#erro
# }else if(i==16){
#   base_original <-read.arff("banana.arff")
#   bd_nome <- "banana"
  
  
#bases para serem incluidas no WCCI (IJCNN) - selecionadas por Cainan  
# }else if(i==26){  
#   base_original <-read.arff("blood-transfusion-service.arff")
#   bd_nome <- "blood-transfusion-service"
  
}

classe <- "class"

#setando o diret?rio local para ser a pasta onde esta o projeto
setwd("C:\\local_R\\projeto_karliane")
# setwd("~/R/karliane/projeto_karliane")
