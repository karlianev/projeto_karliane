print("carregando os dados")

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
  base_original <- read.arff("haberman.arff")
  bd_nome <- "haberman"
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
  base_original <-read.arff("waveform-5000.arff")
  bd_nome <- "waveform"
}else if(i==14){
  base_original <-read.arff("mushroom.arff")
  bd_nome <- "mushroom"
}else if(i==15){
  base_original <-read.arff("sick.arff")
  bd_nome <- "sick"
}else if(i==16){
  base_original <-read.arff("leaf.arff")
  bd_nome <- "leaf"
}else if(i==17){  
    base_original <-read.arff("cleveland.arff")
  bd_nome <- "cleveland"
}else if(i==18){  
  base_original <-read.arff("splice.arff")
  bd_nome <- "splice"
}

classe <- "class"


