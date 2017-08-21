print("criando diretório local")

setwd("C:\\local_R\\projeto_karliane")

print("instalação dos pacotes")

#pacote que inclui: data splitting, pre-processing, feature selection, model tuning using resampling, variable importance estimation
#install.packages("caret")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#pacote que inclui self-training e outros algoritmos de aprendizado semisupervisionado
#install.packages("ssc")
#install.packages("DMwR")
#install.packages("caTools")
#install.packages("RWeka")

print("carregar os pacotes")
library("caret") #parece n?o ser necess?rio
library("ssc") #esse ? obrigat?rio
library("plyr") #pacote q tem a fun??o join_all
library("RWeka")

#USANDO A FUN??O SELFTRAIN (USADA POR ALEXANDRE)

library("DMwR2")
library("DMwR")
library("datasets")

library("e1071")
