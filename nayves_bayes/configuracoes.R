print("criando diretório local")

# setwd("~/R/karliane/projeto_karliane")
setwd("C:\\local_R\\projeto_karliane")

print("instalação dos pacotes")

#pacote que inclui: data splitting, pre-processing, feature selection, model tuning using resampling, variable importance estimation
#install.packages("caret")
install.packages("caret", dependencies = c("Depends", "Suggests"))
#pacote que inclui self-training e outros algoritmos de aprendizado semisupervisionado
install.packages("ssc")
install.packages("plyr")
install.packages("DMwR")
install.packages("caTools")
install.packages("RWeka")
install.packages("rminer")
install.packages("datasets")
install.packages("e1071")
install.packages("ggplot2")

print("carregar os pacotes")
library("ggplot2")
library("caret") #parece n?o ser necess?rio
library("ssc") #esse ? obrigat?rio
library("plyr") #pacote q tem a fun??o join_all
library("DMwR2")
library("DMwR")
library("caTools")
library("RWeka")
library("rminer")
library("datasets")
library("e1071")

