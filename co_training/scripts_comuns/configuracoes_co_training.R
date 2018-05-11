#install.packages("NetPreProc")
#install.packages("Rcpp")
#install.packages("caret")
# install.packages("proxy") #mesmo problema do graph
# install.packages("xgboost")
# install.packages("klaR")
# install.packages("e1071")
# install.packages("prodlim")
# install.packages("purr")

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph", suppressUpdates = TRUE)

# install.packages("SSL")

library("SSL")

#carregando todos os pacotes do self-training 
library("ggplot2")
library("caret") 
library("ssc") #esse eh obrigatorio
library("plyr") #pacote q tem a funcao join_all
library("DMwR2")
library("DMwR")
library("caTools")
library("RWeka")
library("rminer")
library("datasets")
library("e1071")
library("graph")

#pacote para usar a funcao partitio.matrix ou partition.vector
#install.packages("Hmisc")
library("Hmisc")
