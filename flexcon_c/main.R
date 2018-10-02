#' @description This function set the class atribute to NA without change the class of selected samples
#'
#' @usage newBase(base_rotulada, ids_treino_rot)
#'
#' @param base_rotulada the full dataset without changes
#' @param ids_treino_rot the vector with the selected samples
#'
#' @return a new dataset with some percents of the samples have the NA in class atribute
#'
#' @examples
#' data(iris)
#'
#' H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100), mode = "stratified")
#' base <- newBase(base_rotulada_treino, ids_treino_rot)
#' ids_treino_rot <- H2$tr
#'
#' @seealso rminer.holdout
newBase <- function(base_rotulada, ids_treino_rot){
  base_rotulada[- ids_treino_rot, "class"] <- NA
  return (base_rotulada)
}

#' Função chega o SO utilizado e seta o diretório
#' This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if(mySystem[[1]] == "Linux"){
    setwd("~/R/karliane/projeto_karliane/flexcon_c")
  }else{
    setwd("C:\\local_R\\projeto_karliane\\flexcon_c")
  }
}

setWorkspace()

# Carregando o script com as funções
# Loading functions script
source('functions.R')
installNeedPacks()
source('crossValidation.R')

initGlobalVariables()
defines()

medias_c1_s <- cleanVector(medias_c1_s)
medias_c1_v <- cleanVector(medias_c1_v)
medias_c2 <- cleanVector(medias_c2)

for (cr in change_rate) {
  for (cl in 1:length(classifiers)) {
    for(i in 0:30) {
      source('databases.R')
      k_NN <- attKValue(base_original)
      qtd_exem_por_classe <- ddply(base_original, ~class, summarise, number_of_distinct_orders = length(class))
      qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$number_of_distinct_orders) * 0.1)
      folds <- crossValidation(base_original, base_original$class)
      for(j in 1:5) {
        taxa <- j * 5
        acc_c1_s <- cleanVector(acc_c1_s)
        acc_c1_v <- cleanVector(acc_c1_v)
        acc_c2 <- cleanVector(acc_c2)
        for (fold in 1:length(folds)) {
          base_teste <- base_original[folds[[fold]], ]
          base <- base_original[- folds[[fold]], ]
          treinamento <<- base_rotulada_treino <- base
          #sorteando os exemplos que ficarão rotulados inicialmente
          cat("\nCR:", cr, "   CL:", cl, "   BD:", i, "   TX:", j, "   FOLD:", fold)
          H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100), mode = "stratified")
          ids_treino_rot <- H2$tr
          base <- newBase(base_rotulada_treino, ids_treino_rot)
          base_rotulados_ini <- base_rotulada_treino[ids_treino_rot, ]
          source('training.R')
        }
        medias_c1_s <- appendVectors(medias_c1_s, acc_c1_s)
        medias_c1_v <- appendVectors(medias_c1_v, acc_c1_v)
        medias_c2 <- appendVectors(medias_c2, acc_c2)
      }
    }
    output_archive(cr, as.character(classifiers[cl]), medias_c1_s, medias_c1_v, medias_c2)
    medias_c1_s <- cleanVector(medias_c1_s)
    medias_c1_v <- cleanVector(medias_c1_v)
    medias_c2 <- cleanVector(medias_c2)
  }
}
