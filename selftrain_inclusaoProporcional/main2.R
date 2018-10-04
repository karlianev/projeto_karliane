# Função chega o SO utilizado e seta o diretório
# This function check the OS and change work directory
setWorkspace <- function() {
  mySystem <- Sys.info()
  if(mySystem[[1]] == "Linux"){
    setwd("~/R/karliane/projeto_karliane/selftrain_inclusaoProporcional/")
  }else{
    setwd("C:\\local_R\\projeto_karliane\\selftrain_inclusaoProporcional")
  }
}

setWorkspace()
source('functions.R')
base_vector <-setDatabases()
medias_accuracy <- cleanVector(medias_accuracy)
# Carregando o script com as funções
# Loading functions script

installNeedPacks()
source('crossValidation.R')
initGlobalVariables()
defines()

newBase <- function(base_rotulada, ids_treino_rot){
    base_rotulada[- ids_treino_rot, "class"] <- NA
    return (base_rotulada)
}

execute <- function(cr,cl,algorithm) {	
	for(i in 1:31) { # Bases
		base_original <- readDatabase(base_vector[i],".arff")
		k_NN <- attKValue(base_original)
		qtd_exem_por_classe <- ddply(base_original, ~class, summarise, number_of_distinct_orders = length(class))
		qtd_exem_menor_classe <- trunc(min(qtd_exem_por_classe$number_of_distinct_orders) * 0.1)
		folds <- crossValidation(base_original, base_original$class)
		for(j in 1:5) {
			taxa <- j * 5
			accuracy <- cleanVector(accuracy)
			for (fold in 1:length(folds)) {
				base_teste <- base_original[folds[[fold]], ]
				base <- base_original[- folds[[fold]], ]
				treinamento <<- base_rotulada_treino <- base
				#sorteando os exemplos que ficarão rotulados inicialmente
				cat("\nCR:", cr, "   CL:", classifiers[cl], "   BD:", i, "   TX:", j, "   FOLD:", fold)
				H2 <- holdout(base_rotulada_treino$class, ratio = (taxa / 100), mode = "stratified")
				ids_treino_rot <- H2$tr
				base <- newBase(base_rotulada_treino, ids_treino_rot)
				base_rotulados_ini <- base_rotulada_treino[ids_treino_rot, ]
           
				partial_acc <- training(cl,base,base_original,base_rotulados_ini,base_teste,as.character(algorithm))
				accuracy <- appendVectors(accuracy, partial_acc)
			}
			medias_accuracy <- appendVectors(medias_accuracy, accuracy) 
		}
	}
	output_archive(cr, as.character(classifiers[cl]), medias_accuracy, as.character(algorithm))
	medias_accuracy <- cleanVector(medias_accuracy)
}		

for (cr in change_rate) {
	for (cl in 3:4) { # classificadores
		for(algorithm in 1:6){ # Algoritmos
			execute(cr,cl,algorithm)
		}
	}
}
