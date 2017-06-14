#PROBLEMAS A RESOLVER
#karliane - tentar pegar os resultados (acuracia) - ver base teste script alexandre
#alan - aprender como colocar os resultados em uma matriz e depois em um arquivo
#alan - incluir as demais bases nesse script
#karliane e alan - aprender a usar outros classificadores que não seja arvore
#dividir a base em treinamento e teste, o q eu fiz não tá certo.
#1 - transformar os atributos não numéricos em numéricos - tentar filtro weka - alan achou paleativo, usaremos de acordo com a necessidade
#2 - descobrir pq a confiança da iris só dá 1 - resolvido, não sei como...

#bases de dados
#bupa, cleveland, ecoli, glass, haberman, iris, monk, pima, vehide, wisconsin
#diretório local para salvar as bases e resultados
setwd("C:\\local_R")

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
library("caret") #parece não ser necessário
library("ssc") #esse é obrigatório
library("plyr") #pacote q tem a função join_all
library("RWeka")

#USANDO A FUNÇÃO SELFTRAIN (USADA POR ALEXANDRE)

library("DMwR2")
library("DMwR")
library("datasets")

print("Função para pegar a base de dados e colocar em uma variável base")
getdata <- function(...)
{
    e <- new.env()
    name <- data(..., envir = e)[1]
    e[[name]]
}

#variaveis para guardar e gravar no arquivo
it_g <-c() 
bd_g <-c()
thrConf_g<-c()
nr_added_exs_g<-c()

for (i in 1:6){

  print("organizando os dados")

  if (i==1) {
    #base de dados IRIS
    base_original <- getdata("iris")
    classe <- "Species"
  }else if (i==2){
    #base de dados ECOLI
    base_original <- read.arff("ecoli.arff")
    classe <- "class"
  }
  else if(i==3){
    base_original <- read.arff("bupa.arff");
    classe <- "selector"
    
  }
  else if(i==4){
    base_original <- read.arff("glass.arff")
    classe <- "Type"
    
  }
  else if(i==5){
    base_original <- read.arff("haberman.arff")
    classe <-"Survival_status"
  }
  else if(i==6){
    base_original <-read.arff("pima.arff")
    classe <- "class"
    
  }
  else if(i==7){
    base_original <-read.arff("cleveland.arff")
    classe <- "num"
    
  }
  #tentando usar filtro do weka para transformar dados nominais em binarios
  #nombi <- make_Weka_filter("weka/filters/supervised/attribute/NominalToBinary") # creates an R interface to the WEKA filter
  #datbin <- nombi(AT1 ~., data=base, control =Weka_control(N=TRUE, A=TRUE)) # Fehlermeldung
  #datbin
  

#NÃO ESTÁ CERTO ASSIM, POIS ALGUNS EXEMPLOS NÃO ESTÃO SENDO USADOS NO TREINAMENTO NUNCA E OUTROS ESTÃO APARECENDO MAIS DE UMA VEZ
  set.seed(100)
  if (i==1){
    indice_treinamento <- createDataPartition(base_original$Species, p=0.75, list=FALSE)
  }else if (i==2){
    indice_treinamento <- createDataPartition(base_original$class, p=0.75, list=FALSE)
  }
    base <- base_original[indice_treinamento,]
    base_teste <- base_original[-indice_treinamento,]
    #PRECISO RENUMERAR OS INDICES, TANTO DE TREINAMENTO QUANTO DE TESTE
  
  
  set.seed(214)# garante que o conjunto de dados escolhido para treinamento será sempre o mesmo - não sei se preciso dessa garantia
  
  #Quantidade de Exemplos
  exemplos = nrow(base)
  
  #taxa inicial de exemplos rotulados erm percentual
  taxa = 10
  taxa_inicial = exemplos*taxa/100
  
  


#sorteio de ids para treinamento
  ids_treino_rot <- sample(exemplos,taxa_inicial, replace=FALSE)
  

  #base de treinamento
  base_treino_rot <- base[ids_treino_rot,]
  base_treino_sem_rot <- base[-ids_treino_rot,]


  if (i==1) base_treino_sem_rot$Species <- NA #para base IRIS
  else if (i==2) base_treino_sem_rot$class <- NA #para base ECOLI
  else if(i==3)  base_treino_sem_rot$selector <- NA # para base BUPA
  else if(i==4)   base_treino_sem_rot$Type <- NA # para base glass
  else if(i==5) base_treino_sem_rot$Survival_status<- NA # para base haberman
  else if (i==6) base_treino_sem_rot$class <- NA #para base pima
  else if (i==7) base_treino_sem_rot$num <- NA #para base cleveland
  
  #base de treinamento rotulada
  base_treino_self_training_rot <- base_treino_rot
  base_treino_self_training_sem_rot <- base_treino_sem_rot
  dfs <- list(base_treino_self_training_rot, base_treino_self_training_sem_rot)
  base_treino_self_training <- join_all(dfs, type="full")
  
  print("iniciando o treinamento")
  #função que será passada como parâmetro predFunc da função selftrain
  f <- function(m,d) {
  	l <- predict(m,d,type='class')
  	c <- apply(predict(m,d),1,max)
  	data.frame(cl=l,p=c)
  }
  
  #setando parametros do selftrain
  
  #classes da base de dados
  if (i==1) form <- Species~.  	#para base IRIS		  #OU form <- basetreinoselftraining$Species
  if (i==2) form <- class~.      #para base ECOLI
  if(i==3) form <- selector~.    #para base puma
  if(i==4)form <- Type~. # para base glass
  if(i==5)form <- Survival_status~.# base haberman
  if (i==6) form <- class~.      #para base pima
  if(i==7) form <- num~. # para base cleveland
  data <- base_treino_self_training	#base de dados
  learn <- learner('rpartXse',list(se=0.5))
  predFunc <- 'f'   			#Uma string com o nome de uma função que irá realizar as tarefas de classificação probabilística que serão necessárias durante o processo de self-training
  thrConf=0.9       			#taxa de confiança dos exemplos a serem incluidos no conjunto de rotulados
  maxIts=10					#número máximo de iterações
  percFull=1					#Um número entre 0 e 1. Se a porcentagem de exemplos rotulados atingir esse valor o processo de self-training é parado
  verbose=TRUE				#Um booleano indicando o nível de verbosidade?? (verbosity??) da função
  
  #adaptação da implementação do selftrain
  data
  N <- NROW(data)
  it <- 0
  
  
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
      repeat {
        
        it <- it+1
  	

      	if (it>1) thrConf <- (thrConf + (soma_Conf/qtd_Exemplos_Rot) + (qtd_Exemplos_Rot/N))/3
      	soma_Conf <- 0
      	qtd_Exemplos_Rot <- 0
#      	cat('zerou variaveis', '\t limiar confiança(thrConf).',thrConf,'\n soma Confiança rotulados. =',soma_Conf , '\n quantidade rotulados. =',qtd_Exemplos_Rot,'\n')
      
  
        model <- runLearner(learn,form,data[sup,])
        probPreds <- do.call(predFunc,list(model,data[-sup,]))
  

        new <- which(probPreds[,2] > thrConf)
  	

  
        

        if (verbose) {
            cat('IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
            ##guardando nas variaveis 
            it_g <-c(it_g,it)
            bd_g <-c(bd_g,i)
            thrConf_g<-c(thrConf_g,thrConf)
            nr_added_exs_g<-c(nr_added_exs_g,length(new))
            ##resultado <-  c(it,",",i,",",thrConf,",",length(new))
            ##write(resultado, file = "result")
            
          
        }

        
        if (length(new)) {
          data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
  
  	      soma_Conf <- sum(soma_Conf, probPreds[new,2])
  	      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
  	      totalrot <- totalrot + qtd_Exemplos_Rot
#   	      cat('dentro do self training', '\n limiar confiança(thrConf).',thrConf,'\n soma Confiança rotulados. =',soma_Conf, '\n quantidade rotulados. =',qtd_Exemplos_Rot,'\n','\n total rotulados. =',totalrot,'\n')
  
          sup <- c(sup,(1:N)[-sup][new])
        } else break
        if (it == maxIts || length(sup)/N >= percFull) break
      }
  
#matriz de confusao do selftraining
#NÃO ESTÁ FUNCIONANDO PARA BASE DE DADOS 2, A MATRIZ NÃO APARECE COM A MESMA QUANTIDADE DE LINHAS E COLUNAS  
    if (i==1){
      matriz_confusao1 = table(predict(model,base_teste,type='class'),base_teste$Species)
      n <- length(base_teste$Species)
  }
  else if (i==2){
    matriz_confusao1 = table(predict(model,base_teste,type='class'),base_teste$class)
    n <- length(base_teste$class)
  }
  #matriz_confusao1
  if (i==1)
    cat("\n Acerto (%) = \n", levels(base_original[, "Species"]), "\n", diag(matriz_confusao1) / colSums(matriz_confusao1) * 100)
  else if (i==2)
    cat("\n Acerto (%) = \n", levels(base_original[, "class"]), "\n", diag(matriz_confusao1) / colSums(matriz_confusao1) * 100)

  cat("\n Acerto global (%) =", sum(diag(matriz_confusao1)) / n * 100)
  
  
#  data
  #predicted predict(model, newdata = base_teste)
  cat('FIM', '\t base de dados ', i, '\n', 'total rotulados: ', totalrot, '\n')
}
#data frame que sera guardado no arquivo
data_arquivo <- data.frame(it_g,bd_g,thrConf_g,nr_added_exs_g)
#escrever no arquivo
write.csv(data_arquivo, "resultado.csv", row.names = FALSE)
