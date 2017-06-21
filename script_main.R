#PROBLEMAS A RESOLVER
#fazer funcionar o Naive bayes
#karliane e alan - aprender a usar outros classificadores (knn, svm, jrip=ripper) que n?o seja arvore, naive pag 223 livro torgo
#resolver o problema da matriz de confusão
#selecionar outras base de dados para somar 10
#postergar:
#alan - no arquivo de resultados colocar os nomes das bases ao invés dos números
#organizar o arquivo com os resultados por iteração (data_arquivo)

#problemas com as bases: 
#base 4 - glass
#Warning messages:
  #1: In createDataPartition(base_original$class, p = 0.75, list = FALSE) :
 # Some classes have no records ( vehic wind non-float ) and these will be ignored
#2: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#3: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#4: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#5: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#6: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
  #longer object length is not a multiple of shorter object length
# além disso, na matriz de confusão as linhas e colunas não estão na mesma ordem.
#base 2 - ecoli
#Warning messages:
#  1: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#2: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#3: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#4: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#5: In diag(matriz_confusao1)/colSums(matriz_confusao1) :
 # longer object length is not a multiple of shorter object length
#base 7 - cleveland
#com o set.seed na matriz de confusão, todos os exemplos são preditos como sendo da classe 0
#sem o set.seed em alguns momentos apresenta o mesmo warning menssages da base ecoli

#1 - transformar os atributos n?o num?ricos em num?ricos - tentar filtro weka - alan achou paleativo, usaremos de acordo com a necessidade
#2 - descobrir pq a confian?a da iris s? d? 1 - resolvido, n?o sei como...

#bases de dados
#bupa, cleveland, ecoli, glass, haberman, iris, monk, pima, vehide, wisconsin
#diret?rio local para salvar as bases e resultados

  #fazer a instalação/carregamento de pacotes e definir diretório local
  source('C:/local_R/projeto_karliane/configuracoes.R')

  #variaveis para guardar e gravar no arquivo
  it_g <-c() 
  bd_g <-c()
  thrConf_g<-c()
  nr_added_exs_g<-c()
  tx_g <- c()
  acc_g <- c()
  bd <- c()
  tx <- c()
  
#  acc <- 0.0

  print("criando funções")
  source('C:/local_R/projeto_karliane/cria_funcoes.R')
  
  for (i in 3:3){
    source('C:/local_R/projeto_karliane/carrega_dados.R')
    
    for (j in 3:3){      
      if (j == 1) taxa = 5
      else if (j == 2) taxa = 10
      else if (j == 3) taxa = 15
      else if (j == 4) taxa = 20
      else if (j == 5) taxa = 25
      
      
      source('C:/local_R/projeto_karliane/organiza_dados.R')
      
      print("iniciando o treinamento")
      source('C:/local_R/projeto_karliane/treinamento.R')
      
    
  }
}
  #data frame que sera guardado no arquivo
  data_arquivo <- data.frame(tx_g,it_g,bd_g,thrConf_g,nr_added_exs_g)
  #data_arquivo_por_taxa <- c(data_arquivo[data_arquivo$tx_g<10,],data_arquivo[data_arquivo$tx_g<15 & data_arquivo$tx_g>5,], data_arquivo[data_arquivo$tx_g<20 & data_arquivo$tx_g>10,], data_arquivo[data_arquivo$tx_g<25 & data_arquivo$tx_g>15,], data_arquivo[data_arquivo$tx_g<30 & data_arquivo$tx_g>20,])
    #escrever no arquivo
  write.csv(data_arquivo, "resultado.csv", row.names = FALSE)
  
  data_arquivo_acc <- data.frame(tx, bd, acc_g)
  data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
  write.csv(data_arquivo_acc_por_taxa, "resultado_acc.csv", row.names = FALSE)

