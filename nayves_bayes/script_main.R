# PROBLEMAS A RESOLVER: 
# ta dando rro enas bases iris (com taxa de 5 %) e cleveland 
#Problema da iris 5%
#Problema da base cleveland para todas as taxas: no momento da predição a classe 4  não é rotulada para nenhum exemplo. Isso acontece pq não existe nenhum exemplo da classe 4 no conjunto de teste
#Posso resolver esse problema forçando a seleção de exemplos a pegar pelo menos um exemplo de cada classe?
#Ou posso resolver alterando a predição para zero rotulados na classe que não aparece no conjunto de teste?



#variaveis globais para guardar no arquivo de resultados

it_g <-c() 
bd_g <-c()
thrConf_g<-c()
nr_added_exs_g<-c()
tx_g <- c()
acc_g <- c()
bd <- c()
tx <- c()
#
source('C:/local_R/projeto_karliane/nayves_bayes/configuracoes.R')

source('C:/local_R/projeto_karliane/nayves_bayes/funcoes.R')
for(i in 4:5){
  for(j in 1:5){
    if(j==1){
      taxa=5
    }
    else if(j==2){
      taxa=10
    }
    else if(j==3){
      taxa=15
    }
    else if(j==4){
      taxa=20
    }
    else if(j==5){
      taxa=25
    }
    source('C:/local_R/projeto_karliane/nayves_bayes/carrega_dados.R')
    source('C:/local_R/projeto_karliane/nayves_bayes/organiza_dados.R')
    source('C:/local_R/projeto_karliane/nayves_bayes/treinamento.R')
  }
  #data frame que sera guardado no arquivo
  data_arquivo <- data.frame(tx_g,it_g,bd_g,thrConf_g,nr_added_exs_g)
  data_arquivo_por_taxa <- c(data_arquivo[data_arquivo$tx_g<10,],data_arquivo[data_arquivo$tx_g<15 & data_arquivo$tx_g>5,], data_arquivo[data_arquivo$tx_g<20 & data_arquivo$tx_g>10,], data_arquivo[data_arquivo$tx_g<25 & data_arquivo$tx_g>15,], data_arquivo[data_arquivo$tx_g<30 & data_arquivo$tx_g>20,])
  #escrever no arquivo
  write.csv(data_arquivo, "resultado_nb.csv", row.names = FALSE)
  
  data_arquivo_acc <- data.frame(tx, bd, acc_g)
  data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
  write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb.csv", row.names = FALSE)
}



