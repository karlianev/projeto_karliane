# PROBLEMAS A RESOLVER: 
# ta dando nas bases iris e cleveland com taxa de 5 %






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
for(i in 5:5){
  for(j in 1:1){
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
}



#data frame que sera guardado no arquivo
data_arquivo <- data.frame(tx_g,it_g,bd_g,thrConf_g,nr_added_exs_g)
data_arquivo_por_taxa <- c(data_arquivo[data_arquivo$tx_g<10,],data_arquivo[data_arquivo$tx_g<15 & data_arquivo$tx_g>5,], data_arquivo[data_arquivo$tx_g<20 & data_arquivo$tx_g>10,], data_arquivo[data_arquivo$tx_g<25 & data_arquivo$tx_g>15,], data_arquivo[data_arquivo$tx_g<30 & data_arquivo$tx_g>20,])
#escrever no arquivo
write.csv(data_arquivo, "resultado_nb.csv", row.names = FALSE)

data_arquivo_acc <- data.frame(tx, bd, acc_g)

write.csv(data_arquivo_acc, "resultado_acc_nb.csv", row.names = FALSE)