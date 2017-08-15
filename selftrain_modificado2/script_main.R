# PROBLEMAS A RESOLVER: 
#EXTRATIFICAR os conjuntos de treinamento e teste, resolve : 
#ta dando erro enas bases iris (com taxa de 5 %) e cleveland 
#Problema da base cleveland para todas as taxas: no momento da predição a classe 4  não é rotulada para nenhum exemplo. Isso acontece pq não existe nenhum exemplo da classe 4 no conjunto de teste
#Posso resolver esse problema forçando a seleção de exemplos a pegar pelo menos um exemplo de cada classe?
#fazer funcionar o Naive bayes para bases iris e cleveland

#Tentar dados categoricos(splice com nb deu certo)

#aprender a usar outros classificadores (knn, svm, jrip=ripper) que n?o seja arvore, naive pag 223 livro torgo
#selecionar outras base de dados para somar 15
#fazer experimentos com o outro calculo da taxa de confian?a (thrconf)
#comparar selftrain com co-training


#variaveis globais para guardar no arquivo de resultados

it_g <-c() 
bd_g <-c()
thrConf_g<-c()
nr_added_exs_g<-c()
tx_g <- c()
acc_g <- c()


bd <- c()
tx <- c()
teste<-c()
teste2<-c()
source('C:/local_R/projeto_karliane/selftrain_modificado2/configuracoes.R')
# source('~/R/karliane/projeto_karliane/nayves_bayes/configuracoes.R')

source('C:/local_R/projeto_karliane/selftrain_modificado2/funcoes.R')
# source('~/R/karliane/projeto_karliane/nayves_bayes/funcoes.R')

for (t in 1:2) { #1 = taxa 0,9 2 = taxa 0,95
  for(k in 1:2){  # 1 = NB, 2 = AD
    it_g <-c() 
    bd_g <-c()
    thrConf_g<-c()
    nr_added_exs_g<-c()
    tx_g <- c()
    acc_g <- c()
    bd <- c()
    tx <- c()
    for(i in 2:15){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/selftrain_modificado2/carrega_dados.R')
        source('C:/local_R/projeto_karliane/selftrain_modificado2/organiza_dados.R')


        source('C:/local_R/projeto_karliane/selftrain_modificado2/treinamento.R')

        # source('~/R/karliane/projeto_karliane/nayves_bayes/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/treinamento.R')
      }    #FIM DO J
    }  #FIM DO I
    print("gerando data frame para arquivos")
    #data frame que sera guardado no arquivo
    data_arquivo <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g)
  

    data_arquivo_acc <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])

    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (k==1){ #NB
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_nb_09.csv", row.names = FALSE)
      
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_09.csv", row.names = FALSE)
        
      }else if (k==2){ #AD
        #escrever no arquivo AD
        write.csv(data_arquivo, "resultado_ad_09.csv", row.names = FALSE)
       
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_09.csv", row.names = FALSE)
        
      }      
    }else if (t == 2){ #TAXA 0.95
      if (k==1){ #NB
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_nb_095.csv", row.names = FALSE)
        
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_095.csv", row.names = FALSE)
       
        
      }else if (k==2){ #AD
        #escrever no arquivo AD
        write.csv(data_arquivo, "resultado_ad_095.csv", row.names = FALSE)
       
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_095.csv", row.names = FALSE)
        
      }      
    }
  }#FIM DO K
}    #FIM DO T

