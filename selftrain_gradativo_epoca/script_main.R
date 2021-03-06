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
acc_g_o <- c()
acc_g_gra <- c()

bd <- c()
tx <- c()

grad_g<-c()
grad_g_acc<-c()
source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')

for (t in 1:2) { #1 = taxa 0,9 2 = taxa 0,95
  for(k in 1:2){  # 1 = NB, 2 = AD
  
    
  
  
    it_g_gra <-c() 
    bd_g_gra <-c()
    thrConf_g_gra <-c()
    nr_added_exs_g_gra <-c()
    tx_g_gra <- c()
    acc_g_gra <- c()
    
    bd <- c()
    tx <- c()
    for(i in 4:5){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        for(g in 1:4){
          grad<-g*0.05
          source('C:/local_R/projeto_karliane/selftrain_gradativo_epoca/treinamento.R')  
        
        }
        
        
        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/treinamento.R')
      }    
    }
    print("gerando data frame para arquivos")
    #data frame que sera guardado no arquivo
   
    data_arquivo_gra <- data.frame(bd_g_gra,tx_g_gra,grad_g,it_g_gra,thrConf_g_gra,nr_added_exs_g_gra)

    #data_arquivo_acc[data_arquivo_acc$grad_g_acc<0.1&data_arquivo_acc$tx<10,]
    data_arquivo_acc <- data.frame(tx,grad_g_acc, bd, acc_g_gra)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    
    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (k==1){
        #escrever no arquivo NB
       
        write.csv(data_arquivo_gra, "resultado_nb_gra_09.csv", row.names = FALSE)
      
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_gra_09.csv", row.names = FALSE)
        
      }else if (k==2){
        #escrever no arquivo AD
      
        write.csv(data_arquivo_gra, "resultado_ad_gra_09.csv", row.names = FALSE)
       
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_gra_09.csv", row.names = FALSE)
      }      
    }else if (t == 2){ #TAXA 0.95
      if (k==1){
        #escrever no arquivo NB
       
        write.csv(data_arquivo_gra, "resultado_nb_gra_095.csv", row.names = FALSE)
       
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_gra_095.csv", row.names = FALSE)
        
      }else if (k==2){
        #escrever no arquivo AD
      
        write.csv(data_arquivo_gra, "resultado_ad_gra_095.csv", row.names = FALSE)
        
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_gra_095.csv", row.names = FALSE)
      }      
    }
  }
}    
  