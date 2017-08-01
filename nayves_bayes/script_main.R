# PROBLEMAS A RESOLVER: 
#EXTRATIFICAR os conjuntos de treinamento e teste, resolve : 
#ta dando erro enas bases iris (com taxa de 5 %) e cleveland 
#Problema da base cleveland para todas as taxas: no momento da predição a classe 4  não é rotulada para nenhum exemplo. Isso acontece pq não existe nenhum exemplo da classe 4 no conjunto de teste
#Posso resolver esse problema forçando a seleção de exemplos a pegar pelo menos um exemplo de cada classe?
#fazer funcionar o Naive bayes para bases iris e cleveland

#Tentar dados categoricos(splice com nb deu certo)

#karliane e alan - aprender a usar outros classificadores (knn, svm, jrip=ripper) que n?o seja arvore, naive pag 223 livro torgo
#aprender como transformar dados categ?ricos em num?ricos
#selecionar outras base de dados para somar 10
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


#
source('C:/local_R/projeto_karliane/nayves_bayes/configuracoes.R')

source('C:/local_R/projeto_karliane/nayves_bayes/funcoes.R')
for(k in 1:2){  # 1 = NB, 2 = AD
  it_g <-c() 
  bd_g <-c()
  thrConf_g<-c()
  nr_added_exs_g<-c()
  tx_g <- c()
  acc_g <- c()
  
  it_g_o <-c() 
  bd_g_o <-c()
  thrConf_g_o <-c()
  nr_added_exs_g_o <-c()
  tx_g_o <- c()
  acc_g_o <- c()

  it_g_gra <-c() 
  bd_g_gra <-c()
  thrConf_g_gra <-c()
  nr_added_exs_g_gra <-c()
  tx_g_gra <- c()
  acc_g_gra <- c()
  
  bd <- c()
  tx <- c()
  for(i in 2:9){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N�O FUNCIONA - da erro
        if(j==1){             
          taxa=5              #base 10 - SPLICE AD N�O FUNCIONA - trava
        }                     #base 11 - LEAF NB N�O FUNCIONA - da erro
        else if(j==2){        #base 12 - CLEVELAND N�O FUNCIONA (AD NEM NB) - da erro - base com ruído
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
  if(k==1){
    #data frame que sera guardado no arquivo
    data_arquivo <- data.frame(tx_g,it_g,bd_g,thrConf_g,nr_added_exs_g)
    data_arquivo_por_taxa <- c(data_arquivo[data_arquivo$tx_g<10,],data_arquivo[data_arquivo$tx_g<15 & data_arquivo$tx_g>5,], data_arquivo[data_arquivo$tx_g<20 & data_arquivo$tx_g>10,], data_arquivo[data_arquivo$tx_g<25 & data_arquivo$tx_g>15,], data_arquivo[data_arquivo$tx_g<30 & data_arquivo$tx_g>20,])
    #escrever no arquivo
    write.csv(data_arquivo, "resultado_nb.csv", row.names = FALSE)

    data_arquivo_o <- data.frame(tx_g_o,it_g_o,bd_g_o,thrConf_g_o,nr_added_exs_g_o)
    data_arquivo_por_taxa_o <- c(data_arquivo_o[data_arquivo_o$tx_g_o<10,],data_arquivo_o[data_arquivo_o$tx_g_o<15 & data_arquivo_o$tx_g_o>5,], data_arquivo_o[data_arquivo_o$tx_g_o<20 & data_arquivo_o$tx_g_o>10,], data_arquivo_o[data_arquivo_o$tx_g_o<25 & data_arquivo_o$tx_g_o>15,], data_arquivo_o[data_arquivo_o$tx_g_o<30 & data_arquivo_o$tx_g_o>20,])
    #escrever no arquivo
    write.csv(data_arquivo_o, "resultado_nb_o.csv", row.names = FALSE)

    data_arquivo_gra <- data.frame(tx_g_gra,it_g_gra,bd_g_gra,thrConf_g_gra,nr_added_exs_g_gra)
    data_arquivo_por_taxa_gra <- c(data_arquivo_gra[data_arquivo_gra$tx_g_gra<10,],data_arquivo_gra[data_arquivo_gra$tx_g_gra<15 & data_arquivo_gra$tx_g_gra>5,], data_arquivo_gra[data_arquivo$tx_g_gra<20 & data_arquivo_gra$tx_g_gra>10,], data_arquivo_gra[data_arquivo_gra$tx_g_gra<25 & data_arquivo_gra$tx_g_gra>15,], data_arquivo_gra[data_arquivo_gra$tx_g_gra<30 & data_arquivo_gra$tx_g_gra>20,])
    #escrever no arquivo
    write.csv(data_arquivo_gra, "resultado_nb_gra.csv", row.names = FALSE)
    
    data_arquivo_acc <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb.csv", row.names = FALSE)
    #write.csv(data_arquivo_acc, "resultado_acc_nb.csv", row.names = FALSE)
    
    data_arquivo_acc <- data.frame(tx, bd, acc_g_o)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_o.csv", row.names = FALSE)

    data_arquivo_acc <- data.frame(tx, bd, acc_g_gra)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_gra.csv", row.names = FALSE)
    
  }
  if(k==2){
    #data frame que sera guardado no arquivo
    data_arquivo <- data.frame(tx_g,it_g,bd_g,thrConf_g,nr_added_exs_g)
    #data_arquivo_por_taxa <- c(data_arquivo[data_arquivo$tx_g<10,],data_arquivo[data_arquivo$tx_g<15 & data_arquivo$tx_g>5,], data_arquivo[data_arquivo$tx_g<20 & data_arquivo$tx_g>10,], data_arquivo[data_arquivo$tx_g<25 & data_arquivo$tx_g>15,], data_arquivo[data_arquivo$tx_g<30 & data_arquivo$tx_g>20,])
    #escrever no arquivo
    write.csv(data_arquivo, "resultado_ad.csv", row.names = FALSE)

    data_arquivo_o <- data.frame(tx_g_o,it_g_o,bd_g_o,thrConf_g_o,nr_added_exs_g_o)
    data_arquivo_por_taxa_o <- c(data_arquivo_o[data_arquivo_o$tx_g_o<10,],data_arquivo_o[data_arquivo_o$tx_g_o<15 & data_arquivo_o$tx_g_o>5,], data_arquivo_o[data_arquivo_o$tx_g_o<20 & data_arquivo_o$tx_g_o>10,], data_arquivo_o[data_arquivo_o$tx_g_o<25 & data_arquivo_o$tx_g_o>15,], data_arquivo_o[data_arquivo_o$tx_g_o<30 & data_arquivo_o$tx_g_o>20,])
    #escrever no arquivo
    write.csv(data_arquivo_o, "resultado_ad_o.csv", row.names = FALSE)

    data_arquivo_gra <- data.frame(tx_g_gra,it_g_gra,bd_g_gra,thrConf_g_gra,nr_added_exs_g_gra)
    data_arquivo_por_taxa_gra <- c(data_arquivo_gra[data_arquivo_gra$tx_g_gra<10,],data_arquivo_gra[data_arquivo_gra$tx_g_gra<15 & data_arquivo_gra$tx_g_gra>5,], data_arquivo_gra[data_arquivo_gra$tx_g_gra<20 & data_arquivo_gra$tx_g_gra>10,], data_arquivo_gra[data_arquivo_gra$tx_g_gra<25 & data_arquivo_gra$tx_g_gra>15,], data_arquivo_gra[data_arquivo_gra$tx_g_gra<30 & data_arquivo_gra$tx_g_gra>20,])
    #escrever no arquivo
    write.csv(data_arquivo_gra, "resultado_ad_gra.csv", row.names = FALSE)
    
    data_arquivo_acc <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad.csv", row.names = FALSE)
    
    #arquivo para funcao original
    data_arquivo_acc <- data.frame(tx, bd, acc_g_o)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_o.csv", row.names = FALSE)

    data_arquivo_acc <- data.frame(tx, bd, acc_g_gra)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_gra.csv", row.names = FALSE)
    
    }
}

