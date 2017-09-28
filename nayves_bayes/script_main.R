# PROBLEMAS A RESOLVER: 
#EXTRATIFICAR os conjuntos de treinamento e teste, resolve : 
#ta dando erro enas bases iris (com taxa de 5 %) e cleveland 
#Problema da base cleveland para todas as taxas: no momento da predi√ß√£o a classe 4  n√£o √© rotulada para nenhum exemplo. Isso acontece pq n√£o existe nenhum exemplo da classe 4 no conjunto de teste
#Posso resolver esse problema for√ßando a sele√ß√£o de exemplos a pegar pelo menos um exemplo de cada classe?
#fazer funcionar o Naive bayes para bases iris e cleveland

#Tentar dados categoricos(splice com nb deu certo)

#aprender a usar outros classificadores (knn, svm, jrip=ripper) que n?o seja arvore, naive pag 223 livro torgo
#selecionar outras base de dados para somar 15
#fazer experimentos com o outro calculo da taxa de confian?a (thrconf)
#comparar selftrain com co-training


#variaveis globais para guardar no arquivo de resultados

it_g <-c() #iteraÁıes
bd_g <-c() #base de dados
thrConf_g<-c() #taxa de confianÁa para inclus„o de novos exemplos
nr_added_exs_g<-c() #numero de exemplos adicionados ao conj dos rotulados na iteraÁ„o corrente
tx_g <- c() #percentual de exemplos rotulados inicialmente
acc_g <- c() #acur·cia (percentual de acerto) do metodo modificado
acc_g_o <- c() #acur·cia (percentual de acerto) do metodo original
acc_g_gra <- c() #acur·cia (percentual de acerto) do metodo gradativo

bd <- c()
tx <- c()
grad_g<-c()
grad<-c()

source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')

for (t in 1:1) { #1 = taxa 0,9 2 = taxa 0,95
  for(c in 3:3){  # 1 = NB, 2 = AD 3 = JRip
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
    for(i in 2:2){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        source('C:/local_R/projeto_karliane/nayves_bayes/treinamento.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/treinamento.R')
      }    
    }
    print("gerando data frame para arquivos")
    #data frame que sera guardado no arquivo
    data_arquivo <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g)
    data_arquivo_o <- data.frame(bd_g_o,tx_g_o,it_g_o,thrConf_g_o,nr_added_exs_g_o)
    data_arquivo_gra <- data.frame(bd_g_gra,tx_g_gra,it_g_gra,thrConf_g_gra,nr_added_exs_g_gra)

    data_arquivo_acc <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    data_arquivo_acc_o <- data.frame(tx, bd, acc_g_o)
    data_arquivo_acc_por_taxa_o <- c(data_arquivo_acc_o[data_arquivo_acc_o$tx<10,],data_arquivo_acc_o[data_arquivo_acc_o$tx<15 & data_arquivo_acc_o$tx>5,], data_arquivo_acc_o[data_arquivo_acc_o$tx<20 & data_arquivo_acc_o$tx>10,], data_arquivo_acc_o[data_arquivo_acc_o$tx<25 & data_arquivo_acc_o$tx>15,], data_arquivo_acc_o[data_arquivo_acc_o$tx<30 & data_arquivo_acc_o$tx>20,])
    data_arquivo_acc_gra <- data.frame(tx, bd, acc_g_gra)
    data_arquivo_acc_por_taxa_gra <- c(data_arquivo_acc_gra[data_arquivo_acc_gra$tx<10,],data_arquivo_acc_gra[data_arquivo_acc_gra$tx<15 & data_arquivo_acc_gra$tx>5,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<20 & data_arquivo_acc_gra$tx>10,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<25 & data_arquivo_acc_gra$tx>15,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<30 & data_arquivo_acc_gra$tx>20,])
    
    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (c==1){
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_nb_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_nb_gra_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_nb_gra_09.csv", row.names = FALSE)
        
      }else if (c==2){
        #escrever no arquivo AD
        write.csv(data_arquivo, "resultado_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_ad_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_ad_gra_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_ad_gra_09.csv", row.names = FALSE)
            
      }else if (c==3){
        #escrever no arquivo ripper
        write.csv(data_arquivo, "resultado_JRip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_JRip_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_JRip_gra_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_JRip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_JRip_gra_09.csv", row.names = FALSE)
      }      
    
    }else if (t == 2){ #TAXA 0.95
      if (c==1){
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_nb_095.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_nb_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_nb_gra_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_nb_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_nb_gra_095.csv", row.names = FALSE)
        
      }else if (c==2){
        #escrever no arquivo AD
        write.csv(data_arquivo, "resultado_ad_095.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_ad_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_ad_gra_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_ad_gra_095.csv", row.names = FALSE)
            
      }else if (c==3){
        #escrever no arquivo ripper
        write.csv(data_arquivo, "resultado_JRip_095.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_JRip_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_JRip_gra_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_JRip_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_JRip_gra_095.csv", row.names = FALSE)
      }      
    
    }
  }
}    
  