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
grad<-c()
source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')

for (t in 1:2) { #1 = taxa 0,9 2 = taxa 0,95
  for(c in 1:1){  # 1 = RIPPER
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
    
#????????DEU ERRO NA BASE 15    ????????
    for(i in 2:15){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        source('C:/local_R/projeto_karliane/ripper/treinamento.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/ripper/treinamento.R')
      }    
    }
    print("gerando data frame para arquivos")
    #data frame que sera guardado no arquivo
    data_arquivo <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g)
    data_arquivo_o <- data.frame(bd_g_o,tx_g_o,it_g_o,thrConf_g_o,nr_added_exs_g_o)
    data_arquivo_gra <- data.frame(bd_g_gra,tx_g_gra,it_g_gra,thrConf_g_gra,nr_added_exs_g_gra)

    data_arquivo_acc <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    data_arquivo_acc <- data.frame(tx, bd, acc_g_o)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    data_arquivo_acc <- data.frame(tx, bd, acc_g_gra)
    data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    
    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (c==1){  #ripper
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_ripper_09.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_ripper_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_ripper_gra_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_gra_09.csv", row.names = FALSE)
        
      # }else if (c==2){
        #escrever no arquivo AD
        # write.csv(data_arquivo, "resultado_ad_09.csv", row.names = FALSE)
        # write.csv(data_arquivo_o, "resultado_ad_o_09.csv", row.names = FALSE)
        # write.csv(data_arquivo_gra, "resultado_ad_gra_09.csv", row.names = FALSE)
        # write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_09.csv", row.names = FALSE)
        # write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_o_09.csv", row.names = FALSE)
        # write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_gra_09.csv", row.names = FALSE)
      }      
    }else if (t == 2){ #TAXA 0.95
      if (c==1){
        #escrever no arquivo NB
        write.csv(data_arquivo, "resultado_ripper_095.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_ripper_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_ripper_gra_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ripper_gra_095.csv", row.names = FALSE)
        
      # }else if (c==2){
      #   #escrever no arquivo AD
      #   write.csv(data_arquivo, "resultado_ad_095.csv", row.names = FALSE)
      #   write.csv(data_arquivo_o, "resultado_ad_o_095.csv", row.names = FALSE)
      #   write.csv(data_arquivo_gra, "resultado_ad_gra_095.csv", row.names = FALSE)
      #   write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_095.csv", row.names = FALSE)
      #   write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_o_095.csv", row.names = FALSE)
      #   write.csv(data_arquivo_acc_por_taxa, "resultado_acc_ad_gra_095.csv", row.names = FALSE)
      }      
    }
  }
}    
  