#testar base 12 (ks kp) 10% NB segunda it n?o inclui nada a partir da terceira nao muda confian?a
conj_treino <- c()

#variaveis globais para guardar no arquivo de resultados
treinamento <- c()
it_g <-c() 
bd_g <-c()
thrConf_g<-c()
nr_added_exs_g<-c()
tx_g <- c()
acc_g <- c()
acertou_g <- c() #quantidade de exemplos rotulados corretamente



it_g_3 <-c() 
bd_g_3 <-c()
thrConf_g_3 <-c()
nr_added_exs_g_3 <-c()
tx_g_3 <- c()
acc_g_3 <- c()
acertou_g_3 <- c() #quantidade de exemplos rotulados corretamente

grad_g <- c()

bd <- c()
tx <- c()

#fazendo teste com classificador supervisionado
acc_g_sup <- c() #acuracia (percentual de acerto) do metodo supervisionado

source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')
for (t in 2:2) { #1 = taxa 0,9 2 = taxa 0,95
  for(c in 2:2){  # 1 = NB, 2 = AD 3 = ripper 4 = IBK
    it_g <-c() 
    bd_g <-c()
    thrConf_g<-c()
    nr_added_exs_g<-c()
    tx_g <- c()
    acc_g <- c()
    acertou_g <- c()
    
    it_g_3 <-c() 
    bd_g_3 <-c()
    thrConf_g_3 <-c()
    nr_added_exs_g_3 <-c()
    tx_g_3 <- c()
    acc_g_3 <- c()
    acertou_g_3 <- c()
    
    bd <- c()
    tx <- c()
    #fazendo teste com classificador supervisionado
    acc_g_sup <- c()
    
    for(i in 30:30){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        source('C:/local_R/projeto_karliane/selftrain_modificado2/treinamento.R')

        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/selftrain_modificado2/treinamento.R')
      }    #FIM DO J
    }  #FIM DO I
    print("gerando data frame para arquivos")
  
    #fazendo teste com classificador supervisionado
    data_arquivo_acc_sup <- data.frame(tx, bd, acc_g_sup)
    data_arquivo_acc_por_taxa_sup <- c(data_arquivo_acc_sup[data_arquivo_acc_sup$tx<10,],data_arquivo_acc_sup[data_arquivo_acc_sup$tx<15 & data_arquivo_acc_sup$tx>5,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<20 & data_arquivo_acc_sup$tx>10,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<25 & data_arquivo_acc_sup$tx>15,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<30 & data_arquivo_acc_sup$tx>20,])
    
    #data frame que sera guardado no arquivo
    data_arquivo_modif2 <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g, acertou_g)
    data_arquivo_modif3 <- data.frame(bd_g_3,tx_g_3,it_g_3,thrConf_g_3,nr_added_exs_g_3, acertou_g_3)
  

    data_arquivo_acc_modif2 <- data.frame(tx, bd, acc_g)
    data_arquivo_acc_modif3 <- data.frame(tx, bd, acc_g_3)
    
    data_arquivo_acc_por_taxa_modif2 <- c(data_arquivo_acc_modif2[data_arquivo_acc_modif2$tx<10,],data_arquivo_acc_modif2[data_arquivo_acc_modif2$tx<15 & data_arquivo_acc_modif2$tx>5,], data_arquivo_acc_modif2[data_arquivo_acc_modif2$tx<20 & data_arquivo_acc_modif2$tx>10,], data_arquivo_acc_modif2[data_arquivo_acc_modif2$tx<25 & data_arquivo_acc_modif2$tx>15,], data_arquivo_acc_modif2[data_arquivo_acc_modif2$tx<30 & data_arquivo_acc_modif2$tx>20,])
    data_arquivo_acc_por_taxa_modif3 <- c(data_arquivo_acc_modif3[data_arquivo_acc_modif3$tx<10,],data_arquivo_acc_modif3[data_arquivo_acc_modif3$tx<15 & data_arquivo_acc_modif3$tx>5,], data_arquivo_acc_modif3[data_arquivo_acc_modif3$tx<20 & data_arquivo_acc_modif3$tx>10,], data_arquivo_acc_modif3[data_arquivo_acc_modif3$tx<25 & data_arquivo_acc_modif3$tx>15,], data_arquivo_acc_modif3[data_arquivo_acc_modif3$tx<30 & data_arquivo_acc_modif3$tx>20,])

    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (c==1){ #NB
        #fazendo teste com classificador supervisionado
        write.csv(data_arquivo_acc_sup, "resultado_modif2_NB_sup_09.csv", row.names = FALSE)
        
        #escrever no arquivo NB
        write.csv(data_arquivo_modif2, "resultado_modif2_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_nb_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_nb_09.csv", row.names = FALSE)
        
      }else if (c==2){ #AD
        #fazendo teste com classificador supervisionado
        write.csv(data_arquivo_acc_sup, "resultado_modif2_ad_sup_09.csv", row.names = FALSE)
        
        #escrever no arquivo AD
        write.csv(data_arquivo_modif2, "resultado_modif2_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_ad_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ad_09.csv", row.names = FALSE)
        
        
      }else if (c==3){
        #fazendo teste com classificador supervisionado
        write.csv(data_arquivo_acc_sup, "resultado_modif2_rip_sup_09.csv", row.names = FALSE)
        
        #escrever no arquivo rip
        write.csv(data_arquivo_modif2, "resultado_modif2_rip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_rip_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_rip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_rip_09.csv", row.names = FALSE)
        
      }else if (c==4){ #IBK
        #fazendo teste com classificador supervisionado
        write.csv(data_arquivo_acc_sup, "resultado_modif2_ibk_sup_09.csv", row.names = FALSE)
        
        #escrever no arquivo rip
        write.csv(data_arquivo_modif2, "resultado_modif2_ibk_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_ibk_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_ibk_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ibk_09.csv", row.names = FALSE)
        
      }
    }else if (t == 2){ #TAXA 0.95
      if (c==1){ #NB
        #escrever no arquivo NB
        write.csv(data_arquivo_modif2, "resultado_modif2_nb_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_nb_095.csv", row.names = FALSE)
       
        write.csv(data_arquivo_modif3, "resultado_modif3_nb_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_nb_095.csv", row.names = FALSE)
        
      }else if (c==2){ #AD
        #escrever no arquivo AD
        write.csv(data_arquivo_modif2, "resultado_modif2_ad_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_ad_095.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_ad_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ad_095.csv", row.names = FALSE)
        
      }else if (c==3){ #ripper
        #escrever no arquivo ripper
        write.csv(data_arquivo_modif2, "resultado_modif2_rip_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_rip_095.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_rip_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_rip_095.csv", row.names = FALSE)
        
      }else if (c==4){ #ibk
        #escrever no arquivo ripper
        write.csv(data_arquivo_modif2, "resultado_modif2_ibk_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_ibk_095.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_ibk_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ibk_095.csv", row.names = FALSE)
        
      }
    }
  }#FIM DO K
}    #FIM DO T


# caso seja necessario trazer a fun??o para o main esses s?o os parametros.
# form = as.formula(paste(classe,'~', '.'))
# data = base_treino_self_training
# learner = learner("naiveBayes", list())
# predFunc = 'f'
# thrConf=0.9
# maxIts=10
# percFull=1
# verbose=F
