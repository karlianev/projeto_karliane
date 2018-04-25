#NAIVE RODANDO ORIGINAL E GRADATIVO PARA AS 30 BASES
num_metodo <- 2 # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
conj_treino <- c()

#variaveis globais para guardar no arquivo de resultados
it_g_o <-c()
bd_g_o <-c()
thrConf_g_o<-c()
nr_added_exs_g_o<-c()
nr_added_exs_g_o_v1<-c()
nr_added_exs_g_o_v2<-c()
tx_g_o <- c()
acc_g_o <- c()

grad_g <- c()
metodo_g_o <- c()

# treinamento <- c()
# it_g <-c()
# bd_g <-c()
# thrConf_g<-c()
# nr_added_exs_g<-c()
# tx_g <- c()
# acc_g <- c()
# acertou_g <- c() #quantidade de exemplos rotulados corretamente
# 
# 
# 
# it_g_3 <-c() 
# bd_g_3 <-c()
# thrConf_g_3 <-c()
# nr_added_exs_g_3 <-c()
# tx_g_3 <- c()
# acc_g_3 <- c()
# acertou_g_3 <- c() #quantidade de exemplos rotulados corretamente
# 
# grad_g <- c()
# 
bd <- c()
tx <- c()
metodo <- c()
# 
# #fazendo teste com classificador supervisionado
# acc_g_sup <- c() #acuracia (percentual de acerto) do metodo supervisionado

source('C:/local_R/projeto_karliane/co_training/scripts_comuns/configuracoes_co_training.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

source('C:/local_R/projeto_karliane/co_training/scripts_comuns/funcoes_co_training.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')
for (t in 2:2) { #1 = taxa 0,9 2 = taxa 0,95
  for(c in 2:2){  # 1 = NB, 2 = AD 3 = ripper 4 = IBK
    it_g <-c() 
    bd_g <-c()
    thrConf_g<-c()
    nr_added_exs_g<-c()
    nr_added_exs_g_v1<-c()
    nr_added_exs_g_v2<-c()
    tx_g <- c()
    acc_g_o <- c()
    acertou_g <- c()

    grad_g <- c()
    metodo_g_o <- c()
    
    grad<-c()
    bd <- c()
    tx <- c()
    metodo <- c()
    #fazendo teste com classificador supervisionado
    # acc_g_sup <- c()
    
    for(i in 30:30){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        source('C:/local_R/projeto_karliane/co_training/treinamento_co_training.R')
        
        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/selftrain_modificado2/treinamento.R')
      }    #FIM DO J
    }  #FIM DO I
    print("gerando data frame para arquivos")
    
    #cirando data frame que sera guardado no arquivo com os seguintes dados: base, %rotulados inicialmente, iteracao, taxa de confian?a, numero de exemplos adicionados, corretude e cobertura
    data_arquivo_o <- data.frame(bd_g_o,tx_g_o,it_g_o,thrConf_g_o,nr_added_exs_g_o)#, acertou_g_o)
    # data_arquivo <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g, acertou_g, corretude_g, cobertura_g )
    # data_arquivo_gra <- data.frame(bd_g_gra,tx_g_gra,it_g_gra,thrConf_g_gra,nr_added_exs_g_gra, acertou_g_gra)
    # 
    # #cirando data frame que sera guardado no arquivo com os seguintes dados: %rotulados inicialmente, base, acuracia
    # 
    # #fazendo teste com classificador supervisionado
    # data_arquivo_acc_sup <- data.frame(tx, bd, acc_g_sup)
    # data_arquivo_acc_por_taxa_sup <- c(data_arquivo_acc_sup[data_arquivo_acc_sup$tx<10,],data_arquivo_acc_sup[data_arquivo_acc_sup$tx<15 & data_arquivo_acc_sup$tx>5,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<20 & data_arquivo_acc_sup$tx>10,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<25 & data_arquivo_acc_sup$tx>15,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<30 & data_arquivo_acc_sup$tx>20,])
    # #cirando data frame que sera guardado no arquivo com os seguintes dados: %rotulados inicialmente, base, acuracia
    data_arquivo_acc_o <- data.frame(tx, metodo, bd, acc_g_o)
    data_arquivo_acc_por_taxa_o <- c(data_arquivo_acc_o[data_arquivo_acc_o$tx<10,],data_arquivo_acc_o[data_arquivo_acc_o$tx<15 & data_arquivo_acc_o$tx>5,], data_arquivo_acc_o[data_arquivo_acc_o$tx<20 & data_arquivo_acc_o$tx>10,], data_arquivo_acc_o[data_arquivo_acc_o$tx<25 & data_arquivo_acc_o$tx>15,], data_arquivo_acc_o[data_arquivo_acc_o$tx<30 & data_arquivo_acc_o$tx>20,])
    # data_arquivo_acc <- data.frame(tx, bd, acc_g)
    # data_arquivo_acc_por_taxa <- c(data_arquivo_acc[data_arquivo_acc$tx<10,],data_arquivo_acc[data_arquivo_acc$tx<15 & data_arquivo_acc$tx>5,], data_arquivo_acc[data_arquivo_acc$tx<20 & data_arquivo_acc$tx>10,], data_arquivo_acc[data_arquivo_acc$tx<25 & data_arquivo_acc$tx>15,], data_arquivo_acc[data_arquivo_acc$tx<30 & data_arquivo_acc$tx>20,])
    # data_arquivo_acc_gra <- data.frame(tx, bd, acc_g_gra)
    # data_arquivo_acc_por_taxa_gra <- c(data_arquivo_acc_gra[data_arquivo_acc_gra$tx<10,],data_arquivo_acc_gra[data_arquivo_acc_gra$tx<15 & data_arquivo_acc_gra$tx>5,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<20 & data_arquivo_acc_gra$tx>10,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<25 & data_arquivo_acc_gra$tx>15,], data_arquivo_acc_gra[data_arquivo_acc_gra$tx<30 & data_arquivo_acc_gra$tx>20,])
    
    print("Gravando arquivos")    
    if (t == 1){ #TAXA 0.9
      if (c==1){
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_o_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_o_09.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_nb_gra_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_gra_09.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_soma_09.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_voto_09.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c1_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c1_soma_09.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c1_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c1_voto_09.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c2_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c2_09.csv", row.names = FALSE)
        }
        
      }else if (c==2){
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_o_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_o_09.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_ad_gra_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_gra_09.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_soma_09.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_voto_09.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c1_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c1_soma_09.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c1_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c1_voto_09.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c2_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c2_09.csv", row.names = FALSE)
        }
      }else if (c==3){
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_o_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_o_09.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_JRip_gra_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_gra_09.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_soma_09.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_voto_09.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c1_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRipJRip_flexcon_c1_soma_09.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c1_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_c1_voto_09.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c2_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_c2_09.csv", row.names = FALSE)
        }
      }else if (c==4){
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_o_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_o_09.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_IBK_gra_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_gra_09.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_soma_09.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_voto_09.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c1_soma_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c1_soma_09.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c1_voto_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c1_voto_09.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c2_09.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c2_09.csv", row.names = FALSE)
        }
      }       
      
    }else if (t == 2){ #TAXA 0.95
      if (c==1){
        #escrever no arquivo NB
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_o_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_o_095.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_nb_gra_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_gra_095.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_soma_095.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_voto_095.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c1_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c1_soma_095.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c1_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c1_voto_095.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_nb_flexcon_c2_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_nb_flexcon_c2_095.csv", row.names = FALSE)
        }
        
      }else if (c==2){
        # #escrever no arquivo AD
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_o_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_o_095.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_ad_gra_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_gra_095.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_soma_095.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_voto_095.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c1_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c1_soma_095.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c1_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c1_voto_095.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_ad_flexcon_c2_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_ad_flexcon_c2_095.csv", row.names = FALSE)
        }
      }else if (c==3){
        #escrever no arquivo ripper
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_o_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_o_095.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_JRip_gra_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_gra_095.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_soma_095.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_voto_095.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c1_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRipJRip_flexcon_c1_soma_095.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c1_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_c1_voto_095.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_JRip_flexcon_c2_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_JRip_flexcon_c2_095.csv", row.names = FALSE)
        }
      }else if (c==4){
        #escrever no arquivo ibk
        if (metodo==1){ # 1=original 2= gradativo 3=flexcon_soma 4=flexcon_voto 5=flexcon-C1_soma 6=flexcon-C1_voto 7=flexcon-C2
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_o_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_o_095.csv", row.names = FALSE)
        }else if (metodo==2){
          write.csv(data_arquivo_o, "resultado_IBK_gra_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_gra_095.csv", row.names = FALSE)
        }else if (metodo==3){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_soma_095.csv", row.names = FALSE)
        }else if (metodo==4){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_voto_095.csv", row.names = FALSE)
        }else if (metodo==5){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c1_soma_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c1_soma_095.csv", row.names = FALSE)
        }else if (metodo==6){
          #escrever no arquivo NB
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c1_voto_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c1_voto_095.csv", row.names = FALSE)
        }else if (metodo==7){
          write.csv(data_arquivo_o, "resultado_IBK_flexcon_c2_095.csv", row.names = FALSE)
          write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBK_flexcon_c2_095.csv", row.names = FALSE)
        }
      }       
    } 
  }
  cat('FIM', '\n')
}    

