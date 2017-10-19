
#inicializando variaveis globais para guardar no arquivo de resultados

it_g <-c() #iteracoes
bd_g <-c() #base de dados
thrConf_g<-c() #taxa de confianca para inclusao de novos exemplos
nr_added_exs_g<-c() #numero de exemplos adicionados ao conj dos rotulados na iteracao corrente
corretude_g <- c() #corretude do metodo modificado
cobertura_g <- c() #cobertura do metodo modificado

tx_g <- c() #percentual de exemplos rotulados inicialmente
acc_g <- c() #acuracia (percentual de acerto) do metodo modificado
acc_g_o <- c() #acuracia (percentual de acerto) do metodo original
acc_g_gra <- c() #acur?cia (percentual de acerto) do metodo gradativo

#fazendo teste com classificador supervisionado
acc_g_sup <- c() #acuracia (percentual de acerto) do metodo supervisionado

bd <- c() #base de dados
tx <- c() #percentual de exemplos rotulados inicialmente
grad_g <-c()
grad<-c()

#chamada para o script que faz as configuracoes necessárias ao código (instalacao e carregamento de pacotes)
source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/configuracoes.R')

#chamada para o script que cria as funções original, gradativo, modificado e modificado2
source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/scripts_comuns/funcoes.R')

#loop para definir a taxa de confiança da primeira iteracao
for (t in 1:1) { #1 = taxa 0,9 2 = taxa 0,95
  #loop para definir qual classificador sera usado
  for(c in 4:4){  # 1 = NB, 2 = AD, 3 = JRip ,4 = IBK
    #inicialização das variáveis
    it_g <-c() 
    bd_g <-c()
    thrConf_g<-c()
    nr_added_exs_g<-c()
    corretude_g <- c()
    cobertura_g <- c()
  
    tx_g <- c()
    acc_g <- c()
    #fazendo teste com classificador supervisionado
    acc_g_sup <- c()
    
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
    
    #loop para definir a base de dados a ser utilizada (no script carrega dados tem os nomes das bases)
    for(i in 2:2){  # bases de dados - testar bases 2 (2000), 3(20000), 4(300), 8 (900), 13(5000)
      #loop para definir o percentual de exemplos que ficarão rotulados inicialmente
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        #chamada do script que ler o .arff
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        #chamada do script que divide a base em conjunto de treinamento e teste, sorteia os exemplos inicialmente rotulados e tira o rótulo dos demais exemplos 
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        #chama o script que tem as chamadas das funções original, gradativo, modificado e modificado2
        source('C:/local_R/projeto_karliane/nayves_bayes/treinamento.R')
        
        # source('~/R/karliane/projeto_karliane/scripts_comuns/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/scripts_comuns/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/treinamento.R')
      }    
    }
    print("gerando data frame para arquivos")
    #cirando data frame que sera guardado no arquivo com os seguintes dados: base, %rotulados inicialmente, iteracao, taxa de confiança, numero de exemplos adicionados, corretude e cobertura
    data_arquivo <- data.frame(bd_g,tx_g,it_g,thrConf_g,nr_added_exs_g, corretude_g, cobertura_g)
    data_arquivo_o <- data.frame(bd_g_o,tx_g_o,it_g_o,thrConf_g_o,nr_added_exs_g_o)
    data_arquivo_gra <- data.frame(bd_g_gra,tx_g_gra,it_g_gra,thrConf_g_gra,nr_added_exs_g_gra)

    #cirando data frame que sera guardado no arquivo com os seguintes dados: %rotulados inicialmente, base, acuracia
    
    #fazendo teste com classificador supervisionado
    data_arquivo_acc_sup <- data.frame(tx, bd, acc_g_sup)
    data_arquivo_acc_por_taxa_sup <- c(data_arquivo_acc_sup[data_arquivo_acc_sup$tx<10,],data_arquivo_acc_sup[data_arquivo_acc_sup$tx<15 & data_arquivo_acc_sup$tx>5,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<20 & data_arquivo_acc_sup$tx>10,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<25 & data_arquivo_acc_sup$tx>15,], data_arquivo_acc_sup[data_arquivo_acc_sup$tx<30 & data_arquivo_acc_sup$tx>20,])
    #cirando data frame que sera guardado no arquivo com os seguintes dados: %rotulados inicialmente, base, acuracia
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
        #fazendo teste com classificador supervisionado
        write.csv(data_arquivo_acc_sup, "resultado_ad_sup_09.csv", row.names = FALSE)
        
        
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
      }else if (c==4){
        #escrever no arquivo IBk
        write.csv(data_arquivo, "resultado_IBk_09.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_IBk_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_IBk_gra_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_IBk_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBk_o_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_IBk_gra_09.csv", row.names = FALSE)
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
      }else if (c==4){
        #escrever no arquivo ibk
        write.csv(data_arquivo, "resultado_IBk_095.csv", row.names = FALSE)
        write.csv(data_arquivo_o, "resultado_IBk_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_gra, "resultado_IBk_gra_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa, "resultado_acc_IBk_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_o, "resultado_acc_IBk_o_095.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_gra, "resultado_acc_IBk_gra_095.csv", row.names = FALSE)
      } 
    
    }
  }
}    
  
