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

source('C:/local_R/projeto_karliane/scripts_comuns/configuracoes.R')
# source('~/R/karliane/projeto_karliane/nayves_bayes/configuracoes.R')

source('C:/local_R/projeto_karliane/scripts_comuns/funcoes.R')
# source('~/R/karliane/projeto_karliane/nayves_bayes/funcoes.R')
for (t in 1:1) { #1 = taxa 0,9 2 = taxa 0,95
  for(c in 1:1){  # 1 = NB, 2 = AD 3 = ripper 4 = IBK
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
    acc_g_3 <- c()
    acertou_g_3 <- c()
    
    bd <- c()
    tx <- c()
    for(i in 4:4){  # bases de dados
      for(j in 1:5){ # taxas  #base 1 - IRIS 5% NB N?O FUNCIONA - da erro
        taxa <- j*5
        source('C:/local_R/projeto_karliane/scripts_comuns/carrega_dados.R')
        source('C:/local_R/projeto_karliane/scripts_comuns/organiza_dados.R')
        source('C:/local_R/projeto_karliane/selftrain_modificado2/treinamento.R')

        # source('~/R/karliane/projeto_karliane/nayves_bayes/carrega_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/organiza_dados.R')
        # source('~/R/karliane/projeto_karliane/nayves_bayes/treinamento.R')
      }    #FIM DO J
    }  #FIM DO I
    print("gerando data frame para arquivos")
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
        #escrever no arquivo NB
        write.csv(data_arquivo_modif2, "resultado_modif2_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_nb_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_nb_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_nb_09.csv", row.names = FALSE)
        
      }else if (c==2){ #AD
        #escrever no arquivo AD
        write.csv(data_arquivo_modif2, "resultado_modif2_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_ad_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_ad_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ad_09.csv", row.names = FALSE)
        
        
      }else if (c==3){
        #escrever no arquivo rip
        write.csv(data_arquivo_modif2, "resultado_modif2_rip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif2, "resultado_acc_modif2_rip_09.csv", row.names = FALSE)
        
        write.csv(data_arquivo_modif3, "resultado_modif3_rip_09.csv", row.names = FALSE)
        write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_rip_09.csv", row.names = FALSE)
        
      }else if (c==4){ #IBK
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


# caso seja necessario trazer a funÁ„o para o main esses s„o os parametros.
# form = as.formula(paste(classe,'~', '.'))
# data = base_treino_self_training
# learner = learner("naiveBayes", list())
# predFunc = 'f'
# thrConf=0.9
# maxIts=10
# percFull=1
# verbose=F
# 
# 
# N <- NROW(data)
# N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
# N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n„o rotulados
# it <- 0
# soma_Conf <- 0
# qtd_Exemplos_Rot <- 0
# totalrot <- 0
# conj_treino <- c()
# 
# 
# sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
# id_conj_treino <- c()
# id_conj_treino_antigo <- c()
# repeat {
#   #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
#   it <- it+1
# 
#   if ((it>1)&&(qtd_Exemplos_Rot>0)){
#     #      N_instancias_por_classe2 <- ddply(data[new,],~class,summarise,number_of_distinct_orders=length(class))
#     N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))
#     treino_valido <- FALSE
#     # teste <<- N_c
#     for (x in 1:nrow(N_instancias_por_classe2)){
# 
#       if (N_instancias_por_classe2$number_of_distinct_orders[x]>= N_classes*5)
#         treino_valido <- TRUE
#       else treino_valido <- FALSE
# 
#     }
# 
# 
#     #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N√ÉO)
#     if (treino_valido){
#       # if (nrow(data[new,])>=N_classes*5){
#       #o conjunto de treinamento serao as instancias inclu√???das (rotuladas)
#       conj_treino <- data[id_conj_treino,]
#       id_conj_treino_antigo <- c()
#       #conj_treino <- data[new,]
#     }else if (length(conj_treino)>=1) {
#       #o conjunto de treinamento ser√° o anterior + as instancias incluidas (rotuladas)
#       #conj_treino <- rbind(data[new,],conj_treino)
#       conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
#       # id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
#       cat("juntou", nrow(conj_treino), "\n")
#     }else break
# 
#     if(k==1){
#       classificador <- naiveBayes(as.factor(class) ~ .,conj_treino)
#       matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
#     }
#     else{
#       #IMPLEMENTAR ARVORE DE DECIS√O
#       classificador <- rpartXse(as.factor(class) ~ .,conj_treino)
#       matriz <- table(predict(classificador,base_rotulados_ini, type="vector"),base_rotulados_ini$class)
#     }
# 
#     acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
#     if(acc_local>=50){
#       thrConf<-thrConf-0.05
#     }else{
#       thrConf<-thrConf+0.05
#     }
#   }
# 
#   soma_Conf <- 0
#   qtd_Exemplos_Rot <- 0
# 
#   model <- runLearner(learner,form,data[sup,])
#   probPreds <- do.call(predFunc,list(model,data[-sup,]))
#   new <- which(probPreds[,2] >= thrConf)
#   if (verbose) {
#     cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')
#     ##guardando nas variaveis
#     it_g <<-c(it_g,it)
#     bd_g <<-c(bd_g,bd_nome)
#     thrConf_g <<-c(thrConf_g,thrConf)
#     nr_added_exs_g <<-c(nr_added_exs_g,length(new))
#     tx_g <<- c(tx_g, taxa)
#   }
# 
#   if (length(new)) {
#     data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
# 
#     soma_Conf <- sum(soma_Conf, probPreds[new,2])
#     qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
#     totalrot <- totalrot + qtd_Exemplos_Rot
# 
#     id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
#     id_conj_treino <- (1:N)[-sup][new]
#     sup <- c(sup,(1:N)[-sup][new])
#   }
#   if(length(new)==0){
#     thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
#     #thrConf<-mean(probPreds[,2])
#   }
#   if (it == maxIts || length(sup)/N >= percFull) break
# 
# } #FIM DO REPEAT
# 
