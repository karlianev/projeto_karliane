#func, f, e f2 retornam um data frame (matriz) com duas colunas: 1)a classe predita pelo classificador; 2) a confian?a dessa predicao
#a diferen?a dessas 3 fun??es ? apenas o type = class (AD) ou raw (NB) ou probability (RIPPER E KNN)
func <- function(m, d){ #NB

  p <- predict(m, d, type = "raw") #col2 armazena a confian?a em cada classe predita pelo classificador (ex: classe 1 = 0.8, classe2 = 0.1, classe 3= 0.1)
  data.frame(c1=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max))
  
  #estava assim, mas otimizei usando o c?digo acima que chama o predict apenas uma vez
  # col1<-predict(m,d, type='class') #col1 armazena a classe predita pelo classificador para cada exemplo
  # col2 <- predict(m, d, type = "raw") #col2 armazena a confian?a em cada classe predita pelo classificador (ex: classe 1 = 0.8, classe2 = 0.1, classe 3= 0.1)
  # data.frame(c1=col1, p = apply(col2,1,max)) #o comando apply seleciona a maior confian?a de cada exemplo/classe armazenada no col2
  
}
f <- function(m,d) { #AD
  p <- predict(m,d,type='prob') #predicao dos dados (d) de acordo com o modelo (m)
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2)
  
  #estava assim, mas otimizei usando o c?digo acima que chama o predict apenas uma vez
  # col1 <- predict(m,d,type='class')
  # col2 <- apply(predict(m,d),1,max) #predict(m,d) = a matriz com os dados(predicao); 1 = trabalha as linhas; max = fun??o a ser aplicada aos dados
  # data.frame(cl=col1,p=col2)
  
}

f2 <- function(m,d) { #JRip e KNN
    p <- predict(m,d,type='probability') # l ? uma matriz com a confian?a da predi??o de cada exemplo
    col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
    col2 <- apply(p,1,max) # valor da maior predicao
    data.frame(cl=col1,p=col2) #um data frame com 2 colunas: 1) a predi??o de cada exemplo; 2) a classe predita para cada exemplo

  #estava assim, mas otimizei usando o c?digo acima que chama o predict apenas uma vez
  #predict(m,d) = a matriz com os dados(predi??o); 1 = trabalha as linhas; max = fun??o a ser aplicada aos dados
  # col1 <- predict(m,d,type='class')  # c ? um vetor com a classe a qual cada exemplo pertence
  # col2 <- apply(predict(m,d,type='probability'),1,max) # l ? uma matriz com a confian?a da predi??o de cada exemplo
  # data.frame(cl=col1,p=col2) #um data frame com 2 colunas: 1) a predi??o de cada exemplo; 2) a classe predita para cada exemplo

}

################################
#                              #
# Funções para o SF-Modificado #
#                              #
################################
# compara se as classes esão iguais e se as confianças são maiores q a da iteração atual
checa_classe <- function(data_1_it, data_x_it, indices, thrConf){
  examples <- c()
  xid <- c() # Vetor de id
  pos <- 0
  for (i in indices){
    if (!is.na(data_1_it[i, 1]) && (data_x_it[i, 1] == data_1_it[i, 1])){
      if ((data_1_it[i, 2] >= thrConf) && (data_x_it[i, 2] >= thrConf)){
        pos <- pos + 1
        xid[pos] <- i
        # print(format(data.frame("thrConf" = thrConf,
        #                         "Iteração x" = data_x_it[i,],"Iteração 1" = data_1_it[i,],
        #                         check.names = FALSE)))
      }
    }
  }
  #cria o data frame com colunas ID e CLASSE
  examples <- data.frame(id = xid,cl = data_x_it[xid, 1])
  return (examples)
}

# compara se as classes são iguais e uma das confiaças é maior qua a confiança da iteração atual
checa_confiaca <- function(data_1_it, data_x_it, indices, thrConf){
  examples <- c()
  xid <- c()
  pos <- 0
  for (i in indices){
    if (!is.na(data_1_it[i, 1]) && (data_1_it[i, 1] == data_x_it[i, 1])){
      if((data_1_it[i, 2] >= thrConf) || (data_x_it[i, 2] >= thrConf)){
        pos <- pos + 1
        xid[pos] <- i
        # print(format(data.frame("thrConf" = thrConf,
        #                         "Iteração x" = data_x_it[i,],"Iteração 1" = data_1_it[i,],
        #                         check.names = FALSE)))
      }
    }
  }
  examples <- data.frame(id = xid,cl = data_x_it[xid, 1])
  return (examples)
}

# compara se as classes são diferentes e as confianças são maiores que a confiança atual
checa_classe_diferentes <- function(data_1_it, data_x_it, indices, thrConf, moda){
  pos <- 0
  classes <- colnames(moda)
  xid <- c()
  ycl <- c()
  for (i in indices){
    maior <- 0
    if ((data_1_it[i, 2] >= thrConf) && (data_x_it[i, 2] >= thrConf)){
      if (data_1_it[i, 1] != data_x_it[i, 1]){
        pos <- pos + 1
        # votação (pesquisa a classe que mais foi atribuida a um exemplo)
        for (j in 1:length(moda[i,])){
          if(moda[i,j] >= maior){
            maior <- moda[i,j] 
            cl <- classes[j] 
          }
        }
        xid[pos] <- i
        ycl[pos] <- cl
      }
    }
  }
  if (length(ycl) > 1){
    runif(n = 1, min = 1, max = length(ycl))
  }
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
}
################################
#                              #
#            FIM               #
#                              #
################################

#funcao self-training modificado
funcSelfTrain <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F){
  
  
  
  #N armazena a quantidade de exemplos na base de dados
  N <- NROW(data)
  #inicializando vari?veis
  it <- 0 #iteracao
  
  
  # soma_Conf <- 0 #soma da confianca
  conf_media <- 0 #confian?a m?dia da predicao dos exemplos rotulados em cada itera??o
  qtd_Exemplos_Rot <- 0 #quantidade de eemplos rotulados
  totalrot <- 0 #total de exemplos rotulados
  corret <- 0 #corretude
  cobert <- 0 #cobertura
  #sup recebe o indice de todos os exemplos rotulados
  sup <- which(!is.na(data[,as.character(form[[2]])])) 
  #quantidade de linhas do conjunto de dados retirando os exemplos rotulados, ou seja, a quantidade de exemplos n?o rotulados no conjunto de dados
  N_nao_rot <- NROW(data[-sup,])
  repeat {
    acertou <- 0
    it <- it+1
    new <- c()
    rotulados <- c()
    #O c?lculo da taxa de confianca (thrConf) ser? realizado a partir da segunda iteracao e se houver exemplos rotulados
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      thrConf <- (thrConf + conf_media + (qtd_Exemplos_Rot/N_nao_rot))/3
    }
    
    # soma_Conf <- 0
    conf_media <- 0
    qtd_Exemplos_Rot <- 0
    
    #model armazena o modelo gerado utilizando o aprendiz learner (AD, NB, KNN OU RIPPER), a base data[sup,] que s?o os dados rotulados e a classe ? passada no par?metro form
    model <- runLearner(learner,form,data[sup,])
    #a predicao e gerada de acordo com predFunc (func ou f1 ou f2 que foi passado como par?metro)
    probPreds <- do.call(predFunc,list(model,data[-sup,])) #data[-sup,] s?o os dados n?o rotulados
    
    if(it == 1){
      probPreds_1_it <<- probPreds
      moda <<- matrix(data = rep(0,length(data$class)),ncol = length(unique(base_original$class)), nrow = N, byrow = TRUE, 
                      dimnames = list(row.names(data),unique(base_original$class)))# matrix
      #new <- which(probPreds[,2] >= thrConf)
    }else{
      dist_classes <- unique(probPreds[,1]) #pega as classes distintas
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      
      ## Em teste ##
      
      # for (x in indices){
      #   for(y in 1:length(dist_classes)){
      #     if(probPreds[x,1] == dist_classes[y]){
      #       moda[x,y] <<- moda[x,y] + 1
      #       break
      #     }
      #   }
      # }
      
      rotulados <- checa_classe(probPreds_1_it, probPreds, indices, thrConf)
      if (length(rotulados$id) == 0){
        rotulados <- checa_confiaca(probPreds_1_it, probPreds, indices, thrConf)
        # if (length(rotulados$id) == 0){
        #   rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, indices, thrConf, moda)
        #  
        # }
      }
      new <- rotulados$id
    }
    
    ## teste ##
    
    
    if (verbose) {
      #imprime na tela o % de exemplos rotulados inicialmente, a iteracao, a base de dados, a taxa de confianca e a quantidade de exemplos rotulados
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g <<-c(it_g,it)
      bd_g <<-c(bd_g,bd_nome)
      thrConf_g <<-c(thrConf_g,thrConf)
      nr_added_exs_g <<-c(nr_added_exs_g,length(new))
      tx_g <<- c(tx_g, taxa)
    }
    
    #Se existir algum exemplo a ser rotulado, ser?o inseridos no conjunto dos rotulados
    if (length(new)) {
      #quantidade de exemplos n?o rotulados no conjunto de dados
      N_nao_rot <- NROW(data[-sup,])
        
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(rotulados[,2])
      # data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])


      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
      conf_media <- mean(probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot

      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      
      sup <- c(sup,(1:N)[-sup][new])      
    }

    # corret <- (soma_Conf/qtd_Exemplos_Rot)
    corret <- conf_media
    cobert <- (qtd_Exemplos_Rot/N_nao_rot)
    corretude_g <<- c(corretude_g, corret)
    cobertura_g <<- c(cobertura_g, cobert)
    acertou_g <<- c(acertou_g, acertou)
    
    #se n?o existir nenhum exemplo a ser rotulado, atribua a taxa de confianca (thrConf) a maior confian?a na predicao
    if(length(new)==0){
        thrConf<-max(probPreds[,2]) 
        # thrConf<-mean(probPreds[,2])
    }

    #termine se chegar ao n?mero m?ximo de itera??es ou se atingir o percentual m?ximo de exemplos rotulados
    if (it == maxIts || length(sup)/N >= percFull) break
    
  }

  # #codigo usado apenas para avaliar quantos exemplos est?o sendo rotulados errado.
  # acerto <- treinamento[(1:N), as.character(form[2])]== data[(1:N), as.character(form[2])]
  # tam_acerto <- NROW(acerto)
  # for (w in 1:tam_acerto){
  #   if (acerto[w] == TRUE)
  #     acertou <- acertou + 1
  # }
  
  #retorne o modelo criado pelo classificador  
  return(model)  
}

#funcao self-training padrao (original)
SelfTrainOriginal <- function (form, data, learner, predFunc, thrConf = 0.9, maxIts = 10, 
          percFull = 1, verbose = F) 
{
  

  N <- NROW(data)
  it <- 0
  sup <- which(!is.na(data[, as.character(form[[2]])]))
  repeat {
    acertou <- 0
    it <- it + 1
    model <- runLearner(learner, form, data[sup, ])
    probPreds <- do.call(predFunc, list(model, data[-sup,]))
    new <- which(probPreds[, 2] > thrConf)
    if (verbose) {
      #cat("IT.", it, "\t nr. added exs. =", length(new), 
       #   "\n")
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')     
      it_g_o <<-c(it_g_o,it)
      bd_g_o <<-c(bd_g_o,bd_nome)
      thrConf_g_o <<-c(thrConf_g_o,thrConf)
      nr_added_exs_g_o <<-c(nr_added_exs_g_o,length(new))
      tx_g_o <<- c(tx_g_o, taxa)
    }
    if (length(new)) {
      data[(1:N)[-sup][new], as.character(form[[2]])] <- probPreds[new, 1]
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      sup <- c(sup, (1:N)[-sup][new])
      acertou_g_o <<- c(acertou_g_o, acertou)
    }
    else{
      acertou <- 0
      acertou_g_o <<- c(acertou_g_o, acertou)
      break
    }
    
    
    
    if (it == maxIts || length(sup)/N >= percFull) 
      break
  }
  
  
  # #codigo usado apenas para avaliar quantos exemplos est?o sendo rotulados errado.
  # acerto <- treinamento[(1:N), as.character(form[2])]== data[(1:N), as.character(form[2])]
  # tam_acerto <- NROW(acerto)
  # for (w in 1:tam_acerto){
  #   if ((!is.na(acerto[w])) && (acerto[w] == TRUE))
  #     acertou <- acertou + 1
  # }
  
  return(model)
}


#fun??o self-training diminuindo a taxa de confian?a para inclus?o em 5 pontos percentuais a cada itera??o
funcSelfTrainGradativo <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,gradativo=0.05){
  
  
  data
  N <- NROW(data)
  it <- 0
  # soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  # totalrot <- 0
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  repeat {
    acertou <- 0
    it <- it+1


    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      thrConf <- (thrConf - gradativo)
      if (thrConf <= 0.0) thrConf <- (thrConf + gradativo)
    }
    # soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    new <- which(probPreds[,2] >= thrConf)
    
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g_gra <<-c(it_g_gra,it)
      bd_g_gra <<-c(bd_g_gra,bd_nome)
      thrConf_g_gra <<-c(thrConf_g_gra,thrConf)
      nr_added_exs_g_gra <<-c(nr_added_exs_g_gra,length(new))
      tx_g_gra <<- c(tx_g_gra, taxa)
      grad_g<<-c(grad_g,grad)
      
      }
    
    if (length(new)) {
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      # totalrot <- totalrot + qtd_Exemplos_Rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      
      sup <- c(sup,(1:N)[-sup][new])
    }
    
    acertou_g_gra <<- c(acertou_g_gra, acertou)
    
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  }
  
  return(model)  
}

funcSelfTrainModificado2 <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,
                          min_exem_por_classe,
                          limiar=70){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
#substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <- c()
  classificar <- TRUE
  
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
#      cat("entrou if da segunda itera??o", '\n')
      N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))

      treino_valido <- FALSE
      if (NROW(N_instancias_por_classe2)  == N_classes){#TAVA nrow
        # teste <<- N_c
        for (x in 1:NROW(N_instancias_por_classe2)){ #TAVA nrow
          
          if (N_instancias_por_classe2$number_of_distinct_orders[x]>= min_exem_por_classe) #N_classes*5)
            treino_valido <- TRUE
          else treino_valido <- FALSE
        }  
        
      }

      
      #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N?fO)
      if (treino_valido){
        #o conjunto de treinamento serao as instancias inclu????das (rotuladas)
        conj_treino <- data[id_conj_treino,]
        id_conj_treino_antigo <- c()
        classificar <- TRUE
        
      }else if (length(conj_treino)>=1) {
        #o conjunto de treinamento será o anterior + as instancias incluidas (rotuladas)
        conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
        classificar <- TRUE
        cat("juntou", NROW(conj_treino), "\n") #TAVA nrow
      }else classificar <- FALSE #a confian?a permanece a mesma ao inves de parar
      
      if (classificar){
        if(c==1){
          classificador <- naiveBayes(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
        }
        else if (c==2){
          #IMPLEMENTAR ARVORE DE DECIS?O
          classificador <- rpartXse(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini, type="class"),base_rotulados_ini$class)        
        } else if (c==3){
          #IMPLEMENTAR ripper
          classificador <- JRip(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)        
        } else if (c==4){
          #IMPLEMENTAR IBk
          classificador <- IBk(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
        }
          
        
        
        acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
         if((acc_local>(limiar + 1)) && (thrConf-0.05>0.0)){
        #if(acc_local>=limiar){
            thrConf<-thrConf-0.05
          
        }else if((acc_local<(limiar - 1)) && (thrConf+0.05 < 1)){
        #}else{
          thrConf<-thrConf+0.05
        } #caso contrario a confian?a permanecer? a mesma
        
      }  
    }
    
    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    new <- which(probPreds[,2] >= thrConf)

    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g <<-c(it_g,it)
      bd_g <<-c(bd_g,bd_nome)
      thrConf_g <<-c(thrConf_g,thrConf)
      nr_added_exs_g <<-c(nr_added_exs_g,length(new))
      tx_g <<- c(tx_g, taxa)
    }
    
    if (length(new)) {
      
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot
      
      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      
      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      sup <- c(sup,(1:N)[-sup][new])
      

    }
    
    acertou_g <<- c(acertou_g, acertou)    
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  } #FIM DO REPEAT
  
  return(model)  
}

funcSelfTrainModificado3 <- function(form,data,
                                     learner,
                                     predFunc,
                                     thrConf=0.9,
                                     maxIts=10,percFull=1,
                                     verbose=F,
                                     min_exem_por_classe,
                                     limiar=70,
                                     model_supervisionado){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
  #substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <- c()
  classificar <- TRUE
  add_rot_superv <- FALSE
  
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      #      cat("entrou if da segunda itera??o", '\n')
      # conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
      N_instancias_por_classe2 <- ddply(conj_treino,~class,summarise,number_of_distinct_orders=length(class))
      # N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))
      
      treino_valido <- FALSE
      if (NROW(N_instancias_por_classe2) == N_classes){#TAVA nrow
        # teste <<- N_c
        for (x in 1:NROW(N_instancias_por_classe2)){ #TAVA nrow

          if (N_instancias_por_classe2$number_of_distinct_orders[x]>= min_exem_por_classe) #N_classes*5)
            treino_valido <- TRUE
          else treino_valido <- FALSE
        }

      }
      
      
      
      if (treino_valido){
        #o conjunto de treinamento será o anterior + as instancias incluidas (rotuladas)
        classificar <- TRUE
        # cat("juntou", NROW(conj_treino), "\n") #TAVA nrow
      }else classificar <- FALSE #a confian?a permanece a mesma ao inves de parar
      
      if (classificar){
        if(c==1){
          classificador <- naiveBayes(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
        }else if (c==2){
          #IMPLEMENTAR ARVORE DE DECIS?O
          classificador <- rpartXse(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini, type="class"),base_rotulados_ini$class)        
        } else if (c==3){
          #IMPLEMENTAR ripper
          classificador <- JRip(as.factor(class) ~ .,conj_treino)
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)        
        } else if (c==4){
          #IMPLEMENTAR IBk
          classificador <- IBk(as.factor(class) ~ .,conj_treino, control = Weka_control(K=5, X=TRUE))
          matriz <- table(predict(classificador,base_rotulados_ini),base_rotulados_ini$class)
        }
        
        
        
        acc_local <- ((sum(diag(matriz)) / length(base_rotulados_ini$class)) * 100)
        if((acc_local>(limiar + 1)) && (thrConf-0.05>0.0)){
          #if(acc_local>=limiar){
          thrConf<-thrConf-0.05
          
        }else if((acc_local<(limiar - 1)) && (thrConf+0.05 < 1)){
          #}else{
          thrConf<-thrConf+0.05
        } #caso contrario a confian?a permanecer? a mesma
        
      }  
    }
    
    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    probPreds_model_superv <- do.call(predFunc,list(model_supervisionado,data[-sup,]))

    #transformando os dados dos factors probpreds e probpreds_model_superv em caracter para n?o ter problema quando a quantidade de classes preditas em um factor n?o for a mesma do outro
    z <- sapply(probPreds, is.factor)
    probPreds[z] <- lapply(probPreds[z], as.character)
    z <- sapply(probPreds_model_superv, is.factor)
    probPreds_model_superv[z] <- lapply(probPreds_model_superv[z], as.character)
    
    #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
    new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1]==probPreds_model_superv[,1]))
    if (length(new)==0){
      #adiciona exemplos cuja confian?a de um dos classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
      new <- which((probPreds[,2] >= thrConf) | (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1]==probPreds_model_superv[,1]))  
      
      if (length(new)==0){
        #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv seja a mesma
        new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1] != probPreds_model_superv[,1]))  
        add_rot_superv <- TRUE

      }
    }

    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g_3 <<-c(it_g_3,it)
      bd_g_3 <<-c(bd_g_3,bd_nome)
      thrConf_g_3 <<-c(thrConf_g_3,thrConf)
      nr_added_exs_g_3 <<-c(nr_added_exs_g_3,length(new))
      tx_g_3 <<- c(tx_g_3, taxa)
    }
    
    if (length(new)) {
      if (add_rot_superv) {
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds_model_superv[new,1])
      }else{
        data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      }
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot

      acertou <- 0
      acerto <- treinamento[(1:N)[-sup][new], as.character(form[2])]== data[(1:N)[-sup][new], as.character(form[2])]
      tam_acerto <- NROW(acerto)
      for (w in 1:tam_acerto){
        if (acerto[w] == TRUE)
          acertou <- acertou + 1
      }
      
      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][new]
      conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
      
      
      
      
      sup <- c(sup,(1:N)[-sup][new])
    }
    
    acertou_g_3 <<- c(acertou_g_3, acertou)
    
    cat('acertou',acertou,'\n') 
    
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  } #FIM DO REPEAT
  
  return(model)  
}
