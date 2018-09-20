guardar_predicao <- function(predic,iterac){
  if (iterac==1){
    soma <<- predic
    cat("criar vetor com o voto e a soma")  
  }else{
    cat("incrementar o voto e a soma")
    
  }
  
}

func <- function(m, d){ #NB
  p <- predict(m, d, type = "raw") #col2 armazena a confian?a em cada classe predita pelo classificador (ex: classe 1 = 0.8, classe2 = 0.1, classe 3= 0.1)
  predicao <<- data.frame(p, row.names(d))
  data.frame(cl=colnames(p)[apply(p,1,which.max)], p = apply(p,1,max), id=row.names(d))
}

f <- function(m,d) { #AD
  p <- predict(m,d,type='prob') #predicao dos dados (d) de acordo com o modelo (m)
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2, id=row.names(d))
}

f2 <- function(m,d) { #JRip e KNN
  p <- predict(m,d,type='probability') # l ? uma matriz com a confian?a da predi??o de cada exemplo
  predicao <<- data.frame(p, row.names(d))
  col1 <- colnames(p)[apply(p,1,which.max)] #nome da coluna com a maior predicao, ou seja, a classe
  col2 <- apply(p,1,max) # valor da maior predicao
  data.frame(cl=col1,p=col2, id=row.names(d)) #um data frame com 2 colunas: 1) a predi??o de cada exemplo; 2) a classe predita para cada exemplo
}

################################
#                              #
# Funcoes para o ST-Modificado #
#                              #
################################
checa_classe <- function(data_1_it, data_x_it, thrConf){
    examples <- c()
    pos <- 0
    xid <- c() # Vetor de id
    ycl <- c()
    lvls <- match(data_x_it$id, data_1_it$id)
    for (indice in 1:length(lvls)){
      if((as.character(data_1_it[lvls[indice], 1]) == as.character(data_x_it[indice, 1]))){
        if ((data_1_it[lvls[indice], 2] >= thrConf) && (data_x_it[indice, 2] >= thrConf)){
          pos <- pos + 1
          xid[pos] <- indice
          ycl[pos] <- data_x_it[indice,1]
        }
      }
    }
  examples <- data.frame(id = xid, cl = ycl)
  return (examples)
}

# compara se as classes sao iguais e uma das confiacas é maior qua a confianca da iteracao atual
checa_confianca <- function(data_1_it, data_x_it, thrConf){
  examples <- c()
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((data_1_it[lvls[indice], 1] == data_x_it[indice, 1])){
      if ((data_1_it[lvls[indice], 2] >= thrConf) || (data_x_it[indice, 2] >= thrConf)){
        pos <- pos + 1
        xid[pos] <- indice
        ycl[pos] <- data_x_it[indice, 1]
      }
    }
  }
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
}


# compara se as classes sao diferentes e o produto das confiancas e maior que a confianca atual
# checa_classe_diferentes novo - usado no co-training
checa_classe_diferentes <- function(data_1_it, data_x_it, thrConf, moda){
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))){
      if ((data_1_it[lvls[indice], 2] >= thrConf) && (data_x_it[indice, 2] >= thrConf)){
        pos <- pos + 1
        # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
        xid[pos] <- indice
        ycl[pos] <- pesquisa_classe(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
}

# As classes são diferentes, mas uma das confianças é maior que o Limiar, inclui o de maior confiança
checa_confiancas_diferentes <- function(data_1_it, data_x_it, thrConf){
  pos <- 0
  xid <- c()
  ycl <- c()
  lvls <- match(data_x_it$id, data_1_it$id)
  for (indice in 1:length(lvls)){
    if ((as.character(data_1_it[lvls[indice], 1]) != as.character(data_x_it[indice, 1]))){
      if ((data_1_it[lvls[indice], 2] >= thrConf) || (data_x_it[indice, 2] >= thrConf)){
        pos <- pos + 1
        # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
        xid[pos] <- indice
        ycl[pos] <- pesquisa_classe(xid[pos], moda)
      }
    }
  }
  examples <- data.frame(id = xid,cl = ycl)
  return (examples)
}

pesquisa_classe <- function(i, moda){
  maior <- 0
  classes <- colnames(moda)
  for (j in 1:length(moda[i,])){
    if((moda[i,j] >= maior)){
      maior <- moda[i,j] 
      cl <- classes[j] 
    }
  }
  return(cl)
}

#armazena o voto do classificador para cada r?tulo
guarda_moda <- function(probPreds){
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:NROW(probPreds)){
    id <- as.character(probPreds[x,ncol(probPreds)])
    for(y in 1:(length(dist_classes))){
      if(probPreds[x,1] == dist_classes[y]){
        moda[id,dist_classes[y]] <- moda[id,dist_classes[y]] + 1
        break
      }
    }
  }
  return (moda)
}

guarda_soma <- function(p){ # p = predicao
  dist_classes <- unique(base_original$class) #pega as classes distintas
  for (x in 1:nrow(p)){
    id <- as.character(p[x,ncol(p)]) #pega o id que ? a ultima coluna de p
    for(y in 1:length(dist_classes)){ 
      moda[id,dist_classes[y]] <- moda[id,dist_classes[y]] + p[x,dist_classes[y]]
      
    }
  }
  return(moda)
}


# compara se as classes são diferentes e as confianças são maiores que a confiança atual
# checa_classe_diferentes_modif4 <- function(data_1_it, data_x_it, indices, thrConf, moda){
#   pos <- 0
#   classes <- colnames(moda)
#   xid <- c()
#   ycl <- c()
#   for (i in indices){
#     maior <- 0
#     # Este '!is.na(data_1_it[i, 2])' precisa ser verificado, pois ha ocorrencias em que o probPreds gera indices que não contem na primeira iteracao
#     if ((data_1_it[i, 2] >= thrConf) || (data_x_it[i, 2] >= thrConf)){
#       pos <- pos + 1
#       # votacao (pesquisa a classe que mais foi atribuida a um exemplo)
#       for (j in 1:length(moda[i,])){
#         if(moda[i,j] >= maior){
#           maior <- moda[i,j] 
#           cl <- classes[j] 
#         }
#       }
#       xid[pos] <- i
#       ycl[pos] <- cl
#     }
#   }
#   if (length(ycl) > 1){
#     runif(n = 1, min = 1, max = length(ycl))
#   }
#   examples <- data.frame(id = xid,cl = ycl)
#   return (examples)
# }
# 

################################
#                              #
#            FIM               #
#                              #
################################

#funcao self-training modificado, usa uma f?rmula para calcular a nova taxa de confian?a.
#inclui no conjunto dos rotulados os exemplos que possuem mesmo rotulo e taxa de conf. >= thrconf
#caso n?o exista nenhum exemplo com essa caracter?stica, serao incluidos os exemplos que possuem o mesmo
#rotulo e uma das duas confiancas >= thrConf. Se ainda assim n?o existir nenhum exemplo, serao
#incluidos os exemplos cujos rotulos sao diferentes, mas uma das duas confiancas seja >= que thrConf
funcSelfTrain <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,
                          votacao = TRUE){
  
  
  
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
    probPreds$cl <- as.character(probPreds$cl)
    
    if(it == 1){
      probPreds_1_it <<- probPreds
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original),unique(base_original$class)))
      new <- which(probPreds[,2] >= thrConf)
      rotulados <- data.frame(id = new,cl = probPreds[new,1]) 
      
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
      
    }else{
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
        
      
      rotulados <- checa_classe(probPreds_1_it, probPreds, thrConf)
      if (length(rotulados$id) == 0){
        rotulados <- checa_confianca(probPreds_1_it, probPreds, thrConf)
        if (length(rotulados$id) == 0){
            rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, thrConf, moda)
        }
      }
      new <- rotulados$id
    }
    
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
        
      data[(1:N)[-sup][new],as.character(form[[2]])] <- rotulados[,2]
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
#funcao, chamada em modificado2 e 3, que valida se o conjunto de treinamento pode ser utilizado para calcular a nova taxa de confianca
validar_treino<- function(data,id_conj_treino,N_classes,min_exem_por_classe){
  #cat("entrou if da segunda itera??o", '\n')
  N_instancias_por_classe2 <- ddply(data[id_conj_treino,],~class,summarise,number_of_distinct_orders=length(class))
  
  treino_valido <<- FALSE
  if (NROW(N_instancias_por_classe2)  == N_classes){#TAVA nrow
    
    for (x in 1:NROW(N_instancias_por_classe2)){ #TAVA nrow
      
      if (N_instancias_por_classe2$number_of_distinct_orders[x]>= min_exem_por_classe){ #N_classes*5)
        treino_valido <<- TRUE
      }else{
        treino_valido <<- FALSE
        break
      }
    }  
  }
} 

#funcao, chamada em modificado2 e 3, que define o conjunto de treinamento a ser classificado e indica se a classificacao e possivel
validar_classificacao <- function(treino_valido_i, id_conj_treino, id_conj_treino_antigo, data, N_classes, min_exem_por_classe){
  #data[sup,] corresponde os que possuem rotulos (INICIALMENTE ROTULADOS OU N?fO)
  if (treino_valido_i){
    #o conjunto de treinamento serao as instancias inclu????das (rotuladas)
    conj_treino <<- data[id_conj_treino,]
    id_conj_treino_antigo <<- c()
    classificar <- TRUE
    
  }else if (length(id_conj_treino_antigo)>=1) {
    #o conjunto de treinamento será o anterior + as instancias incluidas (rotuladas)
    conj_treino <<- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])
    
    id_conj_treino1 <- c(id_conj_treino, id_conj_treino_antigo)
    validar_treino(data,id_conj_treino1,N_classes,min_exem_por_classe);

    if (treino_valido){
      classificar <- TRUE
    }else{
      classificar <- FALSE
    }
    
  }else classificar <- FALSE #a confian?a permanece a mesma ao inves de parar
  return(classificar)  
}

#funcao que faz o treinamento usando o conjunto de treinamento definido na funcao validar_classificacao e
#calcula a acuracia para ser usada na definicao da nova taxa de confianca. Essa acuracia comparada com o 
#limiar indica se a nova taxa sobe ou desce.
calcular_acc_local <- function(){
  
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
  return(acc_local)
}

#funcao que calcula a nova confianca de acordo com a acuracia local e o limiar
calcular_confianca<-function(acc_local,limiar,txConf){
  cr <- 0.04
  if((acc_local>(limiar + 1)) && (txConf-cr>0.0)){
    txConf<-txConf-cr

  }else if((acc_local<(limiar - 1)) && (txConf+cr <= 1)){

    txConf<-txConf+cr
  } #caso contrario a confianca permanecera a mesma
  return(txConf)
}

# #funcao que calcula a nova confianca de acordo com a acuracia local e o limiar
# #REFIZ PARA TENTAR MUDAR A CONFIAN?A NOS CASOS EM QUE A SOMA PASSA DE 1 OU A SUBTRACAO ? MENOR QUE 0
# #ESTA ALTERA??O SE DEU PQ PERCEBI QUE ESTAVA MANTENDO A CONFIAN?A E A QTDE DE EXEMPLOS INCLUIDOS TAVA 0 OU 1
# calcular_confianca<-function(acc_local,limiar,txConf){
#   if (acc_local>(limiar + 1)){
#     txConf<-txConf-0.05
#     if (txConf< 0){
#       txConf <- 0.1
#     }
#   }else if(acc_local<(limiar - 1)){
#     
#     txConf<-txConf+0.05
#     
#     if (txConf > 1){
#       txConf <- 1
#     }
#       
#   } #caso contrario a confianca permanecera a mesma
#   return(txConf)
# }

#calcula a confianca de acordo com o treinamento do classificador
#so acumula o conjunto de treinamento com o conjunto anterior caso o conjunto de treinamento nao seja valido
#usa votacao para definir o rotulo nos casos em que os classificadores divergem
funcSelfTrainModificado2 <- function(form,data,
                          learner,
                          predFunc,
                          thrConf=0.9,
                          maxIts=10,percFull=1,
                          verbose=F,
                          min_exem_por_classe,
                          limiar=70,
                          votacao = TRUE){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
#substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  it <- 0
  # soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      validar_treino(data,id_conj_treino,N_classes,min_exem_por_classe);
      classificar <- validar_classificacao(treino_valido,id_conj_treino,id_conj_treino_antigo,data, N_classes, min_exem_por_classe)
      
      
      if (classificar){
        acc_local <- calcular_acc_local()
        thrConf <- calcular_confianca(acc_local,limiar,thrConf)  
      }  
    }
    # soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
# predicao <<- c()
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    # new <- which(probPreds[,2] >= thrConf)
    probPreds$cl <- as.character(probPreds$cl)
    
    if(it == 1){
      probPreds_1_it <<- probPreds
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, dimnames = list(row.names(base_original),sort(unique(base_original$class), decreasing = FALSE)))
      
      new <- which(probPreds[,2] >= thrConf)
      rotulados <- data.frame(id = new, cl = probPreds[new,1]) 

      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
      
    }else{
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
      
      rotulados <- checa_classe(probPreds_1_it, probPreds, thrConf)
      if (length(rotulados$id) == 0){
        rotulados <- checa_confianca(probPreds_1_it, probPreds, thrConf)
        if (length(rotulados$id) == 0){
          rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, thrConf, moda)
          if(length(rotulados$id) == 0){
            rotulados <- checa_confiancas_diferentes(probPreds_1_it, probPreds, thrConf)
          }
        }
      }
      new <- rotulados$id
    }
    

    
    if (verbose) {
      # cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')
      ##guardando nas variaveis 
      it_g <<- c(it_g, it)
      bd_g <<- c(bd_g, bd_nome)
      thrConf_g <<- c(thrConf_g, thrConf)
      nr_added_exs_g <<- c(nr_added_exs_g, length(new))
      tx_g <<- c(tx_g, taxa)
    }
    
    if (length(new)) {
      
      data[(1:N)[-sup][new],as.character(form[[2]])] <- as.character(probPreds[new,1])
      
      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
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
    
   #FIM DO REPEAT
  }
  return(model)  
  
}
#calcula a confianca de acordo com o treinamento do classificador
#so acumula o conjunto de treinamento com o conjunto anterior caso o conjunto de treinamento nao seja valido
# IMPLEMENTAR UMA FUNCAO CUJO conjunto de treinamento SEJA cumulativo
#usa O ROTULO DO CLASSIFICADOR SUPERVISIONADO nos casos em que os classificadores divergem
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
  treino_valido <<- FALSE
  repeat {
    acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      validar_treino(data,id_conj_treino,N_classes,min_exem_por_classe);
      classificar <- validar_classificacao(treino_valido,id_conj_treino,id_conj_treino_antigo,data, N_classes, min_exem_por_classe)
      
      if (classificar){
        acc_local <- calcular_acc_local()
        thrConf <- calcular_confianca(acc_local,limiar,thrConf)  
      }  
    }
    
    # soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
#    guardar_predicao(predicao, it)
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
        #adiciona exemplos cuja confian?a dos dois classificadores seja maior que thrconf e cuja predicao de probpreds e probpreds_model_superv nao seja a mesma
        new <- which((probPreds[,2] >= thrConf) & (probPreds_model_superv[,2] >= thrConf) & (probPreds[,1] != probPreds_model_superv[,1]))  
        if (length(new)){
          add_rot_superv <- TRUE
        }
      }
    }

    if (verbose) {
      # cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n')
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
      # soma_Conf <- sum(soma_Conf, probPreds[new,2])
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

#esse comando pode ser usado para tornar o conj_treino cumulativo
# conj_treino <- rbind(data[id_conj_treino,],data[id_conj_treino_antigo,])

      sup <- c(sup,(1:N)[-sup][new])
    }
    
    acertou_g_3 <<- c(acertou_g_3, acertou)
    
    # cat('acertou',acertou,'\n') 
    
    if(length(new)==0){
      thrConf<-max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
  } #FIM DO REPEAT
  
  return(model)  
}

funcSelfTrainInclusaoProp <- function(form,data,
                                     learner,
                                     predFunc,
                                     thrConf=0.9,
                                     maxIts=10,percFull=1,
                                     verbose=F,
                                     min_exem_por_classe,
                                     limiar=70,
                                     votacao = TRUE){
  
  N <- NROW(data)
  N_instancias_por_classe <- ddply(data,~class,summarise,number_of_distinct_orders=length(class))
  #substituido por min_exem_por_classe
  N_classes <- NROW(N_instancias_por_classe)-1 # uso do -1 pq N_instancias_por_classe tem uma linha com a quantidade de exemplos n?o rotulados
  it <- 0
  soma_Conf <- 0
  qtd_Exemplos_Rot <- 0
  totalrot <- 0
  conj_treino <<- c()
  treino_valido <<- FALSE
  classificar <- TRUE
  
  sup <- which(!is.na(data[,as.character(form[[2]])])) #sup recebe o indice de todos os exemplos rotulados
  id_conj_treino <- c()
  id_conj_treino_antigo <- c()
  
  cat('\nBase de dados - Início')
  porc_inicial <- porc_classes(data[which(!is.na(data$class)),"class"]) #Guarda a porcentagem inicial por classe na base
  
  repeat {
    
    #acertou <- 0
    #cat("conj_treino", conj_treino, "nrow(conj_treino)", nrow(conj_treino))
    it <- it+1
    
    if ((it>1)&&(qtd_Exemplos_Rot>0)){
      validar_treino(data,id_conj_treino,N_classes,min_exem_por_classe);
      classificar <- validar_classificacao(treino_valido,id_conj_treino,id_conj_treino_antigo,data, N_classes, min_exem_por_classe)
      
      if (classificar){
        acc_local <- calcular_acc_local()
        thrConf <- calcular_confianca(acc_local,limiar,thrConf)  
      }  
    }
    soma_Conf <- 0
    qtd_Exemplos_Rot <- 0
    
    model <- runLearner(learner,form,data[sup,])
    # predicao <<- c()
    probPreds <- do.call(predFunc,list(model,data[-sup,]))
    # new <- which(probPreds[,2] >= thrConf)
    probPreds$cl <- as.character(probPreds$cl)
    if(it == 1){
      probPreds_1_it <<- probPreds
      moda <<- matrix(data = rep(0,length(base_original$class)),ncol = length(unique(base_original$class)), nrow = NROW(base_original), byrow = TRUE, 
                      dimnames = list(row.names(base_original),unique(base_original$class)))
      new <- which(probPreds[,2] >= thrConf)
      rotulados <- data.frame(id = new,cl = probPreds[new,1]) 
      
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
      
    }else{
      indices <- row.names(probPreds)   # pega o id de cada exemplo 
      if (votacao){
        moda <<- guarda_moda(probPreds) # Armazena a moda das classes
      }else{
        moda <<- guarda_soma(predicao) # Armazena a soma das classes
      }
      
      rotulados <- checa_classe(probPreds_1_it, probPreds, thrConf)
      if (length(rotulados$id) == 0){
        rotulados <- checa_confianca(probPreds_1_it, probPreds, thrConf)
        if (length(rotulados$id) == 0){
          rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, thrConf, moda)
          if(length(rotulados$id) == 0){
            rotulados <- checa_confiancas_diferentes(probPreds_1_it, probPreds, thrConf)
          }
        }
      }
      new <- rotulados$id
      
      
    }
    
    
    if (verbose) {
      cat('tx_incl',taxa,'IT.',it,'BD',i,thrConf,'\t nr. added exs. =',length(new),'\n') 
      ##guardando nas variaveis 
      it_g_ip <<-c(it_g_ip,it)
      bd_g_ip <<-c(bd_g_ip,bd_nome)
      thrConf_g_ip <<-c(thrConf_g_ip,thrConf)
      nr_added_exs_g_ip <<-c(nr_added_exs_g_ip,length(new))
      tx_g_ip <<- c(tx_g_ip, taxa)
    }
    
      ###    Em testes - INICIO    ###
      rotulados <- tratar_dados_faltosos(rotulados,probPreds,probPreds_1_it,indices,thrConf,moda)
      prop_rot <- prop(rotulados) # Encontra as proporções para incluir os rotulos
      new <- estratificar_rot(rotulados$id,probPreds,prop_rot) # Selecionar os rotulos para incluir na base
      ### Em testes #- FIM##
      
    if (length(new)) {
        
      cat('\n    -- Rotulados --')
      porc_classes(probPreds[new,1])
      
      data[(1:N)[-sup][as.integer(new)],as.character(form[[2]])] <- as.character(probPreds[as.integer(new),1])
      
      soma_Conf <- sum(soma_Conf, probPreds[new,2])
      qtd_Exemplos_Rot <- length(data[(1:N)[-sup][new],as.character(form[[2]])])
      totalrot <- totalrot + qtd_Exemplos_Rot

      # acertou <- 0
      # acerto <- treinamento[(1:N)[-sup][as.integer(new)], as.character(form[2])]== data[(1:N)[-sup][as.integer(new)], as.character(form[2])]
      # tam_acerto <- NROW(acerto)
      # for (w in 1:tam_acerto){
      #   if (acerto[w] == TRUE)
      #     acertou <- acertou + 1
      # }


      id_conj_treino_antigo <- c(id_conj_treino_antigo,id_conj_treino)
      id_conj_treino <- (1:N)[-sup][as.integer(new)]
      sup <- c(sup,(1:N)[-sup][as.factor(new)])
    }
    
    # acertou_g_ip <<- c(acertou_g_ip, acertou)    
    if(length(new)==0){
      thrConf <- max(probPreds[,2]) #FALTOU FAZER USANDO A M?DIA DAS PREDI??ES.
      # thrConf<-mean(probPreds[,2])
    }
    if (it == maxIts || length(sup)/N >= percFull) break
    
    #FIM DO REPEAT
  }
  
  cat('\nBase de dados - Fim')
  porc_classes(data[which(!is.na(data$class)),"class"])
  # print(memoria_rot)
  return(model)  
  
}

#Calcula a porcentagem das classes em relacao ao total
porc_classes <- function(classes){
  
  classes_dist <- unique(classes) #Guarda as classes distintas
  qtd_total <- sum(length(classes)) #Guarda o número total de classes
  
  qtd_por_classe <- c(rep(0,length(classes_dist))) #
  porcentagens <- qtd_por_classe                   # Cria os vetores de armazenamento e nomeia as colunas com as classes
  names(qtd_por_classe) <- classes_dist            #
  
  for (i in 1:length(classes_dist)){ #Percorre por classe distinta
    qtd_por_classe[i] <- length(which(classes == classes_dist[i])) #Conta a quantidade por classe e guarda
    porcentagens[i] <- (qtd_por_classe[i]/qtd_total)*100 #Guarda a porcentagem da classe em relação ao todo
  }
  
  cat('    Total: ',qtd_total,'\n')
  cat('    ')
  print(qtd_por_classe)
  cat('    Porcentagem: ',porcentagens,'\n')
  
  return (porcentagens)
}

#1
tratar_dados_faltosos <- function(rotulados,probPreds,probPreds_1_it,indices,thrConf,moda){
  classes_dist <- unique(base_rotulados_ini$class) # Classes distintas na base inicial
  classes_dist_pp <- unique(probPreds$cl)          # Classes distintas no probPreds
  classes_dist_rot <- unique(rotulados$cl)         # Classes distintas no conjunto dos rotulados
  novos_rotulados <- rotulados
  
  while(((length(classes_dist_rot) < length(classes_dist_pp)) || distinguir_classes(classes_dist_pp,classes_dist_rot)) && (thrConf > min(probPreds$p))){ # enquanto houver mais classes no probPreds do que nos rotulados...
    #cat("Classe faltando no conjunto dos rotulados | Recalculando taxa de confiança... thrConf = ")
    # thrConf <- min(probPreds$p) # Baixar confianca para a menor
    thrConf <- thrConf - 0.2
    if(thrConf < min(probPreds$p)){
      thrConf <- min(probPreds$p)
    }
    #cat(thrConf,"\n")
    
    #Rotular novamente
    novos_rotulados <- checa_classe(probPreds_1_it, probPreds, thrConf)
    if (length(novos_rotulados$id) == 0){
      novos_rotulados <- checa_confianca(probPreds_1_it, probPreds, thrConf)
      if (length(novos_rotulados$id) == 0){
        novos_rotulados <- checa_classe_diferentes(probPreds_1_it, probPreds, thrConf, moda)
        if(length(novos_rotulados$id) == 0){
          novos_rotulados <- checa_confiancas_diferentes(probPreds_1_it, probPreds, thrConf)
        }
      }
    }
    
    classes_dist_rot <- unique(novos_rotulados$cl)
  }
  
  cat('thrConf ',thrConf,'\t nr. added exs. =',length(novos_rotulados$id),'\n')
  # print(novos_rotulados)
  # print(classes_dist_pp)
  return (novos_rotulados)
}

#2
prop <- function(rotulados){
  if(length(rotulados)){
    classes_dist_rot <- unique(rotulados$cl)  # Guarda as classes distintas
    qtd_classes_ini <- c(rep(0,length(classes_dist_rot)))
    qtd_classes_rot <- c(rep(0,length(classes_dist_rot)))
    names(qtd_classes_ini) <- classes_dist_rot            # Organiza os titulos de cada 
    names(qtd_classes_rot) <- classes_dist_rot            # posicao por classe
    
    for(cl in classes_dist_rot){                                            # Guarda a quantidade por classe 
      qtd_classes_ini[cl] <- length(which(base_rotulados_ini$class == cl))  # inicialmente rotuladas
      qtd_classes_rot[cl] <- length(which(rotulados$cl == cl)) # guarda a quantidade por classe rotulada
    }
    
    menorCL <- as.character(names(qtd_classes_rot[which(qtd_classes_rot == min(qtd_classes_rot))])) # Classe com menos rotulados
    if(length(menorCL) > 1)
      menorCL <- menorCL[1]
    
    proporcoes <- c(rep(0,length(classes_dist_rot))) # vetor com as proporcoes calculadas
    names(proporcoes) <- as.character(classes_dist_rot) # Organiza os titulos de cada posicao por classe
    
    pos <- 1
    while (pos <= length(classes_dist_rot)){
      cl <- as.character(classes_dist_rot[pos])
      proporcoes[[cl]] <- as.numeric(format((qtd_classes_rot[[menorCL]]*qtd_classes_ini[[cl]])/qtd_classes_ini[[menorCL]],digits = 4)) # Regra de 3
      if(trunc(proporcoes[[cl]]) > trunc(qtd_classes_rot[[cl]])){ #Se o resultado passa do numero disponivel pra rotular, eh pq o anteror
        proporcoes[[cl]] <- as.numeric(format(qtd_classes_rot[[cl]],digits = 4))   #possui mais do que devia
        proporcoes[[menorCL]] <- as.numeric(format(qtd_classes_rot[[cl]]/(qtd_classes_ini[[cl]]/qtd_classes_ini[[menorCL]]),digits = 4))
        qtd_classes_rot[[menorCL]] <- as.numeric(format(qtd_classes_rot[[cl]]/(qtd_classes_ini[[cl]]/qtd_classes_ini[[menorCL]]),digits = 4))
        pos <- 1
      }else
        pos <- pos +1
    }
    
    # Caso a proporcao para uma base fique entre 0 e 1, esta recebera 1, pois nao se pode adicionar classe pela metade
    for(pos in 1:length(proporcoes))
      if(proporcoes[pos] < 1)
        proporcoes[pos] <- 1
        
    return (trunc(proporcoes)) # O trunc passa a parte inteira do numero
  }
  return (c())
}

#3
estratificar_rot <- function(new,probPreds,proporcoes){
  add_prop <- c()
  if(length(proporcoes)){
    pos <- 1 # para controlar as posicoes dos ids a serem adicionados
    for(x in new){
      if(x %in% probPreds[,3]){
        indice <- which(probPreds[,3] == x)
        if((as.character(probPreds[indice,1]) %in% names(proporcoes))  && (proporcoes[[probPreds[indice,1]]] > 0)){
          add_prop[pos] <- indice
          pos <- pos + 1
          proporcoes[[as.character(probPreds[indice,1])]] <- proporcoes[[as.character(probPreds[indice,1])]] - 1
        }
      }
    }
  }
  return (add_prop)
}

# verificar se as classes são iguais
distinguir_classes <- function(classes_dist_pp,classes_dist_rot){ 
  n <- 0
  for(cl in classes_dist_rot){
    aux <- classes_dist_pp == cl
    n <- which(aux == TRUE)
    if(!length(n)){                           # Se nao tiver classe igual retorna positivo (se classes_dist_pp >= classes_dist_rot) ou 
      if(length(classes_dist_rot) < length(classes_dist_pp)) # negativo (se classes_dist_pp < classes_dist_rot) para o rotulamento 
        return (TRUE)
    } 
  }
  return (FALSE)
}

#IMPLEMENTAR MODIFICADO 1 (COM O CALCULO DA NOVA TAXA IGUAL AO MODIFICADO) E MODIFICADO4 (COM O CALCULO DA NOVA TAXA IGUAL AO MODIFICADO2) COM O COMITE USANDO soma
