
setwd("C:\\local_R\\projeto_karliane\\resultados")
arquivo <-c()
arquivo1 <-c()
resultado_dp <-c()
resultado_med <-c()
desvio_padrao <- 0
media <-0 

for (j in 1:4){ #qtde de arquivos a serem lidos
  if (j==1){
    # data <- read.csv("resultado_orig_ad_maximo_095.csv")
    # data <- read.csv("resultado_orig_ad_maximo_09.csv")
    # data <- read.csv("resultado_orig_ad_media_095.csv")
    # data <- read.csv("resultado_orig_ad_media_09.csv")
    # data <- read.csv("resultado_orig_nb_maximo_095.csv")
    data <- read.csv("resultado_orig_nb_maximo_09.csv")
  } 
  else if (j==2) {
    # data <- read.csv("resultado_grad_ad_maximo_095.csv")
    # data <- read.csv("resultado_grad_ad_maximo_09.csv")
    # data <- read.csv("resultado_grad_ad_media_095.csv")
    # data <- read.csv("resultado_grad_ad_media_09.csv")
    # data <- read.csv("resultado_grad_nb_maximo_095.csv")
    data <- read.csv("resultado_grad_nb_maximo_09.csv")
  }
  else if (j==3) {
    # data <- read.csv("resultado_modif_ad_maximo_095.csv")
    # data <- read.csv("resultado_modif_ad_maximo_09.csv")
    # data <- read.csv("resultado_modif_ad_media_095.csv")
    # data <- read.csv("resultado_modif_ad_media_09.csv") #faltou
    # data <- read.csv("resultado_modif_nb_maximo_095.csv")
    data <- read.csv("resultado_modif_nb_maximo_09.csv")
  }
  else if (j==4) {
    # data <- read.csv("resultado_modif2_ad_maximo_095_limiar70.csv")
    # data <- read.csv("resultado_modif2_ad_maximo_09_limiar70.csv")
    # data <- read.csv("resultado_modif2_ad_media_095_limiar70.csv")
    # data <- read.csv("resultado_modif2_ad_media_09_limiar70.csv")
    # data <- read.csv("resultado_modif2_nb_maximo_095_limiar70.csv")
    data <- read.csv("resultado_modif2_nb_maximo_09_limiar70.csv")
  }
  
  resultado_med <- c()
  resultado_dp <- c()
  
  for (i in 1:5){ #percentual rotulados
    
    v <- c(max(subset(data,  tx_g == i*5 & bd_g=="phishing")$it_g),
           max(subset(data,  tx_g == i*5 & bd_g=="letter")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="bupa")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="segment")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="haberman")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="pima")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="vehicle")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="wilt")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="balance-scale")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="car")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="kr-vs-kp")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="waveform")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="mushroom")$it_g),
          max(subset(data,  tx_g == i*5 & bd_g=="sick")$it_g))
    
    desvio_padrao <- sd(v)
    media <- mean(v)
 
    resultado_med <- c(resultado_med, media)
    resultado_dp <- c(resultado_dp, desvio_padrao)

 
  }
  
  if (j==1){
    #o resultado aparece no arquivo com colunas: original, gradativo, modificado e
    #modificado2 e as linhas: 5%, 10%, 15%, 20%, 25% rotulados
    # arquivo <- c(arquivo, data.frame("med_it_ad_max_095", resultado_med))
   # arquivo <- c(arquivo, data.frame("med_it_ad_max_09", resultado_med))
   # arquivo <- c(arquivo, data.frame("med_it_ad_med_095", resultado_med))
    # arquivo <- c(arquivo, data.frame("med_it_ad_med_09", resultado_med))
    # arquivo <- c(arquivo, data.frame("med_it_nb_max_095", resultado_med))
    arquivo <- c(arquivo, data.frame("med_it_nb_max_09", resultado_med))

    #arquivo do desvio padrão
    # arquivo1 <- c(arquivo1, data.frame("dp_it_ad_max_095", resultado_dp))
    # arquivo1 <- c(arquivo1, data.frame("dp_it_ad_max_09", resultado_dp))
    # arquivo1 <- c(arquivo1, data.frame("dp_it_ad_med_095", resultado_dp))
    # arquivo1 <- c(arquivo1, data.frame("dp_it_ad_med_09", resultado_dp))
    # arquivo1 <- c(arquivo1, data.frame("dp_it_nb_max_095", resultado_dp))
    arquivo1 <- c(arquivo1, data.frame("dp_it_nb_max_09", resultado_dp))
    
  }
  else{
    arquivo <- c(arquivo, data.frame(resultado_med))
    arquivo1 <- c(arquivo1, data.frame(resultado_dp))
  }
}

# write.csv(arquivo, "media_iteracoes_ad_maximo_095", row.names = FALSE)
# write.csv(arquivo, "media_iteracoes_ad_maximo_09", row.names = FALSE)
# write.csv(arquivo, "media_iteracoes_ad_media_095", row.names = FALSE)
# write.csv(arquivo, "media_iteracoes_ad_media_09", row.names = FALSE)
# write.csv(arquivo, "media_iteracoes_nb_maximo_095", row.names = FALSE)
write.csv(arquivo, "media_iteracoes_nb_maximo_09", row.names = FALSE)

#gravando desvio padrão
# write.csv(arquivo1, "dp_iteracoes_ad_maximo_095", row.names = FALSE)
# write.csv(arquivo1, "dp_iteracoes_ad_maximo_09", row.names = FALSE)
# write.csv(arquivo1, "dp_iteracoes_ad_media_095", row.names = FALSE)
# write.csv(arquivo1, "dp_iteracoes_ad_media_09", row.names = FALSE)
# write.csv(arquivo1, "dp_iteracoes_nb_maximo_095", row.names = FALSE)
write.csv(arquivo1, "dp_iteracoes_nb_maximo_09", row.names = FALSE)
