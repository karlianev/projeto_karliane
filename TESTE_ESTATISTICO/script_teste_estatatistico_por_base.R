
databases <- c("balance-scale", "blood-transfusion-service", "bupa",  "car", "cnae-9", 
               "connectionist-mines-vs-rocks", "hill-valley-with-noise", "segment",
               "indian-liver-patient", "iris",  "kr-vs-kp",  "leukemia-haslinger",
               "mammographic-mass", "mfeat-karhunen", "mushroom", "musk",
               "ozone-onehr",  "pendigits", "phishingData", "pima","planning-relax",
               "seeds", "semeion", "flare", "spectf-heart","tic-tac-toe","twonorm",
               "vehicle", "waveform-5000", "wilt" )

tst_est <- matrix(rep(0,30), ncol = 8, nrow = 30,
                 dimnames = list(databases, c("original", "limiar_fixo", "flexconG", "flexcon(s)",
                                          "flexcon(v)", "flexcon1(s)", "flexconc1(v)", "flexconc2")))

setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\teste estatistico jhoseph\\self-training")

for (porc in 1:5){
  nome_diretorio <- paste("acuracia bases", (porc*5), "porc", sep = " ")
  setwd(nome_diretorio)
  for (base in 1:30) {
    nome_arquivo <- paste(databases[base], ".csv", sep = "")
    dados <- as.matrix(read.csv(nome_arquivo))  
    fried <- friedman.test(dados)
    for (i in 3:8){
      if (fried$p.value<0.05){
        res_wil <- wilcox.test(dados[,1],dados[,i])
        tst_est[base,i] <- res_wil$p.value
      }else{
        tst_est[base,] <- -1
        break
      }
    }
  }
  setwd("C:\\Users\\karliane\\Dropbox\\doutorado\\tese\\Resultados finais tese\\teste estatistico jhoseph\\self-training")
  write.table(tst_est, paste("tst_estat_por_base", (porc*5), "porc.csv", sep="_"), row.names = FALSE)
}