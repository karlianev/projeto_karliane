install.packages("PMCMR")
library("PMCMR")
#seta o diretorio local
setwd("C:\\local_R\\projeto_karliane\\TESTE_ESTATISTICO")
#atribui os dados a uma matriz
dados <- as.matrix(read.csv("T5.CSV"))
#roda o teste
friedman.test(dados)

a <- pairwiseSignTest(data = dados, method="fdr")
posthoc.friedman.conover.test(dados)
