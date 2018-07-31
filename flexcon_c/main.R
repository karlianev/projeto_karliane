# Função chega o SO utilizado e seta o diretório
# This function check the OS and change work directory
set_workspace <- function() {
  mySystem <- Sys.info()
  if(mySystem[[1]] == "Linux"){
    setwd("~/R/karliane/projeto_karliane/flexcon_c")
  }else{
    setwd("C:\\local_R\\projeto_karliane\\flexcon_c")
  }
}

# Função para definir constantes ao longo do código
# Function to define constants in all code
defines <- function(){
  classifiers <<- c("naiveBayes", "rpartXse", "JRip", "IBk")
  
  cr <<- c(2:8)
  extention <<- ".csv"
}

# Função void que inicia todas as variáveis globais do código
# Void Function to load all global variables of the code
init_global_variables <- function() {
  conj_treino <<- c()
  treinamento <<- c()
  
  # FlexCon-C1 variables
  it_g <<- c()
  bd_g <<- c()
  thrConf_g <<- c()
  nr_added_exs_g <<- c()
  tx_g <<- c()
  acc_g <<- c()
  acertou_g <<- c()
  
  # FlexCon-C2 variables
  it_g_3 <<- c()
  bd_g_3 <<- c()
  thrConf_g_3 <<- c()
  nr_added_exs_g_3 <<- c()
  tx_g_3 <<- c()
  acc_g_3 <<- c()
  acertou_g_3 <<- c()
  
  grad_g <<- c()
  bd <<- c()
  tx <<- c()
  
  #fazendo teste com classificador supervisionado
  acc_g_sup <<- c()
  
}

# Função principal para realizar as chamadas a outras funções
# Main function to call orther in order
main <- function(){
  set_workspace()
  defines()
  # Carregando pacotes necessários
  # Loading necessary packages
  source('configurations.R')
  
  # Carregando o script com as funções
  # Loading functions script
  source('functions.R')
  
  for (t in cr) {
    cat("teste",t)
    if((t%%2)==0){
      cat(" par ")
    }else{
      cat(" impar ")
    }
  }
}

# main()

# for(c in classifiers){
  init_global_variables()
  # for(i in 0:0){
    # cat("\n                          BASE DE DADOS       ",i,"        cl",c,"\n\n\n")
    # for(j in 1:5){
      taxa <- j*5
      i <- 0
      source('databases.R')
      source('splitData.R')
      source('~/R/karliane/projeto_karliane/selftrain_modificado2/treinamento.R')
    # }
  # }
# }  
  #fazendo teste com classificador supervisionado
  # accSupervised <- data.frame(tx, bd, acc_g_sup)

write_archive <- function(cr, cl) {
  flexcon_c1_s <- c()
  flexcon_c1_v <- c()
  flexcon_c2 <- c()
  
  flexcon_c1_s <- paste("flexcon_c1_S_", cl, "_", cr, extention, sep = "")
  flexcon_c1_v <- paste("flexcon_c1_V_", cl, "_", cr, extention, sep = "")
  flexcon_c2 <- paste("flexcon_c2_", cl, "_", cr, extention, sep = "")
  
  auxC1s <- data.frame(acc_g)
  auxC1v <- data.frame(acc_g)
  auxC2 <- data.frame(acc_g_3)
  
  accFlexConC1s <- matrix(auxC1s, ncol = 5, byrow = TRUE)
  accFlexConC1v <- matrix(auxC1v, ncol = 5, byrow = TRUE)
  accFlexConC2 <- matrix(auxC2, ncol = 5, byrow = TRUE)
  
  write.csv(accFlexConC1s, flexcon_c1_s, row.names = FALSE)
  # write.csv(data_arquivo_acc_por_taxa_modif3, "resultado_acc_modif3_ibk_007_soma_095.csv", row.names = FALSE)
  
}


for(t in cr){
  conf <- 90
  acc_local <- 94
  limiar <- 70
  if((acc_local > (limiar + 1)) && ((conf - t) > 0.0)){
    conf <- conf - t
    cat("\nConfiaça: ", conf, "        CR: ", t)
  }
}
