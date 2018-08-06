# Função chega o SO utilizado e seta o diretório
# This function check the OS and change work directory
setWorkspace <- function() {
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
  change_rate <<- c(2:8)
  extention <<- ".csv"
  funcs <<- c('func', 'f', 'f2', 'f2')
  obj <<- c(learner(classifiers[1], list()), learner(classifiers[2], list(se=0.5)), learner(classifiers[3], list()),
           learner(classifiers[4], list(control = Weka_control(K = 15, X = TRUE))))
}

# Função void que inicia todas as variáveis globais do código
# Void Function to load all global variables of the code
initGlobalVariables <- function() {
  conj_treino <<- c()
  treinamento <<- c()
  acc_c1_s <<- c()
  acc_c1_v <<- c()
  acc_c2 <<- c()
  
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
  
}

setWorkspace()

# Carregando o script com as funções
# Loading functions script
source('functions.R')
installNeedPacks()

initGlobalVariables()
defines()

# for (cr in change_rate) {
  # for (cl in 1:length(classifiers)) {
    for(i in 0:0) {
      source('databases.R')
      for(j in 1:1) {
        taxa <- j * 5
        cr <- 5
        cl <- 1
        source('splitData.R')
        source('training.R')
      }
    }
    output_archive(cr, as.character(classifiers[cl]), acc_c1_s, acc_c1_v, acc_c2)
#   }
# }