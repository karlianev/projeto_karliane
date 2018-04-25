if (num_metodo==1){
  source('C:/local_R/projeto_karliane/co_training/treinamento_original.R')
}else if(num_metodo==2){
  source('C:/local_R/projeto_karliane/co_training/treinamento_gradativo.R')
}else if((num_metodo==3) || (num_metodo==4)){
  source('C:/local_R/projeto_karliane/co_training/treinamento_flexCon.R')
}

  