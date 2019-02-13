my_learner <- obj[[cl]]
my_function <- funcs[cl]
classifier_name <- classifiers[cl]
limiar1 <- supAcc(classifier_name, base_rotulados_ini1, base_rotulados_ini1)
limiar2 <- supAcc(classifier_name, base_rotulados_ini2, base_rotulados_ini2)

num_row <- getLength(base_teste1$class)

# # FlexCon-C1 (S)
# flex_con_c1_s <- flexConC(my_learner, my_function, qtd_exem_menor_classe,
#                           limiar, "1")
# matrix_c1_s <- confusionMatrix(flex_con_c1_s)
# partial_acc_c1_s <- getAcc(matrix_c1_s, n)
# acc_c1_s <- appendVectors(acc_c1_s, partial_acc_c1_s)
# cat("\n Acerto global flexcon-c1(s)       (%) =", partial_acc_c1_s)
#
# # FlexCon-C1 (V)
# flex_con_c1_v <- flexConC(my_learner, my_function, qtd_exem_menor_classe,
#                           limiar, "2")
# matrix_c1_v <- confusionMatrix(flex_con_c1_v)
# partial_acc_c1_v <- getAcc(matrix_c1_v, n)
# acc_c1_v <- appendVectors(acc_c1_v, partial_acc_c1_v)
# cat("\n Acerto global flexcon-c1(v)       (%) =", partial_acc_c1_v)
#
# # FlexCon-C2
# flex_con_c2 <- flexConC(my_learner, my_function, qtd_exem_menor_classe,
#                         limiar, "3")
# matrix_c2 <- confusionMatrix(flex_con_c2)
# partial_acc_c2 <- getAcc(matrix_c2, n)
# acc_c2 <- appendVectors(acc_c2, partial_acc_c2)
# cat("\n Acerto global flexcon-c2          (%) =", partial_acc_c2)


# CO-Training Original
# co_training_GRA <- coTrainingGradativo(my_learner, my_function)

#@param metodo - 1 = co-training original (k=10%)
#                2 = co-training baseado no metodo de Felipe (k=limiar)
#                3 = co-training gradativo (k=limiar que diminui 5% a cada iteracao)
#                4 = co-training FlexCon
if (method==1)||(method==2)||(method==3){
  co_training <- coTrainingOriginal(my_learner, my_function, base1, base2,metodo=method)
}else if (method==4){
  co_training <- coTrainingFlexCon(my_learner, my_function, base1, base2, F)
}

matrix_self_model1 <- confusionMatrix(co_training[[1]], base_teste1)
matrix_self_model2 <- confusionMatrix(co_training[[2]], base_teste2)
partial_acc_self_model1 <- getAcc(matrix_self_model1, num_row)
partial_acc_self_model2 <- getAcc(matrix_self_model2, num_row)
acc_self <- appendVectors(acc_self, mean(c(partial_acc_self_model1, partial_acc_self_model2)))
acc_co_v1 <- appendVectors(acc_co_v1, partial_acc_self_model1)
acc_co_v2 <- appendVectors(acc_co_v2, partial_acc_self_model2)
cat("\n Acerto global co-Training O. 1 Model (%) =", partial_acc_self_model1)
cat("\n Acerto global co-Training O. 2 Model (%) =", partial_acc_self_model2)
cat("\n Acerto global co-Training O.         (%) =", mean(c(partial_acc_self_model1, partial_acc_self_model2)))
