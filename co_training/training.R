my_learner <- obj[[cl]]
my_function <- funcs[cl]
classifier_name <- classifiers[cl]
limiar <- supAcc(classifier_name, base_rotulados_ini)

n <- getLength(base_teste$class)

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
co_training <- coTrainingOriginal(my_learner, my_function)
matrix_self_model1 <- confusionMatrix(co_training[[1]])
matrix_self_model2 <- confusionMatrix(co_training[[2]])
partial_acc_self_model1 <- getAcc(matrix_self_model1, n)
partial_acc_self_model2 <- getAcc(matrix_self_model2, n)
acc_self <- appendVectors(acc_self, mean(c(partial_acc_self_model1, partial_acc_self_model2)))
cat("\n Acerto global co-Training O. 1 Model (%) =", partial_acc_self_model1)
cat("\n Acerto global co-Training O. 2 Model (%) =", partial_acc_self_model2)
cat("\n Acerto global co-Training O.         (%) =", mean(c(partial_acc_self_model1, partial_acc_self_model2)))