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


# Self-Training Original
self_training <- SelfTrainOriginal(my_learner, my_function)
matrix_self <- confusionMatrix(self_training)
partial_acc_self <- getAcc(matrix_self, n)
acc_self <- appendVectors(acc_self, partial_acc_self)
cat("\n Acerto global Self-Training O.    (%) =", partial_acc_self)
