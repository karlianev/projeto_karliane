my_learner <- obj[[cl]]
my_function <- funcs[cl]
classifier_name <- classifiers[cl]
limiar <- supAcc(classifier_name)

flex_con_c1_s <- flexConC(my_learner, my_function, qtd_exem_menor_classe, limiar, "1")
flex_con_c1_v <- flexConC(my_learner, my_function, qtd_exem_menor_classe, limiar, "2")
flex_con_c2 <- flexConC(my_learner, my_function, qtd_exem_menor_classe, limiar, "3")

matrix_c1_s <- confusionMatrix(flex_con_c1_s)
matrix_c1_v <- confusionMatrix(flex_con_c1_v)
matrix_c2 <- confusionMatrix(flex_con_c2)

n <- getLength(base_teste$class)
partial_acc_c1_s <- getAcc(matrix_c1_s, n)
partial_acc_c1_v <- getAcc(matrix_c1_v, n)
partial_acc_c2 <- getAcc(matrix_c2, n)

acc_c1_s <- incrementAcc(acc_c1_s, partial_acc_c1_s)
acc_c1_v <- incrementAcc(acc_c1_v, partial_acc_c1_v)
acc_c2 <- incrementAcc(acc_c2, partial_acc_c2)

cat("\n Acerto global flexcon-c1(s) (%) =", partial_acc_c1_s)
cat("\n Acerto global flexcon-c1(v) (%) =", partial_acc_c1_v)
cat("\n Acerto global flexcon-c2    (%) =", partial_acc_c2)
