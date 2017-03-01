resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                          pred=predict(model, type="class"))
rtab <- table(resultframe)
rtab

# Overall model accuracy: 73% of the predictions were correct.
# [1] 0.728
sum(diag(rtab))/sum(rtab)

# Model precision: 76% of the applicants predicted as bad really did default
# [1] 0.7592593
sum(rtab[1,1])/sum(rtab[,1])

# Model recall: the model found 14% of the defaulting loans
# [1] 0.1366667
sum(rtab[1,1])/sum(rtab[1,])

# False positive rate: 2% of the good applicants were mistakenly identified as bad.
# [1] 0.01857143
sum(rtab[2,1])/sum(rtab[2,])