bank = read.table("C:\\Users\\alexa\\Documents\\UTC\\IM04 - Depart etranger\\ML01\\bank-blc.csv", sep =";", header =TRUE,)
summary(bank)
plot(bank)

library(MASS)
library(naivebayes)
library(pROC)
library(nnet)

#Split data in training and test sets
n = nrow(bank)

ntrain = round (2*n/3)
ntest = n - ntrain

train = sample(n, ntrain)

bank.train = bank[train,]
bank.test = bank[-train,]

#LDA + QDA + Naives Bayes + Logistic Regression
#1) LDA
#fit the lda model based on the training data set

fit.lda = lda(y~., bank.train)
fit.lda

#validate the lda model by using the testing data set
pred.lda<-predict(fit.lda,newdata=bank.test)
perf <-table(bank.test$y,pred.lda$class)
print(perf)

err.lda <- 1-sum(diag(perf))/ntest  # error rate
err.lda

#2) QDA

fit.qda = qda(y~ age + balance + day  + duration + campaign + pdays + previous, bank.train)
fit.qda

pred.qda = predict(fit.qda,newdata=bank.test)

perf = table(bank.test$y,pred.qda$class)
print(perf)

err.qda <-1-sum(diag(perf))/ntest  # error rate
err.qda


# Naive Bayes
fit.nb<- naive_bayes(as.factor(y)~.,data=bank.train)
pred.nb<-predict(fit.nb,newdata=bank.test,type="class")
pred.nb.prob<-predict(fit.nb,newdata=bank.test,type="prob")
perf <-table(bank.test$y,pred.nb)
print(perf)
err.nb <-1-sum(diag(perf))/ntest  # error rate
err.nb

# Logistic regression

fit.logreg<- glm(as.factor(y)~.,data=bank.train,family=binomial)
pred.logreg<-predict(fit.logreg,newdata=bank.test,type='response')
perf <-table(bank.test$y,pred.logreg>0.5)
print(perf)
err.logreg <-1-sum(diag(perf))/ntest  # error rate

print(c(err.lda,err.qda,err.nb,err.logreg))

