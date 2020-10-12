#-------------------------------------------------------------------------------
# G e n e r a l   I n f o r m a t i o n
#-------------------------------------------------------------------------------
# Name: 
#
# Usage: 
#
# Description: This
#
# Inputs: 
#
# Outputs: Accuracy Plots
#
# Auxiliary Files: 
#
# Special Instructions:  
#
#-------------------------------------------------------------------------------
# C o d e   H i s t o r y
#-------------------------------------------------------------------------------
# Version: 1.0
#
# Author(s): Bryce Robinette
#            brycerobinette@mail.weber.edu
#
# Modifications:
# 12 Oct 2020 - 
#
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# I m p o r t   L i b r a r i e s
#-------------------------------------------------------------------------------
library('MASS')
library('leaps')
library('class')
library(pROC)
#-------------------------------------------------------------------------------

#Data generating function
data.generate = function(mu1, mu2, s, n, p){
  y = as.factor(c(rep(1,n), rep(2,n)))
  M = matrix(NA, nrow = 2*n, ncol = p, byrow = 1)
  df = data.frame(y)
  for (i in c(1:p)){
    M[,i] = c(rnorm(n,mu1,s), rnorm(n,mu2,s))
  }
  df = cbind(y,M)
  df = data.frame(df)
  return(df)
}

df = data.generate(1, -1, 10, 50, 2) #generates data. means, variance, n obs, predictors
head(df)
attach(df)

#---------------------------------------------------------------------------------
# Data Sampling
test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)

###############################################
# test.one.index = sample(which(df == '1'), .5.3length(which(df == '1')), replace = FALSE )
# test.index = c(test.one.index, test.two.index)
###############################################

#training and testing data
train.X = df[-test.index,]
test.X = df[test.index,]
test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
train.Y = y[-test.index]


#---------------------------------------------------------------------------------
# KNN Accuracy Plot
pred.knn = knn(train.X, test.X, train.Y, k = 2)
table(pred.knn, test.Y)

knn.error = 1 - (mean(pred.knn == test.Y))
knn.error

# Best K
k.values = c(1:70) # Watch the lengths of our test data
result = 0
bestK = 0
error.stored = c()
for (k in c(1:70)) {
  pred.knn <- knn(train.X, test.X, train.Y, k = k)
  error = sum(pred.knn == test.Y)/length(test.Y)
  error.stored = c(error.stored,error)
  if (error > result){
    result = error
    bestK = k
  }
}
plot(k.values,error.stored,type = 'l')
print(result)
print(bestK)

#--------------------------------------------------------------------------------
# KNN Accuracy Plot against the Variance
var.values = seq(0.5, 3, 0.01)
knn.accuracy = c()
for (i in var.values){
  df = data.generate(1, -1, i, 50, 2)
  test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
  train.X = df[-test.index,]
  test.X = df[test.index,]
  test.Y = y[test.index]   
  train.Y = y[-test.index]
  pred.knn = knn(train.X, test.X, train.Y, k = bestK)
  knn.means = mean(pred.knn == test.Y)
  knn.accuracy = c(knn.accuracy, knn.means)
}
knn.accuracy
knn.plot = plot(var.values, knn.accuracy, type = 'l', main='KNN', xlab='Variance', 
                ylab='Accuracy', col='blue')




#-------------------------------------------------------------------------------------------- 
# Plot of LDA Accuracy against Variance

var.values = seq(1, 10, 0.1)
lda.accuracy = c()
set.seed(2)
for(i in var.values){
  df = data.generate(1, -1, i, 50, 2)
  test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
  train.X = df[-test.index,]
  test.X = df[test.index,]
  test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
  train.Y = y[-test.index]
  lda.fit = lda(y~.-y, data = df, subset = -test.index)
  lda.pred = predict(lda.fit, df[test.index,])
  means = mean(lda.pred$class == test.Y)
  lda.accuracy = c(lda.accuracy, means)
}
lda.accuracy
plot(var.values, lda.accuracy, type = 'l', main='LDA', xlab='Variance', ylab='Accuracy')



#---------------------------------------------------------------------------------------------
# Plot of QDA Accuracy against Variance
var.values = seq(1, 2, 0.01)
qda.accuracy = c()
set.seed(3)
for(i in var.values){
  df = data.generate(1, -1, 2, 50, 2)
  test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
  train.X = df[-test.index,]
  test.X = df[test.index,]
  test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
  train.Y = y[-test.index]
  qda.fit = qda(y~.-y, data = df, subset = -test.index)
  qda.pred = predict(qda.fit, df[test.index,])
  qda.means = mean(qda.pred$class == test.Y)
  qda.accuracy = c(qda.accuracy, qda.means)
}
qda.accuracy
qda.plot = plot(var.values, qda.accuracy, type = 'l', main='QDA', xlab='Variance', ylab='Accuracy')




# Still need accuracy plot for GLM




df = data.generate(1, -1, 2, 50, 2)
big.func = function(mu1, mu2, s, n, p){
  for(i in var.values){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    train.X = df[-test.index,]
    test.X = df[test.index,]
    test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
    train.Y = y[-test.index]
    lda.fit = lda(y~.-y, data = df, subset = -test.index)
    lda.pred = predict(lda.fit, df[test.index,])
    means = mean(lda.pred$class == test.Y)
    lda.accuracy = c(lda.accuracy, means)
  }
  return(mean(lda.accuracy))
}

big.func = function(mu1, mu2, s, n, p){
  df = data.generate(mu1, mu2, s, n, p)
  for (i in c(1:m))
  {
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    
    lda.fit = lda(y~.-y, data = data,subset = -test.index)
    lda.fit
    
    lda.pred = predict(lda.fit,df[test.index,])
    lda.pred$class
    
    #mytable = table(lda.pred$class,df[test.index,'y'])
    #print(mytable)
    
    accuracy.lda = (1-(mean(lda.pred$class==df[test.index,'y']))) * 100
    #print(accuracy.lda)
    sum.accuracy.lda = (sum.accuracy.lda + accuracy.lda)
    #print(sum.accuracy.lda)
  }
  print('LDA: Average level of Accuracy = ')
  print(sum.accuracy.lda / (N2*m))
}
}
df = data.generate(1,-1,3,25,2)
lda.function = function(df,N)
{
  N2 = N*2
  m=20
  sum.accuracy.lda = 0
  for (i in c(1:m))
  {
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    
    lda.fit = lda(y~.-y, data = data,subset = -test.index)
    lda.fit
    
    lda.pred = predict(lda.fit,df[test.index,])
    lda.pred$class
    
    #mytable = table(lda.pred$class,df[test.index,'y'])
    #print(mytable)
    
    accuracy.lda = (1-(mean(lda.pred$class==df[test.index,'y']))) * 100
    #print(accuracy.lda)
    sum.accuracy.lda = (sum.accuracy.lda + accuracy.lda)
    #print(sum.accuracy.lda)
  }
  print('LDA: Average level of Accuracy = ')
  print(sum.accuracy.lda / (N2*m))
}
lda.function(df,25)


###########################################################################


s = seq(0.1, 2, 0.1)
sims = data.frame(matrix(NA, nrow = length(s), ncol = 4, byrow = 1))
names(sims) = c('lg', 'knn', 'lda', 'qda')

for (i in c(1:length(s))){
  sims[i,] = sapply(big.func(1,-1,s[i],25,2), mean, na.rm = T)
}
plot(s,sims[,3], main = "LDA Accuracy vs Variance")


#############################################################################

s.values = seq(1, 2, 0.1)
#simsim = seq(1,2,0.01)
qda.averages = c()
qda.accuracy = c()
for(i in s.values){
  df = data.generate(1, -1, i, 50, 2)
  for (i in s.values){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    train.X = df[-test.index,]
    test.X = df[test.index,]
    test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
    train.Y = y[-test.index]
    qda.fit = qda(y~.-y, data = df, subset = -test.index)
    qda.pred = predict(qda.fit, df[test.index,])
    qda.means = mean(qda.pred$class == test.Y)
    qda.accuracy = c(qda.accuracy, qda.means)
    qda.average = mean(qda.accuracy)
  }
  qda.averages = c(qda.averages, qda.average)
}

qda.averages
qda.plot = plot(s.values, qda.averages, type = 'l', main='QDA', xlab='Variance', 
                ylab='Accuracy', col = "red")


s.values = seq(0.1, 2, 0.1)
lda.averages = c()
lda.accuracy = c()
for(i in s.values){
  df = data.generate(1, -1, i, 50, 2)
  for (i in s.values){
    test.index = sample(c(1:dim(df)[1]), size = floor(.3*dim(df)[1]), replace = FALSE)
    train.X = df[-test.index,]
    test.X = df[test.index,]
    test.Y = y[test.index]   # How to get Y data test/train? Oh yeah... attach df to script
    train.Y = y[-test.index]
    lda.fit = lda(y~.-y, data = df, subset = -test.index)
    lda.pred = predict(lda.fit, df[test.index,])
    lda.means = mean(lda.pred$class == test.Y)
    lda.accuracy = c(lda.accuracy, lda.means)
    lda.average = mean(lda.accuracy)
  }
  lda.averages = c(lda.averages, lda.average)
}

lda.averages
lda.plot = plot(s.values, lda.averages, type = 'l', main='LDA', xlab='Variance', 
                ylab='Accuracy', col = "blue")


